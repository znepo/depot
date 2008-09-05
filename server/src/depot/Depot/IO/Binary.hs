{-# OPTIONS_GHC -fglasgow-exts -fallow-incoherent-instances -cpp#-}
module Depot.IO.Binary
    where
    
import System.IO
import Data.List
import Data.Char    
import Data.Bits
import Data.Word
import Data.Int
import GHC.Exts
import GHC.IOBase

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable hiding (sizeOf)
import Foreign.C.Types
import Foreign.C.String
import Data.PackedString
import Data.Int
import Data.Map as Map
import Prelude hiding (catch)
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM



import qualified Depot.Logging as LOG


-- Represents any data type that can be serialised to binary format.
class Binary a | a -> a where
    put     :: Handle -> a -> IO ()
    get     :: Handle -> IO a
    putAt   :: Handle -> Integer -> a -> IO ()
    getAt   :: Handle -> Integer -> IO a
    sizeOf  :: a -> Integer

    putAt h i a = do hSeek h AbsoluteSeek i
                     put h a
    getAt h i   = do hSeek h AbsoluteSeek i
                     got <- get h
                     return got
    
-- Implementation ----------------------------------------------------

-- These deal with Packed Strings since normal Strings are 
-- treated as generic lists
putString h s = do put h $ lengthPS s
                   hPutPS h s                             

getString h = do len <- get h :: IO Int
                 str <- hGetPS h len
                 return str


numSizeOf :: Integer -> Integer
numSizeOf (S# i#) = sizeOf (0::Word8) + sizeOf (I# i#)
numSizeOf (J# s# a#) = sizeOf (0::Word8) + (sizeOf (0::Int) * 2) +
                       (S# (sizeofByteArray# a#))



-- putNum and getNum (along with accompanying byte array operations)
-- taken mainly from NewBinary with only slight adjustments

putNum :: Handle -> Integer -> IO ()
putNum h (S# i#)    = put h (0::Word8) >> put h (I# i#)
putNum h (J# s# a#) = do 
  put h (1::Word8)
  put h (I# s#)
  let sz# = sizeofByteArray# a# -- actual size in bytes
  put h (I# sz#)
  let  loop n#
           | n# ==# sz# = return()
           | otherwise = do
                        let val = ((indexByteArray a# n#)::Word8)
                        put h val
                        loop (n# +# 1#)
  loop 0#
  
indexByteArray a# n# = fromIntegral (I# (ord# (indexCharArray# a# n#))) 
       
getNum :: Handle -> IO Integer
getNum h = do
  c <- get h :: IO Word8
  if c == 0
     then do
          (I# i#) <- get h 
          return (S# i#)
     else do
          (I# s#)   <- get h 
          (I# sz#)  <- get h
          (MBA arr) <- newByteArray sz#
          let loop n
                   | n ==# sz# = return ()
                   | otherwise = do 
                w <- get h
                writeByteArray arr n w
                loop (n +# 1#)
          loop 0#
          (BA a#) <- freezeByteArray arr
          return (J# s# a#)
                
               
           
data ByteArray = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newByteArray# sz s of { (# s, arr #) ->
  (# s, MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s, arr #) ->
  (# s, BA arr #) }

writeByteArray :: MutableByteArray# RealWorld -> Int# -> Word8 -> IO ()

writeByteArray arr i w8 = IO $ \s ->
  case fromIntegral w8 of { W# w# -> 
  case writeCharArray# arr i (chr# (word2Int# w#)) s  of { s ->
  (# s , () #) }}         

-- END OF STUFF FROM NewBinary


-- Fixed numeric types -----------------------------------------------

putBigNum h i = allocaBytes 8 $ putFixedNum h i 8
getBigNum h   = allocaBytes 8 $ getFixedNum h 8


putInt h i = allocaBytes 4 $ putFixedNum h i 4
getInt h   = allocaBytes 4 $ getFixedNum h 4

putCChar h c = allocaBytes 1 $ putFixedNum h c 1
getCChar h   = allocaBytes 1 $ getFixedNum h 1

putWord8 = putCChar
getWord8 = getCChar


{-# INLINE putFixedNum #-}
putFixedNum :: (Num a, Storable a) => Handle -> a -> Int -> Ptr a -> IO ()
putFixedNum h i size buff = do 
  poke buff i
  hPutBuf h buff size



{-# INLINE getFixedNum #-}
getFixedNum :: (Num a, Storable a) => Handle -> Int -> Ptr a -> IO a
getFixedNum h size buff = do
  pos <- hGetBuf h buff size
  i   <- peek buff 
  return i



------- Utilities -----------------------------------------------------

rawHandleCopy :: Integer -> Handle -> Handle -> IO ()
rawHandleCopy len from to =  rhc hGetBuf hPutBuf len from to

rawHandleCopyNB :: Integer -> Handle -> Handle -> IO ()
rawHandleCopyNB len from to = rhc hGetBufNonBlocking hPutBufNonBlocking len from to

rhc getbuf putbuf len from to = do
  flush 
  buff <- mallocBytes 8192
  (catch ((rhc' len buff) `finally` free buff)
         (\e -> do {LOG.errorIO $ "Caught in raw copy " ++ show e; throw e}))
  flush 
    where
      rhc' n _ | n <= 0  = return ()
      rhc' n buff        = do
          let toCopy = if n > 8192 then 8192 else n
              n'     = n - toCopy               
          readbytes <- getbuf from buff (fromInteger toCopy)
          putbuf to buff readbytes
          rhc' n' buff 
      flush = do 
        catch (hFlush to) (\e -> LOG.errorIO "error flushing to handle")
        catch (hFlush from) (\e -> LOG.errorIO "error flushing from handle")

-- Performs a non blocking io copy from one handle to another
rawHandleCopyWithTimeout :: Integer -> Handle -> Handle -> IO ()
rawHandleCopyWithTimeout len from to = doCopy 0 len from to 
    where
      doCopy _ 0 _ _                 = return ()
      doCopy i _ _ _ | i >= maxwaits = return ()
      doCopy i len from to           = do        
        LOG.debugIO $! "Raw copying " ++ show len ++ " bytes"
        loc1 <- hTell to        
#if mingw32_HOST_OS
        rawHandleCopyNBWin32 len from to
#else
        rawHandleCopyNB len from to
#endif  
        loc2 <- hTell to
        let copied = loc2 - loc1
        if copied == len 
           then return ()
           else if copied == 0 
                then waitAndTryAgain (i+1) len
                else waitAndTryAgain i (len - copied)
      waitAndTryAgain i l = do
        LOG.debugIO $ "Copy blocked, trying again in 50ms, has blocked " ++ show i ++ " times"
        threadDelay 250
        doCopy i l from to
      
maxwaits = 20

#if mingw32_HOST_OS
-- performs non blocking io on a Windows platform (a hack until GHC
-- fixes hGetBufNonBlocking on Windows). This contains no Windows
-- specific code but seems to perform a lot slower than
-- rawHandleCopyNB
rawHandleCopyNBWin32 len from to = do
  loc1 <- hTell to
  thrd <- forkIO $ rawHandleCopy len from to
  pollRead loc1 thrd to
      where
        pollRead loc1 thrd to = do
          threadDelay 200
          loc2 <- hTell to        
          if loc2 == loc1 
             then killThread thrd
             else if (loc2 - loc1) == len
                     then return ()
                     else pollRead loc1 thrd to
#endif
                
hGetLineNB h timeout =  do
  buff <- atomically newEmptyTMVar 
  tout <- atomically newEmptyTMVar 
  rthr <- forkIO $ hgl h buff
  tthr <- forkIO $ countDown tout rthr
  line <- atomically (waitForData buff tout) `catch` 
          (\e -> error toutmsg)              `finally` 
          mapM_ (killThread) [rthr,tthr] 
  return line
    where
      toutmsg               = "Timed out reading line in " ++ show timeout ++ "ms"
      hgl h buff            = hGetLine h >>= atomically . putTMVar buff
      countDown tout rthr   = threadDelay (timeout * 1000) >>
                              atomically (putTMVar tout True)
      getData buff          = takeTMVar buff >>= return 
      checkTimeout tout     = takeTMVar tout >>  error "Time out"
      waitForData buff tout = getData buff `orElse` checkTimeout tout

-- Standard data type implementations --------------------------------

instance Binary Bool where
    put h True  = put h (1::Word8)
    put h False = put h (0::Word8)
    get h       = do i <- get h::IO Word8
                     case i of
                       1 -> return True
                       0 -> return False
    sizeOf _    = sizeOf (0::Word8)

instance Binary Integer where
    put         = putNum
    get         = getNum
    sizeOf a    = numSizeOf a 


instance Binary PackedString where
    put         = putString
    get         = getString
    sizeOf a    = toInteger (lengthPS a) + (sizeOf $ lengthPS a)

instance Binary Int64 where
    put         = putBigNum
    get         = getBigNum
    sizeOf a    = 8
                           
instance Binary Int where
    put = putInt
    get = getInt
    sizeOf a = 4

instance Binary Int32 where
    put = putInt
    get = getInt
    sizeOf a = 4


instance Binary Word32 where
    put = putInt 
    get = getInt 
    sizeOf a = 4

instance Binary Word64 where
    put = putBigNum 
    get = getBigNum
    sizeOf a = 8

instance Binary Char where
    put = hPutChar
    get = hGetChar
    sizeOf a = 1

instance Binary Word8 where
    put h w = putWord8 h w
    get = getWord8
    sizeOf a = 1

instance Binary CChar where
    put = putCChar
    get = getCChar
    sizeOf _ = 1
                   

instance (Binary a) => Binary [a] where
    put h xs = put h (length xs) >> mapM_ (put h) xs
    get h    = do len <- get h :: IO Int
                  getAmount h len []
        where
          getAmount _ 0 acc = return $ reverse acc
          getAmount h n xs  = do
            x <- get h 
            getAmount h (n-1) (x:xs)
    sizeOf xs = foldl (\acc a -> sizeOf a + acc) (sizeOf (length xs)) xs


instance (Ord a, Binary a, Binary b) => Binary (Map a b) where
    put h m = let (ks,vs) = unzip $ Map.toList m in do
                put h ks
                put h vs
    get h   = do
      ks <- get h
      vs <- get h
      return $ Map.fromList (zip ks vs)
    sizeOf m = let keysize = sizeOf (Map.size m) in
               Map.foldWithKey (\k v acc -> sizeOf k + sizeOf v + acc) (keysize*2) m


-- TODO tuple implementations