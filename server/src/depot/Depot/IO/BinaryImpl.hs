module Depot.IO.BinaryImpl
    where
      
import Depot.Base      
import Depot.IO.Binary
import Data.Int
import Data.Word
import Data.Bits
import Data.List as List
import Data.Set as Set
import Data.Map as Map
import System.IO

instance Binary Span where
    put h (Span s es) = put h s >> put h es
    get h = do
      s <- get h
      es <- get h
      return (Span s es)
    sizeOf (Span s es) = sizeOf s + sizeOf es
                           
instance Binary FreeBlocks where
    put h (FB nxt free) = put h nxt >> put h free
    get h = do
      nxt  <- get h
      free <- get h
      return (FB nxt free)
    sizeOf (FB nxt free) = sizeOf nxt + sizeOf free

-- Binary serialization implementation for BPtrs -----------------------------

instance Binary BlockPtr where
    put                   = putBPtr
    get                   = getBPtr 
    sizeOf BNilPtr        = sizeOf (-1::Int32) + sizeOf (-1::Int32)
    sizeOf (BPtr fid loc) = sizeOf fid + sizeOf loc


putBPtr h (BPtr fid addr) = do put h fid
                               put h addr
--                               LOG.infoIO ("putting " ++ show (BPtr fid addr))
putBPtr h (BNilPtr)       = do put h (-1 :: Int32)
                               put h (-1 :: Int32)
--                               LOG.infoIO ("putting " ++ show BNilPtr)

getBPtr h                 = do ptr <- get h :: IO Word32
                               loc <- get h :: IO Word32
                               -- minus 1 is equal to the max word32 value
                               -- (at least on the mac ;-)
                               if ptr /= 4294967295 || loc /= 4294967295
                                  then return (BPtr ptr loc)
                                  else return BNilPtr

-- BTree Bin Serialisation -------------------------------------------

instance Binary BPageHeader where
    put h (BP s v f r) = put h s >> put h v >> put h f >> put h r
    get h              = do
      s <- get h
      v <- get h
      f <- get h
      r <- get h
      return (BP s v f r)
    sizeOf (BP s v f r) = sizeOf s + sizeOf v + sizeOf f + sizeOf r
                          

instance Binary BTreeEntry where
    put h entry = do 
      put h $ versionBE entry
      put h $ lastOidBE entry
      put h $ (pathBE entry) /// "dat" /// (lastdatfile entry) ++ ".dat" -- dat file location
      put h $ lastWriteBE entry
      put h $ numEntriesBE entry
      put h $ metaDataBE entry
    -- not to be used directly
    get h = error "not to be read directly, use readDirectory"      
    sizeOf entry = sizeOf (versionBE entry) + 
                   sizeOf (lastOidBE entry) +
                   sizeOf (lastdatfile entry) + 
                   sizeOf (numEntriesBE entry) + 
                   sizeOf (metaDataBE entry) + 
                   sizeOf (lastWriteBE entry) 

lastdatfile entry = show (case lastWriteBE entry of 
                            BNilPtr       -> 0
                            (BPtr fid _)  -> fid)


branchByte         = 0x01 :: Word8
leafByte           = 0x02 :: Word8
-- Even though BTreeIndex derives from Binary general production
-- writing and reading of BTree nodes should not be done using put h
-- Btree and get h :: BTreeIndex
instance (Binary b) => Binary (BTreeIndex b) where
    put h (Branch parent keys kids) = do
      put h branchByte
      put h parent
      put h BNilPtr
      put h BNilPtr
      put h keys
      put h kids
    put h (Leaf parent vals prev next) = do
      put h leafByte
      put h parent
      put h prev
      put h next
      put h vals
    get h = do        
        b <- get h :: IO Word8
        case b of
          0x01 -> do
                 parent <- get h
                 hSeek h RelativeSeek ((sizeOf BNilPtr) * 2) -- skip empties
                 keys   <- get h
                 kids   <- get h
                 return (Branch parent keys kids)
          0x02 -> do
                 parent <- get h
                 prev   <- get h
                 next   <- get h
                 vals   <- get h
                 return (Leaf parent vals prev next)
          _ -> error $ "Expecting branch or leaf byte during index deserialization but got : " ++ (show b)
    sizeOf (Branch parent keys kids)    = sizeOf parent + sizeOf branchByte +
                                          sizeOf keys + sizeOf kids
    sizeOf (Leaf parent vals prev next) = sizeOf parent + sizeOf vals + 
                                          sizeOf prev  + sizeOf next +
                                          sizeOf leafByte
        


-- permissions -------------------------------------------------------

-- NOTE: binary instances for Permission, UserAuth, and Principal are
-- located in Depot.Sec.Auth because they are too interwoven into
-- the evaluation functions in that module.


