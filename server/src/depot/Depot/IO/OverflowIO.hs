module Depot.IO.OverflowIO
    where
      
import Depot.Base
import qualified Depot.Logging as LOG

import Depot.IO.Binary
import Depot.IO.BinaryImpl
import Depot.Record.DataStore

import Data.Word
import Data.List as List
import Data.Map  as Map

import qualified Control.Monad.State as ST
import Control.Monad
import System.IO
    
    
-- index data is stored in blocks which may be of two types: index
-- blocks or overflow blocks. index blocks consist of: 
--
-- -----------------------------------------------------------
-- | sanity byte | node header | spans | optional child data |
-- -----------------------------------------------------------
-- 
-- overflow blocks consist of
-- 
-- --------------
-- | child data |
-- --------------
--
-- The sanity byte is the integer 23, A node header consists of the
-- identity byte of the node (0x01 for a branch and 0x02 for a leaf)
-- the parent pointer of a node, the next node, and the previous
-- node. In the case of a Branch node the next and previous pointers
-- will be nil. Both leaf and branch nodes have a variable amount of
-- children consisting of keys and pointer values. This is considered
-- child data.  Spans are data structures that map a block based
-- address to the number of list items stored in the child data area
-- of a block.
--
-- A group of spans might might look like this: 
-- 
-- ---------------
-- | ptr    | n  |
-- ---------------
-- | (3 0)  | 3  |
-- ---------------
-- | (3 39) | 19 |
-- ---------------
-- | (4 0)  | 32 |
-- ---------------
--
-- meaning that ptr (3, 0) stores 3 entries ptr (3, 39) stores 19
-- entries, and ptr (4, 0) stores 32. In the case a pointer has an
-- offset value other than 0 that generally means that one list has
-- finished in the middle of a block and the next entry represents a
-- new list.
--

sanityByte         = 23 :: Word8
idxBlockSize       = 512 -- TODO make this configurable somewhere
idxFormatVersion   = 1  :: Word8
headSize           = sizeOf branchByte +
                     ((sizeOf BNilPtr) * 3)



-- the amount left in a block after an empty Span list and the sanity
-- are written
minIndexThreshold  = 
    let overflowSize = sizeOf ([]::[Span]) + sizeOf sanityByte + headSize
        threshold    = idxBlockSize - overflowSize 
    in
      if threshold > 0 
         then threshold
         else error "can't have threshold < 0"
                    


-- a block consists of a header area which contains spans and
-- possibly the header of a btree node. 
-- writeNode :: Handle -> BTreeIndex a -> BlockPtr -> FreeBlocks -> IO FreeBlocks
writeNode h node ptr fbs@(FB _ free) = do
  seekPtr h ptr
  sb    <- get h
  if sb == sanityByte then reclaimBlock else newBlock
   where
     reclaimBlock  = do
       loc          <- hTell h
       hSeek h RelativeSeek headSize
       oldSpans     <- get h 
       let freeptrs = ptrsFromSpans oldSpans
           oldfree  = freeFB fbs
       (spans,fbs') <- ST.runStateT (spansFor node ptr) fbs{freeFB=freeptrs ++ oldfree}
       hSeek h AbsoluteSeek loc
       putNodeData spans fbs'
     newBlock = do
       loc <- hTell h
       adjustLocation h loc
       put h sanityByte
       (spans,fbs') <- ST.runStateT (spansFor node ptr) fbs
       putNodeData spans fbs'
     putNodeData spans fbs = do
       when ((sizeOf spans + headSize) > idxBlockSize) (error ("HUGE! " ++ show (sizeOf spans)))
       putNodeHeader h node
       put h spans
       putWithSpans h spans node
       loc <- hTell h
       return fbs
     adjustLocation h loc = 
      if (loc `mod` idxBlockSize) == 0
         -- at end of file, double size
         then if loc == 0
                 then hSetFileSize h (idxBlockSize * 2)              
                 else hSetFileSize h (loc * 2)                          
         -- still have space, rewind to beginning of block
         else hSeek h RelativeSeek (-1) 
             
             
ptrsFromSpans sps = reverse $ foldl (\ps (Span ptr@(BPtr _ n) _) -> 
                                         case n of 
                                            0 -> ptr:ps
                                            _ -> ps) [] sps

putWithSpans h [] (Branch _ ks cs)  = put h ks >> put h cs
putWithSpans h [] (Leaf _ cs _ _)   = let (l1,l2) = unzip (Map.toList cs) in
                                      put h l1 >> put h l2
                             
putWithSpans h (s:pans) node = 
    case node of
      (Branch _ ks cs) -> putTwoLists ks cs
      (Leaf _ cs _ _)  -> let (ks,vs) = unzip (Map.toList cs) in 
                          putTwoLists ks vs
      where putTwoLists l1 l2 = do
                left1 <- writeSpan h s l1 True
                (s':pans') <- writeSpanningList h pans left1
                left2 <- writeSpan h s' l2 True
                writeSpanningList h pans' left2
                return ()
                                                  

writeSpan h (Span ptr n) xs withLength = do
  let (toWrite,left) = splitAt n xs
  seekPtr h ptr
  when withLength $ put h (length xs)
  mapM_ (put h) toWrite
  return left

writeSpanningList _ spans []              = return spans
writeSpanningList h (s:pans) xs = do
  left <- writeSpan h s xs False
  writeSpanningList h pans left

twoListsFromNode (Leaf _ cs _ _)  = unzip $ Map.toList cs
twoListsFromNode (Branch _ ks cs) = (ks, cs)


putNodeHeader h (Leaf parent _ prev next) = do 
  put h leafByte
  put h parent
  put h prev
  put h next
putNodeHeader h (Branch parent _ _) = do
  put h branchByte
  put h parent
  put h BNilPtr
  put h BNilPtr

readNode h ptr = do
  seekPtr h ptr
  sb <- get h
  when (sb /= sanityByte) $ error ("corrupt block, got insane value: " 
                                     ++ show sb ++ " at " ++ show ptr)
  lorb <- get h :: IO Word8
  case lorb of
    0x01 -> getBranch -- branch byte
    0x02 -> getLeaf   -- leaf byte
   where
     getLeaf = do
              parent  <- get h 
              prev    <- get h
              next    <- get h
              spans   <- get h
              (ks,vs) <- getChildren spans
              let kids = Map.fromList (zip ks vs)
              return (Leaf parent kids prev next)
     getBranch = do
              parent <- get h
              hSeek h RelativeSeek ((sizeOf BNilPtr) * 2)
              spans <- get h 
              (ks,vs) <- getChildren spans
              return (Branch parent ks vs)
     getChildren spans = case spans of 
                           [] -> do
                             ks <- get h
                             vs <- get h
                             return (ks,vs)
                           _  -> do
                             (ks,vs) <- getListsWithSpans h spans
                             return (ks,vs)
getListsWithSpans h spans = do
  (l1,rest) <- getListWithSpans h spans
  (l2,_)    <- getListWithSpans h rest
  return (l1,l2)
         
getListWithSpans h (Span ptr slen:pans) = do
  seekPtr h ptr
  len <- get h :: IO Int
  xs  <- readNItems h slen
  getListWithSpans' h xs pans (len - slen)
    where
      getListWithSpans' h acc pans 0                = return (acc, pans)
      getListWithSpans' h acc (Span ptr len:pans) n = do
                                           seekPtr h ptr
                                           xs <- readNItems h len
                                           getListWithSpans' h (acc ++ xs) pans (n - len)

readNItems h n = readNItems' h n []
    where readNItems' h 0 acc = return $ reverse acc
          readNItems' h n acc = do
            i <- get h 
            readNItems' h (n-1) (i:acc)


-- note that there's a theoretical problem with this algorithm in that
-- it is possible for there to be so many spans that a block
-- overflows.  this should be so infintessimally unlikely that we
-- won't deal with it yet but it should be fixed in a 1.1 release.
spansFor node ptr = 
    if (osize node) < minIndexThreshold
       then return [] -- no spanning
       else case node of
              (Branch _ ks cs) -> getSpans ks cs
              (Leaf _ cs _ _)  -> let (ks,vs) = unzip $ Map.toList cs in
                                  getSpans ks vs
    where 
      osize (Branch _ ks cs) = sizeOf ks + sizeOf cs
      osize (Leaf _ cs _ _)  = let (ks,vs) = unzip $ Map.toList cs in
                               sizeOf ks + sizeOf vs
      getSpans l1 l2 = do
             fbs <- ST.get
             -- immediately overflow (i.e. we're wasting the remaining space in this block)
             let (fbs',sps)   = fillSpans l1 l2 fbs 
             ST.put fbs'
             return sps
                       

fillSpans l1 l2 fbs = 
    let 
        (start,fbs1)          = nextFree fbs
        (rem1,fbs2,span1)     = spanList l1 start fbs1 intsize
        (rem2,fbs3,span2)     = spanList l2 rem1  fbs2 (intsize + fromIntegral (bpOffset rem1))
                               
        (_,fbs4)             = case rem2 of
                                (BPtr _ 0) -> (BNilPtr,fbs3)
                                _          -> nextFree fbs3
    in (fbs4,span1 ++ span2)
        where
          intsize                 = sizeOf (0::Int) -- List.length is an Int
          seedval sp              = case sp of Nothing -> []; Just s -> [s]
          spanList xs ptr fbs off = 
              let 
                  (rest, nxt, span) = fillSpan xs ptr off
                  seed              = seedval span
                  (ptr',fbs')       = nextIncr nxt fbs
              in
                fillSpans' rest ptr' fbs' seed
                  

nextIncr n fbs = case n of Nothing -> nextFree fbs; Just p -> (p, fbs)

fillSpans' [] ptr fbs acc = (ptr,fbs,reverse acc)
fillSpans' xs ptr fbs acc = 
    let
        (rest, nxt, span) = fillSpan xs ptr 0
        (ptr',fbs')       = nextIncr nxt fbs
        acc'              = case span of
                              Nothing -> acc
                              Just s  -> s:acc
    in
      fillSpans' rest ptr' fbs' acc'
               
-- FILL SPAN ALGORITHM
-- set the amount that is available in the current block                                  
-- get the remaining free bytes and the number of items that can be written in this block
-- subtract the number of written items from the list that still need to be written out
-- the new pointer offset is the block size minus the remaining free bytes
-- if there is a remainder, use the new offset otherwise increment the pointer to the next block
-- only accumulate a new span if there were items written out 
fillSpan xs ptr@(BPtr f _) offset = 
    let
        writable    = idxBlockSize - offset
        (rem, xlen) = foldl (incrementForListItem) (writable, 0) xs
        rest        = List.drop xlen xs
        newoffset   = fromInteger $ idxBlockSize - rem
        nxt         = if rem  > 0 then Just (BPtr f newoffset) else Nothing
        span        = if xlen > 0 then Just (Span ptr xlen)    else Nothing
    in
      (rest, nxt, span)

incrementForListItem (remaining, len) x = 
    let s = sizeOf x in
    if s < remaining 
       then (remaining - s, len + 1)
       else (0, len)
                                        

-- utilities ---------------------------------------------------------

seekPtr h (BPtr fid loc) = do
              let pos = (toInteger fid * idxBlockSize)                         
              hSeek h AbsoluteSeek pos 
              hSeek h RelativeSeek $ toInteger loc 
seekPtr h BNilPtr = error "error:seekPtr Illegal, can't seek to nil pointer"

nextFree (FB nxt [])     = (nxt, (FB (incrPtr nxt) []))
nextFree (FB nxt (x:xs)) = (x,   (FB nxt xs))
     
-- offsets and margins -----------------------------------------------
parentOffset :: Offset
parentOffset = fromInteger $ 
               sizeOf sanityByte +
               sizeOf nodeTypeByte
    where
      nodeTypeByte = leafByte
                     
prevPtrOffset = parentOffset + (fromInteger $ sizeOf BNilPtr)
nextPtrOffset = prevPtrOffset + (fromInteger $ sizeOf BNilPtr)

-- surgical updates (e.g. overwriting parent ptrs and nxt/prev) ------
overWriteParent h newparent targ  = do
  seekPtr h targ{bpOffset=parentOffset}
  put h newparent

updateNodeChildParents h ptr (Leaf _ _ _ _)    = return ()
updateNodeChildParents h ptr (Branch _ _ kids) = mapM_ (overWriteParent h ptr) kids

overwriteNextPrev h (Branch _ _ _) _ _ = return ()
overwriteNextPrev h (Leaf _ _ _ _) l r = do
  seekPtr h l{bpOffset=nextPtrOffset}
  put h r
  seekPtr h r{bpOffset=prevPtrOffset}
  put h l  
