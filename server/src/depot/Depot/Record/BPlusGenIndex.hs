module Depot.Record.BPlusGenIndex where
    
import Depot.Base
import Depot.Error
import qualified Depot.Logging as LOG
import Depot.IO.Binary
import Depot.IO.BinaryImpl
import Depot.IO.BPlusIO    
import Depot.IO.OverflowIO    
import Depot.Record.DataStore

import System.IO
import Data.Map as Map
import Data.List as List
import Control.Monad (unless)
import Control.Monad.Trans
import Control.Monad.Error
import Control.Exception
import qualified Control.Monad.State as ST

import Prelude hiding (catch)

import Debug.Trace

insertBT :: (MonadIO m, Binary a, Show a) => Integer -> a -> RHOHS a -> m (Insertion a,RHOHS a)
insertBT k v = ST.runStateT $ do
  (root,handle,order,header,_) <- ST.get
  let rootptr = (rootPtrBPH header)
      res     = bInsert root order k v
  insertBT' rootptr res
    where
      insertBT' ptr (Inserted node)  = finish ptr node (Inserted)
      insertBT' ptr (Updated node)   = finish ptr node (Updated)
      insertBT' _   (InsNext nxtptr) = do
                  (_,h,order,_,_) <- ST.get
                  nxt             <- ST.liftIO $ readNode h nxtptr
                  insertBT' nxtptr (bInsert nxt order k v)

      -- handles a node split, writing the left and right node to disk
      -- and passing their median key up to the parent.
      insertBT' leftptr (InSplit (SplitNode l r medKey)) = do
                  rightptr    <- writeLeftAndRight leftptr l r
                  (_,h,o,_,_) <- ST.get
                  let parptr = parentBT l
                  parent      <- ST.liftIO $ readNode h parptr
                  insertBT' parptr (bInsertKeyFromSplit parent o medKey rightptr leftptr)
                            
      -- handles a root split, writing the left and right nodes to
      -- disk and taking a new pointer from the free stack for the
      -- root node
      insertBT' leftptr (InSplit (SplitRoot l r root)) = do
                  rightptr <- writeLeftAndRight leftptr l r
                  (_,h,o,header,scm) <- ST.get
                  let (rootptr,fbs) = nextFree (freeBlocksBPH header)
                      root'         = root{kidsBT=[leftptr,rightptr]}
                  updateRoot rootptr root' fbs
                  ST.liftIO $ updateNodeChildParents h rootptr root'
                  -- this is of course ALWAYS an insertion (can't have
                  -- a split on an update)
                  insertBT' rootptr (Inserted root')
                            
      -- writes a left and right pair of nodes to disk starting with a
      -- left pointer and taking the right from the free stack
      writeLeftAndRight leftptr left right = do
                   (_,h,o,header,_)   <- ST.get
                   result             <- ST.liftIO $ writeNode h left leftptr (freeBlocksBPH header)
                   let (rightptr,fbs) = nextFree result
                   result'            <- ST.liftIO $ writeNode h right rightptr fbs
                   updateNextPtr result'
                   ST.liftIO $ overwriteNextPrev h left leftptr rightptr
                   ST.liftIO $ updateNodeChildParents h rightptr right
                   return rightptr       
                          
      -- writes the final updated or created node to disk
      finish ptr node insertion = do
        (_,h,_,header,_) <- ST.get
        result           <- ST.liftIO $ writeNode h node ptr (freeBlocksBPH header)
        updateNextPtr result
        updateRootIfNecessary ptr node result
        -- ensure the handle is always fully flushed after each write
        ST.liftIO $ hFlush h
        return $ insertion node

updateRootIfNecessary ptr n@(Leaf BNilPtr _ _ _) fbs = updateRoot ptr n fbs
updateRootIfNecessary ptr n@(Branch BNilPtr _ _) fbs = updateRoot ptr n fbs
updateRootIfNecessary _ _ _ = return ()
updateRoot ptr n fbs = do
  (_,h,o,hdr,scm) <- ST.get
  -- refactor this into BPlusIO
  let hdr' = hdr{rootPtrBPH=ptr,freeBlocksBPH=fbs}
  ST.liftIO $ seekPtr h (BPtr 0 0)
  ST.liftIO $ put h hdr'
  ST.put (n,h,o,hdr',scm)
     


updateNextPtr fbs = do
  (root,h,ord,header,scm) <- ST.get
  ST.put (root,h,ord,header{freeBlocksBPH=fbs},scm)




type DepotErroT  = Control.Monad.Error.ErrorT DepotError IO DepotStatus
type SelectFun a = Integer -> Selection a -> IO DepotErroT

-- Select functions --------------------------------------------------

selectBT :: (Binary b) =>
 	    (Selection b -> IO t) 
                -> Integer -> (BTreeIndex b, Handle, c, d, e) -> IO t
selectBT f key (root,handle,_,_,_) = searchByKey f handle root key
  

searchByKey f handle node key = 
  case bSelect node key of
    LookNext ptr -> do
                 node' <- readNode handle ptr
                 searchByKey f handle node' key
    result       -> f result


-- Does a "SELECT ALL" on an index returning all of its contents.
-- Takes a passed function to handle the result set, the result
-- function is applied to the results along with incrementing the
-- number of result cycles in the passed function
selectAllBT :: (Binary a) =>
 	       (Bool -> Selection a -> IO b) -> RHOHS a -> IO b
selectAllBT  f (root,handle,order,header,_) = selectAllBT' f True handle root
selectAllBT' f isfirst handle node =  
    case bSelectAll node of
      LookNext nxt -> do
                        node' <- readNode handle nxt
                        selectAllBT' f isfirst handle node'                     
      inc@(Incomplete ptrs nxt) -> do
                        f isfirst inc                   
                        node' <- readNode handle nxt
                        selectAllBT' f False handle node'
      -- on a select all there is no "Not Found"n
      SelectionNotFound -> f isfirst EmptySelection               
      finished          -> f isfirst finished


selectExpBT datf idxf rhoh (Scalar a)          = selectBT (datf True) (idxf a) rhoh
selectExpBT datf idxf rhoh (CommaDelimited xs) = selectCommaDelimited datf idxf xs rhoh
selectExpBT datf idxf rhoh QueryMin            = selectMinBT (datf True) rhoh
selectExpBT datf idxf rhoh QueryMax            = selectMaxBT (datf True) rhoh
selectExpBT datf idxf rhoh r                   = selectRangeBT datf idxf r rhoh


selectCommaDelimited datf idxf (x:xs) rhoh = do
  res <- selectCommaDelimited'' True datf idxf x rhoh
  case xs of
    [] -> return res
    _  -> selectCommaDelimited' datf idxf xs rhoh

selectCommaDelimited' datf idxf [x] rhoh = selectCommaDelimited'' False datf idxf x rhoh
selectCommaDelimited' datf idxf (x:xs) rhoh  = do
  selectCommaDelimited'' False datf idxf x rhoh
  selectCommaDelimited' datf idxf xs rhoh
  

selectCommaDelimited'' isfirst datf idxf x rhoh@(root,handle,_,_,_) = 
    case x of
      Scalar a -> selectBT (datf isfirst) (idxf a) rhoh                   
      QueryMin -> selectMinBT (datf isfirst) rhoh
      QueryMax -> selectMaxBT (datf isfirst) rhoh
      range    -> do
          let (from,to) = makeFromTo idxf range 
          selectRangeBT' datf isfirst from to handle root
                                
          
                  


selectRangeBT datf idxf range (root,handle,_,_,_) = 
    let (from,to) =  makeFromTo idxf range
    in selectRangeBT' datf True from to handle root

makeFromTo idxf (RangeTo a) = 
    let from = mkRangeKey (idxf a)
        to   = PositiveInfinity
    in (from,to)
makeFromTo idxf (RangeFrom a) = 
    let from = NegativeInfinity
        to   = mkRangeKey (idxf a)
    in (from,to)
makeFromTo idxf (RangeInclusive a b) = 
    let from = mkRangeKey (idxf a)
        to   = mkRangeKey (idxf b)
    in (from,to)



selectRangeBT' datf isfirst from to handle node = 
    case bSelectRange node from to of
      LookNext nxt -> do
        node' <- readNode handle nxt
        selectRangeBT' datf isfirst from to handle node'
      inc@(Incomplete ptrs nxt) -> do
        datf isfirst inc
        node' <- readNode handle nxt
        selectRangeBT' datf False from to handle node'
      SelectionNotFound -> datf isfirst EmptySelection
      finished          -> datf isfirst finished

mkRangeKey i = R i


selectMinBT :: (Binary b) => (Selection b -> IO t) -> (RHOHS b) -> IO t
selectMinBT f (root,handle,_,_,_) = selectSingle bSelectMin f handle root

selectMaxBT :: (Binary b) => (Selection b -> IO t) -> (RHOHS b) -> IO t
selectMaxBT f (root,handle,_,_,_) = selectSingle bSelectMax f handle root

-- TODO, refactor search functions that take a key to take it as their
-- first argument so that they can be curried with selectSingle

selectSingle selFun datFun handle node = 
    case selFun node of
      LookNext ptr -> do
        node' <- readNode handle ptr
        selectSingle selFun datFun handle node' 
      result       -> datFun result



-- Query functions ---------------------------------------------------
-- All query functions only traverse over a single node. The return
-- types of the functions give the new node (or nodes in the case of a
-- split). Since nearly all of these functions handle tree traversal
-- it is common to return a type LookNext, Incomplete, or InsNext
-- accompanied by a pointer to the node that should be read into
-- memory next. This allows clean seperation between the IO code for
-- accessing the actual node data on disk and the traversal algorithm
-- itself.  
--


-- selection, does a one-item select from a btree, either returning a
-- result or a pointer to the next node that should be searched
bSelect :: BTreeIndex a -> Integer -> Selection a
bSelect (Leaf _ vs _ _) key = case Map.lookup key vs of
                                Nothing -> SelectionNotFound
                                Just x  -> Found [x]

bSelect (Branch _ keys kids) key = look keys kids
    where
      look [] [ptr]       = LookNext ptr
      look (k:ks) (c:cs)  = case compare key k of
                              LT -> LookNext c
                              _  -> look ks cs

-- selects all elements from a tree. Either returns the next node to
-- look at, node elements and a pointer to the next node, or a found
-- result with the elements of that node
bSelectAll :: BTreeIndex a -> Selection a
bSelectAll (Branch _ keys (k:kids)) = LookNext k -- run down the right side of the tree
bSelectAll (Leaf _ vs _ BNilPtr)    = Found (Map.elems vs)
bSelectAll (Leaf _ vs _ next)       = Incomplete (Map.elems vs) next

instance Ord RangeKey where
    compare NegativeInfinity _     = LT
    compare PositiveInfinity _     = GT
    compare (R _) NegativeInfinity = GT
    compare (R _) PositiveInfinity = LT
    compare (R i) (R i')           = compare i i'

-- finds a range, inclusive of the "to" value 
bSelectRange :: BTreeIndex a -> RangeKey -> RangeKey -> Selection a
bSelectRange b@(Branch _ keys kids) from _ = look keys kids
    where
      look [] [ptr]      = LookNext ptr
      look (k:ks) (c:cs) = case compare from (mkRangeKey k) of
                             LT -> LookNext c
                             _  -> look ks cs
                                   
bSelectRange (Leaf _ vs _ nxt) from to     = 
    let 
        (last,ptrs) = foldWithKey (accumRange from to) (NegativeInfinity,[]) vs
    in case compare to last of
         LT -> SelectionNotFound
         EQ -> Found ptrs
         GT -> case nxt of 
                 BNilPtr -> Found ptrs
                 _       -> Incomplete ptrs nxt
             
accumRange from to key x (last,xs) = 
    let k = mkRangeKey key 
    in if from <= k && to >= k
          then (k,x:xs)
          else (last,xs)

-- grabs the "first" value in a btree, running down the left side of
-- the tree
bSelectMin (Branch _ _ (k:_)) = LookNext k
bSelectMin (Leaf _ vs _ _)    = let (_,ptr) = Map.findMin vs in 
                                if Map.size vs > 0 then Found [ptr] else EmptySelection
                                              
-- grabs the "last" value in a btree, running down the right side of
-- the tree
bSelectMax (Branch _ _ kids)  = let (k:_)   = reverse kids in LookNext k
bSelectMax (Leaf _ vs _ _)    = let (_,ptr) = Map.findMax vs in
                                if Map.size vs > 0 then Found [ptr] else EmptySelection
                                               

-- Insertion Functions -----------------------------------------------

-- insertion, adds a value to a btree, returns one of several states,
-- either success, a pointer to a child node, or success and a split
-- node.
bInsert :: BTreeIndex a -> Order -> Integer -> a -> Insertion a
bInsert n@(Leaf _ vs _ _) order key ptr = 
    let n'    = n{ valsBT = Map.insert key ptr vs }
        size  = Map.size (valsBT n')
        oldsz = Map.size (valsBT n )
        inserted = if size > oldsz then Inserted n' else Updated n'
    in
      if size > order then InSplit (bSplit n' order) else inserted

bInsert n@(Branch _ keys kids) _ key _ = ins keys kids
    where
      ins [] [ptr]      = InsNext ptr
      ins (k:ks) (c:cs) = case compare key k of
                            LT -> InsNext c
                            _  -> ins ks cs
    

-- Splitting functions.  All nodes are created _sans_ new
-- pointers. They can be determined after the nodes are written to
-- disk by the calling function 
-- 
-- I don't particularly like returning incomplete nodes but them's the
-- breaks. A small price to pay for my extreme sexiness.
bSplit :: BTreeIndex a -> Int -> Split a 
bSplit (Leaf BNilPtr vs _ _) order = 
    let
        (left, right, mid) = splitLeafVals vs order
        nodeLeft           = (Leaf BNilPtr left BNilPtr BNilPtr)
        nodeRight          = (Leaf BNilPtr right BNilPtr BNilPtr)
        nodeRoot           = (Branch BNilPtr [mid] [])
    in
      SplitRoot nodeLeft nodeRight nodeRoot 
                
bSplit (Leaf parent vs prv nxt) order = 
    let 
        (left, right, mid) = splitLeafVals vs order
        nodeLeft           = (Leaf parent left prv BNilPtr)
        nodeRight          = (Leaf parent right BNilPtr nxt)
    in
      SplitNode nodeLeft nodeRight mid

bSplit (Branch parent keys kids) order  =
    let 
        degree            = order `div` 2
        (lkey, mkey:rkey) = splitAt degree keys
        (lkids,rkids)     = splitAt (degree + 1) kids
        nodeLeft          = (Branch parent lkey lkids)
        nodeRight         = (Branch parent rkey rkids)
    in
      case parent of 
        BNilPtr -> SplitRoot nodeLeft nodeRight (Branch BNilPtr [mkey] [])
        _       -> SplitNode nodeLeft nodeRight mkey


bInsertKeyFromSplit ::
    BTreeIndex a -> Order -> Integer -> BlockPtr -> BlockPtr -> Insertion a 
bInsertKeyFromSplit (Branch parent keys kids) order key right left =
    let
        keys'    = List.insert key keys
        -- ok to assert because we just inserted this idx
        Just idx = List.elemIndex key keys'
        (ka,_:kb)  = List.splitAt (idx) kids
        kids'    = ka ++ [left,right] ++ kb
        size     = List.length keys'
        node     = (Branch parent keys' kids')
    in
      if size > order 
         then InSplit (bSplit node order) 
         else Inserted node
      

splitLeafVals vs order = let degree            = order `div` 2
                             (l,r@((mid,_):_)) = splitAt degree (Map.toList vs)
                             left              = Map.fromList l
                             right             = Map.fromList r
                         in (left, right, mid)
        


--- DEBUG ------------------------------------------------------------
               
traceChildren (Leaf _ v _ _) = LOG.infoIO $ "num kids: " ++ (show (Map.size v))
traceChildren (Branch _ k v) = LOG.infoIO $ "num kids: " ++ (show (List.length v))

oidTable = "/Users/weeksie/workspace/flash-save/dist/build/src/scratch/a/test/idx/X-Object-Id/idx.bptree"

readNodeAndPrint fid = do
  h <- openBinaryFile oidTable  ReadMode
  n <- readNode h (BPtr fid 0) :: IO IdxInt
  hClose h
  LOG.infoIO $ "parent: " ++ (show (parentBT n))
  traceChildren n  
  printAllChildren n

printAllChildren :: IdxInt -> IO ()
printAllChildren (Leaf _ kids _ _) = do
    putStrLn $ showChildren (\k -> show  k) (Map.keys kids)
printAllChildren (Branch _ keys vals) = do
    putStrLn $ showChildren (\k -> show k) keys
    --putStrLn $ showChildren  (\v -> show (bpFile v)) vals

printAllChildPtrs (Leaf _ kids _ _) = do
    putStrLn $ showChildren (\k -> show (bpFile k)) (Map.elems kids)
printAllChildPtrs (Branch _ _ vals) = do
    putStrLn $ showChildren  (\v -> show (bpFile v)) vals


showChildren f ptrs = 
    let kids = foldl (\a b -> a ++ " " ++ f b) "[" ptrs  in
    kids ++ " ]"

printTreeFrom fid = do
   h    <- openBinaryFile oidTable ReadMode
   traceNode "" h (BPtr fid 0)
   hClose h

traceNumChildren (Leaf _ v _ _) = putStr $ "VALS: " ++ (show (Map.size v)) ++ " "
traceNumChildren (Branch _ k v) = putStr $ "num kids: " ++ (show (List.length v)) ++ " "
traceNodeType (Leaf _ _ _ _)    = putStr "leaf "
traceNodeType (Branch _ _ _)    = putStr "branch "
traceNode prefix h ptr = do
  putStr prefix
  node <- readNode h ptr :: IO IdxInt   
  putStr "node: "
  traceNodeType node
  traceParent node
  traceNumChildren node
  printAllChildren node
  traceNode' (prefix ++ "-") h node
traceNode' prefix h (Branch _ _ v) = do
  mapM_ (traceNode prefix h) v
traceNode' _ _ (Leaf _ _ _ _) = return ()
traceParent node = 
          case (parentBT node) of
            BNilPtr    -> putStr " <root> "
            (BPtr f _) ->  putStr $ " <" ++  (show f) ++ "> "
   