module FlashSave.Record.BPlusTreeTest
    where
      
import FlashSave.IO.Binary
import FlashSave.Record.BPlusIndex
import FlashSave.Record.BPlusQuery
import System.IO
import System.Directory
import Debug.Trace    
import Prelude 
import System.Random
import Data.List 

randList :: (Random Integer) => [Integer]
randList = randomRs (1,500) $ mkStdGen 1

start = do testbtInsert "test/id" 90
           testSearch "test/id" 90
           -- cleanUp `catch` (\err -> putStrLn (show err))


startR = do putTraceMsg ("BTINSERTING: " ++ (show (take 200 randList)))
            testInsList $ take 200 randList
            testGetList $ take 200 randList
                        

testInsList []     = return ()
testInsList (x:xs) = do btInsert ((KI x):: Key Integer) (show x) "test/id"
                        testInsList xs
                                    

testGetList []     = do putStrLn "FUCKING ROCK STAR WEEKSIE! YOU ARE AWESOME"
testGetList (x:xs) = do ptr <- btLookup ((KI x)::Key Integer) "test/id"
                        case ptr of
                         Nothing -> do putStrLn "didn't match:"
                                       putStrLn ("Key " ++ (show x))
                         Just _  -> do testGetList xs


testInsertStrs    = do testInsertStr "abc" "xyz"
                       testInsertStr "def" "ghi"
                       return ()

testInsertStr k v = do btInsert ((KS k):: Key String) v "test-str/id"
                       

testSearchStr k   = do  ptr <- btLookup ((KS k)::Key String) "test-str/id"
                        case ptr of
                          Nothing -> do putStrLn "not found!"
                          Just p  -> do putStrLn ("found " ++ (show p))



testbtInsert _   0 = return ()
testbtInsert tid n = do btInsert ((KI n)::Key Integer) (show n) tid
                        testbtInsert tid (n-3)


testSearch tid 0 = do putStrLn "Success!"
testSearch tid n = do ptr <- btLookup ((KI n)::Key Integer) tid
                      case ptr of
                        Nothing -> do putStrLn "didn't match:"
                                      putStrLn ("Key " ++ (show n))
                        Just _  -> do -- putStrLn "match..."
                                      testSearch tid (n-3)


clean   = do removeDirectoryRecursive "test"

 
testBtLookup k = do res <- btLookup ((KI k)::Key Integer) "test/id"
                    putStr (show res)
                         

showTestTree = do (_,node) <- readRoot "test/id" :: (Binary Integer, Ord Integer) => IO ((BlockPtr, BNode String))
                  putStr (show node)
                         

testBtLookup200 = do testBtLookupUntil 50 0 0
                     return ()
    where
    testBtLookupUntil 0 c i = do putStrLn ("Correct: " ++ (show c))
                                 putStrLn ("Incorrect: " ++ (show i))
                                 return ()
    testBtLookupUntil n c i = do ptr <- btLookup ((KI n)::Key Integer) "test/id"
                                 (case ptr of
                                  Nothing -> do testBtLookupUntil (n-1) c (i+1)
                                  Just _  -> do testBtLookupUntil (n-1) (c+1) i)
                                          


putStrTree tid = do (_,root) <- readRoot tid :: (Binary Integer, Ord Integer) => IO (BlockPtr, BNode String)
                    putBNode root ""


testUpdateParent = case updateParent dummyParent (KI 1) (BPtr 0 5) (BPtr 0 6) of
                   (Branch _ 4 _ [KI 1, KI 2, KI 4, KI 6, 
                                     KI 8, KI 10, KI 12, KI 14]
                    [BPtr 0 2, BPtr 0 5, BPtr 0 4,  
                          BPtr 0 6, BPtr 0 8,
                              BPtr 0 10, BPtr 0 12, 
                                   BPtr 0 14, BPtr 0 16]) -> True
                   (Branch _ _ _ ks cs)                   -> trace (show ks)
                                                             trace (show cs)
                                                             False

testUpdateParent' =
                case updateParent dummyParent (KI 7) (BPtr 0 5)  (BPtr 0 6) of
                (Branch _ 4 _ [KI 2, KI 4, KI 6, KI 7, 
                                     KI 8, KI 10, KI 12, KI 14]
                    [BPtr 0 2, BPtr 0 4,  
                          BPtr 0 6, BPtr 0 8, BPtr 0 5,
                              BPtr 0 10, BPtr 0 12, 
                                   BPtr 0 14, BPtr 0 16]) -> True
                (Branch _ _ _ ks cs)                      -> trace (show ks)
                                                             trace (show cs)
                                                             False


testReadData tid ptr = do dat <- readData tid ptr
                          putTraceMsg ("Read: " ++ dat)
                          return ()

dummyParent  :: BNode Integer
dummyParent = Branch "test/id" 3 BNilPtr
                             [KI 2, KI 4, KI 6, KI 8, KI 10, KI 12, KI 14]
                             [BPtr 0 2, BPtr 0 4, BPtr 0 6, BPtr 0 8,
                              BPtr 0 10, BPtr 0 12, BPtr 0 14, BPtr 0 16]

putRoot = do (ptr, r) <- readRoot "test/id" :: (Binary Integer, Ord Integer) => IO (BlockPtr, BNode Integer)
             putStr (show ptr)
             return ()


testReadNode :: (Binary Integer, Ord Integer) => BlockPtr -> IO ()
testReadNode (BPtr fid loc) = do h <- openBinaryFile (idxName "test/id" fid) ReadMode
                                 n <- getAt h loc 
                                 putStr (show (n::BNode Integer))
                                 


testBTreeFindAll = do (_,root) <- readRoot "test/id"
                      ptrs     <- btGetAll (root::BNode Integer) []
                      putTraceMsg (show ptrs)
                      return ()



--testbtInsertKey  :: (Binary String, Ord String) => IO ()
--testbtInsertKey  = 
--    let leaf = Leaf "test" 3 BNilPtr
--                 [(KI 2, BPtr 0 1), (KI 4, BPtr 0 2), (KI 6, BPtr 0 3)]
--                 BPtr 0 1 :: BNode String
--    in
    


testPutBlockPtr ptr = do h <- openBinaryFile "test.dat" WriteMode
                         put h ptr
testGetBlockPtr     = do h <- openBinaryFile "test.dat" ReadMode
                         ptr <- get h :: IO BlockPtr
                         putStrLn (show ptr)