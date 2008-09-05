module FlashSave.Record.BPlusIndexTest
    where
      
import FlashSave.Record.BPlusIndex
import FlashSave.IO.Binary
import Test.HUnit
import Test.QuickCheck
import System.Directory
import System.IO

fixture = runTestTT $ TestList [TestLabel "create table" testCreateTable]


table    = "/tmp/a/new/table"
tableDef = table ++ "/def"
treeId   = table ++ "/id"

testCreateTable = TestCase $ do 
  createTable (TD table [treeId] [5])
  tableDirExists <- doesDirectoryExist table
  indexDirExists <- doesDirectoryExist treeId
  tableDefExists <- doesFileExist tableDef
  h <- openBinaryFile (treeId ++ "/0.idx") ReadMode
  actual <- hFileSize h
  assertEqual "create directories" [True,True,True] [tableDirExists, indexDirExists, tableDefExists]
  assertEqual "index size" (toInteger maxDegree * 5 + 4) actual
  -- clean up
  removeDirectoryRecursive "/tmp/a"
     


                     
          
---- TODO: create tests for: 
---- Def creation from createTable
---- aggregate returns
---- 
               

               