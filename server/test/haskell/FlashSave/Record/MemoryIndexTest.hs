{-# OPTIONS_GHC -fglasgow-exts #-}
module FlashSave.Record.MemoryIndexTest
    where
      
import FlashSave.Record.MemoryIndex
import FlashSave.IO.Binary
import Test.HUnit
import Test.QuickCheck
import System.Directory
import System.IO
import Data.Map as Map
import Debug.Trace
    
import FlashSave.Base

fixture = runTestTT $ TestList [TestLabel "create directory" testCreateDirectory,
                                TestLabel "serialize map"    testSerializeMap]


table    = "/tmp/a/new/table"
tableIdx = table ++ "/idx"
treeId   = tableIdx ++ "/Content-Type.idx"

testCreateDirectory = TestCase $ do 
  createDepot table
  tableDirExists <- doesDirectoryExist table
  indexDirExists <- doesDirectoryExist tableIdx
  tableDefExists <- doesFileExist treeId
  h <- openBinaryFile treeId ReadMode
  actual <- hFileSize h
  hClose h
  assertEqual "create directories" [True,True,True] [tableDirExists, indexDirExists, tableDefExists]
  --clean up
  removeDirectoryRecursive "/tmp/a"
                           

testSerializeMap = TestCase $ do
                     createDepot table
                     h    <- getIndexHandle table "Content-Type"
                     doWriteMap h 
                     size <- hFileSize h
                     assertEqual "file changed size" True (size > 0)
                     hSeek h AbsoluteSeek 0
                     m    <- get h :: IO (Map String [String])
                     assertEqual "[sauce]" (m Map.! "text/plain") ["sauce"]
                     assertEqual "[p1,p2]" (m Map.! "text/html") ["pantaloon1", "pantaloon2"]
                     --assertEqual "sizeOf: " 81 $ sizeOf m
                     hClose h
    where
      doWriteMap h = do put h makeMap
      makeMap :: Map String [String]
      makeMap      = let 
                         m    = Map.singleton "text/plain" ["sauce"]
                         m'   = Map.insert "text/rtf" ["ortega"] m
                         m''  = Map.insert "text/html" ["pantaloon1","pantaloon2"] m'
                     in 
                       m''
               
