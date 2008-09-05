{-# OPTIONS_GHC -fglasgow-exts #-}
module FlashSave.Record.JournalingTest
    where
      
import FlashSave.Record.Journaling
import FlashSave.Record.DataStore
import FlashSave.IO.Binary
import Test.HUnit
import Test.QuickCheck
import System.Directory
import System.IO
import Data.Map as Map
import Debug.Trace
    
fixture = runTestTT $ TestList [--TestLabel "read/write journal" testReadWriteJournal,
                                TestLabel "test serialization" testSerialization]


entries = [ (JE INSERT 1 13 "text/test" "name" "owner1" (BPtr 0 1))
          , (JE INSERT 3 13 "text/test" "name" "owner2" (BPtr 0 2))
          , (JE INSERT 7 13 "text/test" "name" "owner4" (BPtr 0 4))
          , (JE INSERT 7 13 "text/test" "name" "owner4" (BPtr 0 4))
          , (JE INSERT 7 13 "text/test" "name" "owner4" (BPtr 0 4000865665476))
          , (JE INSERT 7 13 "text/test" "name" "owner4" (BPtr 0 4))
          , (JE INSERT 7 13 "text/test" "name" "owner4" (BPtr 0 4))
          , (JE INSERT 8 13 "text/test" "name" "owner5" (BPtr 0 5))
          , (JE INSERT 19 13 "text/test" "name" "owner7" (BPtr 0 7))
          ]

testReadWriteJournal = TestCase $ do 
  h <- openBinaryFile "test.jrn" WriteMode
  mapM_ (makeJournalEntry h) entries
  hClose h
  h' <- openBinaryFile "test.jrn" ReadMode
  actual <- processJournal h' (\e acc -> e:acc) [] `catch` (\e -> do 
                                                                putTraceMsg (show e)
                                                                return [])
  hClose h'
  assertEqual "read in journal entries" entries $ reverse actual
  removeFile "test.jrn"
             

testSerialization = TestCase $ do
  h <- openBinaryFile "test.bin" WriteMode
  put h (123433399889998899 :: Integer)
  putTraceMsg "\n"
  hClose h
  h' <- openBinaryFile "test.bin" ReadMode
  i <- get h' :: IO Integer
  size <- hFileSize h'
  pos  <- hTell h'
  hClose h'
  assertEqual "size and pos" size pos
  removeFile "test.bin"
