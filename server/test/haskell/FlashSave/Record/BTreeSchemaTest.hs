module FlashSave.Record.BTreeSchemaTest
    where
      
      
import FlashSave.Record.BPlusGenIndex
import FlashSave.Record.BTreeSchemaParser
    


import Test.HUnit
import Test.QuickCheck
import System.Directory
import System.IO
import Data.Map as Map
import Debug.Trace
    
import FlashSave.Base

fixture = runTestTT $ TestList [TestLabel "parse schema" testParser]


testSchema = "/Users/weeksie/workspace/flash-save/test/FlashSave/Record/test.schema"

expectedIdxs = [Schema "test1" SInt SInt 50,
                Schema "test2" SStr SPtr 50,
                Schema "test3" SStr SPtrs 50,
                Schema "test4" SStr SPtrs 4,
                Schema "test5" SPtr SInt 50]

testParser = TestCase $ do 
               schema <- parseSchema testSchema
               case schema of
                 Left error -> assertFailure (show error)
                 Right idxs -> assertEqual "idxs" expectedIdxs idxs








