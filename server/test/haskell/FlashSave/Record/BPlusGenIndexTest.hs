{-# OPTIONS_GHC -fglasgow-exts -fno-monomorphism-restriction#-}
module FlashSave.Record.BPlusGenIndexTest
    where
      
import Test.HUnit
import Test.QuickCheck
import System.Directory
import System.IO
import Debug.Trace
import System.Random

import FlashSave.Base
import Data.Generics
import Ext.PList

import FlashSave.Record.BPlusGenIndex
import FlashSave.IO.BPlusIO

import Data.List as List
import Data.Map as Map
import Data.PackedString
    
fixture = runTestTT $ TestList [
                                --TestLabel "create directory" testCreateDirectory,
                                TestLabel "create directory" testReadDirectory
                                --TestLabel "write stuffs" testWriteStuffs
                               ]


table      = "/tmp/a/new/table"
tableIdx   = table /// "idx"
schemaFile = tableIdx /// "dir.schema"



testCreateDirectory = TestCase $ do
  entry <- createDepotDirectory table
  tableDirExists <- doesDirectoryExist table
  indexDirExists <- doesDirectoryExist tableIdx
  schemaExists   <- doesFileExist schemaFile
  assertEqual "create directories" [True,True,True] [tableDirExists, 
                                                     indexDirExists, 
                                                     schemaExists]
  case entry of 
    Left err      -> assertFailure (show err)
    Right (h,entry')  -> do
--                assertEqual "entries" (Just emptyRoot :: Maybe IdxPSMult)
--                                (pLookup "Content-Type" (idxRootsBE entry'))
               assertEqual "object ids" (Just emptyRoot :: Maybe IdxInt)
                               (pLookup "X-Object-Id" (idxRootsBE entry'))
               lookupState entry'
               case lookupState entry' of
                  Nothing -> do closeEntryHandles entry'
                                hClose h
                                assertFailure "couldn't lookup state"
                  Just state  -> do
                        (r,hdl,o,hdr) <- doInserts state 1000
                        let roots      = pDelete "X-Object-Id" (idxRootsBE entry')
                            headers  = Map.insert "X-Object-Id" hdr (idxHeadersBE entry')
                            orders   = Map.insert "X-Object-Id" o (idxOrdersBE entry')
                            handles  = Map.insert "X-Object-Id" hdl (idxHandlesBE entry')
                            entry'' = entry'{idxRootsBE   = pCons ("X-Object-Id",r) roots,
                                             idxHeadersBE = headers,
                                             idxHandlesBE = handles,
                                             idxOrdersBE  = orders}
                        writeEntry h entry''
                        closeEntryHandles entry''
                        hClose h
--                        put h entry
  -- assuming that if the above test passes then all the indexes are present.
  -- clean up
  
  -- removeDirectoryRecursive "/tmp/a"

testReadDirectory = TestCase $ do
     result <- readDepotDirectory table
     case result of
       Left err        -> assertFailure (show err)
       Right (h,entry) -> do
           case lookupState1 entry of 
             Nothing -> do closeEntryHandles entry
                           assertFailure "couldn't get state"
             Just state@(_,_,_,hdr) -> do
                    print hdr
                    let inserts = (take 100) randList
                    assertInsertionsPresent state inserts 100
                    writeEntry h entry
                    closeEntryHandles entry
                    hClose h


doInserts state n = do
      let inserts = (take n randList)
      putStrLn " "
      putStr $ "***** inserting " ++ (show (length inserts))
      putStrLn " random numbers ****"
      state' <- doInsert state inserts n
      putStrLn "beginning selects"
      assertInsertionsPresent state' inserts n


lookupState entry = do
            order  <- Map.lookup "X-Object-Id" (idxOrdersBE entry)
            header <- Map.lookup "X-Object-Id" (idxHeadersBE entry)
            handle <- Map.lookup "X-Object-Id" (idxHandlesBE entry)
            return (emptyRoot :: IdxInt,handle,order,header)

lookupState1 entry = do
            order  <- Map.lookup "X-Object-Id" (idxOrdersBE entry)
            header <- Map.lookup "X-Object-Id" (idxHeadersBE entry)
            handle <- Map.lookup "X-Object-Id" (idxHandlesBE entry)
            root   <- pLookup "X-Object-Id" (idxRootsBE entry)
            return (root :: IdxInt,handle,order,header)

doInsert state _ 0      = return state
doInsert state (rand:xs) n = do 
  let x = if rand < 0 then rand * (-1) else rand
  (_,state') <- insertBT x (BPtr (fromInteger x) 0) state
  doInsert state' xs (n-1)


assertInsertionsPresent state _ 0 = return state
assertInsertionsPresent state (rand:xs) n = do
  let x = if rand < 0 then rand * (-1) else rand
  ptrs <- selectBT (toInteger x) state
  assertEqual "insertion: " 1 (length ptrs)
  assertInsertionsPresent state xs (n-1)


randList = randomRs (1,1000) (mkStdGen 1) :: [Integer]
           
-- ctype = anyprop ("X-Object-Id", emptyRoot :: IdxInt),
-- ctype = anyprop ("X-Owner", emptyRoot :: IdxPSMult),
-- ctype = anyprop ("X-Object-Name", emptyRoot :: IdxPSMult),
-- ctype = anyprop ("X-Creation-Date", emptyRoot :: IdxIntMult)] 
        
         
              
