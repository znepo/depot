module FlashSave.Record.DataStoreTest
    where

import FlashSave.Base
import FlashSave.Record.DataStore
import FlashSave.IO.Binary
import Test.HUnit
import Test.QuickCheck
import System.Directory
import System.IO
import Data.Map as Map
import Debug.Trace
    
fixture = runTestTT $ TestList [TestLabel "read/write data" testReadWriteData]

dpath    = "test-data"

testReadWriteData = TestCase  $ do
      createDirectoryIfMissing True (dpath /// "dat")
      h     <- getDatFileHandleFor "test-data" DatHdlNil (BPtr 0 0)
      metas <- writeN 200 "test" h
      -- mapM_ (putTraceMsg . show) metas
      metas' <- readN 200 h
      hClose h
      --removeDirectoryRecursive dpath
                           



writeN n dat hdl = writeN' n dat hdl (BPtr 0 0) []
  
writeN' 0 _ _ _ acc     = return acc
writeN' n dat h ptr acc = 
    let dat' = dat ++ (show n)
        len  = toInteger $ length dat'
        meta = DB { dbObjectId     = n,
                    dbOwner        = "test",
                    dbCreationTime = show n,
                    dbObjectName   = dat',
                    dbContentType  = "text/test" }               
    in do
      input     <- openBinaryFile "tmpinput" ReadWriteMode
      let loc   = fromInteger (sizeOf meta) + bpOffset ptr
      hPutStr input dat'
      hSeek input AbsoluteSeek 0 
      (h',ptr') <- writeData dpath meta len ptr{bpOffset=loc} input (DatHdl h)
      hClose input
      removeFile "tmpinput"
      writeN' (n-1) dat h' ptr' (meta:acc) 
                           
                    

readN n h       = readN' n h (BPtr 0 0) 
readN' 0 _ _    = return ()
readN' n h ptr  = do
  readData dpath False stdout (DatHdl h) ptr
  loc <- hTell h
  putTraceMsg "" 
  -- putTraceMsg $ "new loc " ++ show loc
  readN' (n-1) h ptr{bpOffset=fromInteger loc} 