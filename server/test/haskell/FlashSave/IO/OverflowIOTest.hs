{-# OPTIONS_GHC -fglasgow-exts #-}

module OverflowIOTest
    where
      
import Test.HUnit
import Test.QuickCheck
import Data.Map as Map
import Data.Char
import System.IO
    
import FlashSave.Base
import FlashSave.IO.Binary
import FlashSave.IO.OverflowIO
import BinaryExplorer

    
import Control.Exception
import Control.Monad
import Control.Monad.State as ST
import Debug.Trace    
import qualified FlashSave.Logging as LOG    



testbin    = "overflow-io.bin"
testscript = "overflow-io.binscript"
rwscript   = "overflow-io-reclaim.binscript"

testValidateOverflow = do
  h <- openBinaryFile testbin ReadWriteMode
  performTest h `finally` (closeIfStillOpen h)
  h' <- openBinaryFile testbin ReadWriteMode
  performRewriteTest h' `finally` (closeIfStillOpen h)
  where performTest h = do
          hSetFileSize h 0
          hSetFileSize h 16323
          hSetBuffering h NoBuffering
          writeNode h (Branch BNilPtr nums0 ptrs0) (BPtr 0 0) (FB (BPtr 1 0) [])
          hClose h
          exploreBinaryWith testscript testbin stdout
        performRewriteTest h = do
          hSetBuffering h NoBuffering
          LOG.infoIO "writing node (reclaim)"
          writeNode h (Branch BNilPtr nums1 ptrs1) (BPtr 0 0) (FB (BPtr 4 0) [])
          LOG.infoIO "wrote node"
          node <- readNode h (BPtr 0 0) :: IO (BTreeIndex Integer)
          LOG.infoIO $ "read node " ++ show node
          hClose h
          exploreBinaryWith rwscript testbin stdout
          
          
                            
-- an array of integers, each of size 25 
nums0 = [
         10000000000,10000000001,10000000002,10000000003,
         10000000004,10000000005,10000000006,10000000007,
         10000000008,10000000009,10000000010,10000000011
        ]
ptrs0 = [
         BPtr 0 1, BPtr 0 2, BPtr 0 3, BPtr 0 4, BPtr 0 5,
         BPtr 0 6, BPtr 0 7, BPtr 0 8, BPtr 0 9, BPtr 0 10,
         BPtr 0 11, BPtr 0 12, BPtr 0 13
        ]
nums1 = nums0 ++ [10000000012]
ptrs1 = ptrs0 ++ [BPtr 0 14]
          


closeIfStillOpen h = do isOpen <- hIsOpen h
                        when isOpen (hClose h)

-- these tests only work for a block size of 256

testSpansFor = let branch  = Branch BNilPtr nums0 ptrs0                            
                   exp     = [Span (BPtr 2 0) 10, Span (BPtr 3 0) 2,
                              Span (BPtr 3 50) 12, Span (BPtr 4 0) 1 ]
               in do
                 (sps,_) <- ST.runStateT (spansFor branch (BPtr 1 0)) (FB (BPtr 2 0) [])
                 if sps == exp
                    then putStrLn $ "success: " ++ show sps
                    else error $ "\nexpected " ++ show exp ++ "\ngot      " ++ show sps
testReclaimSpans = 
    let branch  = Branch BNilPtr nums0 ptrs0                            
        exp     = [Span (BPtr 9 0) 10, Span (BPtr 13 0) 2,
                   Span (BPtr 13 50) 12, Span (BPtr 2 0) 1 ]
    in do
        (sps,_) <- ST.runStateT (spansFor branch (BPtr 1 0)) (FB (BPtr 2 0) [(BPtr 9 0), (BPtr 13 0)])
        if sps == exp
           then putStrLn $ "success: " ++ show sps
           else error $ "\nexpected " ++ show exp ++ "\ngot      " ++ show sps
                 
 
testFillSpans = fillSpans' nums0 (BPtr 2 0) (FB (BPtr 1 0) []) []
                