{-# OPTIONS_GHC -fglasgow-exts #-}

module BPlusIOTest
    where
      
import Test.HUnit
import Test.QuickCheck
import Data.Map as Map
import Data.Char
import System.IO
    
import FlashSave.Base
import FlashSave.IO.Binary
import FlashSave.IO.BPlusIO
import FlashSave.Record.BPlusGenIndex
import BinaryExplorer

    
import Control.Exception
import Control.Monad
import Debug.Trace    
import qualified FlashSave.Logging as LOG    



testbin = "test.idx.overflow"
testscript = "bplusio-overflow-test.binscript"

testValidateOverflow = do
  h <- openBinaryFile testbin ReadWriteMode
  performTest h `finally` (closeIfStillOpen h)
  where performTest h = do
          hSetFileSize h 0
          hSetFileSize h 16323
          hSetBuffering h NoBuffering
          (OverflowTo ptr, _) <- writeNode h testLeaf (BPtr 0 0) (BPtr 1 0)
          hSeek h AbsoluteSeek 0 -- rewind
          (OverflowTo ptr', _) <- writeNode h testLeaf{valsBT=valsL'} (BPtr 0 0) ptr
          hSeek h AbsoluteSeek 0 -- rewind again
          writeNode h testLeaf{valsBT=valsL} (BPtr 0 0) ptr'
          hClose h
          exploreBinaryWith testscript testbin stdout
                            

          
closeIfStillOpen h = do isOpen <- hIsOpen h
                        when isOpen (hClose h)


intostr x = read $ foldl (\acc a -> show (ord a) ++ acc) "" x

testLeaf = Leaf BNilPtr valsL BNilPtr BNilPtr
valsL = Map.fromList [(10000000000, BPtr 10 0),
                               (10000000001, BPtr 11 0),
                               (10000000002, BPtr 12 0),
                               (10000000003, BPtr 13 0)]

valsL' = Map.fromList [(10000000000, BPtr 10 0),
                               (10000000001, BPtr 11 0),
                               (10000000002, BPtr 12 0),
                               (10000000003, BPtr 13 0),
                               (10000000004, BPtr 14 0)]


testLeaf2 = Leaf BNilPtr valsL2 BNilPtr BNilPtr
valsL2 = Map.fromList [(intostr "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", BPtr 1 0),
                               (intostr "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", BPtr 2 0),
                               (intostr "cccccccccccccccccccccccccccccccccc", BPtr 3 0),
                               (intostr "dddddddddddddddddddddddddddddddddd", BPtr 4 0),
                               (intostr "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee", BPtr 5 0),
                               (intostr "ffffffffffffffffffffffffffffffffff", BPtr 6 0),
                               (intostr "gggggggggggggggggggggggggggggggggg", BPtr 6 0),
                               (intostr "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh", BPtr 6 0),
                               (intostr "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii", BPtr 6 0),
                               (intostr "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj", BPtr 6 0),
                               (intostr "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk", BPtr 6 0),
                               (intostr "llllllllllllllllllllllllllllllllll", BPtr 6 0),
                               (intostr "mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm", BPtr 6 0),
                               (intostr "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn", BPtr 6 0),
                               (intostr "oooooooooooooooooooooooooooooooooo", BPtr 6 0),
                               (intostr "pppppppppppppppppppppppppppppppppp", BPtr 6 0),
                               (intostr "qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", BPtr 6 0),
                               (intostr "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr", BPtr 6 0),
                               (intostr "ssssssssssssssssssssssssssssssssss", BPtr 6 0),
                               (intostr "tttttttttttttttttttttttttttttttttt", BPtr 6 0),
                               (intostr "uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu", BPtr 6 0),
                               (intostr "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv", BPtr 6 0),
                               (intostr "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww", BPtr 17 0)]


traceATest = do
  h <- openBinaryFile "a/test/idx/X-Object-Id/idx.bptree" ReadMode
  traceNode "" h (BPtr 1 0)
  hClose h
--   (h,rpair,_,_) <- openRoot "a/test" (Schema "X-Object-Id" SInt SPtr 4)"X-Object-Id"
--   let Just (n::IdxInt) = pLookup "X-Object-Id" [rpair]
--   hClose h
--   printAllChildPtrs n
                   


blockstart = "bool,ptr"
leafstart  = ";int1, ptr, ptr, ptr"

-- contents = ""

-- verifyContents = do
  