module BinaryExplorer
    where
      
import System.IO
import FlashSave.Base
import FlashSave.Util.ParserImports
import FlashSave.IO.Binary
import FlashSave.IO.BinaryImpl
import FlashSave.IO.OverflowIO
import FlashSave.Record.DataStore
import Data.Word
import Data.Char
import Data.Int
import Numeric

import Control.Monad
import Control.Exception hiding (try)
import qualified Text.ParserCombinators.Parsec.Token as P
    
-- syntax: comma seperated values signifying int sizes or strings
-- int: num,int1, int4, int8, char, str, ptr, 
-- line seperator is ; and strings are enclosed in quotes.

parseExploreString str = 
    case parse exploreString "depot binary explorer string" str of
      Left err    -> error (show err)
      Right lines -> lines
                     


data BinaryEntity = BComment String
                  | BLineBreak
                  | BLine [BinaryPrim]
                  | BTest [BinaryAssertion]
                    deriving (Show)

data BinaryPrim = BInt1
                | BInt4
                | BInt8
                | BNum
                | BBool
                | BChar
                | BStr
                | BPointer
                | BLocation
                  deriving (Show)

data BinaryAssertion = BInt1Assert Word8
                     | BInt4Assert Int
                     | BInt8Assert Int64
                     | BNumAssert Integer
                     | BBoolAssert Bool
                     | BCharAssert Char
                     | BStrAssert String
                     | BPointerAssert BlockPtr
                     | BJumpTo BlockPtr
                       deriving Show
semi    = symbol ";"
colon   = symbol ":"
comma   = symbol ","
lbrace   = symbol "{"
rbrace   = symbol "}"
lparen  = symbol "("
rparen  = symbol ")"
decimal = P.decimal lexer
stringLiteral = P.stringLiteral lexer
true    = symbol "true"  >> return True         
false   = symbol "false" >> return False         
boolval = choice [true, false]

fixeddec :: (Integral a) => GenParser Char () a
fixeddec = decimal >>= return . fromInteger
           
pointerVal = choice [nilptr, ptrval]

nilptr = symbol "nil" >> return BNilPtr
ptrval =  do
  lparen
  f <- fixeddec 
  whiteSpace
  o <- fixeddec
  rparen
  return (BPtr f o)

num   = symbol "num"   >> return BNum
bool  = symbol "bool"  >> return BBool
int1  = symbol "int1"  >> return BInt1
int4  = symbol "int4"  >> return BInt4
int8  = symbol "int8"  >> return BInt8
char_ = symbol "char"  >> return BChar
str   = symbol "str"   >> return BStr
ptr   = symbol "ptr"   >> return BPointer
loc   = symbol "loc"   >> return BLocation


-- num_as :: GenParser Char st BinaryAssertion
num_as   = num            >> colon >> decimal >>= return . BNumAssert         
bool_as  = bool           >> colon >> boolval >>= return . BBoolAssert
int1_as  = symbol "int1"  >> colon >> fixeddec >>= return . BInt1Assert
int4_as  = symbol "int4"  >> colon >> fixeddec >>= return . BInt4Assert
int8_as  = symbol "int8"  >> colon >> fixeddec >>= return . BInt8Assert
char_as  = symbol "char"  >> colon >> anyChar >>= return . BCharAssert
str_as   = symbol "str"   >> colon >> stringLiteral >>= return . BStrAssert
ptr_as   = symbol "ptr"   >> colon >> pointerVal >>= return . BPointerAssert
jump_to   = symbol "jump"   >> colon >> pointerVal >>= return . BJumpTo


prims = num <|> char_ <|> str <|> ptr <|> bool <|> loc <|>
        (try int1) <|> (try int4) <|> (try int8) 

asserts = num_as <|> char_as <|> str_as <|> ptr_as <|> bool_as <|> jump_to <|>
          (try int1_as) <|> (try int4_as) <|> (try int8_as) 


stringLit = do
  oneOf "{"
  s <- manyTill anyChar (try rbrace)
  return (BComment s)

linebreak = semi >> return BLineBreak

bline = do
  prims <- prims `sepBy1` comma 
  return (BLine prims)

bassert = do
  asses <- asserts `sepEndBy1` comma
  return (BTest asses)

exploreItem  = (try bassert) <|> (try bline) <|> (try stringLit) <|> linebreak

exploreString =  whiteSpace >> many1 exploreItem
                

exploreBinaryWith :: String -> String -> Handle -> IO ()
exploreBinaryWith scriptfile binfile to = do
  h        <- openBinaryFile binfile ReadMode
  hs       <- openBinaryFile scriptfile ReadMode
  contents <- hGetContents hs
  let entities = parseExploreString contents
  (mapM_ (printEntity h to) entities) `finally` (hClose h `finally` hClose hs)
  
        
exploreHandle h script = do
  let entities = parseExploreString script
  mapM_ (printEntity h stdout) entities
  putStrLn ""

printEntity h to (BComment s)  = hPutStr to s
printEntity h to BLineBreak    = hPutStr to "\n"
printEntity h to (BLine prims) = mapM_ (printPrim h to) prims
printEntity h to (BTest asses) = mapM_ (printAss  h to) asses


printAss h to ass = do
    strrep <- case ass of
                BNumAssert x     -> assertEq (get h :: IO Integer) x
                BBoolAssert x    -> assertEq (get h :: IO Bool) x
                BCharAssert x    -> assertEq (get h :: IO Char) x
                BInt1Assert x    -> assertEq (get h :: IO Word8) x
                BInt4Assert x    -> assertEq (get h :: IO Int) x
                BInt8Assert x    -> assertEq (get h :: IO Int64) x
                BStrAssert x     -> assertEq (get h :: IO String) x
                BPointerAssert x -> assertEq (get h :: IO BlockPtr) x
                BJumpTo x        -> seekPtr h x >> return ("seeking to : " ++ show x)
    hPutStr to ("asserted: " ++ strrep)
    hPutStr to " "

assertEq getter a = do
  b <- getter
  when (a /= b) (expected a b)
  return (show b)         


expected a b = error $ "expected: " ++ show a ++ " got: " ++ show b

printPrim h to prim = do
    strrep <- case prim of
                BNum     -> retStr (get h :: IO Integer)
                BBool    -> retStr (get h :: IO Bool)
                BChar    -> retStr (get h :: IO Char)
                BInt1    -> retStr (get h :: IO Word8)
                BInt4    -> retStr (get h :: IO Int)
                BInt8    -> retStr (get h :: IO Int64)
                BStr     -> retStr (get h :: IO String)
                BPointer -> retStr (get h :: IO BlockPtr)
                BLocation -> do
                          loc <- hTell h
                          return ("location: " ++ show loc)
    hPutStr to strrep
    hPutStr to ","

retStr getter = getter >>= return . show 
                

