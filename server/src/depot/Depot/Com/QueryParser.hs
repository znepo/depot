{-# OPTIONS_GHC -fglasgow-exts -package parsec#-}
module Depot.Com.QueryParser 
    (handleRequest, readHeader, parseQueryExp, parsePostData,headers)
        where

import Depot.Base 
import Depot.Error
import Depot.Com.Protocol

import Depot.Util.ParserImports

import Data.List
import Data.Char
import Data.Map as Map

import System.IO
import Control.Exception hiding (try)
import Control.Monad.Error


import Numeric
import Debug.Trace
import Depot.IO.Binary

import qualified Depot.Logging as LOG

import Prelude hiding (catch)

-- TODO: thinking of syntax for joins
--
-- /foo/bar/[poo p,butter b]?b.id=p.pooface&b.id=..23
-- /[foo f,/a/b/c alph]?alph.id=f.id&f.id=32..
--
-- /foo/bar/[poo.color=black]/corn?kernel=round
-- /foo/bar/poo?poo.color=black&sofa.poo=poo.id
-- 
-- OR perhaps query joins are built with full URLs and regular query
-- strings afterword. Assuming that queries will be constructed by the
-- client software. e.g.
-- /foo/bar,/baz/boz,/rar/roo?baz.id=boz.id&bar.id=boz.id
-- 
-- query.addurl /foo/bar
-- query.addurl /foo/baz
-- query.addurl /foo/boz
-- query.param('baz.id',eq,'boz.id')
-- query.param('baz.id',eq,'bar.id')
-- query.directive('limit','20'), query.directive('limit','1..20') 
--
-- also need a function syntax: 
-- 
-- /foo/bar?id=(max)
-- /foo/bar?id=(max),2..5,(min)
--
-- ... and perhaps limit?
-- /foo/bar/?id=(300...600,limit 5)
--     returns ids 300..305 ?
-- need to work on this

-- Parsing combinators -----------------------------------------------

colon      = symbol ":"
equals     = symbol "="
ampersand  = symbol "&"
qmark      = symbol "?"
linebreak  = symbol "\r" <|> symbol "\n"

version    = string "HTTP/" >> float
urlPart    = many1 $ escaped <|> (noneOf "/;?:@&=+$, ")
queryPart' = many $ escaped <|> (noneOf "&\r\n ")
queryPart  = many1 $ escaped <|> (noneOf "\r\n ")
escaped    = do
  char '%' 
  d1 <- hexDigit
  d2 <- hexDigit
  let (ascii,_):_ = readHex [d1,d2]
  return $ chr ascii

anOrDash   = alphaNum <|> char '-'

-- accepts an http request as a list of each line in the request. The
-- first line is the "request line", the rest are headers. Accepts a
-- function for handling input. Probably should be done monadically
-- but I can't be fucked right now.
handleRequest f headerstring = do
    resp <- (case parse headers "headers" headerstring of
               Left error    -> parserror error
               Right request -> f request)
    return resp
    where
      parserror e = failure $ BadRequest (show e) []


parsePostData str = case parse postString "POST data block" str of
                      Left err  -> failure $ BadRequest (show err) []
                      Right dat -> success dat

postString = do
  varvals <- urlVarParse `sepEndBy` ampersand
  return $ Map.fromList varvals

headers :: Parser Header           
headers = do
  hline   <- requestLine 
  varvals <- many1 headerVar
  return $ H {hdrLine = hline, hdrVars = Map.fromList varvals}


requestLine :: Parser HeaderLine
requestLine = do 
  act  <- action
  sub  <- subject
  vars <- option [] query
  ver  <- version
  whiteSpace
  return $ HLine act sub vars (show ver)
            
action :: Parser Method
action = do act <- try (symbol "PUT")  <|> 
                   symbol "GET"  <|> 
                   symbol "HEAD" <|> 
                   symbol "POST" <|> 
                   symbol "DELETE"
            return (case act of
                    "PUT"    -> PUT
                    "POST"   -> PUT -- no POST requests 
                    "GET"    -> GET
                    "DELETE" -> DELETE
                    _        -> HEAD)
                    

subject :: Parser Subject
subject = do
  root <- option "" (string "/")
  url  <- urlPart `sepEndBy` (string "/")
  whiteSpace
  case foldl (\dir dir' -> dir ++ "/" ++ dir') "" url of
    (x:xs) -> return xs
    xs     -> return "."

query :: Parser [(Var,Val)]
query = do qmark
           vv <- urlVarParse `sepEndBy` ampersand
           whiteSpace
           return $ vv


urlVarParse :: Parser (Var, Val)
urlVarParse = do var <- urlPart
                 val <- option "" (equals >> queryPart')
                 return (var, val)

    
headerVar :: Parser (Var, Val)
headerVar = do var <- many1 anOrDash
               whiteSpace
               colon
               val <- (manyTill anyChar $ try linebreak) <?> "expecting value after ':'" --lookitmeh
               return (downcase var, val)             



-- query parsers -----------------------------------------------------


inclusive  = symbol ".."   
exclusive  = symbol "..."  
comma      = symbol ","    
nocomma    = noneOf ","
minfun     = symbol "(min)"
maxfun     = symbol "(max)"
             
incExp = do
  v1 <- manyTill nocomma $ try inclusive
  v2 <- many nocomma
  giveRange ".." (RangeInclusive) v1 v2

-- still considered a TODO schedule for another release  
excExp = do
  v1 <- manyTill nocomma $ try exclusive
  v2 <- many nocomma
  giveRange "..." (RangeExclusive) v1 v2
            
rangemsg str = 
    "Range is missing boundaries, syntax is val1"++str++"val2 " ++
    "where either val2 or val2 is required. For example " ++
    "1"++str++"2, "++str++"2, and 1"++str++" are legal ranges"

giveRange str rangector v1 v2 = 
  case (v1,v2) of 
    ("","") -> fail   (rangemsg str)
    ("",_)  -> return (RangeTo v2)
    (_,"")  -> return (RangeFrom v1)
    (_,_)   -> return (rangector v1 v2)

scalExp = do
  v <- many1 (noneOf ".,")
  return (Scalar v)

minExp = minfun >> return QueryMin
maxExp = maxfun >> return QueryMax


--- value is anything until qexpSep
--- if we terminate before reaching one then we have
--- a scalar. 
expFuns = [try minExp, try maxExp, try incExp, scalExp]

queryExpressionParser = do
  val <- sepBy1 (choice expFuns) comma -- try excExp, 
  case val of
    [v] -> return v
    []  -> fail "can't have an empty comma delimited set"
    vs  -> return (CommaDelimited vs)
  

parseQueryExp exp = parse queryExpressionParser "qexp" exp 
                                  



-- IO functions for reading from HTTP requests -----------------------

readHeader :: Handle -> IO String
readHeader h = do
  l <- hGetLineNB h 5000
  if emptyLine l
     then readHeader h
     else readHeader' l h >>= return . unlines          

readHeader' l h = do
  if emptyLine l
     then return []
     else do 
           l' <- hGetLineNB h 5000 
           ls <- readHeader' l' h `catch`
                 -- catch this because we may have an oddly terminated
                 -- header. It's better to try to process odd data
                 -- than throw it right out (liberal in what you
                 -- accept, strict in what you respond with)
                 (\timeout -> 
                  LOG.errorIO "timeout on header read." >> 
                  return [])
           return (l:ls)

emptyLine "\r" = True
emptyLine _    = False
                 

