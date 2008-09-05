{-# OPTIONS_GHC -fglasgow-exts #-}
module Depot.Record.BTreeSchemaParser (parseSchema,parseSchemaStr) where
    
import Depot.Base

import Depot.Util.ParserImports hiding (lexer,symbol,whiteSpace)
import qualified Text.ParserCombinators.Parsec.Token as P

lexer :: P.TokenParser ()
lexer      = P.makeTokenParser haskellDef
symbol     = P.symbol lexer
whiteSpace = P.whiteSpace lexer


colon      = symbol ":"
sk_int     = symbol "int"       >> return SInt
sk_str     = symbol "str"       >> return SStr
sk_ptr     = symbol "ptr"       >> return SPtr
sk_hex     = symbol "hex"       >> return SHex
             
sv_int     = symbol "int"       >> return SVInt
sv_str     = symbol "str"       >> return SVStr
sv_auth    = symbol "auth"      >> return SVAuth             
sv_pri     = symbol "principal" >> return SVPri
             


s_name     = many1 (alphaNum <|> oneOf ".-") 

s_order    = try (do n <- P.natural lexer             
                     if n < 2 
                        then unexpected ("order value: " ++ (show n))
                        else return n) <?> "order >= 2"
             
sk_type     = sk_int <|> sk_str <|> sk_ptr <|> sk_hex  <?> "key data type"

sv_type     = sv_int <|> sv_str <|> sv_auth <|> sv_pri <?> "value data type"

s_idx_decl = do
  name  <- s_name
  colon
  ktype <- sk_type
  colon
  vtype <- sv_type
  (do colon 
      order <- s_order
      return (Schema name ktype vtype (fromInteger order))
   <|> return (Schema name ktype vtype 50))
   
  
s_schema = do
  whiteSpace
  idxs <- many1 s_idx_decl
  return idxs
  
-- takes the path to a schema file and returns a list of index schemas
parseSchema :: SourceName -> IO (Either ParseError [Schema])
parseSchema sfile = parseFromFile s_schema sfile
    
parseSchemaStr :: String -> Either ParseError [Schema]
parseSchemaStr schema = parse s_schema "schema" schema
                        



-- TODO add "read data as" functions for reading raw data that is sent
-- along with a schema.

