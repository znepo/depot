module Depot.Conf.YAMLParser where
    
import Depot.Util.ParserImports hiding (lexer, symbol, whiteSpace, naturalOrFloat, float)
import qualified Text.ParserCombinators.Parsec.Token as P
    
import Data.Map    


-- this is meant to grow into a small subset of yaml, enough to write
-- a depot config file, NOT to conform with the YAML report. Currenty
-- it will parse key value pairs into a YMLMap.


type YMLScalar = String
data YML       = YMLMap (Map YMLScalar YML)
--               | YMLSeq [YML] 
               | YMLScl YMLScalar
               deriving Show
         
-- Public ------------------------------------------------------------

parseYml :: String -> String -> Either ParseError YML
parseYml = parse yml 
           
parseYml' :: String -> Either ParseError YML
parseYml' = parse yml ""

parseYmlFile :: String -> IO (Either ParseError YML)
parseYmlFile = parseFromFile yml


-- Parser definition -------------------------------------------------

yamlDef    = emptyDef { commentLine  = "#", caseSensitive = False }
          
lexer      = P.makeTokenParser yamlDef
symbol     = P.symbol lexer
whiteSpace = P.whiteSpace lexer
strlit     = P.stringLiteral lexer
             

colon      = symbol ":"
dash       = symbol "-"
             
docstart   = symbol "---" >> manyTill anyChar (try linebreak)
docend     = symbol "..."
             
lbrace     = symbol "{"
rbrace     = symbol "}"
         
lbracket   = symbol "["
rbracket   = symbol "]"
             
comma      = symbol ","
linebreak  = (char '\r' <|> char '\n') >> return ()


ymlstr     = whiteSpace >> many1 (letter <|> char ' ' <|> digit <|> oneOf "/-_.") 

keyval     = do
  k <- ymlstr
  colon
  v <- ymlstr
  whiteSpace
  return (k, YMLScl v)
  
mapping = many1 keyval >>= return . YMLMap . fromList

-- our baby yml parser only supports mappings (with no indentation!)
yml     = mapping
          

