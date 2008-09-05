module Depot.Util.ParserImports 
    (module TPP, module TPPE, module TPPL,
     lexer, symbol, whiteSpace,naturalOrFloat,float)
    where
    
-- provides all of the Parsec imports needed for a parser based on an
-- emptyDef. The redefinition of lexer, symbol, and whiteSpace will be
-- required if the parser requires a specific langauge def
            
import Text.ParserCombinators.Parsec as TPP
import Text.ParserCombinators.Parsec.Expr as TPPE
import Text.ParserCombinators.Parsec.Language as TPPL
import qualified Text.ParserCombinators.Parsec.Token as P

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser emptyDef

symbol = P.symbol lexer
whiteSpace = P.whiteSpace lexer

naturalOrFloat = P.naturalOrFloat lexer
float = P.float lexer
                 