{-# OPTIONS_GHC -package parsec -fglasgow-exts #-}
module Depot.Com.Protocol
    where


import Depot.IO.Binary
import Depot.Util.ParserImports
import Depot.Error
import Depot.Base 
import qualified Depot.Logging as LOG
import Data.Map as Map
import Data.Char
    
import System.IO

-- this module is in dire need of refactoring

-- http data types ---------------------------------------------------
type Subject  = String
type Var      = String
type Val      = String
type Dat      = String   -- perhaps explicitly base64 or binary later...
type Ver      = String
type HttpCode = String
type HVars    = Map Var Val   -- header variables
type UVars    = [(Var, Val)]        -- url variables
type Status   = String -- e.g. "404 Not Found"


data Method   = PUT | GET | DELETE | HEAD deriving Show

data HeaderLine = HLine { hdrMethod       :: Method,
                          hdrSubject      :: Subject,
                          hdrUVars        :: UVars,
                          hdrHttpVers     :: Ver } 
                deriving Show

data Header   = H { hdrLine :: HeaderLine,
                    hdrVars :: HVars }              
              | BadH ParseError String deriving Show

data Body    = B Dat | BEmpty deriving Show

data Request = Req Header Body 
             | BadReq ParseError String
               deriving Show                        

data Response = Res Status HVars Body

instance Show Response where
    show r = showRes r


showRes :: Response -> String
showRes (Res line headers body) = 
    line ++ showHeaders headers ++  show body

                    
showHeaders headers = 
    let hdrs = Map.foldWithKey (\k v acc ->
                                    (acc ++ k ++ ":" ++ v ++ "\r\n")) "" headers
        term = "\r\n"
    in hdrs ++ term

responseLine status = "HTTP/1.1 " ++ statusCode status ++ "\r\n"
responseErr err     = "HTTP/1.1 " ++ errorCode err ++ "\r\n"

contentLength (H _ hdrs) = case Map.lookup "Content-Length" hdrs of
                             Just length -> read length :: Integer
                             Nothing     -> 0

contentType = headerVal "Content-Type"

headerVal name (H _ hdrs) = case Map.lookup name hdrs of
                              Just val -> val
                              Nothing  -> error ("Header "++ name ++" doesn't exist")


readIdFromVars vars = Map.lookup "id" vars

instance Binary Body where
    put h (B dat) = put h dat
    get h         = do str <- get h :: IO String
                       return (B str)
    sizeOf (B dat) = sizeOf dat

{- becomes: 
   
   HTTP/1.1 200 OK
   Date:Wed, 16 Nov 2005 02:28:42 GMT
   Server:Flash Save Server
   Cache-Control:no-cache
   Connection:close
   Content-Length:11
   Content-type:text/plain

   HELLO WORLD
-}
   


stdHeaders extra = Map.fromList ([("Server", "Snepo Depot Server"),
                                  ("Cache-Control", "no-cache"),
                                  ("Connection", "close"),
                                  ("Content-Type","text/plain")] ++ extra)

stdHeaders1 = stdHeaders []              

defaultResponse status = defaultResponse' (responseLine status) (messageSTAT status) (headersSTAT status)
defaultErrResponse err = defaultResponse' (responseErr err)     (messageERR err)  (headersERR err)

defaultResponse' statline mess xtra = 
    let extrahdrs = ("Content-Length", show (length mess)):xtra
        hdrs      = stdHeaders extrahdrs 
    in  (statline ++ showHeaders hdrs ++ mess)
                                 


writeStdResponse hvars handle status = 
    let term = case Map.lookup "x-null-terminated" hvars of
                 Nothing -> "\r\n\r\n"
                 Just _  -> [chr 0]
        body = messageSTAT status ++ "\r\n" ++ term
        xtra = ("Content-Length",show (length body)):(headersSTAT status)
        hdrs = stdHeaders xtra
        resp = responseLine status ++ showHeaders hdrs ++ body
    in LOG.infoIO ("\n" ++ resp) >> hPutStr handle resp
