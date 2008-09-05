module Depot.Sec.BPAuthInterface where
    
import Depot.Base
import Depot.IO.Binary
import qualified Depot.Logging as LOG
import Depot.Sec.Auth 
import Depot.Com.DepotSocketCombinators
import Depot.Record.DataStore
import Depot.IO.BPlusIO
import Depot.Util.ParserImports
    
import System.IO
import Data.Map as Map
import Data.Word



import Control.Monad.Trans
import qualified Control.Monad.State as ST
   
colon      = symbol ":"
lbrace     = symbol "{"
rbrace     = symbol "}"
comma      = symbol ","
quote      = symbol "\""
squote     = symbol "'"
            
       
pperm = char 'c' <|> char 'r' <|> char 'u' <|> char 'd'

parsePerm = many pperm

permsFromString name str = case parse parsePerm name str of
                             Left err -> Left err
                             Right ps -> Right $ accumPerms ps

accumPerms permstr = peval $ foldl acc perm permstr
    where acc ps 'c' = ps `pallow` perm_create
          acc ps 'r' = ps `pallow` perm_retrieve
          acc ps 'u' = ps `pallow` perm_update
          acc ps 'd' = ps `pallow` perm_delete

defaultAdminPerms = DirAuth perm perm


    
initAdminDirectories adminPass = do
  createImpl userpath defaultAdminPerms defaultPrincipalSchema  
  key <- liftIO $ generateDepotKey
  liftIO $ writeDepotKey key
  state <- ST.get
  ST.put state{depotKeyAS = key}
  createImpl noncepath defaultAdminPerms defaultNonceSchema
  insertAdmin adminPass
  return key

writeDepotKey :: Word64 -> IO ()
writeDepotKey key = do
  h <- openBinaryFile (userpath /// ".depot-key") WriteMode
  put h (toInteger key)
  hClose h
         
readDepotKey :: IO Word64
readDepotKey = do
  h <- openBinaryFile (userpath /// ".depot-key") ReadMode  
  k <- get h
  hClose h
  return (fromInteger k)


-- writes the admin password to the /com/snepo/auth/users store
insertAdmin pass = do
  (h,entry)     <- assertExists (readDepotDirectory) userpath 
  (_,_,_,_,scm) <- getQueryState "user-name" entry
  state         <- ST.get
  let adstr     = show Admin
      key       = depotKeyAS state
      pass64    = encryptPassword key pass
      meta      = Map.singleton "user-name" adstr
      idx       = indexToInteger scm adstr
  entry'        <- btreeInsert (addIndex idx "user-name")
                               (writeData meta (UserAuth Admin pass64))
                               entry
  updateDirCache userpath (h,entry') 



