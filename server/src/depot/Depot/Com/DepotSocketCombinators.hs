{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

module Depot.Com.DepotSocketCombinators where
     
import Depot.Base
import Depot.Com.QueryParser    
import Depot.Com.Protocol
import Depot.Error
import Depot.IO.Binary
import Depot.IO.BPlusIO
import Depot.IO.OverflowIO
import Depot.Record.DataStore
import Depot.Sec.Auth
import Depot.Record.BPlusGenIndex
import qualified Depot.Logging as LOG

import Data.Map as Map
import Data.List as List
import Data.Set as Set
import Data.Char
import Data.Typeable
import System.Directory
import System.IO
import System.IO.Unsafe
import System.Time
import System.Locale
import Control.Monad.Trans
import Debug.Trace

import Numeric

import Control.Monad.Error
import Control.Monad
import qualified Control.Monad.State as ST

#if defined(DEMO)
demoLimit = 20 :: Integer
#endif

-- Assertions --------------------------------------------------------
-- short and simple way to look for data that should fucking be there,
-- complete with matched up HTTP error codes on failure.

-- asserts the existance of a directory, otherwise returns a 404
assertExists readDir path = do
    state <- ST.get
    let dirs = pageCachesAS state
    case Map.lookup path dirs of
      Just entry -> updateDirCache path entry >> success entry
      Nothing    -> do
        result  <- liftIO $ readDir path
        case result of 
          Left err    -> failure $ NotFound err []
          Right entry -> updateDirCache path entry >> success entry

-- checks that there are no existing repositories under the passed
-- path. Returns the path itself on success.
assertNotExist path = do 
    state <- ST.get
    let dirs = pageCachesAS state
    case Map.lookup path dirs of
      Nothing    -> do
        exists <- liftIO $ doesDirectoryExist path
        if exists then failExists else success path
      Just entry -> failExists 
    where
      failExists = failure $ Conflict ("Path " ++ path ++ " already exists") []

assertDeletedDirectory path = do
  deleted <- liftIO $ doesDirectoryExist (deleteNameFor path)
  if deleted then success () else failure $ Conflict ("Path " ++ path ++ " is not restorable") []
     
-- Can be used for any <<Map String String>> but is used for looking
-- up HTTP headers in a request and ensuring their presence.
assertHeader hvars name = 
    assertInCollection (Map.lookup) name hvars (BadRequest ("Bad request no " ++ name ) [])               

-- more general purpose assertion that a value exists in a Map
assertInMap name amap =
    assertInCollection (Map.lookup) name amap (ServerError 
                                               ("Couldn't find key '" 
                                                ++ name ++ "' in map") [])

assertInList name alist = 
    assertInCollection (List.lookup) name alist (ServerError 
                                                 ("Couldn't find '" 
                                                  ++ name ++ "' in list") [])

assertCryptLessThan a b = 
    case a `compare` b of
      LT -> success ()
      _  -> failure $ BadRequest ("Exceeded max length of " ++ 
                                  show b ++ " for encrypted strings") []

assertInCollection f k c err = 
    case f k c of 
      Nothing -> failure err
      Just v  -> success v

assertHeaderOr hvars name option = 
    case Map.lookup name hvars of
      Nothing -> success option
      Just v  -> success v

#if defined(DEMO)
assertDemoLimitNotExceeded entry =
    let errmsg = "There is a maximum of " ++ show demoLimit ++ 
                 " entries per directory with a demo license"
    in if numEntriesBE entry > demoLimit
          then failure $ ServerError errmsg []
          else success ()
#endif

-- get information about the principal from the data store
assertPrincipal path meth hvars = 
    case Map.lookup "authorization" hvars of
       Nothing   -> success Anonymous
       Just auth -> do
         case parseHTTPAuth auth of
           Left err       -> failure $ BadRequest ("Bad Authorization header: " ++ show err) []
           Right httpauth -> do
               (challenge,n) <- liftIO $ challengeHeader path
               incrementNonceCount n (0::Integer)
               let name = usernameHA httpauth
               state  <- ST.get
               result <- getPrincipal (depotKeyAS state) name
               case result of 
                 -- No principal matching the sent username, this is
                 -- pretty suss
                 Nothing            -> failure $ UnAuthorized ("Unauthorized, bad user name: " ++ name) challenge
                 Just (princ,pass) -> do
                      let h1       = ha1 name (realmHA httpauth) pass
                          h2       = ha2 meth (uriHA httpauth)
                          cliresp  = responseHA httpauth
                          clinc    = ncHA httpauth
                          cnonce   = cnonceHA httpauth
                          qop      = qopHA httpauth
                          nce      = nonceHA httpauth
                      result   <- getNonce nce
--                       LOG.info $ "H1 (" ++ name ++ ":" ++ (realmHA httpauth) ++ ":" ++ pass ++ ")"
--                       LOG.info $ "H2 (" ++ meth ++ ":" ++ (uriHA httpauth) ++ ")"
--                       LOG.info $ "RE (" ++ h1 ++ ":" ++ nce ++ ":" ++ clinc ++ ":" ++ cnonce ++ ":" ++ qop ++ ":" ++ h2 ++ ")"
                      clincint <- hexStrToInteger clinc
                      case result of
                        Nothing         -> failure $ UnAuthorized "Nonce not found" 
                                                    ([("Stale","true")] ++ challenge)
                        Just (nonce,nc) -> 
                         if nc > clincint && nonce == nonceHA httpauth
                           then failure $ UnAuthorized ("Nonce count is too low, could be a replay") 
                                                     ([("Stale","true")] ++ challenge)
                                --- TODO, cache chkresp in the application state keyed by cnonce
                                --- that way costly user lookups and hashing can be avoided
                           else let chkresp = generateResponse h1 nonce clinc cnonce qop h2 in
                              if chkresp == cliresp
                                 then incrementNonceCount nonce nc >> success princ
                                 else failure $ UnAuthorized ("Hash didn't match") challenge
                      
       
debugperms = peval $ perm_create `pallow` perm_retrieve `pallow` perm_delete


-- TODO, detect when more than one login attempt has been made or 
-- if the user is authorised as someone other than Anonymous. Then
-- give a PermissionDenied type error rather than just challenging
-- for a re-authorisation. 
assertAuthorized :: String -> Map String String -> 
                    String -> Permission -> (DirAuth -> Permission) -> 
                              ErrorStateIO DepotError (Handle, BTreeEntry)
assertAuthorized path hvars method action permgetter = do
  (h,entry) <- assertExists (readDepotDirectory) path
  principal <- assertPrincipal path method hvars
  case principal of
    -- super user can do anything, regardless of restrictions.
    Admin -> success (h, entry)
    _     -> do
      dir_auth  <- getPerms path principal
#if defined(DEBUG)                   
      let entryperms = debugperms
#else
      let entryperms = permgetter dir_auth 
#endif
      case authorize action entryperms (h, entry) of 
        Allowed a -> success a
        Denied    -> case principal of
                       Anonymous -> do
                         (chead,n) <- liftIO $ challengeHeader path
                         incrementNonceCount n (0::Integer)
                         failure $ UnAuthorized unauthmsg chead
                       User u    -> -- untested
                         failure $ Forbidden ("User " ++ u ++ 
                                              " does not have permission" ++
                                              " to perform this task") []
    

-- The pathModifier argument is a String -> String function that is
-- for handling special directives such as GET_PERM that authorize
-- against one directory while retrieving another. E.g. a GET_PERM on
-- /larry/anus authorizes against /larry/anus but uses the
-- /com/snepo/auth/dirs/larry/anus directory
assertAdmin path method hvars pathModifier =  do
#if defined(DEBUG)                   
     LOG.debug "auto performing admin tasks (DEBUG MODE)"
     assertExists (readDepotDirectory) path
#else 
       principal <- assertPrincipal path method hvars
       case principal of 
         Admin -> assertExists (readDepotDirectory) (pathModifier path)
         _     -> failure $ Forbidden ("Must be an administrator to perform this task") []
#endif


unauthmsg = "Either you are not logged in or "       ++
            "you do not have permission to perform " ++
            "the action you attempted"
            
challengeHeader path = do 
  let realm = if path == "." then "/" else "/" ++ path
  (ch,n) <- generateChallenge realm
  return ([("WWW-Authenticate",ch)],n)


assertQueryExpParse keystr = 
    case parseQueryExp keystr of
      Left err  -> failure (BadRequest (show err) [])
      Right exp -> success exp


acceptsChunkEncoding hvars = do
  client <- assertHeaderOr hvars "x-snepo-client" "Other"
  let (name,ver) = span (\c -> c /='/') client
  case name of
    "Ajax Depot Client" -> success True
    "Flash Depot Client" -> success True
    -- assume it's a web browser
    other                -> success True

----------------------------------------------------------------------






-- Result builders ---------------------------------------------------
                      
-- surrounds dir names in <dir></dir>                       
dirxml children = 
    foldl (\a b ->
           a ++ 
              "<sub-directory>"                           ++
                 "<meta-data>"                            ++
                   "<object-name>"++ b ++"</object-name>" ++ 
                 "</meta-data>"                           ++
              "</sub-directory>") "" children


clientDataBlock hvars id = do
  own <- assertHeaderOr hvars "x-owner" "nobody"
  cti <- assertHeaderOr hvars "x-creation-date" timestamp
  nam <- assertHeaderOr hvars "x-object-name" "object"
  cty <- assertHeaderOr hvars "content-type" "text/plain"
  return $ Map.fromList [("X-Object-Id",show id),
                         ("X-Owner",own),
                         ("X-Creation-Date",cti),
                         ("X-Object-Name",nam),
                         ("Content-Type",cty)]  

objectDataBlock hvars id = do
  cty    <- assertHeader hvars "content-type"
  idname <- assertHeader hvars "x-id-name"
  return $ Map.fromList [("X-Id-Name", idname),
                         ("Content-Type", cty),
                         ("X-Creation-Date",timestamp)]
              

  
timestamp = unsafePerformIO $ do
              clock  <- getClockTime
              cal    <- toCalendarTime clock
              return (formatCalendarTime defaultTimeLocale "%Y%m%d%H%M" cal)
              


cueRead (DatHdl h) (BPtr _ off) = do hSeek h AbsoluteSeek $ toInteger off
                                     return h
cueRead DatHdlNil _ = error "Nil handle when trying to cue data read"
cueRead _ BNilPtr   = error "Nil Block Pointer when trying to cue data read"

startResult h  = writeChunk h "<?xml version='1.0'?><depot-result>" >> LOG.debugIO "<depot-result>"
endResult h nt = writeChunk h "</depot-result>"  >> endChunked h nt >> LOG.debugIO "</depot-result>"
startEntry h   = writeChunk h "<entry>" >> LOG.debugIO "<entry>"
endEntry h     = writeChunk h "</entry>" >> LOG.debugIO "</entry>"
            

endChunked h True  = hPutStr h "1\r\n\0\r\n0\r\n"
endChunked h False = hPutStr h "0\r\n"

writeChunk h ""    = return () 
writeChunk h chunk = 
             let len = length chunk
                 lhx = showHex len ""
                 dat = lhx ++ "\r\n" ++ chunk ++ "\r\n"
             in hPutStr h dat

  

readChildren chandle path = do
  children <- readChildPaths path
  let xml = dirxml children   
  writeChunk chandle xml
  LOG.debugIO xml

readDirectoryMetaData chandle path entry = do
  let xml = "<directory><meta-data>" ++
            "<object-name>" ++ path ++ "</object-name>" ++
            "<entries>" ++ show (numEntriesBE entry) ++ "</entries>" ++
            Map.foldWithKey (m2x) "" (metaDataBE entry) ++               
            "</meta-data></directory>"
      m2x k v acc = "<" ++ k ++ ">" ++ v ++ "</" ++ k ++ ">" ++ acc
  writeChunk chandle xml 
  LOG.debugIO xml
 

readMeta chandle dhandle = do 
  meta <- get dhandle
  writeChunk chandle (metaToXml meta)
  LOG.debugIO $ metaToXml meta
          
readData iscdata schema chandle dhandle = do
  let prefix = "<data>" ++ (if iscdata then "<![CDATA[" else "")
      suffix = (if iscdata then "]]>" else "") ++ "</data>"
  writeChunk chandle prefix 
  len <- get dhandle :: IO Integer
  readDataFrom schema len dhandle chandle 
  writeChunk chandle suffix 
  LOG.debugIO "<data>...</data>"

readDataFrom schema len dhandle chandle = 
  let valtype = valSC schema in
  case valtype of
    SVStr  -> do      
      hPutStr chandle (showHex len "")
      hPutStr chandle "\r\n"
      rawHandleCopy len dhandle chandle
      hPutStr chandle "\r\n"
    SVInt  -> do
      i <- get dhandle :: IO Integer
      writeChunk chandle (show i)
    SVAuth -> do
      au <- get dhandle :: IO DirAuth
      writeChunk chandle (show au) 
    SVPri -> do
      pr <- get dhandle :: IO Principal
      writeChunk chandle (show pr) 


each xs f   = mapM_ f xs              

readChildPaths path = do
  dirs      <- getDirectoryContents path
  let paths = List.filter (nodots) dirs
      root  = case path of "." -> "/"; _ -> "/" ++ path
  depotdirs <- filterOutNonDepotDirs paths []
  return (List.map (\p -> root /// p) depotdirs)
      where
        nodots (p:_) = p /= '.'
        filterOutNonDepotDirs [] acc = return $ reverse acc
        filterOutNonDepotDirs (p:paths) acc = do
              isdepotdir <- doesDepotDirExist (path /// p)
              currdir <- getCurrentDirectory
              if isdepotdir 
                 then filterOutNonDepotDirs paths (p:acc)
                 else filterOutNonDepotDirs paths acc


doesDepotDirExist path = liftIO $ doesFileExist (path /// "idx/dir.schema")

-- System directory access -------------------------------------------



{-# INLINE getQueryState #-}
getQueryState idxname entry = do  
  (root,handle,order,header,schema) <- assertInMap idxname (idxRootsBE entry)
  return (root,handle,order,header,schema)

getAnyQueryState entry = 
  let roots      = idxRootsBE entry
      size       = Map.size roots
      (minkey,_) = Map.findMin roots
  in if size > 0 
        then getQueryState minkey entry
        else failure (ServerError "empty root map" [])

-- 9595787978659595 <=> __ANON__
indexToInteger :: Schema -> String -> Integer
indexToInteger (Schema _ SStr _ _) x = read $ foldl (\acc a -> show (ord a) ++ acc) "0" x
indexToInteger (Schema _ SHex _ _) x = 
    case readHex x of
      (n,""):_   -> n
      _          -> error ("fuck~!!!!!!! bad parse of " ++ x)
indexToInteger (Schema _ SInt _ _) x = read x

dirpath   = "com/snepo/auth/dirs" -- then suffix this with the path name
userpath  = "com/snepo/auth/users"
noncepath = "com/snepo/auth/nonces"

getPerms path principal = do
   let nam   = uname principal
       path' = dirpathfor path
   arbitraryGet (permHandler) path' "user-name" nam
     where
       permHandler h (Found [x]) = do
                 seekPtr h x
                 -- TODO add something to meta data that allows us to skip it
                 get h :: IO (Map String String) -- consume meta-data
                 get h :: IO Integer             -- consume sizeof perms
                 ps   <- get h :: IO DirAuth
                 return ps 
       
       permHandler _ (Found ptrs)      = error $ "[getPerms] Currupt user store: Multiple entries under this username at ptrs: " ++ show ptrs
       permHandler _ SelectionNotFound = return $ DirAuth perm perm

    
getPrincipal key name = arbitraryGet (princHandler) userpath "user-name" name
    where
      princHandler h (Found [x]) = do
        seekPtr h x
        meta   <- get h :: IO (Map String String)
        let uname = meta ! "user-name" 
        get h :: IO Integer
        (UserAuth prince ws) <- get h 
        return $ Just (prince, decryptPassword key ws)
      princHandler _ SelectionNotFound = return Nothing

getNonce nonce = arbitraryGet (nonceHandler) noncepath "nonce-count" nonce
    where
      nonceHandler h (Found [x]) = do
        seekPtr h x
        nce <- liftIO $ get h :: IO (Map String String)
        let nceval = nce ! "nonce"
        get h :: IO Integer
        nc    <- get h :: IO Integer
        return $ Just (nceval,nc)
      nonceHandler _ SelectionNotFound = return Nothing


arbitraryGet f path keyname key = do
  (h,entry)          <- assertExists (readDepotDirectory) path
  rhoh@(_,_,_,_,scm) <- getQueryState keyname entry
  let DatHdl dathdl = datHdlBE entry
  result <- liftIO $ selectBT (f dathdl) (indexToInteger scm key) rhoh
  return result



-- TODO alter this to be called arbitraryPut and adjust its interface
-- to mimic arbitraryGet
btreeInsert idxfun datfun entry  = do
   let dhandle        = datHdlBE entry
       last_write     = case lastWriteBE entry of
                             BNilPtr -> BPtr 0 0
                             p       -> p
   writeResult    <- liftIO $ datfun last_write dhandle
   (dhandle',
    last_write')  <- case writeResult of 
                       Nothing  -> failure (RequestTimeout "Stalled reading data from client" [])
                       Just res -> success res
   entry'         <- idxfun entry last_write
   return entry'{datHdlBE=(DatHdl dhandle'),lastWriteBE=last_write'}
               
 

-- TODO, perhaps put an upper limit on cached entries and pop some off
-- the stack when it gets to be too full. ONLY do this after profiling
-- memory with a shitload of directories loaded.
updateDirCache path entry = do
  state <- ST.get
  LOG.debug "updating directory cache"
  let dirs = Map.insert path entry (pageCachesAS state) 
  ST.put state{pageCachesAS = dirs}
     
removeFromDirCache path = do
  state <- ST.get
  LOG.debug $ "removing " ++ path ++ " from directory cache"
  case Map.lookup path (pageCachesAS state) of
    Nothing    -> return ()
    Just entry -> ST.liftIO $ closeEntry entry      
  let dirs  = Map.delete path (pageCachesAS state)
  ST.put state{pageCachesAS = dirs}
    where
      closeEntry (h,BEntry{datHdlBE=dh,idxRootsBE=roots}) = do
        hClose h                            
        closeDatHdl dh
        mapM_ (\(_,h,_,_,_) -> hClose h) (Map.elems roots)


createPermissionDirFor path = do
    let path'   = dirpathfor path
        idxpath = path'   /// "idx"
    createDirectoryIfMissing True idxpath 
    result     <- createDepotDirectory path' defaultPermSchema True
    case result of
      Left err        -> error ("error [" ++ 
                                show err ++ 
                                "] permission directory for " ++ path)
      Right (h,entry) -> return (h, entry)
        where
          checkforroot "." = ""

dirpathfor path = case path of "." -> dirpath; _ -> dirpath /// path


addIndex idx idx_name entry ptr = do
  let roots          = idxRootsBE entry
      oldnumentries  = numEntriesBE entry
  rhoh              <- getQueryState idx_name entry
  (insertion,rhoh') <- insertBT idx ptr rhoh
  let newroots       = Map.insert idx_name rhoh' roots
  case insertion of
    Updated _    -> return entry{ idxRootsBE   = newroots }
    Inserted _   -> return entry{ idxRootsBE   = newroots, 
                                  numEntriesBE = oldnumentries + 1 }
                     

createImpl path anonPerms schema = do
  result <- liftIO $ createDepotDirectory path schema False
  case result of
    Left err        -> failure (ServerError err []) 
    Right (h,entry) -> do
                 (ph, permentry) <- liftIO $ createPermissionDirFor path
                 (_,_,_,_,scm)   <- getQueryState "user-name" permentry
                 let astr = show Anonymous
                     anon = indexToInteger scm astr
                     meta = Map.singleton "user-name" astr
                 permentry'      <- btreeInsert 
                                      (addIndex anon "user-name")
                                      (writeData meta anonPerms) permentry
                 liftIO $ writeEntry ph permentry'
                 liftIO $ writeEntry h entry
                 updateDirCache path (h,entry)
                 updateDirCache (dirpathfor path) (ph,permentry') 


incrementNonceCount nonce nc = do
  LOG.debug $ "incrementing nonce: " ++ nonce
  n <- hexStrToInteger nonce
  let metadat = Map.fromList [("nonce", nonce)]
  (h, entry) <- assertExists (readDepotDirectory) noncepath
  entry'     <- btreeInsert (addIndex n "nonce-count")
                            (writeData metadat (nc+1)) entry
  updateDirCache noncepath (h,entry')
  liftIO $ writeEntry h entry'


hexStrToInteger nonce = 
    case readHex nonce of
      (n::Integer,""):_   -> success n
      (n::Integer,junk):_ -> failure (ServerError 
                               ("Couldn't parse " ++ nonce ++ 
                                "junk was: " ++ junk) [])
      []           -> failure (ServerError "Couldn't parse nonce at all " [])
                         

userAuthFromClient handle len = do
   raw   <- liftIO $ iterateM (hGetChar handle) len []
   dat   <- parsePostData raw 
   LOG.debug (show dat)
   uname <- assertInMap "user-name" dat
   pass  <- assertInMap "password" dat
   state <- ST.get
   let key = depotKeyAS state
       p64 = encryptPassword key pass
       pr  = principalFromString uname            
   success $ UserAuth pr p64

permsFromClient handle len = do
   raw    <- liftIO $ iterateM (hGetChar handle) len []
   dat    <- parsePostData raw 
   LOG.debug (show dat)
   dperms <- assertInMap "dirs" dat
   fperms <- assertInMap "entries" dat   
   princ  <- assertInMap "user-name" dat   
   success (princ, dperms, fperms)

readUser _ _ _ _  BNilPtr = return ()
readUser hvars chandle dhandle _ ptr = do
  let hasdata = not $ Map.member "x-metadata-only" hvars
  h  <- cueRead dhandle ptr
  startEntry chandle 
  readMeta chandle h 
  when hasdata $ do
    get h :: IO Integer -- consume sizeof
    (UserAuth p _) <- get h :: IO UserAuth
    writeChunk chandle ("<data><user><user-name>" ++ show p ++ 
                     "</user-name><password>****</password></user></data>") 
  endEntry chandle 
             

readPerms _ _ _ _ BNilPtr = return ()
readPerms hvars chandle dhandle _ ptr = do
--  LOG.info "reading perms"
  let hasdata = not $ Map.member "x-metadata-only" hvars
  h  <- cueRead dhandle ptr
  startEntry chandle 
  readMeta chandle h 
  when hasdata $ do
    get h :: IO Integer -- consume sizeof
    dirAuth <- get h :: IO DirAuth
    writeChunk chandle ("<data>" ++ dirAuthToXml dirAuth ++ "</data>") 
  endEntry chandle 
             
dirAuthToXml (DirAuth dir entry) = 
    let dp = permToXml dir
        ep = permToXml entry
    in "<directory-auth>" ++ 
           "<directory-perm>" ++ dp ++ "</directory-perm>" ++ 
           "<entry-perm>" ++ ep ++ "</entry-perm>" ++ 
       "</directory-auth>"
        where
          permToXml (Perm acts) = foldl actTag "" (List.sort (Set.toList acts))
          actTag acc Create   = acc ++ "<perm>CREATE</perm>"
          actTag acc Retrieve = acc ++ "<perm>RETRIEVE</perm>"
          actTag acc Update   = acc ++ "<perm>UPDATE</perm>"
          actTag acc Delete   = acc ++ "<perm>DELETE</perm>"
                                

   
