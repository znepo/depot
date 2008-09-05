{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
module Depot.Com.BPSocketInterface (
        getObject,
        getDirectorySize,
        putObject,
        putCreateDirectory,
        putUser,
        putPerm,
        deleteObject,
        deleteDirectory,
        destroyDirectory,
        restoreDirectory
  ) where

-- BPlus Index Socket Interface --------------------------------------

-- This module exports top level functions for manipulating the values
-- in a directory that is btree indexed. I now realise that this is
-- probably the wrong way to look at things and the btree
-- implementation should probably be abstracted a bit farther
-- away. such is life, I'm still learning how to handle abstractions
-- in Functional Programming in general and Haskell in particular.

-- all top level functions have the signature: 
-- 
-- topLevel :: (MonadIO (ErrorT DepotError m), Monad m) =>
-- 	        Handle -> Path -> BTreeCache -> UVars -> HVars 
--                                -> ErrorT DepotError m BTreeCache
--
-- Each of the top level functions implements the monad from
-- Depot.Error. This allows for various assertions (such as
-- header, index, and url variable lookups). These can be dealt with
-- in a monadic fashion to avoid nests of case statements.

-- e.g.
-- cthulhu <- assertHeader hvars "r'yleh"
-- hagbard <- assertHeader hvars "ftagn"
-- return (cthulu,hagbard)
-- 
-- rather than
-- case lookup "r'yleh" hvars of
--   Nothing -> ServerError "NO CTHULHU"
--   Just c  -> case lookup "ftagn" hvars of
--                 Nothing -> ServerError "NO HAGBARD"
--                 Just h  -> return (c,h)


-- TODO
--
-- Define a system for LIMIT headers. This should work like a range in
-- that 1..3 should limit the results to those from 1 to 3. However sending
-- a scalar value should limit the results to 0..LIMIT

-- The whole business of returning success ok is a but fucked as
-- well... Need to take another look at the monad
-- transformers. Perhaps check out the thing that Bulat Z. mentioned
-- in Haskell-Cafe about removing all the liftIO statements.
----------------------------------------------------------------------


import Depot.Base
import Depot.Error
import Depot.Com.Protocol
import Depot.Com.QueryParser
import Depot.Record.DataStore
import Depot.Record.BPlusGenIndex
import Depot.Com.DepotSocketCombinators    
import Depot.IO.BPlusIO
import Depot.Sec.Auth
import Depot.Sec.BPAuthInterface
import Depot.Logging as LOG
    
import Data.Map as Map
import Data.List as List
    
import System.IO
import System.Directory

import GHC.Base

import Debug.Trace

import Prelude hiding (catch)

import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.State as ST
import Control.Exception
import Control.Monad.Error hiding (when)



ok = Ok "OK" [("Content-Type","text/xml")]

--- Through a happy chance of fate the following happens when a user
--- is retrieved: the UserAuth value consists of a string followed by
--- an array of word64's. Since a Principal is serialised as a string
--- the first byte of the UserAuth record is the string length. Since
--- the data from a normal "get" is retrieved as a string only the
--- user name is returned. 

getDirectorySize handle path uvars hvars = do
  (h, entry) <- assertAuthorized path hvars "GET" perm_retrieve (dirDA)
  let dirsize = show $ numEntriesBE entry
      dirxml  = "<depot-result><entry><meta-data><directory-size>" ++ dirsize ++ 
                "</directory-size></meta-data><data>"++ dirsize ++"</data></entry></depot-result>"
      status  = (Ok dirxml  [("X-Directory-Size",dirsize)])
  liftIO $ writeStdResponse hvars handle status
  success ok

getObject handle path uvars hvars = do
  -- dirty quick hack here. fixme when we refactor getObject  
  (h,entry)   <- case Map.lookup "x-snepo-directive" hvars of 
                   Just "GET_PERM" -> assertAdmin path "GET" hvars dirpathfor
                   _               -> assertAuthorized path hvars "GET" perm_retrieve (entryDA)
  let dhandle     = datHdlBE entry
      ptr         = lastWriteBE entry
      nullTerm    = case Map.lookup "x-null-terminated" hvars of Nothing -> False; _ -> True
      servererr e = return (failure $ ServerError ("Server error: " ++ show e) []) 
      -- assign base data reader and handles
      dataReader  = case Map.lookup "x-snepo-directive" hvars  of
                      Just "GET_USER"  -> readUser hvars handle dhandle 
                      Just "GET_PERM"  -> readPerms hvars handle dhandle 
                      _                -> readEntry hvars handle dhandle 
      -- select from the directory and stream data down the client pipe
      runSelect   =  case uvars of 
                      [] -> do
                         rhoh@(_,_,_,_,scm) <- getAnyQueryState entry                         
                         let handler         = selectHandler (dataReader scm) handle 
                         res  <- liftIO $ selectAllBT handler rhoh
                         liftIO $ readDirectoryMetaData handle path entry 
                         liftIO $ readChildren handle path 
                         res
                      [(k,x)]  -> do
                        let keyname = keyNameFor k
                        rhoh@(_,_,_,_,scm) <- getQueryState keyname entry
                        queryExp           <- assertQueryExpParse x
                        let -- compose index function and selection handler
                            idxf            = indexToInteger scm
                            handler         = selectHandler (dataReader scm) handle 
                        case queryExp of
                          Scalar k   -> do
                            res <- liftIO $ selectBT (handler True) (idxf x) rhoh
                            res
                          _          -> do                                        
                            res <- liftIO $ selectExpBT handler idxf rhoh queryExp
                            res
                      _ -> failure (NotImplemented "Query style not implemented" [])
  LOG.debug "Before runSelect"
  runSelect 
  LOG.debug "After runSelect"
  liftIO $ endResult handle nullTerm
  success (Ok "" [("Content-Type","text/xml")])

-- TODO add other header tags such as Content-Type and 
-- X-Creation-Date when they become indexable
keyNameFor k = case k of
                 "id" -> "X-Object-Id"
                 _    -> k


selectHandler :: (BlockPtr -> IO ()) -> Handle -> Bool ->
                 Selection BlockPtr -> IO DepotErrorState
selectHandler dataRead chandle isfirst result = 
    case result of 
      EmptySelection -> do
--        let resp = "<depot-result></depot-result>"
        printHeader chandle 
--        printContentLength chandle (length resp)
--        printEmpty chandle False
        return (success ok)
                  
      Incomplete ptrs _ -> do 
        when isfirst $ printHeader chandle 
        each ptrs dataRead
        return (success ok)

      Found [BNilPtr] -> do 
        return $ failure (NotFound "Result not found" [])

      Found [ptr] -> do 
        when isfirst $ printHeader chandle 
        dataRead ptr                 
        return (success ok)

      Found ptrs -> do 
        when isfirst $ printHeader chandle 
        each ptrs dataRead 
        return (success ok)
        
      SelectionNotFound   -> do -- TODO add some semantics around this so we can ignore
                                -- SelectionNotFound on comma delimited selections 
        return $ failure (NotFound "Result not found" [])

printContentLength chandle len = do
  hPutStr chandle $ "Content-Length: " ++ show len ++ "\r\n"
  LOG.debugIO $ "Content-Length: " ++ show len ++ "\r\n"

printHeader chandle  =  do
    let chnkhdr = [("Transfer-Encoding","chunked"),("Content-Type","text/xml")]
        resp    = responseLine ok ++ showHeaders (stdHeaders chnkhdr)
    LOG.infoIO "RESPONSE HEADER"
    LOG.infoIO $! "\n" ++ resp
    LOG.infoIO "END RESPONSE HEADER"
    hPutStr chandle $! resp
    startResult chandle 
          

readEntry :: Map String String -> Handle -> DataHandle -> Schema -> BlockPtr -> IO ()
readEntry _ _ _ _ BNilPtr                   = return ()                      
readEntry hvars chandle dhandle schema ptr = do
    let hasdata = not $ Map.member "x-metadata-only" hvars
        iscdata = not $ Map.member "x-retrieve-as-xml" hvars
    h  <- cueRead dhandle ptr
    startEntry chandle
    readMeta chandle h
    when hasdata (readData iscdata schema chandle h)
    endEntry chandle 

printEmpty chandle = startEntry chandle >> endEntry chandle


maxCryptStrLen = 64 :: Int


putObject handle path uvars hvars = do
   LOG.debug "putting raw data"
   let meth         = methodString hvars "PUT"
   (h,entry)       <- assertAuthorized path hvars meth perm_create (entryDA)
   len             <- assertHeader hvars "content-length"   
   let (id,entry')  = getInsertId entry uvars
   meta            <- clientDataBlock hvars id
#if defined(DEMO)
   assertDemoLimitNotExceeded entry
#endif
   entry''  <- btreeInsert (addIndex id "X-Object-Id") 
                           (writeDataStream (read len) meta handle) entry'
   liftIO $ hFlush handle
   liftIO $ writeEntry h entry''
   updateDirCache path (h,entry'') 
   let status = (Created "added entry" [("X-Object-Id",show id)])
   liftIO $ writeStdResponse hvars handle status              
   return status
       

-- TODO refactor this and perms into "put data structure"
putUser handle _ uvars hvars = do
  let path = userpath
      meth = methodString hvars "PUT"
  LOG.debug "putting user"
  (h, entry)    <- assertAuthorized path hvars meth perm_create (entryDA)
  (_,_,_,_,scm) <- getQueryState "user-name" entry
  len           <- assertHeader hvars "content-length"
  userauth      <- userAuthFromClient handle (read len)
  let namestr    = show (princeUA userauth)
      idx        = indexToInteger scm namestr
      meta       = Map.singleton "user-name" namestr
  entry'        <- btreeInsert (addIndex idx "user-name")
                                  (writeData meta userauth) entry
  updateDirCache path (h, entry')
  liftIO $ writeEntry h entry'
  liftIO $ writeStdResponse hvars handle ok 
  return ok


putPerm handle p uvars hvars = do
  let path = dirpathfor p
      meth = methodString hvars "PUT"
  LOG.debug "putting permission"
  (h, entry)    <- assertAuthorized path hvars meth perm_update (entryDA)
  (_,_,_,_,scm) <- getQueryState "user-name" entry
  len           <- assertHeader hvars "content-length"
  (name,
   dirPerms,
   filePerms)   <- permsFromClient handle (read len)
  let meta       = Map.singleton "user-name" name
      idx        = indexToInteger scm name
  perms         <- parseDirAndFilePerms dirPerms filePerms
  entry'        <- btreeInsert (addIndex idx "user-name")
                   (writeData meta perms) entry
  liftIO $ writeEntry h entry'
  updateDirCache path (h,entry')
  liftIO $ writeStdResponse hvars handle ok
  return ok
      where
        parseDirAndFilePerms dir file = 
            case permsFromString "dir permissions" dir of
              Left err -> failure $ BadRequest (show err) []
              Right dp -> case permsFromString "file permissions" file of
                            Left err -> failure $ BadRequest (show err) []
                            Right fp -> success $ DirAuth dp fp

-- the implementation of delete object is a hack, although I think it
-- is a very nice one. Instead of trying to actually delete a b-tree
-- entry, and deal with the accompanying hoo-haw of reorganisation,
-- the entry is replaced with a BNilPtr. Nil pointers are ignored when
-- aggregates are returned and treated as 404's otherwise.
--
-- Since the b-tree datastructure is meant to hold fuckloads of data
-- this shouldn't be a problem, there's not a whole hell of a lot of
-- difference between 10,000,000 records and 3,000,000 records as far
-- as search time is concerned. 
-- 
-- A better formalisation of the hack would be to store values in
-- btree leaves that were tagged. e.g. LeafValue a | DeletedValue a
-- this would allow for 1 stage of rollback. Storing a p-queue would
-- work as well, allowing for unlimited rollback but I don't think
-- that's necessary as yet. Certainly worth consideration though.
deleteObject handle path uvars hvars = do
  let meth   = methodString hvars "DELETE"
  LOG.info meth
  (h,entry) <- assertAuthorized path hvars meth perm_delete (entryDA)               
  pk        <- assertInList "id" uvars
  entry'    <- btreeInsert (deleteIndex pk "X-Object-Id") (\a (DatHdl b) -> return (Just (b,a))) entry
  updateDirCache path (h,entry')
  let status  = Deleted "Deleted Entry" [("X-Object-Id",pk)]
  liftIO $ writeStdResponse hvars handle status
  return status

deleteDirectory handle path@(x:_) uvars hvars = do
  -- check for a leading "/", deleteDirectoryRecursive or rename could
  -- be extra bad news on these paths
  when (x == '/') $ failure (UnAuthorized "Cannot delete absolute paths or root directory" [])
  LOG.debug $ "DELETING DIRECTORY " ++ path
  let meth   = methodString hvars "DELETE"
  assertAuthorized path hvars meth perm_delete (dirDA)
  let deleted  = deleteNameFor path
      del_perm = deleteNameFor $ dirpathfor path
  removeFromDirCache path
  removeFromDirCache (dirpathfor path)
  wasDeleted <- liftIO $ doesDirectoryExist deleted
  when wasDeleted (liftIO $ do
    removeDirectoryRecursive deleted
    removeDirectoryRecursive del_perm)
  liftIO $ renameDirectory path deleted
  liftIO $ renameDirectory (dirpathfor path) del_perm
  let status = Deleted ("Deleted directory " ++ deleted) []
  liftIO $ writeStdResponse hvars handle status  
  return status
         

methodString hvars str = 
    case Map.lookup "x-snepo-request-kludge" hvars of
      Nothing   -> str
      Just meth -> "POST" -- default to POST for feeble browsers.


destroyDirectory handle path@(x:_) uvars hvars = do
  when (x == '/') $ failure (UnAuthorized "Cannot destroy absolute paths or root directory" [])
  let meth   = methodString hvars "DELETE"
  assertAuthorized path hvars meth perm_delete (dirDA)
  LOG.debug $ "DESTROYING DIRECTORY " ++ path
  removeFromDirCache path
  removeFromDirCache (dirpathfor path)
  liftIO $ removeDirectoryRecursive path
  liftIO $ removeDirectoryRecursive (dirpathfor path)
  let status = Deleted ("Destroyed directory " ++ path) []
  liftIO $ writeStdResponse hvars handle status
  LOG.debug $ (defaultResponse status)
  return status


restoreDirectory handle path uvars hvars = do
  assertDeletedDirectory path
  assertDeletedDirectory (dirpathfor path)
  parent_path <- findFirstParent path
  checkAuthForDirectoryCreate parent_path hvars
  liftIO $ renameDirectory (deleteNameFor path) path
  liftIO $ renameDirectory (deleteNameFor (dirpathfor path)) (dirpathfor path)
  let status = Created ("Restored directory" ++ path) []
  liftIO $ writeStdResponse hvars handle status
  return status

-- check for deleted directories in "create"

getInsertId entry uvars = 
    let 
        nextOid = 1 + lastOidBE entry
    in case List.lookup "id" uvars of
         Nothing -> (nextOid, entry{lastOidBE=nextOid})
         Just id -> (read id, entry)
  

deleteIndex id idx_name entry _ = do
  let roots            = idxRootsBE entry
      numentries       = numEntriesBE entry
  rhoh@(_,_,_,_,scm)  <- getQueryState idx_name entry
  (_,rhoh')           <- insertBT (indexToInteger scm id) BNilPtr rhoh
  let newroots         = Map.insert idx_name rhoh' roots
  return entry{ idxRootsBE = newroots, numEntriesBE = numentries - 1 }



-- Reindexing functions ----------------------------------------------
-- TODOS include reinserting keys based on schemas, etc... -----------

-- reIndex h entry = do
--    let (DatHdl dhandle) = datHdlBE entry
--    walkData 0 0 dhandle entry
--    writeEntry h entry
--    return (h,entry)
         
-- walkData n pos dhandle entry = do
--    eof <- hIsEOF dhandle
--    if eof 
--       then return entry
--       else do
--             runErrorT $ addIndex n entry (BPtr (fromInteger pos) 0) "X-Object-Id"
--             len <- get dhandle
--             hSeek dhandle RelativeSeek len
--             walkData (n+1) (pos+len) dhandle entry


-- creating directories, have to search parent directories for
-- permissions. in the case that no parent directories exist
-- table creation must be allowed.

putCreateDirectory handle path _ hvars  = do  
  path'       <- assertNotExist path 
  parent_path <- findFirstParent path'
  checkAuthForDirectoryCreate parent_path hvars
  wasDeleted  <- liftIO $ doesDirectoryExist (deleteNameFor path)
  if wasDeleted
     then do
           LOG.info $ "Deleted directory found, restoring /" ++ path' ++ "."
           liftIO $ renameDirectory (deleteNameFor path') path'
           liftIO $ renameDirectory (deleteNameFor (dirpathfor path')) (dirpathfor path')
           liftIO $ writeStdResponse hvars handle (Created path' [])
           success ok
     else do
           -- ensure that the directory is clear before continuing
           removeFromDirCache path' 
           removeFromDirCache (dirpathfor path') 
           liftIO $ removeDirectoryRecursive (dirpathfor path') `catch` (\ignore -> return ())
           createIfNotPresent path'        
           liftIO $ writeStdResponse hvars handle (Created path' [])
           success ok

-- NOTE ON THIS, will have to make a root dir somehow....
-- probably with createImpl "." dirs defaultPerms

createIfNotPresent ""        = do
  exists <- doesDepotDirExist "."
  if exists 
     then success ok 
     else createImpl "." defaultPerms defaultSchema >> success ok
createIfNotPresent path      = do
  let nxt = parentDirFor path
  exists <- doesDepotDirExist path
  if exists 
     then success ok
     else do
           createImpl path defaultPerms defaultSchema
           createIfNotPresent nxt 
                              

findFirstParent ""   = return Nothing
findFirstParent path = do
  let parent = parentDirFor path
  exists <- doesDepotDirExist parent
  if exists then return (Just parent) else findFirstParent parent


checkAuthForDirectoryCreate path hvars = 
    case path of
      Nothing -> success ok 
      Just pp -> do
        LOG.debug $ "looking for directory create perms on " ++ pp
        let meth = methodString hvars "PUT"
        assertAuthorized pp hvars meth perm_create (dirDA)
        success ok
        





                      
