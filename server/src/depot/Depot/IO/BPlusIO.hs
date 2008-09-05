{-# OPTIONS_GHC -fglasgow-exts #-}
module Depot.IO.BPlusIO
    where

import Data.Map as Map    
import Data.List as List
import Data.Word
import Data.PackedString

import Depot.Base
import qualified Depot.Logging as LOG
import Depot.Sec.Auth
import Depot.IO.Binary
import Depot.IO.BinaryImpl
import Depot.IO.OverflowIO
import Depot.Record.DataStore
import Depot.Record.BTreeSchemaParser

import qualified Control.Monad.State as ST
import Control.Monad

import Control.Exception (try)

import System.IO
import System.Directory

----------------------------------------------------------------------
-- !!!!!!!!!!!!!!!!        TODO        !!!!!!!!!!!!!
-- do a bounds check on ALL seeks. Since they can enlarge a file
-- automatically we don't want to have a corrupt file cause a seek to 
-- some infinitely huge number and scorch somebody's hard disk.
----------------------------------------------------------------------



-- defaults ----------------------------------------------------------
emptyRoot    = Leaf BNilPtr Map.empty BNilPtr BNilPtr
               
-- permissions settings ----------------------------------------------
defaultPerms      = DirAuth defaultDirPerms defaultEntryPerms 
defaultEntryPerms = peval $ perm' Create `pallow` perm' Retrieve
defaultDirPerms   = peval $ perm' Create `pallow` perm' Retrieve


-- schemas -----------------------------------------------------------
-- A very nice thing to do when creating a directory would be to
-- analyse a schema and adjust things like block size and order to
-- both the schema and the machine architecture so that there are less
-- cache misses.

defaultSchema          = let n = "\n" in 
                         "-- default data schema" ++ n ++
                         "X-Object-Id:int:str:100"   ++ n 
--                "Content-Type:str:[ptr]:30" ++ n ++
--                "X-Owner:str:[ptr]:4"       ++ n ++
--                "X-Object-Name:str:[ptr]"   ++ n ++
--                "X-Creation-Date:int:[ptr]" ++ n

defaultPermSchema      = let n = "\n" in 
                         "-- default permissions schema" ++ n ++
                         "user-name:str:auth:50"          ++ n 
defaultNonceSchema     = let n = "\n" in 
                         "-- default nonce schema" ++ n ++
                         "nonce-count:hex:int:50"    ++ n 

defaultPrincipalSchema = let n = "\n" in 
                         "-- default principal schema" ++ n ++
                         "user-name:str:principal:20"    ++ n 

defaultSchemaName      = "dir.schema"
entryFormatVersion     = 0x01 :: Int

----------------------------------------------------------------------


-- Directory Creation ------------------------------------------------

open_header path name = let path' = path ++ "/idx/" ++ name ++ "/idx.hdr" in do
                        h   <- openBinaryFile path' ReadMode
                        hdr <- get h
                        hClose h
                        return (name, hdr)


createDepotDirectory path schemaStr force = do
  let datpath = datFileName path 0 -- check for first dat file
  exists <- doesFileExist datpath
  if exists && force  
     then return $ Left ("Directory already exists at " ++ path) 
     else do
           createDirectoryIfMissing True path
           createDirectoryIfMissing True $ path /// "idx"
           createDirectoryIfMissing True $ path /// "dat"
           let schemaPath = path /// "idx" /// defaultSchemaName
           exists' <- doesFileExist schemaPath
           schema  <- if exists' 
                         then parseSchema schemaPath 
                         else do
                               writeSchema schemaPath schemaStr
                               return (parseSchemaStr schemaStr)
           case schema of
             Left err   -> return $ Left (show err)
             Right scms -> let indexes   = List.map (scmidxs) scms 
                               indexhdrs = List.map (scmhdrs) scms
                               indexords = List.map (scmords) scms
                               basedir   = path /// "idx"
                           in do
                           mapM_ (createdirs basedir) scms
                           hdls <- mapM (createidxs basedir) scms                           
                           h    <- getDatFileHandleFor1 path 0
                           mapM_ (puthdr) (zip hdls indexhdrs)
                           let complete = List.map (idxmap)  (zip5 indexes hdls indexords indexhdrs scms)
                           let entry =  BEntry { versionBE     = entryFormatVersion,
                                                 pathBE        = path,
                                                 lastOidBE     = 0,
                                                 datHdlBE      = DatHdl h,
                                                 lastWriteBE   = BNilPtr,
                                                 idxRootsBE    = Map.fromList complete,
                                                 numEntriesBE  = 0,
                                                 metaDataBE    = Map.fromList [("description","depot directory")]
                                               }
                           entryHandle <- openBinaryFile (path /// "idx/dir.entry") ReadWriteMode
                           put entryHandle entry
                           return $ Right (entryHandle, entry)
      where
        scmidxs (Schema name _ _ _)         = emptyRoot
        scmhdrs (Schema name _ _ _)         = let sanity  = "**** Snepo Depot B+-Tree Index file ****\n"
                                                  rootptr = (BPtr 1 0)
                                                  freeptr = (FB (BPtr 2 0) [])
                                                  hdr     = (BP sanity 0x01 freeptr rootptr)
                                              in
                                                hdr
        -- write out the header for an index in the first block of its
        -- index file TODO move the header to its own file so that the
        -- free stack can be arbitrarily large
        puthdr (hdl,hdr)                    = do hSeek hdl AbsoluteSeek 0
                                                 put hdl hdr
                                                 -- type signature is meaningless
                                                 -- here because this is writing 
                                                 -- empty lists.
                                                 writeNode hdl (emptyRoot::IdxInt) (BPtr 1 0) (FB (BPtr 2 0) [])
                                                 hFlush hdl
        scmords (Schema name _ _ ord)       = ord
        createdirs path (Schema name _ _ _) = createDirectoryIfMissing True (path /// name)
        createidxs path (Schema name _ _ _) = let path'  = path /// name /// "idx.bptree" in
                                              do h <- openBinaryFile path' ReadWriteMode
                                                 return h
        idxmap (idx, hdl, ord, hdr, scm)     = (nameSC scm,(idx,hdl,ord,hdr,scm))

schemaMap scms = Map.fromList $ List.map (\s -> (nameSC s, s)) scms

readDepotDirectory path = do
  let schemaPath = path /// "idx" /// defaultSchemaName
  exists <- doesFileExist schemaPath
  if (not exists)
     then return $ Left ("No depot directory exists at path " ++ path ++ 
                         ". Maybe there is a missing schema file?")
     else do
           schema <- parseSchema schemaPath
           case schema of 
             Left err   -> return $ Left (show err)
             Right scms -> do
                            values       <- mapM (readIndex) scms
                            entryHandle  <- openBinaryFile (path /// "idx/dir.entry") ReadWriteMode
                            version      <- get entryHandle
                            oid          <- get entryHandle
                            datfile      <- get entryHandle
                            lastwrite    <- get entryHandle
                            numEntries   <- get entryHandle
                            metaData     <- get entryHandle
                            dathandle    <- openBinaryFile datfile ReadWriteMode
                            let entry = BEntry { versionBE     = version,
                                                 pathBE        = path,
                                                 lastOidBE     = oid,
                                                 datHdlBE      = DatHdl dathandle,
                                                 lastWriteBE   = lastwrite,
                                                 idxRootsBE    = Map.fromList values,
                                                 numEntriesBE  = numEntries,
                                                 metaDataBE    = metaData }
                            return $ Right (entryHandle, entry)
    where
      -- TODO merge this with openRoot as this is a bit redundant
      readIndex scm = do
        let name = nameSC scm
        (handle, root, header,order) <- openRoot path scm name
        return (name,(root,handle,order,header,scm))



-- NOTE REGARDING PERMS.. perhaps it would be a goood idea to assign
-- permissions on a per index basis (and per entry) rather than a
-- per-directory basis. That way people could have access to things
-- like names and creation dates but not OIDs (for example).


closeDirMap dmap = do mapM_ (\(h,entry) -> 
                                 do hClose h; closeEntryHandles entry) (Map.elems dmap)
                                                     

closeEntryHandles entry = do
  LOG.debugIO "closing entry...."
  closeDatHandle (datHdlBE entry)
  mapM_ (\(_,h,_,_,_) -> hClose h) (Map.elems (idxRootsBE entry))
    where
      closeDatHandle (DatHdl h) = hClose h
      closeDatHandle _          = return ()
                                  
writeEntry entryHandle entry = do
  hSeek entryHandle AbsoluteSeek 0
  put entryHandle entry
  -- always flush after writing out a page entry
  hFlush entryHandle 

writeSchema path schema = do
  h <- openBinaryFile path WriteMode
  hPutStr h schema
  hClose h  

                      
openRoot path schema name = 
    let 
        path' = path /// "idx" /// name /// "idx.bptree"
    in do
        handle <- openBinaryFile path' ReadWriteMode
        header <- get handle             -- read in header from first block
        LOG.debugIO $ "before root read of " ++ name ++ ": " ++ (show (rootPtrBPH header))
        root   <- readNode handle (rootPtrBPH header)
        LOG.debugIO "after root read"
        return (handle,root,header, orderSC schema)
   

-- For rebuilding the index files in a directory. This deletes
-- existing indexes but leaves the dat file untouched and hands a
-- newly created directory entry to the passed function
-- TODO finish this --------------------------------------------------
rebuildIndexes f path = do
   removeDirectoryRecursive (path /// "idx")
   result <- createDepotDirectory path defaultSchema True
   case result of 
     Right (eh, entry) -> f eh entry
     _ -> return ()
   return result

