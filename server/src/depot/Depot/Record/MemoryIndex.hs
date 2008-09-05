{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- An implementation of the data store index so that the entire index
-- is stored in memory. The map may be serialised and deserialised
-- (e.g. it is an instance of Binary). The map consists of key values
-- that point to lists of data pointers.
-- 
module Depot.Record.MemoryIndex
    where            

import Data.Map as Map
import Data.List as List
import System.IO
import System.Directory
    
import Depot.Base
import Depot.Record.DataStore    
import Depot.IO.Binary


kTypeInt :: KType -> Int
kTypeInt KString  = 0x01
kTypeInt KInteger = 0x02
                    
intKType 0x01     = KString
intKType 0x02     = KInteger

currentHeaderVersion = 0x01 :: Version

-- Binary Serialisatin for Directory Headers -------------------------

instance Binary DirHeader where
    put h DirHdrNil = put h (-1::Int)
    put h dh        = do
      put h $ dhVersion dh
      put h $ dhLastWrite dh
      put h $ dhLastOid dh
      put h $ dhIndexes dh            
    get h           = do 
      vers <- get h
      if vers == (-1) 
         then return DirHdrNil
         else do
               lastWrite <- get h
               lastOid   <- get h
               len       <- get h :: IO Int
               idxs      <- get h
               return (DirHdr vers lastWrite lastOid idxs)
    sizeOf DirHdrNil = sizeOf x where x = 1::Version
    sizeOf dh        = sizeOf (dhVersion dh) +
                       sizeOf (dhLastWrite dh) +
                       sizeOf (dhLastOid dh) +
                       sizeOf (length (dhIndexes dh)) +
                       sum [sizeOf x | x <- dhIndexes dh]


-- Binary Serialisation for Index Headers ----------------------------
instance Binary IdxHeader where
    put h (IH v kt)  = do put h v
                          put h $ kTypeInt kt
    get h            = do v <- get h :: IO Version
                          t <- get h :: IO Int
                          return (IH v (intKType t))
    sizeOf (IH v kt) = sizeOf x + sizeOf x 
        where x = 1 :: Version


-- Utities -----------------------------------------------------------

-- return the path of the index file store based on the index name and
-- the depot path
idxName path idx = (path ++ "/idx/" ++ idx ++ ".idx")

-- Initialise a depot directory 
-- !!watch rereading indexes for an existing directory
createDepot :: String -> IO DirHeader
createDepot path = 
    do 
      dirhdr <- readDirHeader path
      case dirhdr of
         DirHdrNil -> 
             do 
               createDirectoryIfMissing True path
               createDirectoryIfMissing True (path ++ "/idx")
               createDirectoryIfMissing True (path ++ "/dat")
               createIndex KString "Content-Type"
               createIndex KString "X-Owner"
               createIndex KString "X-Object-Name"
               createIndex KInteger "X-Creation-Date"
               createIndex KInteger "X-Object-Id"
               hdr   <- writeDirHdr path (DirHdr 
                                          (currentHeaderVersion)
                                          (BPtr 0 0)
                                          0
                                          ["Content-Type",
                                           "X-Owner",
                                           "X-Object-Name",
                                           "X-Creation-Date",
                                           "X-Object-Id"])
               return hdr
         dirhdr     -> return dirhdr                                    
    where
      createIndex kt idx    = do h <- openBinaryFile (idxName path idx) WriteMode
                                 put h (IH currentHeaderVersion kt)
                                 case kt of
                                   KInteger -> do put h (Map.empty :: Map Integer BlockPtr)
                                   KString  -> do put h (Map.empty :: Map String  BlockPtr)
                                 hClose h
                                 return idx
                                        
writeDirHdr path hdr   = do h <- openBinaryFile (path ++ "/idx/idx.hdr") WriteMode
                            put h hdr
                            hClose h
                            return hdr

writeIdx path name idx = 
    do h <- openBinaryFile (idxName path name) WriteMode
       put h idx
       hClose h
                                   
-- Return a file handle to the index store for a given path and index
-- name
getIndexHandle path idx = do h <- openBinaryFile (idxName path idx) ReadWriteMode
                             return h

getIndexesAndDatHdls path dirhdr = 
      case dirhdr of
        DirHdrNil -> return ([], DatHdlNil)
        _         -> do
                    idxs <- mapM (readIdxFromPath path) (dhIndexes dirhdr)
                    h    <- openBinaryFile (datFileName path $ bpFile $ dhLastWrite dirhdr) ReadWriteMode                    
                    return (idxs, (DatHdl h))
          where
            readIdxFromPath path idxname = do 
                  h   <- openBinaryFile (path ++ "/idx/" ++ idxname ++ ".idx") ReadMode
                  idx <- get h :: IO (Map String [BlockPtr])
                  hClose h
                  return (idxname, idx)


readIdxAt :: (Binary (Map a b)) => String -> String -> IO (Map a b)
readIdxAt path idxname = do h <- openBinaryFile (path ++ "/idx/" ++ idxname ++ ".idx") ReadMode
                            idx <- get h
                            hClose h
                            return idx


 


readDirHeader path = do
  exists <- doesDirectoryExist path
  if exists 
     then do
           h   <- openBinaryFile (path ++ "/idx/idx.hdr") ReadMode
           hdr <- get h
           hClose h
           return hdr
     else return DirHdrNil
