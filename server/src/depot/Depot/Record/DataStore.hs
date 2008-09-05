--
-- The DataStore module is responsible for the actual
-- disk storage of Depot data. The data types contained within
-- relate to storage of content types and pointer types.
-- 
module Depot.Record.DataStore
    where
      
      
import Depot.Base
import qualified Depot.Logging as LOG
import Depot.IO.Binary

import System.IO
import System.Directory
import Data.Map as Map
import Data.List as List
import Debug.Trace
import Data.Word
import Data.Int
import Control.Monad


-- utils -----------------------------------------------------------------

-- TODO adjust so data files are represented more as block pointers
-- than simple offsets.
writeDataStream :: Integer -> Map String String -> 
                   Handle -> BlockPtr -> DataHandle ->
                             IO (Maybe (Handle, BlockPtr))
writeDataStream len meta chandle ptr@(BPtr _ off) (DatHdl h) = do
  hSeek h AbsoluteSeek $ toInteger off 
  put h meta
  put h len
  LOG.infoIO $ "writing data of len: " ++ show len
  loc1       <- hTell h
  --rawHandleCopyNB len chandle h
  rawHandleCopyWithTimeout len chandle h
  loc       <- hTell h
  LOG.infoIO $ "sanity: " ++ show (loc - loc1)
  if (loc - loc1 /= len) 
     then return Nothing
     else return $ Just (h, ptr{bpOffset=fromInteger loc})
          
writeDataStream  _ _ _ _  DatHdlNil = error "Nil data handle!"

writeData meta dat ptr@(BPtr _ off) (DatHdl h) = do
  hSeek h AbsoluteSeek $ toInteger off
  put h meta
  put h (sizeOf dat)
  put h dat
  hFlush h
  loc <- hTell h
  return $ Just (h, ptr{bpOffset=fromInteger loc})
  


-- TODO move me to DepotSocketCombinators module
metaToXml metaMap =
    let xml = Map.foldWithKey (mdToXml) "<meta-data>" metaMap
    in xml ++ "</meta-data>"
    where
      mdToXml "X-Object-Id"     v acc = acc ++ "<object-id>" ++ v ++ "</object-id>"
      mdToXml "X-Owner"         v acc = acc ++ "<owner>" ++ v ++ "</owner>"
      mdToXml "X-Creation-Date" v acc = acc ++ "<creation-date>" ++ v ++ "</creation-date>"
      mdToXml "X-Object-Name"   v acc = acc ++ "<object-name>" ++ v ++ "</object-name>"
      mdToXml "Content-Type"    v acc = acc ++ "<content-type>" ++ v ++ "</content-type>"
      mdToXml k v acc                 = acc ++ "<" ++ k ++ ">" ++ v ++ "</" ++ k ++ ">"
          

getDatFileHandleFor path DatHdlNil ptr = 
    let fid = case ptr of
                BNilPtr -> 0
                (BPtr f _) -> f
    in do
  -- LOG.infoIO $ datFileName path fid
  h <- openBinaryFile (datFileName path fid) ReadWriteMode
  return h
getDatFileHandleFor path (DatHdl h) ptr  = do
    isOpen <- hIsOpen h
    if isOpen 
       then return h
       else getDatFileHandleFor path DatHdlNil ptr
                                      
getDatFileHandleFor1 path fid = getDatFileHandleFor path DatHdlNil (BPtr fid 0)

datFileName path fid    = (path ++ "/dat/" ++ (show fid) ++ ".dat")

closeDatHdl DatHdlNil = return ()
closeDatHdl (DatHdl hdl) = hClose hdl
            

-- advances pointer to next file id, offset 0
incrPtr (BPtr fid _) = BPtr (fid+1) 0
decrPtr (BPtr fid _) = BPtr (fid-1) 0



