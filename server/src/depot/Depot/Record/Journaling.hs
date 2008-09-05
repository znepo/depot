{-# OPTIONS_GHC -fglasgow-exts #-}
module Depot.Record.Journaling
    where
      
import System.IO
      
import Depot.Base
import Depot.IO.Binary
import Depot.Record.DataStore
import Debug.Trace
                
-- serialisation for journal entries ---------------------------------
instance Binary Action where
    put h INSERT = put h (0x01 :: Int)
    put h UPDATE = put h (0x02 :: Int)
    put h DELETE = put h (0x03 :: Int) 
    get h        = do t <- get h :: IO Int
                      case t of
                        0x01 -> return INSERT
                        0x02 -> return UPDATE
                        0x03 -> return DELETE
                        n    -> do
                                 size <- hFileSize h
                                 pos  <- hTell h
                                 error ("out of range action byte: " ++ (show n))
    sizeOf a     = sizeOf (0::Int)
                   
-- The legacy of why these are in stupid order is that there was an
-- issue with Integer serializatoin overflowing. That's been fixed but
-- I want to maintain compatability with current indices.
instance Binary JournalEntry where
    put h j = do 
      put h (jePtr j)         
      put h (jeObjectId j)     
      put h (jeCreationTime j) 
      put h (jeContentType j)  
      put h (jeObjectName j)   
      put h (jeOwner j)        
      put h (jeAction j)       
    get h   = do               
      ptr <- get h
      oid <- get h
      cti <- get h
      cty <- get h
      nam <- get h
      own <- get h
      act <- get h
      return (JE act oid cti cty nam own ptr)
    sizeOf j = sizeOf (jeAction j) +
               sizeOf (jeObjectId j) +
               sizeOf (jeCreationTime j) +
               sizeOf (jeContentType j) +
               sizeOf (jeObjectName j) +
               sizeOf (jeOwner j) +
               sizeOf (jePtr j)

---- functions -----------------------------------------------------------------

-- writes a journal to a handle
makeJournalEntry :: Handle -> JournalEntry -> IO ()
makeJournalEntry = put
      

-- reads journals from a handle, processes each journal entry with the
-- passed fun, accumulating the result.
processJournal h fun acc = do
  eof <- hIsEOF h
  if eof 
     then return acc
     else do
           je <- get h
           processJournal h  (fun) (fun je acc)
      
-- returns the handle for a journal file at a given directory path
getJournalHandle path = do
  h <- openBinaryFile (path ++ "/idx/idx.jrn") ReadWriteMode
  hSetBuffering h NoBuffering
  return (JrnHdl h)
         

closeJrnHdl (JrnHdl h) = hClose h
                         

