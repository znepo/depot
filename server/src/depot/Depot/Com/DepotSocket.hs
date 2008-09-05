{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
module Depot.Com.DepotSocket (acceptLoop)
   where
     
import Prelude hiding (catch)
import Text.ParserCombinators.Parsec
import Network hiding (accept)
import Debug.Trace    
import System.Directory  
import System.IO
import System.Time
import System.Locale
import Data.Map as Map
import Data.List as List
import Data.Char

import Control.Concurrent.STM
import Control.Concurrent

import Control.Exception 
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import Depot.Base
import Depot.Error
import qualified Depot.Logging as LOG
import Depot.IO.Binary 
import Depot.IO.BPlusIO 

import Depot.Com.Protocol
import Depot.Com.QueryParser
import Depot.Com.BPSocketInterface
import Depot.Util.Utils
    
import System.IO.Error hiding (catch)
    


#if defined(DEMO)    
maxConnections = 1 :: Int
#else
maxConnections = 23 :: Int
#endif

-- Top level accept loop for server                  
acceptLoop sock stateVar openConns =  do
       open <- atomically $! readTMVar openConns
       LOG.infoIO $ "there are now " ++ show open ++ " connections"
       atomically $ addConnection openConns maxConnections
--       LOG.infoIO "added connection"
       (chandle, _) <- accept sock
--       LOG.infoIO "accepted connection"
       t <- forkIO (catch (acceptConnection chandle stateVar openConns `finally` cleanUp chandle)
                           (\e -> LOG.errorIO ("unexpected : " ++ show e)))
       acceptLoop sock stateVar openConns
           where
             cleanUp chandle    = do 
--               LOG.infoIO ("closing client handle") 
               isOpen <- hIsOpen chandle
               when isOpen (catch (hClose chandle)
                                  (handleCloseError))               
               atomically $ removeConnection openConns
--               open <- atomically $ readTMVar openConns               
--               LOG.infoIO ("removed connection, " ++ show open ++ " left") 
             handleCloseError e = LOG.errorIO
                                     ("error closing client handle: " ++ 
                                      show e) 

-- processes a connection and request atomically
acceptConnection chandle stateVar openConns = do
        hstr <- readHeader chandle
        LOG.infoIO "received: " 
        LOG.infoIO $ "\n" ++ hstr   
        state  <- atomically $ takeTMVar stateVar
--        LOG.infoIO "-----------------------------------------------------------"
--        LOG.infoIO "MAKING REQUEST " 
        start <- getClockTime
        state' <- (catch (dispatchRequest hstr state chandle)
                         (badRequest hstr state chandle))                   
        fin   <- getClockTime
        LOG.infoIO $ "EXECUTED REQUEST IN: " ++ (formatExecutionTime start fin)
--        LOG.infoIO "-----------------------------------------------------------"
        atomically  $ putTMVar stateVar state' 
            where                         
              badRequest hstr state chandle e =
                  let mess  = "Unexpected exception " ++ show e ++ "\nRequest was:\n" ++ hstr
                      err   = ServerError mess []
                  in do
                      LOG.errorIO (show e) 
                      hPutStr chandle $ defaultErrResponse err
                      return state


-- Handles the delegation of a request to a processor function
dispatchRequest hstr state handle = do
  (result,state') <- runStateErrorT (handleRequest (httpdelegate) hstr) state
  case result of
      Right status   -> do
          LOG.infoIO (show status) 
          return state'
      Left err  -> 
          let
              code     = responseErr err
              body     = messageERR err ++ " \nRequest was:\n" ++ hstr ++ "\r\n\r\n\0"
              response = defaultResponse' code body (headersERR err)
          in do
              LOG.errorIO (show err) 
              LOG.debugIO response
              hPutStr handle $! response
              return state'
  where
    getDelegate hvars = 
        case Map.lookup "x-snepo-directive" hvars of
          Just "DIR_SIZE"  -> getDirectorySize
          _                -> getObject

    createDelegate hvars = 
        case Map.lookup "x-snepo-directive" hvars of
          Just "CREATE"      -> putCreateDirectory
          -- perhaps should infer this from content-type
          -- yup, a content-type parser, complete with profiles
          -- for each possible type. TODO
          Just "PUT_USER"    -> putUser
          Just "PUT_PERM"    -> putPerm
          Just "RESTORE_DIR" -> restoreDirectory
          _                  -> putObject

    deleteDelegate hvars = 
        case Map.lookup "x-snepo-directive" hvars of
          Nothing            -> deleteObject
          Just "DELETE_DIR"  -> deleteDirectory
          Just "DESTROY_DIR" -> destroyDirectory

    httpdelegate request = 
            let
                hline = hdrLine    request
                hvars = hdrVars    request
                meth  = 
                    case Map.lookup "x-snepo-request-kludge" hvars of 
                          Nothing -> hdrMethod  hline
                          Just m  -> case m of
                                       "PUT"    -> PUT
                                       "GET"    -> GET
                                       "DELETE" -> DELETE
                path  = filterPath $ hdrSubject hline
                uvars = hdrUVars   hline
                unsup = ServerError ("Method " ++ show meth ++ " not supported") []
                f     = case meth of
                          GET    -> getDelegate hvars
                          PUT    -> createDelegate hvars
                          DELETE -> deleteDelegate hvars
                          _      -> (\_ _ _ _ -> failure unsup)
            in do 
                state <- f handle path uvars hvars
                liftIO $ hFlush handle
                return state
               where
                 -- Kludge for proxy servers. This is nasty because
                 -- people WILL name their directories "depot" which
                 -- may create a world of confusion.
                 filterPath path = if "depot/" `isPrefixOf` path
                                      then drop 6 path
                                   else path





