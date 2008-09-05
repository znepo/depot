{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
module Main (main, depotMainFunction)
    where

import Prelude hiding (catch)
import Network
import Network.Socket

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
    
import Data.Map as Map
import Data.List as List
import Data.Word
    

#if  !(mingw32_HOST_OS)
import System.Posix
#else    
import Depot.Win32.Service
#endif

import System.IO
import System.Exit
import System.Environment
import System.Directory
import Debug.Trace


import Depot.Base
import Depot.Com.DepotSocket
import Depot.Sec.BPAuthInterface
import Depot.Conf.YAMLParser
import Depot.Util.Utils
 
import qualified Depot.Logging as LOG
    
foreign export ccall depotMainFunction :: IO ()

main   = do
#if  !(mingw32_HOST_OS)     
         installHandler sigPIPE Ignore Nothing -- ignore broken http pipes
         depotMainFunction
#else            
         sas <- win32ServiceTableInit 0
         if sas /= asConsole 
            then LOG.infoIO "running as service" 
            else LOG.infoIO "running as console" >> depotMainFunction
#endif  
depotMainFunction = do
         (depotHome, config) <- confMap
         let YMLScl dataDir   = maybeDefault (YMLScl "data") (Map.lookup "data-directory" config) 
             YMLScl adminPass = maybeDefault (YMLScl "cthulu") (Map.lookup "admin-password" config) 
             YMLScl logTo     = maybeDefault (YMLScl "STDOUT") (Map.lookup "log-to" config) 
             YMLScl portNum   = maybeDefault (YMLScl "2323") (Map.lookup "port-number" config)
         portNumber <- readIO portNum `catch` (\e -> LOG.errorIO "error reading port number, defaulting to 2323" >> return 2323)
         setCurrentDirectory $ depotHome /// dataDir                             
         LOG.setGlobalLogHandles [logTo]
         LOG.infoIO $ "    Starting depot server on port: " ++ show portNumber
         LOG.infoIO $ "    Logging output to            : " ++ logTo
         LOG.infoIO $ "    Using data directory         : " ++ dataDir            
#if defined(DEMO)
         LOG.infoIO "-------------------------------------------------------"
         LOG.infoIO "     *** depot demo: we can see up your dress ***      "
         LOG.infoIO "     *** CONNECTION LIMIT: 1                  ***      "
         LOG.infoIO "     *** MAX RECORDS PER DIRECTORY: 20        ***      "
         LOG.infoIO "-------------------------------------------------------"
#endif
         result     <- checkForAdminDirectories initstate adminPass `catch`
                       (\e -> LOG.errorIO (show e) >> exitFailure)
         case result of
           Left _      -> exitWith ExitSuccess
           Right state -> do
                       stateVar   <- atomically $ newTMVar state
                       startDepot (mkPortNum portNumber) stateVar

startDepot port state = withSocketsDo $! do
  logStarted 
  openConns  <- atomically $ newTMVar 0
  bracket (listenOn port)
          (\s -> sClose s) 
          (\s -> acceptLoop s state openConns)                       


initstate    = AppState 0 Map.empty
logStarted   = LOG.infoIO "depot started"
logFinished  = LOG.infoIO "depot finished"

                        

checkForAdminDirectories state passwd = do
  exists <- doesDirectoryExist "com/snepo/auth"  
  if exists 
     then do
           key <- readDepotKey 
           (result, state') <- runStateErrorT (insertAdmin passwd) state{depotKeyAS=key}
           case result of 
             Left err -> printInitError err >> return (Left err)
             Right _ -> return (Right state')
     else do
#if defined(DEBUG)
           LOG.infoIO "-------------------------------------------------------"
           LOG.infoIO "     *** DEPOT DEBUG MODE I have your corn    ***      "
           LOG.infoIO "     *** ADMIN PASSWORD DEFAULT: cthulu       ***      "
           LOG.infoIO "-------------------------------------------------------"
#endif       
           LOG.infoIO "Initializing depot admin directories..."
           (result,state') <- runStateT (runErrorT (initAdminDirectories passwd)) state
           case result of 
             Left err   -> printInitError err >> return (Left err)
             Right _    -> do
                         LOG.infoIO "Initialized, welcome to depot!"
                         LOG.infoIO "- you may log in to the administration application"                    
                         LOG.infoIO "- to add and remove users. "                    
                         return (Right state')

printInitError err = do
                         LOG.infoIO "There was an error initialising"
                         LOG.infoIO "the depot admin directories. "
                         LOG.infoIO $ ">> " ++ show err ++ "<<"
                         LOG.infoIO "Try deleting the /com directory and starting again."


confMap = do
#if (mingw32_HOST_OS)
  depotHome <- System.Environment.getEnv "DEPOT_HOME" `catch` (\e -> LOG.errorIO (show e) >> return "C:/depot")
#else
  depotHome <- System.Environment.getEnv "DEPOT_HOME" `catch` (\_ -> return "./")
#endif  
  LOG.infoIO $ "DEPOT_HOME set to " ++ depotHome
  let path   = depotHome /// "conf/depot.config"
  ymlresult <- parseYmlFile path
  case ymlresult of
    Left error       -> LOG.infoIO (show error) >> exitFailure
    Right (YMLMap m) -> return (depotHome, m)

                                                  
maybeDefault defaultVal mb = case mb of Nothing -> defaultVal; Just x -> x
