{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
module Main (main)
    where

import WebServer
import Depot.Util.CmdLineParser
import Depot.Util.Utils
import qualified Depot.Logging as LOG    
import System.IO
import System.IO.Unsafe

import Data.IORef
import System.Exit

import Network 
import Data.List as List    

import Data.Maybe (fromMaybe)

                    
main = do
  (loghandles, portnumber) <- parseCommandLineOpts "depot-http" collectArgs
  LOG.setGlobalLogHandles loghandles
  startHTTP portnumber
                   

collectArgs app (hs,pn) opt = 
    case opt of
      HelpMsg        -> printHelp app >> exitWith ExitSuccess
      LogTo file     -> return (file:hs,pn)
      Quiet          -> return (List.delete "stdout" hs, pn)
      FlagPortNum n  -> return (hs, mkPortNumStr n)
      WorkingDir dir -> writeIORef wwwroot dir >> return (hs,pn)
      HttpPortNum n  -> do
               if n == "80" 
                  then error noport80
                  else writeIORef httpport (mkPortNumStr n) >> return (hs, pn)
                        
noport80 = "*************************************** \n" ++ 
           "Can't run Depot HTTP Server on port 80! \n" ++ 
           "This server is for development only, \n" ++ 
           "please use Apache or another production \n" ++ 
           "HTTP server to proxy depot."
           