{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
module Depot.Util.Utils where

import System.Time
import System.Locale
import System.IO
import System.Exit
import System.Environment
import System.Directory
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Control.Concurrent.STM
import Network    
import Depot.Util.CmdLineParser

import qualified Network.Socket as Socket
import Data.List as List    

-- concatenation operators -------------------------------------------

-- colon seperator
x .:. y = x ++ ":" ++ y
-- term .=. value => term="value"
x .=. y = x ++ "=\"" ++ y ++ "\""
-- comma seperator
x .#. y = x ++ "," ++ y
-- solidus seperator
a /// b = a ++ "/" ++ b


-- composite transformers --------------------------------------------

runStateErrorT = runStateT . runErrorT


-- assorted utilities ------------------------------------------------
iterateM f 0 acc             = return (reverse acc)
iterateM f n acc | n > 0     = do x <- f; iterateM f (n-1) (x:acc)
                 | otherwise = error "passed negative number to iterateM"
             
maybeM_  :: (Monad m) => a -> (a -> m b) -> Maybe a  -> m b
maybeM_ defaultval f maybeval = 
    case maybeval of
      Nothing -> f defaultval
      Just a  -> f a

downcase str = map dcase str
    where
      dcase 'A' = 'a'
      dcase 'B' = 'b'
      dcase 'C' = 'c'
      dcase 'D' = 'd'
      dcase 'E' = 'e'
      dcase 'F' = 'f'
      dcase 'G' = 'g'
      dcase 'H' = 'h'
      dcase 'I' = 'i'
      dcase 'J' = 'j'
      dcase 'K' = 'k'
      dcase 'L' = 'l'
      dcase 'M' = 'm'
      dcase 'N' = 'n'
      dcase 'O' = 'o'
      dcase 'P' = 'p'
      dcase 'Q' = 'q'
      dcase 'R' = 'r'
      dcase 'S' = 's'
      dcase 'T' = 't'
      dcase 'U' = 'u'
      dcase 'V' = 'v'
      dcase 'W' = 'w'
      dcase 'X' = 'x'
      dcase 'Y' = 'y'
      dcase 'Z' = 'z'
      dcase x   = x



-- directory name utils ----------------------------------------------


-- returns the name of a "delete" directory. e.g. /a/b/c => /a/b/._c
deleteNameFor path = let p       = parentDirFor path
                         rest    = filter (\x -> x /= '/') (drop (length p) path)
                     in case p of
                          "" -> "._" ++ rest
                          _ -> p ///  "._" ++ rest

parentDirFor "/" = ""
parentDirFor p   = let (x:xs)    = reverse p 
                       p'        = if x == '/' then xs else (x:xs)
                   in case dropWhile (\n -> n /= '/') p' of
                        (x:xs) -> if x == '/' 
                                     then reverse xs 
                                     else reverse (x:xs)
                        xs     -> reverse xs


hRewind h i = hSeek h RelativeSeek (-i)
              


-- utility http functions --------------------------------------------

-- Custom accept implementation to avoid reverse DNS lookups: EXACTLY
-- the same as the GHC implementation but with the reverse dns lookup
-- removed
accept sock@(Socket.MkSocket _ Socket.AF_INET _ _ _) = do
 ~(sock', (Socket.SockAddrInet port haddr)) <- Socket.accept sock
 handle <- Socket.socketToHandle sock' ReadWriteMode
 return (handle, port)
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
accept sock@(Socket.MkSocket _ Socket.AF_UNIX _ _ _) = do
 ~(sock', (Socket.SockAddrUnix path)) <- Socket.accept sock
 handle <- Socket.socketToHandle sock' ReadWriteMode
 return (handle, -1)
#endif
accept sock@(Socket.MkSocket _ family _ _ _) =
  error $ "Sorry, address family " ++ (show family) ++ " is not supported!"
        


closeIfStillOpen h = do isOpen <- hIsOpen h
                        if isOpen then hClose h else return ()
                           

addConnection openConns maxConnections = do
  open <- readTMVar openConns  
  if (open > maxConnections) 
     then retry
     else swapTMVar openConns (open + 1)


removeConnection openConns = do
  open <- takeTMVar openConns
  putTMVar openConns (open - 1)


formatExecutionTime start fin = 
    let 
        df  = diffClockTimes fin start
        df' = normalizeTimeDiff df
        ms  = (fromIntegral (tdPicosec df')) / (10^9)
        s   = tdSec df'
        m   = tdMin df'

        fmt 0 0 ms = show ms ++ "ms"
        fmt 0 s ms = show s ++ "s " ++ fmt 0 0 ms
        fmt m s ms = show m ++ "m " ++ fmt 0 s ms
    in
      fmt m s ms
         

-- IO stuffs
           
handlesFromStrings hs = mapM (hfroms) hs
    where hfroms "STDOUT" = return stdout
          hfroms s        = do h <- openBinaryFile s AppendMode
                               hSetBuffering h LineBuffering
                               return h

-- currently either returns log handles or
-- exits with a help message
parseCommandLineOpts app collectArgs = do
  args     <- getArgs
  (opts,_) <- cmdOpts app args
  let hs = ["STDOUT", app ++ ".log"]
  foldM (collectArgs app) (hs,PortNumber 2323) (List.sort opts)


mkPortNum    = PortNumber . fromIntegral 
mkPortNumStr = mkPortNum . read 

printHelp app = putStrLn (getHelp app)


-- Strings

splitStr str splt = 
    let splitCheck (strchnk, acc) c = if c == splt
                                         then ("",(reverse strchnk):acc)
                                         else (c:strchnk, acc)
        (h,splits) = foldl (splitCheck) ("",[]) str
    in reverse (reverse h:splits)
