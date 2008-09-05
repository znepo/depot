{-# OPTIONS_GHC -fglasgow-exts -fno-monomorphism-restriction -cpp #-}

module Depot.Logging 
    (error, warn, info, debug,
     errorIO, warnIO, infoIO, debugIO,
     setGlobalLogHandles)
    where
      
import Depot.Base      
import Depot.Error
      
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Time
import System.Locale
    
import Data.IORef

import Control.Monad.State
import Control.Concurrent.STM

import Prelude hiding (error, log)


import Foreign
import Foreign.C
    
-- using win32debug :: String -> IO () creates a log at
-- c:/tmp/depot.log this is useful for debugging depot when it is
-- being run or started as a service

-- import Depot.Win32.Service -- uncomment for win32 logging
    
{-# INCLUDE "depot_logger.h" #-}

foreign import ccall unsafe "c_log" c_log :: CString -> CString -> IO ()                                                

haskellCLog file msg = do
--  win32debug msg -- uncomment for win32 logging 
  file_cstr <- newCString file
  msg_cstr  <- newCString msg
  c_log file_cstr msg_cstr                     


instance Show LogLevel where
    show LogError = "[error] "
    show LogWarn  = "[warn]  "
    show LogInfo  = "[info]  "
    show LogDebug = "[debug] "

br = "\n"

{-# INLINE logline #-}
logline level message = do
  tm <- getClockTime
  ct <- toCalendarTime tm
  let lc   =  defaultTimeLocale
      time = formatCalendarTime lc "[%Y-%m-%d %H:%M:%S] " ct ++ " "
      msg  = time ++ show level ++ message ++ br 
  return msg
  

{-# INLINE logto #-}
logto level message = do
  hs   <- readIORef globalLogHandles
  line <- logline level message
  mapM_ (\h -> haskellCLog h line) hs


{-# INLINE error #-}
{-# INLINE errorIO #-}
errorIO = logto LogError
error   = liftIO . errorIO
{-# INLINE warn #-}
{-# INLINE warnIO #-}
warnIO  = logto LogWarn
warn    = liftIO . warnIO
{-# INLINE info #-}
{-# INLINE infoIO #-}
infoIO :: String -> IO ()
infoIO  = logto LogInfo
info    = liftIO . infoIO
{-# INLINE debug #-}
{-# INLINE debugIO #-}
debugIO :: String -> IO ()
#if defined(DEBUG)
debugIO  = logto LogDebug
#else
debugIO _ = return ()
#endif
debug    = liftIO . debugIO
             
-- NOTE, these are now strings!!! 
setGlobalLogHandles :: [String] -> IO ()
setGlobalLogHandles hdls = writeIORef globalLogHandles hdls 

{-# NOINLINE globalLogHandles #-}
globalLogHandles :: IORef [String]
globalLogHandles = unsafePerformIO $ newIORef ["STDOUT"]



           

