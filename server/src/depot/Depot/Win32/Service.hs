{-# OPTIONS_GHC -fglasgow-exts -cpp#-}
module Depot.Win32.Service (win32ServiceTableInit, asService, asConsole, win32debug)
    where
      
import Foreign.C
import Foreign.C.String
import Foreign
    
-- Win32 types      

type BOOL   = Int      
type LPARAM = Int
type DWORD  = Word32
type LPTSTR = Ptr CString 

-- -# INCLUDE "depotsvc.h" #-
   
-- export the Win32 ServiceMain call that windows requires to 
-- keep tabs on a service.
foreign import ccall "InitServiceTable" svcinit :: IO Int
foreign import ccall "SvcDebugOut" svcdebug :: CString -> DWORD -> IO ()
   
asService = 23 :: Int
asConsole = 32 :: Int


win32ServiceTableInit n = svcinit
  
win32debug str = withCString str (\cstr -> svcdebug cstr 0)
                                        

