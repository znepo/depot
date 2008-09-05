{-# OPTIONS_GHC -fglasgow-exts #-}
module Depot.Error
    where

import Depot.Base
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State

instance Error DepotError where
    noMsg     = ServerError "" []
    strMsg s  = ServerError s []


instance Show DepotError where
    show e      = (errorCode e) ++ " " ++ (messageERR e) 
    
instance Show DepotStatus where
    show s      = (statusCode s) ++ " " ++ (messageSTAT s) 
    
errorCode (BadRequest _ _)     = "400 Bad Request"
errorCode (UnAuthorized _ _)   = "401 Unauthorized"
errorCode (Forbidden _ _)      = "403 Forbidden"
errorCode (NotFound _ _)       = "404 Not Found"
errorCode (RequestTimeout _ _) = "408 Request Timeout"
errorCode (Conflict _ _)       = "409 Conflict"
errorCode (ServerError _ _)    = "500 Internal Server Error"
errorCode (NotImplemented _ _) = "501 Not Implemented"

statusCode (Ok _ _)         = "200 OK"
statusCode (Created _ _)    = "201 Created"
statusCode (Deleted _ _)    = "223 Deleted"

-- Failure and Success functions -------------------------------------

-- these two methods determine the type of monad used in the dispatch
-- functions. All internal functions either call failure or success
-- and the types for the other functions are implied based on the
-- return values of these.
-- failure :: (Monad m, MonadState s m) => DepotError -> (ErrorT DepotError m b)

failure     :: DepotError -> ErrorStateIO DepotError a
failure err = ErrorT $ return $ Left err

success     :: a -> ErrorStateIO DepotError a
success a   = ErrorT $ return $ Right a 




