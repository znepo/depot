{-# OPTIONS_GHC -fglasgow-exts #-}
-- 
-- This module describes an authorization scheme based on table URL
-- and on read/write/update/delete token access. As well restrictions
-- based on content type may apply(?)
--
module Depot.Sec.Auth 
    (actCombine, actExplode, actionBit,
     perm, perm',
     perm_create,perm_retrieve,perm_update,perm_delete,
     pallow, pdeny, ptry, peval,
     pnegate, authorize, allowIf, allowAnd,
     uname, generateChallenge, generateResponse, principalFromString,
     parseHTTPAuth, ha1, ha2,
     encryptPassword, decryptPassword, generateDepotKey)
    where
      
import Ext.Data.Digest.MD5Aux      
import System.Random    
import Ext.Data.Digest.MD5
      

import System.IO
import System.Directory
import System.Time
    
import Data.Map  as Map
import Data.List  as List
import Data.Set  as Set -- if optimisation is needed, use IntSet and change actions to ints
import Data.Tree as Tree
import Data.Bits as Bits
import Data.Word
import Data.Char

import Depot.Base    
import Depot.IO.Binary
import Depot.IO.BinaryImpl
import Depot.Com.UrlParser

import Depot.Util.ParserImports 

import Ext.Codec.Encryption.Blowfish
import Ext.Codec.Utils

import Debug.Trace
    
-- permission binary is here because it needs to have access to peval
instance Binary Permission where
     put h p = do put h $ actCombine (pAct $ peval p)
     get h   = do mask <- get h :: IO Int
                  return (Perm $ actExplode mask)
     sizeOf p = sizeOf (0::Int)

actCombine acts    = Set.fold (\a acc -> acc .|. actionBit a) 0x00 acts
actExplode :: Int -> Set ActionP
actExplode mask  = let 
                    p    = hasPerm (actionBit Retrieve) Retrieve []
                    p'   = hasPerm (actionBit Create)  Create  p
                    p''  = hasPerm (actionBit Update) Update p'
                    p''' = hasPerm (actionBit Delete) Delete p''
                   in
                     Set.fromList p'''
    where
      hasPerm bit act lst = if bit == (mask .&. bit) then act:lst else lst
                        
    

actionBit :: ActionP -> Int
actionBit a = case a of
                Retrieve -> 0x01
                Create   -> 0x02
                Update   -> 0x04
                Delete   -> 0x08

instance Binary Principal where
    put h (Anonymous) = put h (-1::Int)
    put h (Admin)     = put h (-2::Int)
    put h (User u)    = put h (1::Int) >> put h u
    get h = do
      fb <- get h :: IO Int
      case fb of
         -1 ->  return Anonymous
         -2 ->  return Admin
         _  -> do
                u <- get h
                return (User u)
    sizeOf (User u)    = sizeOf (1::Int) + sizeOf u
    sizeOf _           = sizeOf (1::Int)

instance Binary UserAuth where
    put h (UserAuth p ws) = let ws' = List.map (toInteger) ws in
                            put h p >>  put h ws'
    get h = do
      p  <- get h
      ws <- get h :: IO [Integer]
      return (UserAuth p (List.map (fromInteger) ws))
             
    sizeOf (UserAuth p ws) = sizeOf p + sizeOf (List.map toInteger ws)

instance Binary DirAuth where
    put h (DirAuth d e) = put h d >> put h e
    get h               = do
                          d <- get h
                          e <- get h
                          return $ DirAuth d e
    sizeOf (DirAuth d e) = sizeOf d + sizeOf e

--- Authorize monad --------------------------------------------------

instance (Show a) => Show (Authorize a) where
    show (Allowed a) = "Allowed " ++ show a
    show Denied      = "Denied"

instance (Eq a) => Eq (Authorize a) where
    (Allowed a) == (Allowed b)   = a == b
    (Allowed a) == Denied        = False
    Denied      == (Allowed a)   = False
    Denied      == Denied        = True
                               

instance Monad Authorize where
    Denied      >>= f = Denied
    (Allowed a) >>= f = f a
    return            = Allowed


--- Permission combinator primitives ---------------------------------

perm         = Perm Set.empty
perm' act    = Perm (Set.singleton act)

perm_create   = perm' Create
perm_retrieve = perm' Retrieve
perm_update   = perm' Update
perm_delete   = perm' Delete

pallow p p'  = Allow p p'
pdeny p p'   = Deny p p'
ptry p p'    = Try p p'
               
peval p@(Perm _)      = p 
peval (Allow p p')    = let acts  = pAct $ peval p
                            acts' = pAct $ peval p' in
                        peval p' {pAct=acts `Set.union` acts'}
peval (Deny p p')     = let acts  = pAct $ peval p
                            acts' = pAct $ peval p' in
                        peval p' {pAct=acts `Set.difference` acts'}
peval (Try p p')      = let acts  = pAct $ peval p
                            acts' = pAct $ peval p' in
                        peval p' {pAct=acts `Set.intersection` acts'}

-- Permission Combinators --------------------------------------------
pnegate p = pdeny p p               


-- Authorization Functions -------------------------------------------

-- allows or denies an action- I wonder if lazy evaluation can keep
-- the action from being evaluated even if it has side effects, e.g
-- passing handlePut to authorize?
authorize :: Permission -> Permission -> a -> Authorize a
authorize p p' a = let Perm acts = peval (p `ptry` p') in
                   if Set.null acts then Denied else Allowed a


-- Authorizeation combinators, ---------------------------------------
-- Not sure these are needed or wanted but they sure are neat --------
allowIf :: Authorize a -> Authorize b -> Authorize b
allowIf a b = case a of
                Allowed _ -> b
                Denied    -> Denied
                 

allowAnd :: Authorize x -> Authorize y -> Authorize z -> Authorize z
allowAnd x y z = case x `allowIf` y of
                   Allowed x -> z
                   Denied    -> Denied


uname Anonymous = "__ANON__"
uname (User u)  = u
uname Admin     = "admin"
                  

--- HTTP Auth --------------------------------------------------------

opaque = mhash "fnord" -- yeah, yeah, whatev.

pk     = mhash "r'yleh"
  
mhash = md5s . Str

generateChallenge realm = do
  (TOD a b) <- getClockTime
  let nonce = mhash $ pk ++ show a ++ show b
      resp  = "Digest realm" .=. realm  .#.
              "qop"          .=. "auth" .#.
              "nonce"        .=. nonce  .#.
              "opaque"       .=. opaque
  return (resp,nonce)
         

ha1 name realm passwd = mhash $ name .:. realm .:. passwd
ha2 method uri        = mhash $ method .:. uri

generateResponse h1 nonce nc cnonce qop h2 = 
    mhash $ h1 .:. nonce .:. nc .:. cnonce .:. qop .:. h2


parseHTTPAuth str = parse httpAuthParser "Authorization" str 
          

colon  = symbol ":"
comma  = symbol ","
eqsign = symbol "="
quote  = symbol "\""         

prefix = symbol "Digest"
         
quotedstr = do
  quote
  val <- many (noneOf "\"")
  quote
  return val
         
unquotedstr = many (noneOf ",")

nameval = do
  name <- manyTill anyChar (try eqsign)
  val  <- choice [quotedstr, unquotedstr]
  return (name,val)

httpAuthParser = do
  many (char ' ')
  prefix
  nvs  <- sepBy1 nameval comma
  case authFromNameVals nvs of
    -- TODO: return a proper (Left err) instead of calling (error)
    -- directly
    Nothing   -> error "Incomplete authorization header"
    Just auth -> return auth
  
authFromNameVals nvs = do
  uname  <- List.lookup "username" nvs
  realm  <- List.lookup "realm" nvs
  nonce  <- List.lookup "nonce" nvs
  uri    <- List.lookup "uri" nvs
  qop    <- List.lookup "qop" nvs
  nc     <- List.lookup "nc" nvs
  cnonce <- List.lookup "cnonce" nvs
  resp   <- List.lookup "response" nvs
  opaque <- List.lookup "opaque" nvs
  return HTTPAuth { usernameHA=uname,
                    realmHA=realm,
                    nonceHA=nonce,
                    uriHA=uri,
                    qopHA=qop,
                    ncHA=nc,
                    cnonceHA=cnonce,
                    responseHA=resp,
                    opaqueHA=opaque }
         

encryptPassword :: Word64 -> String -> [Word64]
encryptPassword key passwd = List.map enc (toWord64 passwd)
    where enc = encrypt key
decryptPassword :: Word64 -> [Word64] -> String
decryptPassword key passwd = fromWord64 $ List.map dec passwd
    where dec = decrypt key 



toWord64  :: String -> [Word64]
toWord64  str = foldl (\acc s -> [tow64 s] ++ acc) [] (divstrby8 str)
    where tow64 = fromOctets 256 . List.map (fromIntegral . ord) 

divstrby8 str = ds8 str []
    where
      ds8 (a:b:c:d:e:f:g:h:xs) acc = ds8 xs ([a,b,c,d,e,f,g,h]:acc)
      ds8 [] acc                   = reverse acc
      ds8 str acc                  = reverse (str:acc)
                          
          
fromWord64 :: [Word64] -> String
fromWord64 w64s = foldl (\acc w -> frow64 w ++ acc) ""  w64s
    where frow64 = List.map (chr . fromIntegral) . toOctets 256 
               


generateDepotKey :: IO Word64
generateDepotKey = do
  rgen    <- newStdGen
  let rnd_ = head $ randoms rgen :: Integer
      rnd  = if rnd_ < 0 then rnd_ * (-1) else rnd_
      oct  = toOctets 256 rnd
      key  = fromOctets 256 $ hash oct  
  return key
  
