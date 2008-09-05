module Depot.Com.UrlParser (parseUrl) where
    
-- Still under heavy development and slated for next push

import Depot.Base

import Depot.Util.ParserImports
import Control.Monad.Error
import Debug.Trace

import System.IO
import System.Directory

import Data.Tree as T

parseUrl url = case parse urlParser "url" url of
                 Left e   -> error (show e)
                 Right ue -> ue
                             
slash   = char '/'
          

-- terminal = eof         >> return NilDir
root     = slash       >> return RootDir
kstar    = string "*"  >> return KStar
dkstar   = string "**" >> return DoubleKStar
dname    = do
  name <- many1 (noneOf "/*")
  return (DName name)

dirtok  = do 
  tok <- dname <|> try dkstar <|> kstar
  return tok

urlParser  = do
  head <- root
  tail <- dirtok `sepEndBy` (slash >> notFollowedBy slash)
  let exp = foldr (normalize) [] (head:tail)
  return exp

-- reduces ** surrounded by single * or other double **s
-- e.g. /*/** -> /**/
--      /**/* -> /**/ 
normalize KStar (DoubleKStar:x)       = DoubleKStar:x
normalize DoubleKStar (KStar:x)       = DoubleKStar:x
normalize DoubleKStar (DoubleKStar:x) = DoubleKStar:x
normalize a []                        = [a]
normalize a b                         = a:b



-- foldUrlExp f acc url = let (UExp e e') = parseUrl url
--                        in  foldUrlExp' (f) (f acc e) e'
--     where
--       foldUrlExp' f acc NilDir      = acc
--       foldUrlExp' f acc (UExp e e') = foldUrlExp' (f) (f acc e) e'
                         


foldUrlExpM f acc url = let dirs = parseUrl url
                        in do
                            foldUrlExpM' f acc dirs
    where
      foldUrlExpM' f acc []     = return acc
      foldUrlExpM' f acc (h:t)  = do
        acc' <- f acc h
        foldUrlExpM' (f) acc' t


-- foldUrlExpM_ f url   = let (UExp e e') = parseUrl url
--                         in do
--                             f e
--                             foldUrlExpM_' f e'
--     where
--       foldUrlExpM_' f NilDir      = return ()
--       foldUrlExpM_' f (UExp e e') = do 
--         f e
--         foldUrlExpM_' (f) e'


-- TODO use this function to walk down directory tree and collect a
-- list of directories, dealing with wildcards as they come **may have
-- to alter foldurlexpm to fold to the right rather than the left
-- ...**

collectDir :: ([String],[UrlExpression]) -> 
                   UrlExpression -> IO ([String], [UrlExpression])
collectDir (acc,past) DoubleKStar = return (acc,past)
collectDir (acc,past) KStar       = return (acc,past)
collectDir (acc,past) urlpart     = return (acc,past)
 
symsToPaths syms = do paths <- symsToPaths' syms []
                      print paths
                      return paths
    where      
      symsToPaths' [] acc          = return acc
      symsToPaths' (RootDir:xs) acc   = symsToPaths' xs ["."]
      -- star marks divergence, grab last directory and explode
      -- does divergance mean that we follow the tail of the list
      -- for each new path, then join the result?
      symsToPaths' (KStar:xs) (a:cc)  = do
        children <- listDepotDirs a
        let acc' = foldl (\acc c -> (a /// c):acc) [] children
        symsToPaths' xs (acc' ++ cc)
      symsToPaths' (x:xs) acc      = symsToPaths' xs acc

listDepotDirs dirName = do
  dirs      <- getDirectoryContents dirName
  let dirs' = [x | x <- dirs, x /= "dat", head x /= '.', x /= "idx"]
  foldlM (isDepotDir) [] dirs'  
      where
        isDepotDir acc d = do
          let fullpath = dirName /// d
          isdir <- doesDirectoryExist fullpath
          if isdir then return (d:acc) else return acc


foldlM f acc []     = return acc
foldlM f acc (x:xs) = do
  acc' <- f acc x
  foldlM f acc' xs
          
