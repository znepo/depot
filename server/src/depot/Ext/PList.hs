{-# OPTIONS_GHC -fglasgow-exts  #-}
module Ext.PList where
 
import Data.Typeable

-- huge thanks to J. Garrett Morris for posting this on the haskell
-- cafe mailing list. http://www.haskell.org//pipermail/haskell-cafe
-- /2006-February/014218.html

-- Generic Property Lists --------------------------------------------

class Typeable t => Property a t | a -> t where
    label :: a -> String
    value :: a -> t

data AnyProperty where
    AnyProperty :: Property a t => a -> AnyProperty

               
app :: (forall a t. Property a t => a -> r) -> AnyProperty -> r
f `app` (AnyProperty p) = f p
                             
type PList = [AnyProperty]
pLookup :: Typeable a => String -> PList -> Maybe a
pLookup prop pl | [anyProp] <- property = (cast . value) `app` anyProp
                | otherwise             = Nothing
    where
      property = filter ((prop ==) . (label `app`)) pl
                 
pCons :: Property a t => a -> PList -> PList
pCons = (:) . AnyProperty
        
pDelete :: String -> PList -> PList
pDelete prop pl = filter ((prop /=) . (label `app`)) pl


anyprop a = AnyProperty a
