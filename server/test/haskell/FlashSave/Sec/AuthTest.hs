 module FlashSave.Sec.AuthTest
     where
       
       
import FlashSave.Sec.Auth
import Debug.Trace
import Test.HUnit
import Test.QuickCheck
import Control.Monad    

import Data.Set as Set
import Data.Tree as Tree

fixture = runTestTT $ TestList [
--                                TestLabel "create directory" testAuthSimple,
                                TestLabel "create directory" testAllowIf ,
                                TestLabel "create directory" testSerializePerms,
--                                TestLabel "create directory" testPermPlus,
                                TestLabel "create directory" testPerms,
                                TestLabel "create directory" testAllowAnd
                               ]
          
                               

testAllowIf    = TestCase $ do
                 assertEqual "allowif true" (Allowed 2) (Allowed 1 `allowIf` Allowed 2) 
                 assertEqual "allowif false" (Denied::Authorize String) ((Allowed 1) `allowIf` Denied) 
                             
                               

testAllowAnd   = TestCase $ do
                   assertEqual "allowand true true" 
                                   (Allowed 'a')
                                   (allowAnd (Allowed 1)  (Allowed 2) (Allowed 'a'))
                   assertEqual "allowand true false"
                                   Denied
                                   (allowAnd (Allowed 1) Denied (Allowed 'a')) 
                   assertEqual "allowand false false"
                                   Denied 
                                   (allowAnd Denied  Denied (Allowed 'a')) 


testPerms       = TestCase $ do
                    let client = peval $ perm `pallow` r `pallow` w

                    assertEqual "perm arithmetic simple" r    $ peval (client `ptry` r)
                    assertEqual "perm arithmetic simple" w    $ peval (client `ptry` w)
                    assertEqual "perm arithmetic simple" nil  $ peval (client `ptry` u)
                    assertEqual "perm arithmetic simple" nil  $ peval (client `ptry` d)
                    assertEqual "perm arithmetic simple" (peval rwu)  (peval rwu')
                       
testSerializePerms = TestCase $ do
                        assertEqual "rwu" a_rwud $ actExplode (actCombine a_rwud)
                        assertEqual "rwu" a_rwu $ actExplode (actCombine a_rwu)
                        assertEqual "rw"  a_rw $ actExplode (actCombine a_rw)
                        assertEqual "r"  a_r $ actExplode (actCombine a_r)

a_rwud = Set.fromList [Read,Write,Update,Delete]
a_rwu  = Set.fromList [Read,Write,Update]
a_rw   = Set.fromList [Read,Write]
a_r    = Set.fromList [Read]


nil  = Perm $ Set.empty
r    = Perm $ Set.singleton Read
w    = Perm $ Set.singleton Write
u    = Perm $ Set.singleton Update
d    = Perm $ Set.singleton Delete
rw   = r   `pallow` w
rwu  = rw  `pallow` u
rwud = rwu `pallow` d
rwu' = rwud `pdeny` d


-- testAuthWith  = TestCase $ do
--                       assertEqual "allow all" (Allowed 2) $ 
--                                   authorize loc1 "TOK1" PermRead (1 + 1)

-- root dir
-- loc1 = L (Node "/" []) 
--          (Map.singleton "TOK1" (Perms [PermRead, PermWrite]))
