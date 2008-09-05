module Depot.Record.BPlusConc
    where
      
      
import Depot.Base
import qualified Data.Map as Map
-- import Depot.Record.BPlusGenIndex
    
import Control.Concurrent.STM
import Control.Monad    
import Control.Monad.Trans    



data BTreeAction = Reading | Writing | Splitting | Merging
                 
type Lock        = TMVar BTreeAction
    


-- signalling whether to retry the selected action when it is
-- available again
type Retry  = Maybe

newtype Operation a = Op { runOp :: Lock -> BTreeAction -> STM (BTreeAction, a) }

instance Monad (Operation) where
   return value   = Op (\lock action -> return (action,value))
   (Op o) >>= f   = Op (\lock action -> do ensureLock lock action
                                           (action',value) <- o lock action
                                           runOp (f value) lock action')
                                        

ensureLock :: Lock -> BTreeAction -> STM ()
ensureLock lock action = readTMVar lock >>= el action
    where el Writing   Writing   = return ()
          el Splitting Splitting = return ()
          el Merging   Merging   = return ()
          el Reading   _         = return ()
          el Writing   _         = retry
          el Splitting _         = retry
          el Merging   _         = retry
                                   

               



-- select :: Integer -> BTreeIndex a -> Operation (Selection a)
-- select key node = undefined
-- --     case bSelect node key of
-- --                     LookNext ptr -> undefined
-- --                     result       -> return result



-- bSelect :: BTreeIndex a -> Integer -> Selection a
-- bSelect (Leaf _ vs _ _) key = case Map.lookup key vs of
--                                 Nothing -> SelectionNotFound
--                                 Just x  -> Found [x]

-- bSelect (Branch _ keys kids) key = look keys kids
--     where
--       look [] [ptr]       = LookNext ptr
--       look (k:ks) (c:cs)  = case compare key k of
--                               LT -> LookNext c
--                               _  -> look ks cs


-- performsel :: Integer -> BTreeIndex a -> Lock -> IO (Selection a)
-- performsel key idx lock = doOp (select key idx) lock Reading
