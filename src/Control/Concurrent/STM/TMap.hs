-- | This module holds the 'TMap' data type, which is used internally to
-- store bucket contents as well as provide the Hash data structure.
module Control.Concurrent.STM.TMap ( -- * Types
                                     TMap
                                     -- * Constructors
                                   , newTMap
                                   , newTMapIO
                                     -- * Query functions
                                   , size
                                   , null
                                   , lookup
                                   , keys
                                     -- * Modification functions
                                   , insert
                                   , delete
                                     -- * List conversions
                                   , fromList
                                   , toList
                                   ) where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as M
import Prelude hiding (null, lookup)

-- | A 'TMap' is a 'M.Map' wrapped in a 'TVar'. It can be used to store
-- a thread-safe 'M.Map'.
type TMap k v = TVar (M.Map k v)

-- | Create a new 'TMap' in the 'STM' 'Monad'.
newTMap :: STM (TMap k v)
newTMap = fromMap M.empty

-- | Create a new 'TMap' in the 'IO' 'Monad'.
newTMapIO :: IO (TMap k v)
newTMapIO = atomically newTMap

-- | Get thie size of the given 'TMap'.
size :: TMap k v -> STM Int
size = liftM M.size . toMap

-- | Check if the 'TMap' is empty.
null :: TMap k v -> STM Bool
null = liftM M.null . toMap

-- | Lookup a value in the 'TMap'.
lookup :: Ord k => k -> TMap k v -> STM (Maybe v)
lookup k = liftM (M.lookup k) . toMap

-- | Return all of the keys in the 'TMap'.
keys :: TMap k v -> STM [k]
keys = liftM M.keys . toMap

-- | Insert a value into the 'TMap'.
insert :: Ord k => k -> v -> TMap k v -> STM ()
insert k v = flip modifyTVar $ M.insert k v

-- | Delete a value from the 'TMap'.
delete :: Ord k => k -> TMap k v -> STM ()
delete k = flip modifyTVar $ M.delete k

-- | Create new 'TMap' from the given '[]'.
fromList :: Ord k => [(k, v)] -> STM (TMap k v)
fromList = fromMap . M.fromList

-- | Create a new '[]' from the given 'TMap'.
toList :: Ord k => TMap k v -> STM [(k, v)]
toList = liftM M.toList . toMap

-- Convert a 'M.Map' into a 'TMap'.
fromMap :: M.Map k v -> STM (TMap k v)
fromMap = newTVar

-- Convert a 'TMap' into 'M.Map'.
toMap :: TMap k v -> STM (M.Map k v)
toMap = readTVar
