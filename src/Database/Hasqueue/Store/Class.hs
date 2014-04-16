-- | This file holds the 'Store' class, which is used to abstractly define
-- a datastore in term of lifecycle operations and 'Pipes'.
module Database.Hasqueue.Store.Class ( -- * ^ 'Store' Type Class
                                       Store(..)
                                       -- * Type synonyms.
                                     , StoreIn
                                     , StoreOut
                                     ) where

import qualified Database.Hasqueue.Message as M
import qualified Database.Hasqueue.Value as V
import Pipes

-- | A 'Store' is used to actually persist the values in 'Hasqueue'.
class Store a where
    -- | Start the 'Store'.
    startStore :: IO a

    -- Run shutdown operations on the 'Store'.
    stopStore :: a -> IO ()

    -- | Given a 'Store', create a 'Pipe' that waits for a 'StoreIn' and
    -- returns a 'StoreOut'.
    runStore :: a -> Pipe StoreIn StoreOut IO ()

-- | This type is consumed by the 'Store'.
type StoreIn = M.StoreRequest

-- | This type is produced by the 'Store'.
type StoreOut = Either V.HasqueueError M.StoreResponse
