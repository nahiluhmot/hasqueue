{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module holds the 'Service' type class.
module Database.Hasqueue.Core.Service ( Service(..) ) where

import Pipes

-- | A 'Service' is a composable abstraction that can be started, stopped,
-- and converted into an 'Pipe.'
class Service s a b where
    -- | Start the 'Service'.
    startService :: IO s
    -- | Stop the 'Service'.
    stopService :: s -> IO ()
    -- | Access a streaming purpose for the 'Service'.
    toPipe :: s -> Pipe a b IO ()
