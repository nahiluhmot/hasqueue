{-# LANGUAGE FlexibleInstances #-}

-- | This module holds the 'MonadSTM' class.
module Control.Concurrent.STM.Class ( MonadSTM(..)
                                    ) where

import Control.Concurrent.STM
import Control.Monad.Trans.Class

-- | 'MonadSTM' is analogous to 'MonadIO' in that it allows 'STM' actions
-- to be "lifted" much like 'MonadIO' allows 'IO' actions to be "lifted".
class MonadSTM m where
    -- Lift a 'STM' action to a Monad.
    liftSTM :: STM a -> m a

instance MonadSTM STM where
    liftSTM = id

instance MonadSTM IO where
    liftSTM = atomically

instance (Monad m, MonadSTM m, MonadTrans t) => MonadSTM (t m) where
    liftSTM = lift . liftSTM
