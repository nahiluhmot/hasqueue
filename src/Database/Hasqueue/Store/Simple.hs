{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module holds the simplest data store possible -- it uses the
-- 'StateT' 'Monad' and runs in a single thread.
module Database.Hasqueue.Store.Simple ( Simple ) where

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM.Class
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as M
import Database.Hasqueue.Core.Message
import Database.Hasqueue.Core.Service
import qualified Database.Hasqueue.Core.Value as V
import Pipes
import Pipes.Concurrent

data Simple = Simple { storeIn :: Output StoreRequest
                     , storeOut :: Input (Either V.HasqueueError StoreResponse)
                     , threadId :: ThreadId
                     , close :: STM ()
                     }

type SimpleState = M.Map V.BucketID Bucket
type SimpleT m = ErrorT V.HasqueueError (StateT SimpleState m)
type Bucket = M.Map V.ValueID V.Value

instance Service Simple StoreRequest (Either V.HasqueueError StoreResponse) where
    startService = do
        (o, i, s) <- spawn' Unbounded
        (o', i', s') <- spawn' Unbounded
        tid <- forkIO $ do
            runEffect $ fromInput i >-> runSimpleStore M.empty >-> toOutput o'
            performGC
        return $ Simple { storeIn = o
                        , storeOut = i'
                        , threadId = tid
                        , close = s >> s'
                        }
    stopService simple = do
        liftSTM $ close simple
        killThread $ threadId simple
    toPipe s@(Simple i o _ _) = do
        input <- await
        isAlive <- liftSTM $ send i input
        when isAlive $ do
            output <- liftSTM $ recv o
            case output of
                Nothing -> return ()
                Just value -> do
                    yield value
                    toPipe s

runSimpleStore :: Monad m => SimpleState -> Pipe StoreRequest (Either V.HasqueueError StoreResponse) m ()
runSimpleStore world = do
    input <- await
    (output, world') <- lift $ runSimpleT (performOperation input) world
    yield output
    runSimpleStore world'

performOperation :: Monad m => StoreRequest -> SimpleT m StoreResponse
performOperation ListBuckets = liftM Buckets listBuckets
performOperation (CreateBucket bid) = createBucket bid >> return (Bucket bid)
performOperation (DeleteBucket bid) = deleteBucket bid >> return Empty
performOperation (RenameBucket orig new) = renameBucket orig new >> return (Bucket new)
performOperation (ListBucket bid) = liftM Values $ listBucket bid
performOperation (GetValue bid vid) = liftM Value $ getValue bid vid
performOperation (DeleteValue bid vid) = deleteValue bid vid >> return Empty
performOperation (PutValue bid vid val) = putValue bid vid val >> return Empty
performOperation (RenameValue old new) = renameValue old new >> return Empty

listBuckets :: Monad m => SimpleT m [V.BucketID]
listBuckets = gets M.keys

deleteBucket :: Monad m => V.BucketID -> SimpleT m ()
deleteBucket = modify . M.delete

createBucket :: Monad m => V.BucketID -> SimpleT m ()
createBucket = flip putBucket M.empty

getBucket :: Monad m => V.BucketID -> SimpleT m Bucket
getBucket bid = do
    result <- gets $ M.lookup bid
    case result of
        Nothing -> throwError $ V.NoSuchBucket bid
        Just bucket -> return bucket

putBucket :: Monad m => V.BucketID -> Bucket -> SimpleT m ()
putBucket bid = modify . M.insert bid

modifyBucket :: Monad m => V.BucketID -> (Bucket -> Bucket) -> SimpleT m ()
modifyBucket bid f = getBucket bid >>= putBucket bid . f

renameBucket :: Monad m => V.BucketID -> V.BucketID -> SimpleT m ()
renameBucket old new = do
    bucket <- getBucket old
    deleteBucket old
    putBucket new bucket

listBucket :: Monad m => V.BucketID -> SimpleT m [V.ValueID]
listBucket = liftM M.keys . getBucket

getValue :: Monad m => V.BucketID -> V.ValueID -> SimpleT m V.Value
getValue bid vid = do
    bucket <- getBucket bid
    case M.lookup vid bucket of
        Nothing -> throwError $ V.NoSuchValue bid vid
        Just value -> return value

putValue :: Monad m => V.BucketID -> V.ValueID -> V.Value -> SimpleT m ()
putValue bid vid = modifyBucket bid . M.insert vid

deleteValue :: Monad m => V.BucketID -> V.ValueID -> SimpleT m ()
deleteValue bid = modifyBucket bid . M.delete

renameValue :: Monad m => (V.BucketID, V.ValueID) -> (V.BucketID, V.ValueID) -> SimpleT m ()
renameValue old new = do
    value <- uncurry getValue old
    uncurry deleteValue old
    uncurry putValue new value

runSimpleT :: Monad m => SimpleT m a -> SimpleState -> m (Either V.HasqueueError a, SimpleState)
runSimpleT comp world = flip runStateT world $ runErrorT comp
