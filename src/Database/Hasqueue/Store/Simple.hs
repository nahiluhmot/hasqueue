{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module holds the simplest data store possible -- it uses the
-- 'StateT' 'Monad' and runs in a single thread.
module Database.Hasqueue.Store.Simple ( Simple ) where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Map as M
import Database.Hasqueue.Core.Message
import Database.Hasqueue.Core.Service
import qualified Database.Hasqueue.Core.Value as V
import Pipes


newtype Simple = World { runWorld :: M.Map V.BucketID Bucket }

type SimpleT m = ErrorT V.HasqueueError (StateT Simple m)
type Bucket = M.Map V.ValueID V.Value

instance Service Simple StoreRequest (Either V.HasqueueError StoreResponse) where
    startService = return $ World M.empty
    stopService _ = return ()
    toPipe = runSimpleStore

runSimpleStore :: Monad m => Simple -> Pipe StoreRequest (Either V.HasqueueError StoreResponse) m ()
runSimpleStore world = do
    storeIn <- await
    (storeOut, world') <- lift $ runSimpleT (performOperation storeIn) world
    unless (storeOut == Left V.ShuttingDown) $ do
        yield storeOut
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
performOperation Shutdown = shutdown

listBuckets :: Monad m => SimpleT m [V.BucketID]
listBuckets = gets' M.keys

deleteBucket :: Monad m => V.BucketID -> SimpleT m ()
deleteBucket = modify' . M.delete

createBucket :: Monad m => V.BucketID -> SimpleT m ()
createBucket = flip putBucket M.empty

getBucket :: Monad m => V.BucketID -> SimpleT m Bucket
getBucket bid = do
    result <- gets' $ M.lookup bid
    case result of
        Nothing -> throwError $ V.NoSuchBucket bid
        Just bucket -> return bucket

putBucket :: Monad m => V.BucketID -> Bucket -> SimpleT m ()
putBucket bid = modify' . M.insert bid

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

shutdown :: Monad m => SimpleT m a
shutdown = throwError V.ShuttingDown

runSimpleT :: Monad m => SimpleT m a -> Simple -> m (Either V.HasqueueError a, Simple)
runSimpleT comp world = flip runStateT world $ runErrorT comp

gets' :: Monad m => (M.Map V.BucketID Bucket -> a) -> SimpleT m a
gets' f = gets (f . runWorld)

put' :: Monad m => M.Map V.BucketID Bucket -> SimpleT m ()
put' = put . World

modify' :: Monad m => (M.Map V.BucketID Bucket -> M.Map V.BucketID Bucket) -> SimpleT m ()
modify' f = gets' f >>= put'
