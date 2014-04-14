-- | This module holds the base data types and functions to interact with
-- them. This is the lowest-level interface to interact with the datastore.
module Database.Hasqueue.Value ( -- * Base Types
                                 World
                               , Bucket
                               , BucketID
                               , Value(..)
                               , ValueID
                                 -- * Monad and Error
                               , Hasqueue
                               , HasqueueError(..)
                               , runHasqueue
                               , runHasqueue'
                                 -- * Create Functions
                               , createWorld
                               , createBucket
                                 -- * Query Functions
                               , bucketExists
                               , valueExists
                               , getWorld
                               , getBucket
                               , getValue
                               , buckets
                               , values
                                 -- * Insert and Update Functions
                               , putBucket
                               , putValue
                               , renameBucket
                               , renameValue
                               , modifyValue
                                 -- * Delete Functions
                               , deleteBucket
                               , deleteValue
                               ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.Class
import qualified Control.Concurrent.STM.TMap as T
import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as SQ

-- | The 'World' represents all of the data in the datastore.
type World = T.TMap BucketID Bucket

-- | A 'Bucket' is a 'TMap' of values in a 'World'.
type Bucket = T.TMap ValueID Value

-- | Used to uniquely identify 'Bucket's in a 'World'.
type BucketID = B.ByteString

-- | Used to uniquely identify 'Value's in a 'Bucket'.
type ValueID = B.ByteString

-- | A 'Value' is any type that may be stored in the datastore.
data Value = Boolean Bool
           | Int Int
           | Double Double
           | String B.ByteString
           | List (SQ.Seq Value)
           | Hash (M.Map B.ByteString Value)
           | Set (S.Set Value)
           deriving (Eq, Show)

-- | The 'Hasqueue' type wraps 'ErrorT', 'ReaderT', and 'STM' to provide
-- a monadic interface to the datastore.
type Hasqueue = ErrorT HasqueueError (ReaderT World STM)


-- | A 'HasqueueError' is thrown when something unexpected happens during
-- a datastore lookup.
data HasqueueError = InternalError B.ByteString
                   | ParseError B.ByteString
                   | NoSuchBucket BucketID
                   | NoSuchValue BucketID ValueID
                   deriving (Eq, Show)

instance Error HasqueueError where
    strMsg = InternalError . B.pack . map (toEnum . fromEnum)

-- | Run a 'Hasqueue' computation in a new 'World'.
runHasqueue :: Hasqueue a -> STM (Either HasqueueError a)
runHasqueue action = createWorld >>= runHasqueue' action

-- | Run a 'Hasqueue' computation in the given 'World'.
runHasqueue' :: Hasqueue a -> World -> STM (Either HasqueueError a)
runHasqueue' action = runReaderT (runErrorT action)

-- | Create a new 'World'.
createWorld :: STM World
createWorld = T.newTMap

-- | Create a new 'Bucket' with the given 'BucketID'.
createBucket :: BucketID -> Hasqueue ()
createBucket bid = do
    bucket <- liftSTM T.newTMap
    putBucket bid bucket

-- | Test if a 'Bucket' exists.
bucketExists :: BucketID -> Hasqueue Bool
bucketExists bid =
    getBucket bid >> return True `catchError` \_ -> return False

-- | Test if a 'Value' exists.
valueExists :: BucketID -> ValueID -> Hasqueue Bool
valueExists bid vid =
    getValue bid vid >> return True `catchError` \_ -> return False

-- | Get the current view of the world.
getWorld :: Hasqueue World
getWorld = ask

-- | Get a 'Bucket', raising an error if it does not exist.
getBucket :: BucketID -> Hasqueue Bucket
getBucket bid = do
    world <- ask
    result <- liftSTM $ T.lookup bid world
    case result of
        Nothing -> throwError $ NoSuchBucket bid
        Just bucket -> return bucket

-- | Get a 'Value', raising an error if it does not exist.
getValue :: BucketID -> ValueID -> Hasqueue Value
getValue bid vid = do
    bucket <- getBucket bid
    result <- liftSTM $ T.lookup vid bucket
    case result of
        Nothing -> throwError $ NoSuchValue bid vid
        Just value -> return value

-- | Get all of the 'BucketID's.
buckets :: Hasqueue [BucketID]
buckets = do
    world <- ask
    liftSTM $ T.keys world

-- | Get all of the 'ValueID's.
values :: BucketID -> Hasqueue [ValueID]
values bid = do
    bucket <- getBucket bid
    liftSTM $ T.keys bucket

-- | Set a 'Bucket'.
putBucket :: BucketID -> Bucket -> Hasqueue ()
putBucket bid bucket = do
    world <- ask
    liftSTM $ T.insert bid bucket world

-- | Set a 'Value'.
putValue :: BucketID -> ValueID -> Value -> Hasqueue ()
putValue bid vid value = do
    bucket <- getBucket bid
    liftSTM $ T.insert vid value bucket

-- | Rename a 'Bucket'.
renameBucket :: BucketID -> BucketID -> Hasqueue ()
renameBucket old new = do
    bucket <- getBucket old
    deleteBucket old
    putBucket new bucket

-- | Rename a 'Value'.
renameValue :: (BucketID, ValueID) -> (BucketID, ValueID) -> Hasqueue ()
renameValue (oldBID, oldVID) (newBID, newVID) = do
    value <- getValue oldBID oldVID
    deleteValue oldBID oldVID
    putValue newBID newVID value

-- | Change a 'Value' with the given function.
modifyValue :: BucketID -> ValueID -> (Value -> Value) -> Hasqueue ()
modifyValue bid vid func = getValue bid vid >>= putValue bid vid . func

-- | Delete a 'Bucket'.
deleteBucket :: BucketID -> Hasqueue ()
deleteBucket bid = do
    world <- ask
    liftSTM $ T.delete bid world

-- | Delete a 'Value'.
deleteValue :: BucketID -> ValueID -> Hasqueue ()
deleteValue bid vid = do
    bucket <- getBucket bid
    liftSTM $ T.delete vid bucket
