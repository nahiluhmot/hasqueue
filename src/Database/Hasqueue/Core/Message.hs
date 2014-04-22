module Database.Hasqueue.Core.Message ( StoreRequest(..)
                                      , StoreResponse(..)
                                      ) where

import Database.Hasqueue.Core.Value as V

-- | 'StoreRequest's are sent to the persistence layer.
data StoreRequest = ListBuckets
                  | CreateBucket V.BucketID
                  | DeleteBucket V.BucketID
                  | RenameBucket V.BucketID V.BucketID
                  | ListBucket V.BucketID
                  | GetValue V.BucketID V.ValueID
                  | DeleteValue V.BucketID V.ValueID
                  | PutValue V.BucketID V.ValueID V.Value
                  | RenameValue (V.BucketID, V.ValueID) (V.BucketID, V.ValueID)
                  deriving (Eq, Show)

-- | 'StoreResponse's are returned by the persistence layer.
data StoreResponse = Buckets [BucketID]
                   | Bucket BucketID
                   | Values [ValueID]
                   | Value Value   
                   | Empty
                   deriving (Eq, Show)
