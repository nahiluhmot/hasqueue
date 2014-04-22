-- | This module holds the base data types and functions to interact with
-- them. This is the lowest-level interface to interact with the datastore.
module Database.Hasqueue.Core.Value ( -- * Base Types
                                      BucketID
                                    , ValueID
                                    , Value(..)
                                      -- * Error
                                    , HasqueueError(..)
                                    ) where

import qualified Control.Monad.Error.Class as E
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Sequence as SQ

-- | Used to uniquely identify 'Bucket's in a 'World'.
type BucketID = B.ByteString

-- | Used to uniquely identify 'Value's in a 'Bucket'.
type ValueID = B.ByteString

-- | A 'Value' is any type that may be stored in the datastore.
data Value = Null
           | Boolean Bool
           | Int Int
           | Double Double
           | String B.ByteString
           | List (SQ.Seq Value)
           | Hash (M.Map B.ByteString Value)
           deriving (Eq, Show)

-- | A 'HasqueueError' is thrown when something unexpected happens during
-- a datastore lookup.
data HasqueueError = InternalError B.ByteString
                   | ParseError B.ByteString
                   | BucketExists BucketID
                   | NoSuchBucket BucketID
                   | NoSuchValue BucketID ValueID
                   deriving (Eq, Show)

instance E.Error HasqueueError where
    strMsg = InternalError . B.pack . map (toEnum . fromEnum)
