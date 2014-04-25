{-# LANGUAGE OverloadedStrings #-}

module Database.Hasqueue.Store.SimpleSpec ( spec ) where

import Control.Concurrent.STM.Class
import Control.Monad
import Database.Hasqueue
import Data.List (sort)
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent
import Test.Hspec

spec :: Spec
spec = do
    let withSimple :: (Simple -> IO ()) -> IO ()
        withSimple f = do
            simple <- startService
            f simple
            stopService simple

    describe "listing all buckets" $ do
        let buckets = ["bucket-one", "bucket-two", "bucket-three"]
            commands = map CreateBucket buckets ++ [ListBuckets]

        it "returns a list of buckets" $ do
            (output, input) <- spawn Unbounded

            withSimple $ \simple ->
                runEffect $ each commands >-> toPipe simple >-> toOutput output

            forM_ buckets $ \bid -> do
                result <- liftSTM $ recv input
                result `shouldBe` Just (Right (Bucket bid))

            result <- liftSTM $ recv input
            case result of
                Just (Right (Buckets bids)) -> sort bids `shouldBe` sort buckets
                _ -> fail "Could not list buckets."

    describe "creating a bucket" $ do
        let bid = "test-bucket"

        context "when the bucket does not exist" $ do
            let commands = [CreateBucket bid, ListBuckets]

            it "creates the bucket" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple ->
                    runEffect $ each commands >-> toPipe simple >-> toOutput output

                result <- liftSTM $ recv input
                result' <- liftSTM $ recv input

                result `shouldBe` Just (Right (Bucket bid))
                result' `shouldBe` Just (Right (Buckets [bid]))

        context "when the bucket does exist" $ do
            let commands = [CreateBucket bid, CreateBucket bid, ListBuckets]

            it "throws an error" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple ->
                    runEffect $ each commands >-> toPipe simple >-> toOutput output

                result <- liftSTM $ recv input
                result' <- liftSTM $ recv input
                result'' <- liftSTM $ recv input

                result `shouldBe` Just (Right (Bucket bid))
                result' `shouldBe` Just (Left (BucketExists bid))
                result'' `shouldBe` Just (Right (Buckets [bid]))

    describe "deleting a bucket" $ do
        let bid = "sample-bucket"

        context "when the bucket does not exist" $ do
            let commands = [DeleteBucket bid, ListBuckets]

            it "does nothing" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple -> 
                    runEffect $ each commands >-> toPipe simple >-> toOutput output
                
                result <- liftSTM $ recv input
                result' <- liftSTM $ recv input

                result `shouldBe` Just (Right Empty)
                result' `shouldBe` Just (Right (Buckets []))

        context "when the bucket does exist" $ do
            let commands = [CreateBucket bid, ListBuckets, DeleteBucket bid, ListBuckets]

            it "deletes that bucket" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple -> 
                    runEffect $ each commands >-> toPipe simple >-> toOutput output
                
                result <- liftSTM $ recv input
                result' <- liftSTM $ recv input
                result'' <- liftSTM $ recv input
                result''' <- liftSTM $ recv input

                result `shouldBe` Just (Right (Bucket bid))
                result' `shouldBe` Just (Right (Buckets [bid]))
                result'' `shouldBe` Just (Right Empty)
                result''' `shouldBe` Just (Right (Buckets []))

    describe "renaming a bucket" $ do
        let old = "old-bid"
            new = "new-bid"
        context "when the original bucket does not exist" $ do
            let commands = [RenameBucket old new]

            it "returns an error" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple ->
                    runEffect $ each commands >-> toPipe simple >-> toOutput output

                result <- liftSTM $ recv input
                result `shouldBe` Just (Left (NoSuchBucket old))

        context "when the original bucket exists" $ do
            context "but the target bucket exists" $ do
                let commands = [CreateBucket old, CreateBucket new, RenameBucket old new]

                it "returns an error" $ do
                    (output, input) <- spawn Unbounded

                    withSimple $ \simple ->
                        runEffect $ each commands >-> toPipe simple >-> toOutput output

                    result <- liftSTM $ recv input
                    result' <- liftSTM $ recv input
                    result'' <- liftSTM $ recv input

                    result `shouldBe` Just (Right (Bucket old))
                    result' `shouldBe` Just (Right (Bucket new))
                    result'' `shouldBe` Just (Left (BucketExists new))

            context "and the target bucket does not exist" $ do
                let value = String "test"
                    valueID = "test-value-id"
                    commands = [CreateBucket old, PutValue old valueID value, RenameBucket old new]

                it "moves the bucket" $ do
                    (output, input) <- spawn Unbounded

                    withSimple $ \simple ->
                        runEffect $ each commands >-> toPipe simple >-> toOutput output

                    result <- liftSTM $ recv input
                    result' <- liftSTM $ recv input
                    result'' <- liftSTM $ recv input

                    result `shouldBe` Just (Right (Bucket old))
                    result' `shouldBe` Just (Right Empty)
                    result'' `shouldBe` Just (Right (Bucket new))

    describe "listing the contents of a bucket" $ do
        let bid = "tmp-bid"

        context "when the bucket is does not exist" $ do
            let commands = [ListBucket bid]

            it "returns an error" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple ->
                    runEffect $ each commands >-> toPipe simple >-> toOutput output

                result <- liftSTM $ recv input
                result `shouldBe` Just (Left (NoSuchBucket bid))

        context "when the bucket exists" $ do
            let vid = "tmp-vid"
                value = Int 1
                commands = [CreateBucket bid, PutValue bid vid value, ListBucket bid]

            it "lists the keys" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple ->
                    runEffect $ each commands >-> toPipe simple >-> toOutput output

                result <- liftSTM $ recv input
                result' <- liftSTM $ recv input
                result'' <- liftSTM $ recv input

                result `shouldBe` Just (Right (Bucket bid))
                result' `shouldBe` Just (Right Empty)
                result'' `shouldBe` Just (Right (Values [vid]))

    describe "accessing a value" $ do
        let bid = "my-bid"
            vid = "my-vid"

        context "when the value's bucket does not exist" $ do
            let commands = [GetValue bid vid]

            it "returns an error" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple ->
                    runEffect $ each commands >-> toPipe simple >-> toOutput output

                result <- liftSTM $ recv input
                result `shouldBe` Just (Left (NoSuchBucket bid))

        context "when the value's bucket exists" $ do
            context "when the value does not exist" $ do
                let commands = [CreateBucket bid, GetValue bid vid]

                it "returns an error" $ do
                    (output, input) <- spawn Unbounded

                    withSimple $ \simple ->
                        runEffect $ each commands >-> toPipe simple >-> toOutput output

                    result <- liftSTM $ recv input
                    result' <- liftSTM $ recv input

                    result `shouldBe` Just (Right (Bucket bid))
                    result' `shouldBe` Just (Left (NoSuchValue bid vid))

            context "when the value does exists" $ do
                let value = Double 1.2
                    commands = [CreateBucket bid, PutValue bid vid value, GetValue bid vid]

                it "returns that value" $ do
                    (output, input) <- spawn Unbounded

                    withSimple $ \simple ->
                        runEffect $ each commands >-> toPipe simple >-> toOutput output

                    result <- liftSTM $ recv input
                    result' <- liftSTM $ recv input
                    result'' <- liftSTM $ recv input

                    result `shouldBe` Just (Right (Bucket bid))
                    result' `shouldBe` Just (Right Empty)
                    result'' `shouldBe` Just (Right (Value value))

    describe "deleting a value" $ do
        let bid = "sample-bid"
            vid = "sample-vid"

        context "when the bucket does not exist" $ do
            let commands = [DeleteValue bid vid]

            it "returns an error" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple ->
                    runEffect $ each commands >-> toPipe simple >-> toOutput output

                result <- liftSTM $ recv input
                result `shouldBe` Just (Left (NoSuchBucket bid))

        context "when the bucket does exists" $ do
            context "but the value doesn't exist" $ do
                let commands = [CreateBucket bid, ListBucket bid, DeleteValue bid vid, ListBucket bid]

                it "does nothing" $ do
                    (output, input) <- spawn Unbounded

                    withSimple $ \simple ->
                        runEffect $ each commands >-> toPipe simple >-> toOutput output

                    result <- liftSTM $ recv input
                    result' <- liftSTM $ recv input
                    result'' <- liftSTM $ recv input
                    result''' <- liftSTM $ recv input

                    result `shouldBe` Just (Right (Bucket bid))
                    result' `shouldBe` Just (Right (Values []))
                    result'' `shouldBe` Just (Right Empty)
                    result''' `shouldBe` Just (Right (Values []))

            context "and the value exists" $ do
                let value = Null
                    commands = [CreateBucket bid, PutValue bid vid value, ListBucket bid, DeleteValue bid vid, ListBucket bid]

                it "deletes that value" $ do
                    (output, input) <- spawn Unbounded

                    withSimple $ \simple ->
                        runEffect $ each commands >-> toPipe simple >-> toOutput output

                    result <- liftSTM $ recv input
                    result' <- liftSTM $ recv input
                    result'' <- liftSTM $ recv input
                    result''' <- liftSTM $ recv input
                    result'''' <- liftSTM $ recv input

                    result `shouldBe` Just (Right (Bucket bid))
                    result' `shouldBe` Just (Right Empty)
                    result'' `shouldBe` Just (Right (Values [vid]))
                    result''' `shouldBe` Just (Right Empty)
                    result'''' `shouldBe` Just (Right (Values []))

    describe "putting a value" $ do
        let bid = "put-bucket"
            vid = "put-value"
            value = Int 2

        context "when the bucket does not exist" $ do
            let commands = [PutValue bid vid value]

            it "returns an error" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple ->
                    runEffect $ each commands >-> toPipe simple >-> toOutput output

                result <- liftSTM $ recv input
                result `shouldBe` Just (Left (NoSuchBucket bid))

        context "when the bucket does exist" $ do
            context "when the value doesn't exist" $ do
                let commands = [ CreateBucket bid
                               , PutValue bid vid value
                               , GetValue bid vid
                               ]
                it "puts the value" $ do
                    (output, input) <- spawn Unbounded

                    withSimple $ \simple ->
                        runEffect $ each commands >-> toPipe simple >-> toOutput output

                    result <- liftSTM $ recv input
                    result' <- liftSTM $ recv input
                    result'' <- liftSTM $ recv input

                    result `shouldBe` Just (Right (Bucket bid))
                    result' `shouldBe` Just (Right Empty)
                    result'' `shouldBe` Just (Right (Value value))

            context "when the value already exists" $ do
                let value' = Null
                    commands = [ CreateBucket bid
                               , PutValue bid vid value'
                               , GetValue bid vid
                               , PutValue bid vid value
                               , GetValue bid vid
                               ]

                it "puts the value" $ do
                    (output, input) <- spawn Unbounded

                    withSimple $ \simple ->
                        runEffect $ each commands >-> toPipe simple >-> toOutput output

                    result <- liftSTM $ recv input
                    result' <- liftSTM $ recv input
                    result'' <- liftSTM $ recv input
                    result''' <- liftSTM $ recv input
                    result'''' <- liftSTM $ recv input

                    result `shouldBe` Just (Right (Bucket bid))
                    result' `shouldBe` Just (Right Empty)
                    result'' `shouldBe` Just (Right (Value value'))
                    result''' `shouldBe` Just (Right Empty)
                    result'''' `shouldBe` Just (Right (Value value))

    describe "renaming a value" $ do
        let oldBID = "old-bid"
            oldVID = "old-vid"
            newBID = "new-bid"
            newVID = "new-vid"
            old = (oldBID, oldVID)
            new = (newBID, newVID)
            value = String "hello"

        context "when the source bucket does not exist" $ do
            let commands = [RenameValue old new]

            it "returns an error" $ do
                (output, input) <- spawn Unbounded

                withSimple $ \simple ->
                    runEffect $ each commands >-> toPipe simple >-> toOutput output

                result <- liftSTM $ recv input
                result `shouldBe` Just (Left (NoSuchBucket oldBID))

        context "when the source bucket exists" $ do
            context "when the source value does not exist" $ do
                let commands = [CreateBucket oldBID, RenameValue old new]

                it "returns an error" $ do
                    (output, input) <- spawn Unbounded

                    withSimple $ \simple ->
                        runEffect $ each commands >-> toPipe simple >-> toOutput output

                    result <- liftSTM $ recv input
                    result' <- liftSTM $ recv input

                    result `shouldBe` Just (Right (Bucket oldBID))
                    result' `shouldBe` Just (Left (NoSuchValue oldBID oldVID))

            context "when the source value exists" $ do
                context "when the target bucket does not exist" $ do
                    let commands = [CreateBucket oldBID, PutValue oldBID oldVID value, RenameValue old new]

                    it "returns an error" $ do
                        (output, input) <- spawn Unbounded

                        withSimple $ \simple ->
                            runEffect $ each commands >-> toPipe simple >-> toOutput output

                        result <- liftSTM $ recv input
                        result' <- liftSTM $ recv input
                        result'' <- liftSTM $ recv input

                        result `shouldBe` Just (Right (Bucket oldBID))
                        result' `shouldBe` Just (Right Empty)
                        result'' `shouldBe` Just (Left (NoSuchBucket newBID))

                context "when the target bucket exists" $ do
                    context "when the target value does not exist" $ do
                        let commands = [ CreateBucket oldBID
                                       , PutValue oldBID oldVID value
                                       , CreateBucket newBID
                                       , RenameValue old new
                                       , uncurry GetValue new
                                       ]

                        it "renames the value" $ do
                            (output, input) <- spawn Unbounded

                            withSimple $ \simple ->
                                runEffect $ each commands >-> toPipe simple >-> toOutput output

                            result <- liftSTM $ recv input
                            result' <- liftSTM $ recv input
                            result'' <- liftSTM $ recv input
                            result''' <- liftSTM $ recv input
                            result'''' <- liftSTM $ recv input

                            result `shouldBe` Just (Right (Bucket oldBID))
                            result' `shouldBe` Just (Right Empty)
                            result'' `shouldBe` Just (Right (Bucket newBID))
                            result''' `shouldBe` Just (Right Empty)
                            result'''' `shouldBe` Just (Right (Value value))

                    context "when the target value exists" $ do
                        let value' = Null
                            commands = [ CreateBucket oldBID
                                       , PutValue oldBID oldVID value
                                       , CreateBucket newBID
                                       , PutValue newBID newVID value'
                                       , uncurry GetValue new
                                       , RenameValue old new
                                       , uncurry GetValue new
                                       ]

                        it "clobbers the old value" $ do
                            (output, input) <- spawn Unbounded

                            withSimple $ \simple ->
                                runEffect $ each commands >-> toPipe simple >-> toOutput output

                            result <- liftSTM $ recv input
                            result' <- liftSTM $ recv input
                            result'' <- liftSTM $ recv input
                            result''' <- liftSTM $ recv input
                            result'''' <- liftSTM $ recv input
                            result''''' <- liftSTM $ recv input
                            result'''''' <- liftSTM $ recv input

                            result `shouldBe` Just (Right (Bucket oldBID))
                            result' `shouldBe` Just (Right Empty)
                            result'' `shouldBe` Just (Right (Bucket newBID))
                            result''' `shouldBe` Just (Right Empty)
                            result'''' `shouldBe` Just (Right (Value value'))
                            result''''' `shouldBe` Just (Right Empty)
                            result'''''' `shouldBe` Just (Right (Value value))

    describe "shutting down" $ do
        let commands = [CreateBucket "test-bucket"]

        it "does not respond after a shutdown" $ do
            pending
            (output, input) <- spawn Unbounded

            putStrLn "pre start"
            simple <- startService :: IO Simple
            stopService simple
            putStrLn "pre stop"
            runEffect $ each commands >-> toPipe simple >-> toOutput output
            putStrLn "post stop"

            exhausted <- P.null $ fromInput input
            unless exhausted $ fail "Output should be closed"
