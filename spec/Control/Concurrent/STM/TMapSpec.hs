module Control.Concurrent.STM.TMapSpec ( spec ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMap
import Control.Monad
import Prelude hiding (null, lookup)
import Test.Hspec

spec :: Spec
spec = do
    describe "newTMap" $
        it "creates a new empty TMap" $ do
            isEmpty <- atomically $ newTMap >>= null
            unless isEmpty $ fail "TMap is not empty."

    describe "newTMapIO" $
        it "creates a new empty TMap" $ do
            isEmpty <- newTMapIO >>= atomically . null
            unless isEmpty $ fail "TMap is not empty."

    describe "size" $ do
        context "when the TMap is empty" $
            it "returns 0" $ do
                elems <- atomically $ newTMap >>= size
                elems `shouldBe` 0

        context "when the TMap is not empty" $
            it "returns the size" $ do
                elems <- atomically $ do
                    tmap <- newTMap
                    insert "hello" "world" tmap
                    insert "goodbye" "world" tmap
                    size tmap
                elems `shouldBe` 2

    describe "null" $ do
        context "when the TMap is empty" $
            it "returns True" $ do
                isEmpty <- atomically $ newTMap >>= null
                unless isEmpty $ fail "TMap is not empty."

        context "when the TMap is not empty" $
            it "returns False" $ do
                isEmpty <- atomically $ do
                    tmap <- newTMap
                    insert 'a' (15 :: Int) tmap
                    null tmap
                when isEmpty $ fail "TMap is empty."

    describe "lookup" $ do
        context "when the key is not in the TMap" $
            it "returns Nothing" $ do
                result <- atomically $
                    (newTMap :: STM (TMap String ())) >>= lookup "key"
                result `shouldBe` Nothing

        context "when the key is in the TMap" $
            it "returns the associated value" $ do
                result <- atomically $ do
                    tmap <- newTMap
                    insert "test" (15 :: Int) tmap
                    lookup "test" tmap
                result `shouldBe` Just 15

    describe "keys" $
        it "returns each key in the TMap" $ do
            keys' <- atomically $ do
                tmap <- newTMap
                insert 'a' (1 :: Int) tmap
                insert 'b' 2 tmap
                insert 'c' 3 tmap
                keys tmap
            keys' `shouldBe` "abc"

    describe "insert" $
        it "inserts a value into the TMap" $ do
            tmap <- newTMapIO
            firstLookup <- atomically $ lookup "15" tmap
            atomically $ insert "15" '8' tmap
            secondLookup <- atomically $ lookup "15" tmap
            firstLookup `shouldBe` Nothing
            secondLookup `shouldBe` Just '8'

    describe "delete" $ do
        context "when the key does not exist in the TMap" $
            it "does not modify the TMap" $ do
                tmap <- atomically $ do
                    tmap' <- newTMap
                    insert "80" 'a' tmap'
                    insert "alpha" 'x' tmap'
                    return tmap'
                map' <- atomically $ toMap tmap
                atomically $ delete "79" tmap
                map'' <- atomically $ toMap tmap
                map' `shouldBe` map''

        context "when the key does exist in the TMap" $
            it "does not modify the TMap" $ do
                tmap <- atomically $ do
                    tmap' <- newTMap
                    insert (10 :: Int) "greetings" tmap'
                    insert 12 "human" tmap'
                    return tmap'
                result <- atomically $ lookup 10 tmap
                atomically $ delete 10 tmap
                result' <- atomically $ lookup 10 tmap
                result `shouldBe` Just "greetings"
                result' `shouldBe` Nothing

    describe "fromList" $ do
        let input :: [(String, Int)]
            input = [ ("alpha", 1)
                    , ("beta", 2)
                    ]
        it "creates a TMap from the given List" $ do
            tmap <- atomically $ fromList input
            alphaValue <- atomically $ lookup "alpha" tmap
            betaValue <- atomically $ lookup "beta" tmap
            gammaValue <- atomically $ lookup "gamma" tmap
            alphaValue `shouldBe` Just 1
            betaValue `shouldBe` Just 2
            gammaValue `shouldBe` Nothing

    describe "toList" $ do
        let expected :: [(Char, Bool)]
            expected = [ ('o', False)
                       , ('y', True)
                       , ('z', True)
                       ]
        it "creates a List from the given TMap" $ do
            list <- atomically $ do
                tmap <- newTMap
                insert 'z' True tmap
                insert 'y' True tmap
                insert 'o' False tmap
                toList tmap
            list `shouldBe` expected
