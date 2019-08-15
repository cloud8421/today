{-# LANGUAGE OverloadedStrings #-}

module RefsSpec
  ( spec
  ) where

import qualified Refs
import Test.Hspec

spec :: Spec
spec =
  describe "Refs" $ do
    describe "extracting refs" $
      it "finds all tokens" $ do
        let text = "Fix issues in CORE#123 and SERVER#456"
        let expectedRefs =
              [ Refs.Ref "CORE" "123" "CORE#123"
              , Refs.Ref "SERVER" "456" "SERVER#456"
              ]
        Refs.extractRefs text `shouldBe` expectedRefs
    describe "resolving refs" $ do
      it "replaces with the corresponding entry" $ do
        let ref = Refs.Ref "TODAY" "789" "TODAY#789"
        let expectedText = "https://github.com/cloud8421/today/issues/789"
        Refs.resolveRef ref Refs.defaultRefMap `shouldBe` expectedText
      it "returns an error text when unable to resolve" $ do
        let ref = Refs.Ref "OTHER" "789" "TODAY#789"
        let expectedText = "Cannot resolve OTHER#789"
        Refs.resolveRef ref Refs.defaultRefMap `shouldBe` expectedText
    describe "replace refs" $
      it
        "replaces with the corresponding entry, skipping refs that cannot be resolved" $ do
        let text = "Fix issue TODAY#456 and OTHER#789"
        let expectedText =
              "Fix issue https://github.com/cloud8421/today/issues/456 and OTHER#789"
        Refs.replaceRefs text Refs.defaultRefMap `shouldBe` expectedText
