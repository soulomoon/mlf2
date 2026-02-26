module FrozenParitySpec (spec) where

import Test.Hspec

import Parity.FrozenArtifacts
    ( baselineFilePath
    , buildFrozenBaseline
    , metadataFromRenderedJson
    , renderFrozenBaselineJson
    )

spec :: Spec
spec = describe "Frozen parity artifact baseline" $ do
    it "matches legacy replay baseline v1 for solved artifacts and elaborated types" $ do
        expected <- readFile baselineFilePath
        metadata <- requireRight (metadataFromRenderedJson expected)
        actual <- requireRight (buildFrozenBaseline metadata)
        renderFrozenBaselineJson actual `shouldBe` expected

requireRight :: Either String a -> IO a
requireRight value =
    case value of
        Left err -> expectationFailure err >> fail "requireRight"
        Right out -> pure out
