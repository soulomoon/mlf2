{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module PhaseSingletonsSpec (spec) where

import MLF.Constraint.Types.Phase (Next, SPhase (..))
import Test.Hspec

data SomePhase where
    SomePhase :: SPhase p -> SomePhase

phaseName :: SPhase p -> String
phaseName SRaw        = "Raw"
phaseName SNormalized = "Normalized"
phaseName SAcyclic    = "Acyclic"
phaseName SPresolved  = "Presolved"
phaseName SSolved     = "Solved"

nextPhase :: SPhase p -> SPhase (Next p)
nextPhase SRaw        = SNormalized
nextPhase SNormalized = SAcyclic
nextPhase SAcyclic    = SPresolved
nextPhase SPresolved  = SSolved
nextPhase SSolved     = SSolved

spec :: Spec
spec = describe "Phase singleton foundation" $ do
    it "exports singleton constructors for each pipeline phase" $ do
        let phases =
                [ SomePhase SRaw
                , SomePhase SNormalized
                , SomePhase SAcyclic
                , SomePhase SPresolved
                , SomePhase SSolved
                ]

        map (\(SomePhase phase) -> phaseName phase) phases `shouldBe`
            [ "Raw"
            , "Normalized"
            , "Acyclic"
            , "Presolved"
            , "Solved"
            ]

    it "supports Next-typed progression across the pipeline" $ do
        phaseName (nextPhase SRaw) `shouldBe` "Normalized"
        phaseName (nextPhase SNormalized) `shouldBe` "Acyclic"
        phaseName (nextPhase SAcyclic) `shouldBe` "Presolved"
        phaseName (nextPhase SPresolved) `shouldBe` "Solved"
        phaseName (nextPhase SSolved) `shouldBe` "Solved"
