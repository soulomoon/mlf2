{-# LANGUAGE LambdaCase #-}

module BackendStructuralRecursiveDataSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import MLF.Backend.IR
import MLF.Backend.StructuralRecursiveData
import MLF.Constraint.Types.Graph (BaseTy (..))
import Test.Hspec

spec :: Spec
spec = describe "MLF.Backend.StructuralRecursiveData" $ do
  it "requires exact canonical data identity for metadata-light matches" $ do
    metadataLightStructuralDataMatches (BaseTy "Core.T") [] "$Core.T_self" nullaryStructuralBody
      `shouldBe` True

    metadataLightStructuralDataMatches (BaseTy "Other.T") [] "$Core.T_self" nullaryStructuralBody
      `shouldBe` False

    metadataLightStructuralDataMatches (BaseTy "Other.T") [] "$T_self" nullaryStructuralBody
      `shouldBe` False

  it "matches recursive payloads with a self-cycle guard and returns focused field evidence" $ do
    let structuralTy = structuralListTy intTy
        substitution = Map.fromList [("a", intTy)]

    case matchStructuralDataDeclaration Map.empty listData substitution structuralTy of
      Right match -> do
        srdmDataName match `shouldBe` "List"
        srdmParameterSubstitution match `shouldBe` substitution
      Left mismatch ->
        expectationFailure ("expected recursive structural match, got " ++ show mismatch)

    case matchFocusedStructuralConstructor Map.empty listData consConstructor substitution structuralTy of
      Right match ->
        srcmFieldTypes match `shouldBe` [intTy, structuralTy]
      Left mismatch ->
        expectationFailure ("expected focused constructor match, got " ++ show mismatch)

  it "rejects substitution mismatches in recursive payload fields" $ do
    let structuralTy = structuralListTy boolTy
        substitution = Map.fromList [("a", intTy)]

    matchStructuralDataDeclaration Map.empty listData substitution structuralTy
      `shouldSatisfy` isLeft

  it "rejects missing or extra structural constructors under metadata-backed matching" $ do
    matchStructuralDataDeclaration Map.empty listData (Map.fromList [("a", intTy)]) missingConsStructuralListTy
      `shouldSatisfy` isLeft

    matchStructuralDataDeclaration Map.empty listData (Map.fromList [("a", intTy)]) extraConstructorStructuralListTy
      `shouldSatisfy` isLeft

  it "keeps metadata-light skeleton evidence from standing in for full ADT evidence" $ do
    metadataLightStructuralDataMatches (BaseTy "UnitLike") [] "$UnitLike_self" nullaryStructuralBody
      `shouldBe` True

    matchStructuralDataDeclaration Map.empty unitLikeData Map.empty (BTMu "$UnitLike_self" nullaryStructuralBody)
      `shouldSatisfy` isLeft

isLeft :: Either left right -> Bool
isLeft =
  \case
    Left _ -> True
    Right _ -> False

intTy :: BackendType
intTy = BTBase (BaseTy "Int")

boolTy :: BackendType
boolTy = BTBase (BaseTy "Bool")

listTy :: BackendType -> BackendType
listTy arg =
  BTCon (BaseTy "List") (arg :| [])

listData :: BackendData
listData =
  BackendData
    { backendDataName = "List",
      backendDataParameters = ["a"],
      backendDataConstructors = [nilConstructor, consConstructor]
    }

nilConstructor :: BackendConstructor
nilConstructor =
  BackendConstructor
    { backendConstructorName = "Nil",
      backendConstructorForalls = [],
      backendConstructorFields = [],
      backendConstructorResult = listTy (BTVar "a")
    }

consConstructor :: BackendConstructor
consConstructor =
  BackendConstructor
    { backendConstructorName = "Cons",
      backendConstructorForalls = [],
      backendConstructorFields = [BTVar "a", listTy (BTVar "a")],
      backendConstructorResult = listTy (BTVar "a")
    }

unitLikeData :: BackendData
unitLikeData =
  BackendData
    { backendDataName = "UnitLike",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            { backendConstructorName = "UnitLike",
              backendConstructorForalls = [],
              backendConstructorFields = [],
              backendConstructorResult = BTBase (BaseTy "UnitLike")
            }
        ]
    }

structuralListTy :: BackendType -> BackendType
structuralListTy headField =
  BTMu "$List_self" (listStructuralBody headField)

missingConsStructuralListTy :: BackendType
missingConsStructuralListTy =
  BTMu "$List_self" (BTForall "r" Nothing (BTArrow (BTVar "r") (BTVar "r")))

extraConstructorStructuralListTy :: BackendType
extraConstructorStructuralListTy =
  BTMu
    "$List_self"
    ( BTForall
        "r"
        Nothing
        ( BTArrow
            (BTVar "r")
            ( BTArrow
                (BTArrow intTy (BTArrow (BTVar "$List_self") (BTVar "r")))
                (BTArrow (BTVar "r") (BTVar "r"))
            )
        )
    )

listStructuralBody :: BackendType -> BackendType
listStructuralBody headField =
  BTForall
    "r"
    Nothing
    ( BTArrow
        (BTVar "r")
        (BTArrow (BTArrow headField (BTArrow (BTVar "$List_self") (BTVar "r"))) (BTVar "r"))
    )

nullaryStructuralBody :: BackendType
nullaryStructuralBody =
  BTForall "r" Nothing (BTVar "r")
