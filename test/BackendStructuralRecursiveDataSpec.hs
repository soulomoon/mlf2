{-# LANGUAGE LambdaCase #-}

module BackendStructuralRecursiveDataSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import MLF.Backend.IR
import MLF.Backend.StructuralRecursiveData
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId (..))
import MLF.Frontend.Symbol (SymbolIdentity (..), SymbolNamespace (..))
import MLF.Types.Identity (typeBinderIdentityFromNode)
import MLF.Types.Unique (UniqueIdentity (..))
import Test.Hspec

spec :: Spec
spec = describe "MLF.Backend.StructuralRecursiveData" $ do
  it "requires exact canonical data identity for metadata-light matches" $ do
    metadataLightStructuralDataMatches (BaseTy "Core.T") [] "$Core.T_self" nullaryStructuralBody
      `shouldBe` True

    metadataLightStructuralDataMatches (BaseTy "Core.T") [] "$Core.T_self1" nullaryStructuralBody
      `shouldBe` True

    metadataLightStructuralDataMatches (BaseTy "Other.T") [] "$Core.T_self" nullaryStructuralBody
      `shouldBe` False

    metadataLightStructuralDataMatches (BaseTy "Other.T") [] "$T_self" nullaryStructuralBody
      `shouldBe` False

  it "matches metadata-light recursive payload parameters without counting self fields" $
    metadataLightStructuralDataMatches (BaseTy "List") [intTy] "$List_self" (listStructuralBody intTy)
      `shouldBe` True

  it "treats freshened recursive self fields as self payloads" $
    metadataLightStructuralDataMatches (BaseTy "List") [intTy] "$List_self" (freshenedSelfListStructuralBody intTy)
      `shouldBe` True

  it "matches recursive payloads with a self-cycle guard and returns focused field evidence" $ do
    let structuralTy = structuralListTy intTy
        substitution = subst [("a", intTy)]

    case matchStructuralDataDeclaration Map.empty listData substitution structuralTy of
      Right match -> do
        srdmDataName match `shouldBe` "List"
        srdmParameterSubstitution match `shouldBe` substitution
      Left mismatch ->
        expectationFailure ("expected recursive structural match, got " ++ show mismatch)

    case matchFocusedStructuralConstructor Map.empty listData consConstructor substitution structuralTy of
      Right match -> do
        srcmConstructorName match `shouldBe` "Cons"
        srcmFieldTypes match `shouldBe` [intTy, structuralTy]
      Left mismatch ->
        expectationFailure ("expected focused constructor match, got " ++ show mismatch)

  it "matches focused constructors by identity when the constructor name is stale" $ do
    let structuralTy = structuralListTy intTy
        substitution = subst [("a", intTy)]

    case matchFocusedStructuralConstructor Map.empty identityListData staleConsConstructor substitution structuralTy of
      Right match -> do
        srcmConstructorIdentity match `shouldBe` Just consIdentity
        srcmConstructorName match `shouldBe` "Cons"
        srcmFieldTypes match `shouldBe` [intTy, structuralTy]
      Left mismatch ->
        expectationFailure ("expected identity-focused constructor match, got " ++ show mismatch)

  it "rejects substitution mismatches in recursive payload fields" $ do
    let structuralTy = structuralListTy boolTy
        substitution = subst [("a", intTy)]

    matchStructuralDataDeclaration Map.empty listData substitution structuralTy
      `shouldSatisfy` isLeft

  it "rejects missing or extra structural constructors under metadata-backed matching" $ do
    matchStructuralDataDeclaration Map.empty listData (subst [("a", intTy)]) missingConsStructuralListTy
      `shouldSatisfy` isLeft

    matchStructuralDataDeclaration Map.empty listData (subst [("a", intTy)]) extraConstructorStructuralListTy
      `shouldSatisfy` isLeft

  it "matches constructor parameters by identity when binder names collide" $ do
    let leftIdentity = typeBinderIdentityFromNode (NodeId 991301)
        rightIdentity = typeBinderIdentityFromNode (NodeId 991302)
        leftKey = BackendTypeSubstitutionByIdentity leftIdentity
        rightKey = BackendTypeSubstitutionByIdentity rightIdentity
        leftVar = BTVarWithIdentity (Just leftIdentity) "a"
        rightVar = BTVarWithIdentity (Just rightIdentity) "a"
        parameterBounds = Map.fromList [(leftKey, Nothing), (rightKey, Nothing)]
        expected = BTArrow leftVar rightVar
        actual = BTArrow intTy boolTy

    matchBackendTypeParametersWithTypeBounds Map.empty [] parameterBounds Map.empty expected actual
      `shouldBe` Just (Map.fromList [(leftKey, intTy), (rightKey, boolTy)])

  it "checks actual type variable bounds by identity when names are stale" $ do
    let expectedIdentity = typeBinderIdentityFromNode (NodeId 991303)
        actualIdentity = typeBinderIdentityFromNode (NodeId 991304)
        expectedKey = BackendTypeSubstitutionByIdentity expectedIdentity
        actualKey = BackendTypeSubstitutionByIdentity actualIdentity
        parameterBounds = Map.singleton expectedKey (Just intTy)
        typeBounds = Map.fromList [(actualKey, Just intTy), (BackendTypeSubstitutionByName "a", Just boolTy)]
        expected = BTVarWithIdentity (Just expectedIdentity) "a"
        actual = BTVarWithIdentity (Just actualIdentity) "a"

    matchBackendTypeParametersWithTypeBounds typeBounds [] parameterBounds Map.empty expected actual
      `shouldBe` Just (Map.singleton expectedKey actual)

  it "does not match identity-bearing parameters through name-only bounds" $ do
    let identity = typeBinderIdentityFromNode (NodeId 991305)
        parameterBounds = Map.singleton (BackendTypeSubstitutionByName "a") Nothing
        expected = BTVarWithIdentity (Just identity) "a"

    matchBackendTypeParametersWithTypeBounds Map.empty [] parameterBounds Map.empty expected intTy
      `shouldBe` Nothing

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

subst :: [(String, BackendType)] -> Map.Map BackendTypeSubstitutionKey BackendType
subst =
  Map.fromList . map (\(name, ty) -> (BackendTypeSubstitutionByName name, ty))

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

identityListData :: BackendData
identityListData =
  listData
    { backendDataConstructors =
        [nilConstructor, consConstructorWithIdentity]
    }

consConstructorWithIdentity :: BackendConstructor
consConstructorWithIdentity =
  BackendConstructorWithIdentity
    { backendConstructorIdentity = Just consIdentity,
      backendConstructorNameWithIdentity = "Cons",
      backendConstructorForallsWithIdentity = [],
      backendConstructorFieldsWithIdentity = [BTVar "a", listTy (BTVar "a")],
      backendConstructorResultWithIdentity = listTy (BTVar "a")
    }

staleConsConstructor :: BackendConstructor
staleConsConstructor =
  BackendConstructorWithIdentity
    { backendConstructorIdentity = Just consIdentity,
      backendConstructorNameWithIdentity = "$stale_Cons",
      backendConstructorForallsWithIdentity = [],
      backendConstructorFieldsWithIdentity = [BTVar "a", listTy (BTVar "a")],
      backendConstructorResultWithIdentity = listTy (BTVar "a")
    }

consIdentity :: SymbolIdentity
consIdentity =
  SymbolIdentity
    { symbolUniqueIdentity = UniqueIdentity 990101,
      symbolNamespace = SymbolConstructor,
      symbolDefiningModule = "Main",
      symbolDefiningName = "Cons",
      symbolOwnerIdentity = Nothing
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

freshenedSelfListStructuralBody :: BackendType -> BackendType
freshenedSelfListStructuralBody headField =
  BTForall
    "r"
    Nothing
    ( BTArrow
        (BTVar "r")
        (BTArrow (BTArrow headField (BTArrow (BTVar "$List_self1") (BTVar "r"))) (BTVar "r"))
    )

nullaryStructuralBody :: BackendType
nullaryStructuralBody =
  BTForall "r" Nothing (BTVar "r")
