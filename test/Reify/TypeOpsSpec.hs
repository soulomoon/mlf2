module Reify.TypeOpsSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Reify.TypeOps
  ( alphaEqType,
    firstNonContractiveRecursiveType,
    freeTypeVarsFrom,
    freeTypeVarsList,
    freeTypeVarsType,
    freshTypeName,
    freshTypeNameFromCounter,
    matchType,
    parseNameId,
    renameTypeVar,
    splitForalls,
    stripForallsType,
    substTypeCapture,
    substTypeSimple,
  )
import MLF.Types.Elab (BoundType, ElabType, Ty (..))
import Test.Hspec

intTy :: ElabType
intTy = TBase (BaseTy "int")

intBound :: BoundType
intBound = TBase (BaseTy "int")

boolTy :: ElabType
boolTy = TBase (BaseTy "bool")

spec :: Spec
spec = describe "MLF.Reify.TypeOps" $ do
  describe "splitForalls" $ do
    it "returns empty binders for non-forall type" $
      let (binds, body) = splitForalls (TArrow (TVar "a") (TVar "b"))
       in do
            binds `shouldBe` []
            body `shouldBe` TArrow (TVar "a") (TVar "b")

    it "extracts one unbounded forall" $
      let (binds, body) = splitForalls (TForall "a" Nothing (TVar "a"))
       in do
            binds `shouldBe` [("a", Nothing)]
            body `shouldBe` TVar "a"

    it "extracts nested foralls with bounds" $
      let ty = TForall "a" (Just intBound) (TForall "b" Nothing (TArrow (TVar "a") (TVar "b")))
          (binds, body) = splitForalls ty
       in do
            length binds `shouldBe` 2
            case binds of
              ((n1, _) : (n2, _) : _) -> do
                n1 `shouldBe` "a"
                n2 `shouldBe` "b"
              _ -> expectationFailure "expected 2 binds"
            body `shouldBe` TArrow (TVar "a") (TVar "b")

  describe "stripForallsType" $ do
    it "returns non-forall type unchanged" $
      stripForallsType (TVar "x") `shouldBe` TVar "x"

    it "strips a single forall" $
      stripForallsType (TForall "a" Nothing (TVar "a"))
        `shouldBe` TVar "a"

    it "strips nested foralls" $
      stripForallsType (TForall "a" Nothing (TForall "b" Nothing (TArrow (TVar "a") (TVar "b"))))
        `shouldBe` TArrow (TVar "a") (TVar "b")

  describe "freeTypeVarsType" $ do
    it "finds the single free variable" $
      freeTypeVarsType (TVar "a")
        `shouldBe` Set.singleton "a"

    it "finds free vars in an arrow" $
      freeTypeVarsType (TArrow (TVar "a") (TVar "b"))
        `shouldBe` Set.fromList ["a", "b"]

    it "excludes bound variables" $
      freeTypeVarsType (TForall "a" Nothing (TVar "a"))
        `shouldBe` Set.empty

    it "finds only unbound vars in mixed types" $
      freeTypeVarsType (TForall "a" Nothing (TArrow (TVar "a") (TVar "b")))
        `shouldBe` Set.singleton "b"

  describe "freeTypeVarsList" $ do
    it "returns sorted list of free vars for a variable" $
      freeTypeVarsList (TVar "a") `shouldBe` ["a"]

    it "returns sorted free vars for an arrow" $
      freeTypeVarsList (TArrow (TVar "b") (TVar "a"))
        `shouldBe` ["a", "b"]

    it "returns empty list when all vars are bound" $
      freeTypeVarsList (TForall "a" Nothing (TVar "a"))
        `shouldBe` []

  describe "freeTypeVarsFrom" $ do
    it "treats first argument as bound set — bound vars excluded from result" $
      -- bound0 = {"a","b"}, so "a" is bound; only "c" is free
      freeTypeVarsFrom (Set.fromList ["a", "b"]) (TArrow (TVar "a") (TVar "c"))
        `shouldBe` Set.singleton "c"

    it "returns empty when all vars are bound (via forall + bound0)" $
      -- bound0 = {"a"}, forall binds "a" again; "a" is the only var → empty
      freeTypeVarsFrom (Set.singleton "a") (TForall "a" Nothing (TVar "a"))
        `shouldBe` Set.empty

    it "respects forall binders on top of bound0" $
      -- bound0 = {"a","b"}, forall binds "a"; both "a" and "b" are bound → empty
      freeTypeVarsFrom (Set.fromList ["a", "b"]) (TForall "a" Nothing (TArrow (TVar "a") (TVar "b")))
        `shouldBe` Set.empty

  describe "substTypeCapture" $ do
    it "substitutes a matching variable" $
      substTypeCapture "a" intTy (TVar "a")
        `shouldBe` intTy

    it "leaves a different variable untouched" $
      substTypeCapture "a" intTy (TVar "b")
        `shouldBe` TVar "b"

    it "avoids capture by freshening binder" $
      -- substTypeCapture "a" (TVar "b") (∀b. a) should freshen the binder
      let result = substTypeCapture "a" (TVar "b") (TForall "b" Nothing (TVar "a"))
       in do
            -- The bound variable should be renamed to avoid capture
            result `shouldSatisfy` (\ty -> freeTypeVarsType ty == Set.singleton "b")
            -- The body should contain TVar "b" (from substitution)
            alphaEqType result (TForall "b1" Nothing (TVar "b")) `shouldBe` True

  describe "substTypeSimple" $ do
    it "substitutes a matching variable" $
      substTypeSimple "a" intTy (TVar "a")
        `shouldBe` intTy

    it "leaves a different variable untouched" $
      substTypeSimple "a" intTy (TVar "b")
        `shouldBe` TVar "b"

    it "does NOT rename binders (may capture)" $
      -- substTypeSimple "a" (TVar "b") (∀b. a) does NOT freshen the binder
      let result = substTypeSimple "a" (TVar "b") (TForall "b" Nothing (TVar "a"))
       in result `shouldBe` TForall "b" Nothing (TVar "b")

  describe "renameTypeVar" $ do
    it "renames a free variable" $
      renameTypeVar "a" "z" (TVar "a")
        `shouldBe` TVar "z"

    it "leaves a non-matching variable untouched" $
      renameTypeVar "a" "z" (TVar "b")
        `shouldBe` TVar "b"

    it "renames inside nested structure" $
      renameTypeVar "a" "z" (TArrow (TVar "a") (TVar "b"))
        `shouldBe` TArrow (TVar "z") (TVar "b")

  describe "alphaEqType" $ do
    it "recognises equal variables" $
      alphaEqType (TVar "a") (TVar "a") `shouldBe` True

    it "distinguishes different variables" $
      alphaEqType (TVar "a") (TVar "b") `shouldBe` False

    it "recognises alpha-equivalent foralls" $
      alphaEqType
        (TForall "a" Nothing (TVar "a"))
        (TForall "b" Nothing (TVar "b"))
        `shouldBe` True

    it "rejects non-alpha-equivalent foralls" $
      alphaEqType
        (TForall "a" Nothing (TVar "a"))
        (TForall "a" Nothing (TVar "b"))
        `shouldBe` False

  describe "matchType" $ do
    it "matches a pattern variable against a concrete type" $
      matchType (Set.singleton "a") (TVar "a") intTy
        `shouldBe` Right (Map.singleton "a" intTy)

    it "rejects a structural mismatch" $
      case matchType Set.empty (TArrow (TVar "a") (TVar "b")) intTy of
        Left _ -> pure ()
        Right s -> expectationFailure ("Expected mismatch, got: " ++ show s)

    it "matches multiple pattern variables in an arrow" $
      let pat = TArrow (TVar "a") (TVar "b")
          target = TArrow intTy boolTy
       in matchType (Set.fromList ["a", "b"]) pat target
            `shouldBe` Right (Map.fromList [("a", intTy), ("b", boolTy)])

  describe "freshTypeName" $ do
    it "returns u0 for an empty used set" $
      freshTypeName Set.empty `shouldBe` "u0"

    it "skips used names" $
      freshTypeName (Set.singleton "u0") `shouldBe` "u1"

    it "skips multiple used names" $
      freshTypeName (Set.fromList ["u0", "u1"]) `shouldBe` "u2"

  describe "freshTypeNameFromCounter" $ do
    it "returns u0 from counter 0 with empty set" $
      freshTypeNameFromCounter 0 Set.empty `shouldBe` ("u0", 1)

    it "skips used name and increments counter" $
      freshTypeNameFromCounter 0 (Set.singleton "u0") `shouldBe` ("u1", 2)

    it "starts from the given counter" $
      freshTypeNameFromCounter 5 Set.empty `shouldBe` ("u5", 6)

  describe "firstNonContractiveRecursiveType" $ do
    it "returns Nothing for a type without TMu" $
      firstNonContractiveRecursiveType (TArrow (TVar "a") (TVar "b"))
        `shouldBe` Nothing

    it "returns Nothing for a contractive TMu" $
      firstNonContractiveRecursiveType (TMu "a" (TArrow (TVar "a") (TVar "a")))
        `shouldBe` Nothing

    it "returns the non-contractive TMu" $
      firstNonContractiveRecursiveType (TMu "a" (TVar "a"))
        `shouldBe` Just (TMu "a" (TVar "a"))

  describe "parseNameId" $ do
    it "parses t42" $
      parseNameId "t42" `shouldBe` Just 42

    it "rejects non-t-prefixed name" $
      parseNameId "abc" `shouldBe` Nothing

    it "parses t0" $
      parseNameId "t0" `shouldBe` Just 0
