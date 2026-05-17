module FrontendTypeLevelSpec (spec) where

import MLF.Frontend.TypeLevel
import Test.Hspec

spec :: Spec
spec = describe "MLF.Frontend.TypeLevel" $ do
  typeLambdaSpec
  closedFamilySpec

typeLambdaSpec :: Spec
typeLambdaSpec = describe "type lambda normalization" $ do
  it "beta-reduces type lambdas" $ do
    normalizeTypeLevel mempty (TLTApp (TLTLam "a" TLKType (TLTVar "a")) (TLTCon "Int"))
      `shouldBe` Right (TLTCon "Int")

  it "alpha-renames during substitution to avoid capture" $ do
    case substTypeLevel "a" (TLTVar "b") (TLTLam "b" TLKType (TLTApp (TLTVar "a") (TLTVar "b"))) of
      TLTLam renamed TLKType (TLTApp (TLTVar replacement) (TLTVar inner)) -> do
        renamed `shouldNotBe` "b"
        replacement `shouldBe` "b"
        inner `shouldBe` renamed
      other -> expectationFailure ("unexpected substitution result: " ++ show other)

  it "spends fuel for beta reduction" $ do
    normalizeTypeLevelWith 0 mempty (TLTApp (TLTLam "a" TLKType (TLTVar "a")) (TLTCon "Int"))
      `shouldBe` Left (TypeLevelFuelExhausted (TLTApp (TLTLam "a" TLKType (TLTVar "a")) (TLTCon "Int")))

  it "substitutes before reducing families in type-lambda bodies" $ do
    env <- shouldBuildEnv [onlyIntFamily]
    normalizeTypeLevel env (TLTApp (TLTLam "a" TLKType (TLTFamilyApp "OnlyInt" [TLTVar "a"])) (TLTCon "Int"))
      `shouldBe` Right (TLTCon "Bool")

closedFamilySpec :: Spec
closedFamilySpec = describe "closed type-family normalization" $ do
  it "reduces ordered first-match equations" $ do
    env <- shouldBuildEnv [orderedFamily]
    normalizeTypeLevel env (TLTFamilyApp "F" [TLTCon "Int"])
      `shouldBe` Right (TLTCon "Bool")
    normalizeTypeLevel env (TLTFamilyApp "F" [TLTCon "String"])
      `shouldBe` Right (TLTCon "String")

  it "normalizes nested family arguments before matching" $ do
    env <- shouldBuildEnv [orderedFamily, idFamily]
    normalizeTypeLevel env (TLTFamilyApp "Id" [TLTFamilyApp "F" [TLTCon "Int"]])
      `shouldBe` Right (TLTCon "Bool")

  it "substitutes family equations simultaneously" $ do
    env <- shouldBuildEnv [secondFamily]
    normalizeTypeLevel env (TLTFamilyApp "Second" [TLTCon "Int", TLTVar "a"])
      `shouldBe` Right (TLTVar "a")

  it "rejects stuck family applications before the core boundary" $ do
    env <- shouldBuildEnv [boolOnlyFamily]
    normalizeTypeLevel env (TLTFamilyApp "OnlyBool" [TLTCon "Int"])
      `shouldBe` Left (TypeFamilyStuck "OnlyBool" [TLTCon "Int"])

  it "detects recursive family reduction cycles" $ do
    env <- shouldBuildEnv [loopFamily, loopFamily2]
    normalizeTypeLevel env (TLTFamilyApp "LoopA" [TLTCon "Int"])
      `shouldBe` Left (TypeFamilyCycle ["LoopA", "LoopB", "LoopA"])

  it "rejects family arity mismatches" $ do
    env <- shouldBuildEnv [idFamily]
    normalizeTypeLevel env (TLTFamilyApp "Id" [TLTCon "Int", TLTCon "Bool"])
      `shouldBe` Left (TypeFamilyArityMismatch "Id" 1 2)

  it "tracks family-free normalized output" $ do
    env <- shouldBuildEnv [orderedFamily]
    normalizeTypeLevel env (TLTFamilyApp "F" [TLTCon "Int"])
      `shouldBe` Right (TLTCon "Bool")
    isFamilyFree (TLTCon "Bool") `shouldBe` True
    isFamilyFree (TLTFamilyApp "F" [TLTCon "Int"]) `shouldBe` False

  it "carries kind variables in family declarations" $ do
    familyDeclResultKind kindPolymorphicId `shouldBe` TLKVar "k"
    familyDeclParams kindPolymorphicId `shouldBe` [("a", TLKVar "k")]

shouldBuildEnv :: [TypeFamilyDecl] -> IO TypeFamilyEnv
shouldBuildEnv decls =
  case familyEnvFromDecls decls of
    Right env -> pure env
    Left err -> fail err

orderedFamily :: TypeFamilyDecl
orderedFamily =
  TypeFamilyDecl
    { familyDeclName = "F",
      familyDeclParams = [("a", TLKType)],
      familyDeclResultKind = TLKType,
      familyDeclEquations =
        [ TypeFamilyEquation [TLPCon "Int" []] (TLTCon "Bool"),
          TypeFamilyEquation [TLPVar "a"] (TLTVar "a")
        ]
    }

idFamily :: TypeFamilyDecl
idFamily =
  TypeFamilyDecl
    { familyDeclName = "Id",
      familyDeclParams = [("a", TLKType)],
      familyDeclResultKind = TLKType,
      familyDeclEquations = [TypeFamilyEquation [TLPVar "a"] (TLTVar "a")]
    }

boolOnlyFamily :: TypeFamilyDecl
boolOnlyFamily =
  TypeFamilyDecl
    { familyDeclName = "OnlyBool",
      familyDeclParams = [("a", TLKType)],
      familyDeclResultKind = TLKType,
              familyDeclEquations = [TypeFamilyEquation [TLPCon "Bool" []] (TLTCon "Int")]
            }

onlyIntFamily :: TypeFamilyDecl
onlyIntFamily =
  TypeFamilyDecl
    { familyDeclName = "OnlyInt",
      familyDeclParams = [("a", TLKType)],
      familyDeclResultKind = TLKType,
      familyDeclEquations = [TypeFamilyEquation [TLPCon "Int" []] (TLTCon "Bool")]
    }

secondFamily :: TypeFamilyDecl
secondFamily =
  TypeFamilyDecl
    { familyDeclName = "Second",
      familyDeclParams = [("a", TLKType), ("b", TLKType)],
      familyDeclResultKind = TLKType,
      familyDeclEquations = [TypeFamilyEquation [TLPVar "a", TLPVar "b"] (TLTVar "b")]
    }

loopFamily :: TypeFamilyDecl
loopFamily =
  TypeFamilyDecl
    { familyDeclName = "LoopA",
      familyDeclParams = [("a", TLKType)],
      familyDeclResultKind = TLKType,
      familyDeclEquations = [TypeFamilyEquation [TLPVar "a"] (TLTFamilyApp "LoopB" [TLTVar "a"])]
    }

loopFamily2 :: TypeFamilyDecl
loopFamily2 =
  TypeFamilyDecl
    { familyDeclName = "LoopB",
      familyDeclParams = [("a", TLKType)],
      familyDeclResultKind = TLKType,
      familyDeclEquations = [TypeFamilyEquation [TLPVar "a"] (TLTFamilyApp "LoopA" [TLTVar "a"])]
    }

kindPolymorphicId :: TypeFamilyDecl
kindPolymorphicId =
  TypeFamilyDecl
    { familyDeclName = "IdK",
      familyDeclParams = [("a", TLKVar "k")],
      familyDeclResultKind = TLKVar "k",
      familyDeclEquations = [TypeFamilyEquation [TLPVar "a"] (TLTVar "a")]
    }
