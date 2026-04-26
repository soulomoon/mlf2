module BackendIRSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import MLF.Backend.IR
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Syntax (Lit (..))
import Test.Hspec

spec :: Spec
spec = describe "MLF.Backend.IR" $ do
  it "accepts a minimal checked-like backend program" $ do
    validateBackendProgram simpleProgram `shouldBe` Right ()

  it "rejects duplicate modules and duplicate global bindings" $ do
    validateBackendProgram (BackendProgram [emptyModule "Main", emptyModule "Main"] "main")
      `shouldBe` Left (BackendDuplicateModule "Main")

    validateBackendProgram (programWithBindings [mainLiteralBinding, mainLiteralBinding])
      `shouldBe` Left (BackendDuplicateBinding "main")

  it "rejects a missing main binding" $ do
    validateBackendProgram (BackendProgram [moduleWithBindings "Main" [binding "other" intTy (intLit 1)]] "main")
      `shouldBe` Left (BackendMainNotFound "main")

  it "checks global and lexical variable references" $ do
    validateBackendProgram (programWithMainExpr (BackendVar intTy "missing"))
      `shouldBe` Left (BackendUnknownVariable "missing")

    validateBackendProgram
      ( programWithBindings
          [ binding "helper" intTy (intLit 1),
            mainBinding (BackendVar boolTy "helper")
          ]
      )
      `shouldBe` Left (BackendVariableTypeMismatch "helper" intTy boolTy)

    validateBackendProgram (programWithMainExpr letIdentityExpr)
      `shouldBe` Right ()

  it "rejects bindings whose declared type differs from the expression type" $ do
    validateBackendBinding (BackendBinding "main" boolTy (intLit 1) True)
      `shouldBe` Left (BackendBindingTypeMismatch "main" boolTy intTy)

  it "rejects literal nodes with incorrect carried types" $ do
    validateBackendExpr (BackendLit boolTy (LInt 1))
      `shouldBe` Left (BackendLiteralTypeMismatch (LInt 1) intTy boolTy)

  it "rejects invalid application, lambda, and let nodes" $ do
    validateBackendExpr (BackendApp intTy intIdentityExpr (boolLit True))
      `shouldBe` Left (BackendApplicationArgumentMismatch intTy boolTy)

    validateBackendExpr (BackendLam boolTy "x" intTy (BackendVar intTy "x"))
      `shouldBe` Left (BackendLambdaTypeMismatch boolTy idTy)

    validateBackendExpr (BackendLet intTy "x" boolTy (intLit 1) (BackendVar intTy "x"))
      `shouldBe` Left (BackendLetTypeMismatch "x" boolTy intTy)

  it "checks type application against forall nodes" $ do
    validateBackendExpr
      ( BackendTyApp
          { backendExprType = idTy,
            backendTyFunction = BackendVar polyIdTy "id",
            backendTyArgument = intTy
          }
      )
      `shouldBe` Right ()

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = intTy,
            backendTyFunction = intLit 1,
            backendTyArgument = intTy
          }
      )
      `shouldBe` Left (BackendTypeAppExpectedForall intTy)

  it "rejects recursive roll and unroll type mismatches" $ do
    let recTy = BTMu "self" intTy

    validateBackendExpr (BackendRoll recTy (boolLit True))
      `shouldBe` Left (BackendRollPayloadMismatch intTy boolTy)

    validateBackendExpr (BackendUnroll boolTy (BackendVar recTy "boxed"))
      `shouldBe` Left (BackendUnrollResultMismatch boolTy intTy)

  it "accepts ADT construction and case analysis through constructor metadata" $ do
    validateBackendProgram (programWithMainExpr boxCaseExpr)
      `shouldBe` Right ()

  it "rejects unknown constructors and duplicate constructor metadata" $ do
    validateBackendProgram (programWithMainExpr (BackendConstruct boxTy "Missing" []))
      `shouldBe` Left (BackendUnknownConstructor "Missing")

    validateBackendProgram
      ( BackendProgram
          [ BackendModule
              { backendModuleName = "Main",
                backendModuleData =
                  [ BackendData "LeftBox" [] [BackendConstructor "Box" [intTy] boxTy],
                    BackendData "RightBox" [] [BackendConstructor "Box" [intTy] boxTy]
                  ],
                backendModuleBindings = [mainLiteralBinding]
              }
          ]
          "main"
      )
      `shouldBe` Left (BackendDuplicateConstructor "Box")

  it "rejects constructor arity, argument, and result mismatches" $ do
    validateBackendProgram (programWithMainExpr (BackendConstruct boxTy "Box" []))
      `shouldBe` Left (BackendConstructorArityMismatch "Box" 1 0)

    validateBackendProgram (programWithMainExpr (BackendConstruct boxTy "Box" [boolLit True]))
      `shouldBe` Left (BackendConstructorArgumentMismatch "Box" 0 intTy boolTy)

    validateBackendProgram (programWithMainExpr (BackendConstruct boolTy "Box" [intLit 1]))
      `shouldBe` Left (BackendConstructorResultMismatch "Box" boxTy boolTy)

  it "rejects invalid case alternative result and constructor boundaries" $ do
    validateBackendProgram (programWithMainExpr boxCaseWrongResultExpr)
      `shouldBe` Left (BackendCaseResultMismatch intTy boolTy)

    validateBackendProgram (programWithMainExpr boxCaseWrongScrutineeExpr)
      `shouldBe` Left (BackendCaseConstructorScrutineeMismatch "Box" boolTy boxTy)

    validateBackendProgram (programWithMainExpr boxCaseWrongPatternArityExpr)
      `shouldBe` Left (BackendPatternArityMismatch "Box" 1 0)

simpleProgram :: BackendProgram
simpleProgram =
  programWithBindings
    [ binding "id" idTy intIdentityExpr,
      mainBinding
        ( BackendApp
            { backendExprType = intTy,
              backendFunction = BackendVar idTy "id",
              backendArgument = intLit 1
            }
        )
    ]

programWithMainExpr :: BackendExpr -> BackendProgram
programWithMainExpr expr =
  BackendProgram
    [ BackendModule
        { backendModuleName = "Main",
          backendModuleData = [boxData],
          backendModuleBindings = [mainBinding expr]
        }
    ]
    "main"

programWithBindings :: [BackendBinding] -> BackendProgram
programWithBindings bindings =
  BackendProgram [moduleWithBindings "Main" bindings] "main"

moduleWithBindings :: String -> [BackendBinding] -> BackendModule
moduleWithBindings name bindings =
  (emptyModule name) {backendModuleBindings = bindings}

emptyModule :: String -> BackendModule
emptyModule name =
  BackendModule
    { backendModuleName = name,
      backendModuleData = [],
      backendModuleBindings = []
    }

mainBinding :: BackendExpr -> BackendBinding
mainBinding expr =
  BackendBinding "main" (backendExprType expr) expr True

mainLiteralBinding :: BackendBinding
mainLiteralBinding =
  mainBinding (intLit 1)

binding :: String -> BackendType -> BackendExpr -> BackendBinding
binding name ty expr =
  BackendBinding name ty expr False

intIdentityExpr :: BackendExpr
intIdentityExpr =
  BackendLam
    { backendExprType = idTy,
      backendParamName = "x",
      backendParamType = intTy,
      backendBody = BackendVar intTy "x"
    }

letIdentityExpr :: BackendExpr
letIdentityExpr =
  BackendLet
    { backendExprType = intTy,
      backendLetName = "x",
      backendLetType = intTy,
      backendLetRhs = intLit 1,
      backendLetBody = BackendVar intTy "x"
    }

boxData :: BackendData
boxData =
  BackendData
    { backendDataName = "Box",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "Box" [intTy] boxTy]
    }

boxCaseExpr :: BackendExpr
boxCaseExpr =
  boxCaseExprWith
    (BackendConstruct boxTy "Box" [intLit 1])
    (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendVar intTy "n") :| [])

boxCaseWrongResultExpr :: BackendExpr
boxCaseWrongResultExpr =
  boxCaseExprWith
    (BackendConstruct boxTy "Box" [intLit 1])
    (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendLit boolTy (LBool True)) :| [])

boxCaseWrongScrutineeExpr :: BackendExpr
boxCaseWrongScrutineeExpr =
  boxCaseExprWith
    (boolLit True)
    (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendVar intTy "n") :| [])

boxCaseWrongPatternArityExpr :: BackendExpr
boxCaseWrongPatternArityExpr =
  boxCaseExprWith
    (BackendConstruct boxTy "Box" [intLit 1])
    (BackendAlternative (BackendConstructorPattern "Box" []) (intLit 1) :| [])

boxCaseExprWith :: BackendExpr -> NonEmpty BackendAlternative -> BackendExpr
boxCaseExprWith scrutinee alternatives =
  BackendCase
    { backendExprType = intTy,
      backendScrutinee = scrutinee,
      backendAlternatives = alternatives
    }

intLit :: Integer -> BackendExpr
intLit n =
  BackendLit intTy (LInt n)

boolLit :: Bool -> BackendExpr
boolLit b =
  BackendLit boolTy (LBool b)

polyIdTy :: BackendType
polyIdTy =
  BTForall "a" Nothing (BTArrow (BTVar "a") (BTVar "a"))

idTy :: BackendType
idTy =
  BTArrow intTy intTy

intTy :: BackendType
intTy =
  BTBase (BaseTy "Int")

boolTy :: BackendType
boolTy =
  BTBase (BaseTy "Bool")

boxTy :: BackendType
boxTy =
  BTBase (BaseTy "Box")
