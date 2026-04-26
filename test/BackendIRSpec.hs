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

  it "uses capture-avoiding substitution for type application" $ do
    let sourceTy = BTForall "b" Nothing (BTArrow (BTVar "a") (BTVar "b"))
        polyTy = BTForall "a" Nothing sourceTy
        expectedTy = BTForall "b1" Nothing (BTArrow (BTVar "b") (BTVar "b1"))
        capturedTy = BTForall "b" Nothing (BTArrow (BTVar "b") (BTVar "b"))

    substituteBackendType "a" (BTVar "b") sourceTy
      `shouldBe` expectedTy

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = expectedTy,
            backendTyFunction = BackendVar polyTy "poly",
            backendTyArgument = BTVar "b"
          }
      )
      `shouldBe` Right ()

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = capturedTy,
            backendTyFunction = BackendVar polyTy "poly",
            backendTyArgument = BTVar "b"
          }
      )
      `shouldBe` Left (BackendTypeAppResultMismatch capturedTy expectedTy)

  it "does not choose the substituted variable while freshening binders" $ do
    substituteBackendType "a1" (BTVar "a") (BTForall "a" Nothing (BTVar "a"))
      `shouldBe` BTForall "a2" Nothing (BTVar "a2")

    substituteBackendType "a1" (BTVar "a") (BTMu "a" (BTVar "a"))
      `shouldBe` BTMu "a2" (BTVar "a2")

  it "checks bounded type application arguments" $ do
    let boundedIdTy = BTForall "a" (Just intTy) (BTArrow (BTVar "a") (BTVar "a"))
        boolIdTy = BTArrow boolTy boolTy

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = idTy,
            backendTyFunction = BackendVar boundedIdTy "boundedId",
            backendTyArgument = intTy
          }
      )
      `shouldBe` Right ()

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = boolIdTy,
            backendTyFunction = BackendVar boundedIdTy "boundedId",
            backendTyArgument = boolTy
          }
      )
      `shouldBe` Left (BackendTypeAppBoundMismatch intTy boolTy)

  it "compares validator types modulo alpha-equivalence" $ do
    let exprTy = BTForall "a" Nothing (BTArrow (BTVar "a") (BTVar "a"))
        declaredTy = BTForall "b" Nothing (BTArrow (BTVar "b") (BTVar "b"))
        alphaIdentityExpr =
          BackendTyAbs
            { backendExprType = exprTy,
              backendTyParamName = "a",
              backendTyParamBound = Nothing,
              backendTyAbsBody =
                BackendLam
                  { backendExprType = BTArrow (BTVar "a") (BTVar "a"),
                    backendParamName = "x",
                    backendParamType = BTVar "a",
                    backendBody = BackendVar (BTVar "a") "x"
                  }
            }
        appExpectedTy = BTForall "z" Nothing (BTArrow intTy (BTVar "z"))
        appFunctionTy = BTForall "a" Nothing (BTForall "b" Nothing (BTArrow (BTVar "a") (BTVar "b")))

    validateBackendBinding (BackendBinding "poly" declaredTy alphaIdentityExpr False)
      `shouldBe` Right ()

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = appExpectedTy,
            backendTyFunction = BackendVar appFunctionTy "poly",
            backendTyArgument = intTy
          }
      )
      `shouldBe` Right ()

  it "rejects recursive roll and unroll type mismatches" $ do
    let recTy = BTMu "self" intTy

    validateBackendExpr (BackendRoll recTy (boolLit True))
      `shouldBe` Left (BackendRollPayloadMismatch intTy boolTy)

    validateBackendExpr (BackendUnroll boolTy (BackendVar recTy "boxed"))
      `shouldBe` Left (BackendUnrollResultMismatch boolTy intTy)

  it "accepts ADT construction and case analysis through constructor metadata" $ do
    validateBackendProgram (programWithMainExpr boxCaseExpr)
      `shouldBe` Right ()

  it "accepts parameterized constructor metadata at use and case sites" $ do
    validateBackendProgram (programWithDataAndMainExpr [optionData] someIntExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [optionData] optionCaseExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [optionData] someBoolAsOptionIntExpr)
      `shouldBe` Left (BackendConstructorArgumentMismatch "Some" 0 intTy boolTy)

  it "rejects matcher capture from inferred constructor parameters" $ do
    validateBackendProgram captureForallConstructProgram
      `shouldBe` Left (BackendConstructorArgumentMismatch "CaptureForall" 0 captureForallInstantiatedTy captureForallActualTy)

    validateBackendProgram captureMuConstructProgram
      `shouldBe` Left (BackendConstructorArgumentMismatch "CaptureMu" 0 captureMuInstantiatedTy captureMuActualTy)

    validateBackendProgram captureCaseProgram
      `shouldBe` Left (BackendCaseConstructorScrutineeMismatch "CaptureCase" captureCaseScrutineeTy captureCaseTemplateTy)

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
  programWithDataAndMainExpr [boxData] expr

programWithDataAndMainExpr :: [BackendData] -> BackendExpr -> BackendProgram
programWithDataAndMainExpr dataDecls expr =
  programWithDataAndBindings dataDecls [mainBinding expr]

programWithDataAndBindings :: [BackendData] -> [BackendBinding] -> BackendProgram
programWithDataAndBindings dataDecls bindings =
  BackendProgram
    [ BackendModule
        { backendModuleName = "Main",
          backendModuleData = dataDecls,
          backendModuleBindings = bindings
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

optionData :: BackendData
optionData =
  BackendData
    { backendDataName = "Option",
      backendDataParameters = ["a"],
      backendDataConstructors = [BackendConstructor "Some" [BTVar "a"] (optionTy (BTVar "a"))]
    }

captureForallData :: BackendData
captureForallData =
  BackendData
    { backendDataName = "CaptureForall",
      backendDataParameters = ["p"],
      backendDataConstructors =
        [ BackendConstructor
            "CaptureForall"
            [BTForall "a" Nothing (BTVar "p")]
            (captureTy "CaptureForall" (BTVar "p"))
        ]
    }

captureMuData :: BackendData
captureMuData =
  BackendData
    { backendDataName = "CaptureMu",
      backendDataParameters = ["p"],
      backendDataConstructors =
        [ BackendConstructor
            "CaptureMu"
            [BTMu "a" (BTVar "p")]
            (captureTy "CaptureMu" (BTVar "p"))
        ]
    }

captureCaseData :: BackendData
captureCaseData =
  BackendData
    { backendDataName = "CaptureCase",
      backendDataParameters = ["p"],
      backendDataConstructors = [BackendConstructor "CaptureCase" [] captureCaseTemplateTy]
    }

boxCaseExpr :: BackendExpr
boxCaseExpr =
  boxCaseExprWith
    (BackendConstruct boxTy "Box" [intLit 1])
    (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendVar intTy "n") :| [])

someIntExpr :: BackendExpr
someIntExpr =
  BackendConstruct (optionTy intTy) "Some" [intLit 1]

someBoolAsOptionIntExpr :: BackendExpr
someBoolAsOptionIntExpr =
  BackendConstruct (optionTy intTy) "Some" [boolLit True]

captureForallConstructProgram :: BackendProgram
captureForallConstructProgram =
  programWithDataAndBindings
    [captureForallData]
    [ mainBinding (BackendConstruct (captureTy "CaptureForall" (BTVar "a1")) "CaptureForall" [BackendVar captureForallActualTy "polyArg"]),
      binding "polyArg" captureForallActualTy (BackendVar captureForallActualTy "polyArg")
    ]

captureMuConstructProgram :: BackendProgram
captureMuConstructProgram =
  programWithDataAndBindings
    [captureMuData]
    [ mainBinding (BackendConstruct (captureTy "CaptureMu" (BTVar "a1")) "CaptureMu" [BackendVar captureMuActualTy "muArg"]),
      binding "muArg" captureMuActualTy (BackendVar captureMuActualTy "muArg")
    ]

captureCaseProgram :: BackendProgram
captureCaseProgram =
  programWithDataAndBindings
    [captureCaseData]
    [ mainBinding
        BackendCase
          { backendExprType = intTy,
            backendScrutinee = BackendVar captureCaseScrutineeTy "captureCaseArg",
            backendAlternatives = BackendAlternative (BackendConstructorPattern "CaptureCase" []) (intLit 1) :| []
          },
      binding "captureCaseArg" captureCaseScrutineeTy (BackendVar captureCaseScrutineeTy "captureCaseArg")
    ]

optionCaseExpr :: BackendExpr
optionCaseExpr =
  BackendCase
    { backendExprType = intTy,
      backendScrutinee = someIntExpr,
      backendAlternatives = BackendAlternative (BackendConstructorPattern "Some" ["n"]) (BackendVar intTy "n") :| []
    }

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

optionTy :: BackendType -> BackendType
optionTy ty =
  BTCon (BaseTy "Option") (ty :| [])

captureTy :: String -> BackendType -> BackendType
captureTy name ty =
  BTCon (BaseTy name) (ty :| [])

captureForallActualTy :: BackendType
captureForallActualTy =
  BTForall "x" Nothing (BTVar "x")

captureForallInstantiatedTy :: BackendType
captureForallInstantiatedTy =
  BTForall "a" Nothing (BTVar "a1")

captureMuActualTy :: BackendType
captureMuActualTy =
  BTMu "x" (BTVar "x")

captureMuInstantiatedTy :: BackendType
captureMuInstantiatedTy =
  BTMu "a" (BTVar "a1")

captureCaseTemplateTy :: BackendType
captureCaseTemplateTy =
  BTCon (BaseTy "CaptureCase") (BTVar "p" :| [BTForall "a" Nothing (BTVar "p")])

captureCaseScrutineeTy :: BackendType
captureCaseScrutineeTy =
  BTCon (BaseTy "CaptureCase") (BTVar "a1" :| [captureForallActualTy])
