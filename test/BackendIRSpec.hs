module BackendIRSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
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

    validateBackendProgram
      ( programWithBindings
          [ binding "helper" intTy (intLit 1),
            mainBinding (BackendVar (BTVar "a") "helper")
          ]
      )
      `shouldBe` Left (BackendVariableTypeMismatch "helper" intTy (BTVar "a"))

    validateBackendProgram
      ( programWithMainExpr
          ( BackendLam
              (BTArrow (BTVar "a") (BTVar "b"))
              "x"
              (BTVar "a")
              (BackendVar (BTVar "b") "x")
          )
      )
      `shouldBe` Left (BackendVariableTypeMismatch "x" (BTVar "a") (BTVar "b"))

    validateBackendProgram
      ( programWithMainExpr
          ( BackendLam
              (BTArrow (BTVar "a") (BTVar "a1"))
              "x"
              (BTVar "a")
              (BackendVar (BTVar "a1") "x")
          )
      )
      `shouldBe` Left (BackendVariableTypeMismatch "x" (BTVar "a") (BTVar "a1"))

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

    validateBackendExpr (BackendApp intTy intIdentityExpr (BackendVar (BTVar "a") "x"))
      `shouldBe` Left (BackendApplicationArgumentMismatch intTy (BTVar "a"))

    validateBackendExpr (BackendApp (BTVar "a") intIdentityExpr (intLit 1))
      `shouldBe` Left (BackendApplicationResultMismatch (BTVar "a") intTy)

    let listIntTy = listTy intTy
        listFreeTy = listTy (BTVar "a")
        arrowIntBoolTy = BTArrow intTy boolTy
        arrowFreeBoolTy = BTArrow (BTVar "a") boolTy
        varAppIntTy = BTVarApp "f" (intTy :| [])
        varAppFreeTy = BTVarApp "f" (BTVar "a" :| [])
        forallIntTy = BTForall "x" Nothing (BTArrow intTy (BTVar "x"))
        forallFreeTy = BTForall "y" Nothing (BTArrow (BTVar "a") (BTVar "y"))
        structuralBoxIntTy = BTMu "$Box_self" (singleFieldStructuralBody intTy)
        structuralBoxFreeTy = BTMu "$Box_self" (singleFieldStructuralBody (BTVar "a"))
        listIntToBoolTy = BTArrow listIntTy boolTy
        structuralBoxIntToBoolTy = BTArrow structuralBoxIntTy boolTy
        listTyAbsAppExpr =
          BackendTyAbs
            (BTForall "a" Nothing (BTArrow (listTy (BTVar "a")) boolTy))
            "a"
            Nothing
            ( BackendLam
                (BTArrow (listTy (BTVar "a")) boolTy)
                "xs"
                (listTy (BTVar "a"))
                ( BackendApp
                    boolTy
                    (BackendVar listIntToBoolTy "f")
                    (BackendVar (listTy (BTVar "a")) "xs")
                )
            )
        listIntToBoolExpr =
          BackendLam listIntToBoolTy "ys" listIntTy (boolLit True)
        structuralBoxTyAbsAppExpr =
          BackendTyAbs
            (BTForall "a" Nothing (BTArrow structuralBoxFreeTy boolTy))
            "a"
            Nothing
            ( BackendLam
                (BTArrow structuralBoxFreeTy boolTy)
                "xs"
                structuralBoxFreeTy
                ( BackendApp
                    boolTy
                    (BackendVar structuralBoxIntToBoolTy "f")
                    (BackendVar structuralBoxFreeTy "xs")
                )
            )
        structuralBoxIntToBoolExpr =
          BackendLam structuralBoxIntToBoolTy "box" structuralBoxIntTy (boolLit True)

    validateBackendExpr (BackendApp intTy (BackendVar (BTArrow listIntTy intTy) "f") (BackendVar listFreeTy "xs"))
      `shouldBe` Left (BackendApplicationArgumentMismatch listIntTy listFreeTy)

    validateBackendExpr (BackendApp listFreeTy (BackendVar (BTArrow intTy listIntTy) "f") (intLit 1))
      `shouldBe` Left (BackendApplicationResultMismatch listFreeTy listIntTy)

    validateBackendExpr (BackendApp boolTy (BackendVar (BTArrow arrowIntBoolTy boolTy) "f") (BackendVar arrowFreeBoolTy "g"))
      `shouldBe` Left (BackendApplicationArgumentMismatch arrowIntBoolTy arrowFreeBoolTy)

    validateBackendExpr (BackendApp boolTy (BackendVar (BTArrow varAppIntTy boolTy) "f") (BackendVar varAppFreeTy "x"))
      `shouldBe` Left (BackendApplicationArgumentMismatch varAppIntTy varAppFreeTy)

    validateBackendExpr (BackendApp boolTy (BackendVar (BTArrow forallIntTy boolTy) "f") (BackendVar forallFreeTy "poly"))
      `shouldBe` Left (BackendApplicationArgumentMismatch forallIntTy forallFreeTy)

    validateBackendExpr (BackendApp listIntTy (BackendVar (BTArrow intTy listFreeTy) "f") (intLit 1))
      `shouldBe` Left (BackendApplicationResultMismatch listIntTy listFreeTy)

    validateBackendProgram (programWithBindings [binding "f" listIntToBoolTy listIntToBoolExpr, mainBinding listTyAbsAppExpr])
      `shouldBe` Left (BackendApplicationArgumentMismatch listIntTy (listTy (BTVar "a")))

    validateBackendProgram (programWithBindings [binding "f" structuralBoxIntToBoolTy structuralBoxIntToBoolExpr, mainBinding structuralBoxTyAbsAppExpr])
      `shouldBe` Left (BackendApplicationArgumentMismatch structuralBoxIntTy structuralBoxFreeTy)

    validateBackendExpr (BackendApp boolTy (BackendVar (BTArrow structuralBoxFreeTy boolTy) "f") (BackendVar structuralBoxIntTy "box"))
      `shouldBe` Right ()

    validateBackendExpr (BackendLam boolTy "x" intTy (BackendVar intTy "x"))
      `shouldBe` Left (BackendLambdaTypeMismatch boolTy idTy)

    validateBackendExpr (BackendLet intTy "x" boolTy (intLit 1) (BackendVar intTy "x"))
      `shouldBe` Left (BackendLetTypeMismatch "x" boolTy intTy)

  it "validates explicit closure construction and indirect closure calls" $ do
    let closure =
          BackendClosure
            { backendExprType = idTy,
              backendClosureEntryName = "__mlfp_closure$id",
              backendClosureCaptures = [],
              backendClosureParams = [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        capturedClosure =
          BackendClosure
            { backendExprType = idTy,
              backendClosureEntryName = "__mlfp_closure$captured",
              backendClosureCaptures = [BackendClosureCapture "captured" intTy (intLit 7)],
              backendClosureParams = [("x", intTy)],
              backendClosureBody = BackendVar intTy "captured"
            }
        callClosure value =
          BackendLet
            intTy
            "f"
            idTy
            value
            (BackendClosureCall intTy (BackendVar idTy "f") [intLit 1])
        structuralClosureArgTy =
          BTMu "$Box_self" (singleFieldStructuralBody (BTVar "a"))
        structuralClosure =
          BackendClosure
            { backendExprType = BTArrow structuralClosureArgTy boolTy,
              backendClosureEntryName = "__mlfp_closure$structural",
              backendClosureCaptures = [],
              backendClosureParams = [("box", structuralClosureArgTy)],
              backendClosureBody = boolLit True
            }

    validateBackendProgram (programWithMainExpr (callClosure closure))
      `shouldBe` Right ()
    validateBackendProgram (programWithMainExpr (callClosure capturedClosure))
      `shouldBe` Right ()
    validateBackendExpr (BackendClosureCall boolTy structuralClosure [BackendVar structuralBoxTy "box"])
      `shouldBe` Right ()

  it "rejects malformed closure IR" $ do
    let goodClosure entryName =
          BackendClosure
            { backendExprType = idTy,
              backendClosureEntryName = entryName,
              backendClosureCaptures = [],
              backendClosureParams = [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        captureMismatch =
          BackendClosure
            { backendExprType = idTy,
              backendClosureEntryName = "__mlfp_closure$bad_capture",
              backendClosureCaptures = [BackendClosureCapture "captured" boolTy (intLit 7)],
              backendClosureParams = [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        resultMismatch =
          BackendClosure
            { backendExprType = idTy,
              backendClosureEntryName = "__mlfp_closure$bad_result",
              backendClosureCaptures = [],
              backendClosureParams = [("x", intTy)],
              backendClosureBody = boolLit True
            }
        duplicateEntries =
          BackendLet
            intTy
            "f"
            idTy
            (goodClosure "__mlfp_closure$dup")
            ( BackendLet
                intTy
                "g"
                idTy
                (goodClosure "__mlfp_closure$dup")
                (intLit 0)
            )
        entryNameBindingCollision =
          programWithBindings
            [ binding "helper" intTy (intLit 0),
              mainBinding (goodClosure "helper")
            ]
        entryNameRuntimeCollision =
          programWithMainExpr (goodClosure "__mlfp_and")
        duplicateCaptureAndParameter =
          BackendClosure
            { backendExprType = idTy,
              backendClosureEntryName = "__mlfp_closure$duplicate_binder",
              backendClosureCaptures = [BackendClosureCapture "x" intTy (intLit 7)],
              backendClosureParams = [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        nonFunctionClosure =
          BackendClosure
            { backendExprType = intTy,
              backendClosureEntryName = "__mlfp_closure$non_function",
              backendClosureCaptures = [],
              backendClosureParams = [],
              backendClosureBody = intLit 0
            }
        underspecifiedClosureParams =
          BackendClosure
            { backendExprType = idTy,
              backendClosureEntryName = "__mlfp_closure$underspecified_params",
              backendClosureCaptures = [],
              backendClosureParams = [],
              backendClosureBody = intIdentityExpr
            }
        badCall =
          BackendClosureCall intTy (goodClosure "__mlfp_closure$call") [boolLit True]
        nonClosureCall =
          BackendLet
            intTy
            "f"
            idTy
            intIdentityExpr
            (BackendClosureCall intTy (BackendVar idTy "f") [intLit 1])
        unlistedLocalCapture =
          BackendLet
            idTy
            "captured"
            intTy
            (intLit 7)
            ( BackendClosure
                { backendExprType = idTy,
                  backendClosureEntryName = "__mlfp_closure$unlisted_capture",
                  backendClosureCaptures = [],
                  backendClosureParams = [("x", intTy)],
                  backendClosureBody = BackendVar intTy "captured"
                }
            )
        appCalledClosure =
          BackendLet
            intTy
            "f"
            idTy
            (goodClosure "__mlfp_closure$app")
            (BackendApp intTy (BackendVar idTy "f") (intLit 1))
        appCalledLetHeadClosure =
          BackendApp
            intTy
            ( BackendLet
                idTy
                "f"
                idTy
                (goodClosure "__mlfp_closure$app_let_head")
                (BackendVar idTy "f")
            )
            (intLit 1)
        appCalledClosureAlias =
          BackendLet
            intTy
            "g"
            idTy
            (goodClosure "__mlfp_closure$app_alias")
            ( BackendLet
                intTy
                "f"
                idTy
                (BackendVar idTy "g")
                (BackendApp intTy (BackendVar idTy "f") (intLit 1))
            )
        appCalledCaseHeadClosure =
          BackendApp
            intTy
            ( BackendCase
                idTy
                (BackendConstruct boxTy "Box" [intLit 0])
                ( BackendAlternative
                    (BackendConstructorPattern "Box" ["n"])
                    (goodClosure "__mlfp_closure$app_case")
                    :| []
                )
            )
            (intLit 1)
        appCalledCapturedClosure =
          BackendLet
            idTy
            "g"
            idTy
            (goodClosure "__mlfp_closure$app_captured_source")
            ( BackendClosure
                { backendExprType = idTy,
                  backendClosureEntryName = "__mlfp_closure$app_captured",
                  backendClosureCaptures = [BackendClosureCapture "capturedClosure" idTy (BackendVar idTy "g")],
                  backendClosureParams = [("x", intTy)],
                  backendClosureBody = BackendApp intTy (BackendVar idTy "capturedClosure") (intLit 1)
                }
            )
        closureCallMixedCaseHead =
          BackendClosureCall
            intTy
            ( BackendCase
                idTy
                (BackendConstruct boxTy "Box" [intLit 0])
                ( BackendAlternative
                    (BackendConstructorPattern "Box" ["n"])
                    (goodClosure "__mlfp_closure$closure_call_case")
                    :| [BackendAlternative BackendDefaultPattern intIdentityExpr]
                )
            )
            [intLit 1]

    validateBackendProgram (programWithMainExpr captureMismatch)
      `shouldBe` Left (BackendClosureCaptureTypeMismatch "captured" boolTy intTy)
    validateBackendProgram (programWithMainExpr resultMismatch)
      `shouldBe` Left (BackendClosureTypeMismatch "__mlfp_closure$bad_result" idTy (BTArrow intTy boolTy))
    validateBackendProgram (programWithMainExpr duplicateEntries)
      `shouldBe` Left (BackendDuplicateClosureEntry "__mlfp_closure$dup")
    validateBackendProgram entryNameBindingCollision
      `shouldBe` Left (BackendClosureEntryNameCollision "helper")
    validateBackendProgram entryNameRuntimeCollision
      `shouldBe` Left (BackendClosureEntryNameCollision "__mlfp_and")
    validateBackendProgram (programWithMainExpr duplicateCaptureAndParameter)
      `shouldBe` Left (BackendDuplicateClosureParameter "x")
    validateBackendProgram (programWithMainExpr nonFunctionClosure)
      `shouldBe` Left (BackendClosureExpectedFunction "__mlfp_closure$non_function" intTy)
    validateBackendProgram (programWithMainExpr underspecifiedClosureParams)
      `shouldBe` Left (BackendClosureParameterArityMismatch "__mlfp_closure$underspecified_params" 0 1)
    validateBackendProgram (programWithMainExpr badCall)
      `shouldBe` Left (BackendClosureCallArgumentMismatch 0 intTy boolTy)
    validateBackendProgram (programWithMainExpr nonClosureCall)
      `shouldBe` Left (BackendClosureCallExpectedClosureValue idTy)
    validateBackendProgram (programWithMainExpr unlistedLocalCapture)
      `shouldBe` Left (BackendUnknownVariable "captured")
    validateBackendProgram (programWithMainExpr appCalledClosure)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")
    validateBackendProgram (programWithMainExpr appCalledLetHeadClosure)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")
    validateBackendProgram (programWithMainExpr appCalledClosureAlias)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")
    validateBackendProgram (programWithMainExpr appCalledCaseHeadClosure)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "__mlfp_closure$app_case")
    validateBackendProgram (programWithMainExpr appCalledCapturedClosure)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "capturedClosure")
    validateBackendProgram (programWithMainExpr closureCallMixedCaseHead)
      `shouldBe` Left (BackendClosureCallExpectedClosureValue idTy)

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

  it "applies multiple backend type substitutions simultaneously" $ do
    let sourceTy = pairTy (BTVar "a") (BTVar "b")
        substitutions = Map.fromList [("a", BTVar "b"), ("b", BTVar "a")]

    substituteBackendTypes substitutions sourceTy `shouldBe` pairTy (BTVar "b") (BTVar "a")

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

  it "keeps structural recursive owner names module-qualified" $ do
    alphaEqBackendType (BTBase (BaseTy "Core.T")) (BTMu "$Core.T_self" nullaryStructuralBody)
      `shouldBe` True

    alphaEqBackendType (BTBase (BaseTy "Other.T")) (BTMu "$Core.T_self" nullaryStructuralBody)
      `shouldBe` False

    alphaEqBackendType (BTBase (BaseTy "Other.T")) (BTMu "$T_self" nullaryStructuralBody)
      `shouldBe` False

  it "rejects non-structural recursive bodies as nominal data encodings" $ do
    alphaEqBackendType (BTBase (BaseTy "Core.T")) (BTMu "$Core.T_self" BTBottom)
      `shouldBe` False

    validateBackendProgram (programWithMainExpr malformedStructuralBoxConstructExpr)
      `shouldBe` Left (BackendConstructorResultMismatch "Box" boxTy malformedStructuralBoxTy)

  it "checks structural constructor result payloads against metadata" $ do
    alphaEqBackendType boxTy structuralBoxTy
      `shouldBe` False

    validateBackendProgram (programWithMainExpr structuralBoxConstructExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithMainExpr mismatchedStructuralBoxConstructExpr)
      `shouldBe` Left (BackendConstructorResultMismatch "Box" boxTy mismatchedStructuralBoxTy)

    validateBackendProgram mismatchedStructuralBoxCaseProgram
      `shouldBe` Left (BackendCaseConstructorScrutineeMismatch "Box" mismatchedStructuralBoxTy boxTy)

  it "preserves nominal arguments when structural recursive payloads omit them" $ do
    alphaEqBackendType
      (BTCon (BaseTy "Core.Phantom") (intTy :| []))
      (BTMu "$Core.Phantom_self" (BTForall "r" Nothing (BTVar "r")))
      `shouldBe` False

    alphaEqBackendType
      (BTCon (BaseTy "Core.Phantom") (BTVar "a" :| []))
      (BTMu "$Core.Phantom_self" (BTForall "r" Nothing (BTVar "r")))
      `shouldBe` True

  it "recovers structural data arguments in declared parameter order" $ do
    validateBackendProgram (programWithDataAndMainExpr [outOfOrderStructuralData] outOfOrderStructuralConstructExpr)
      `shouldBe` Right ()

  it "rejects recursive roll and unroll type mismatches" $ do
    let recTy = BTMu "self" intTy

    validateBackendExpr (BackendRoll recTy (boolLit True))
      `shouldBe` Left (BackendRollPayloadMismatch intTy boolTy)

    validateBackendExpr (BackendUnroll boolTy (BackendVar recTy "boxed"))
      `shouldBe` Left (BackendUnrollResultMismatch boolTy intTy)

  it "compares vacuous recursive wrappers by their bodies" $ do
    validateBackendProgram vacuousRecursiveVariableMismatchProgram
      `shouldBe` Left (BackendVariableTypeMismatch "x" vacuousRecursiveIntTy vacuousRecursiveBoolTy)

    validateBackendProgram oneSidedVacuousRecursiveMismatchProgram
      `shouldBe` Left (BackendVariableTypeMismatch "x" recursiveArrowIntTy vacuousRecursiveBoolTy)

    validateBackendProgram vacuousRecursiveConstructorMismatchProgram
      `shouldBe` Left
        (BackendConstructorArgumentMismatch "VacuousMuBox" 0 vacuousRecursiveIntTy vacuousRecursiveBoolTy)

  it "accepts ADT construction and case analysis through constructor metadata" $ do
    validateBackendProgram (programWithMainExpr boxCaseExpr)
      `shouldBe` Right ()

  it "accepts parameterized constructor metadata at use and case sites" $ do
    validateBackendProgram (programWithDataAndMainExpr [optionData] someIntExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [optionData] optionCaseExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [optionData] someIntAsOptionVarExpr)
      `shouldBe` Left (BackendConstructorArgumentMismatch "Some" 0 (BTVar "a") intTy)

    validateBackendProgram (programWithDataAndMainExpr [optionData] someBoolAsOptionIntExpr)
      `shouldBe` Left (BackendConstructorArgumentMismatch "Some" 0 intTy boolTy)

  it "substitutes applied type variables in constructor metadata" $ do
    substituteBackendTypes
      (Map.fromList [("f", BTBase (BaseTy "BoxF")), ("a", boolTy)])
      (BTVarApp "f" (BTVar "a" :| []))
      `shouldBe` boxFTy boolTy

    validateBackendProgram (programWithDataAndMainExpr [boxFData, maybeFData] justFBoxBoolExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [boxFData, maybeFData] maybeFCaseExpr)
      `shouldBe` Right ()

  it "uses constructor-level forall metadata when validating constructor fields" $ do
    validateBackendProgram (programWithDataAndMainExpr [packData] packIntExpr)
      `shouldBe` Right ()

  it "enforces constructor-level forall bounds at construct and case boundaries" $ do
    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackIntExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackBoolExpr)
      `shouldBe` Left (BackendConstructorArgumentMismatch "BoundedPack" 0 intTy boolTy)

    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackCaseExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackRepackCaseExpr)
      `shouldBe` Right ()

    validateBackendProgram boundedPackOuterNameCollisionCaseProgram
      `shouldBe` Right ()

    validateBackendProgram boundedPackOuterNameCollisionWrongOuterUseProgram
      `shouldBe` Left (BackendVariableTypeMismatch "outer" (BTVar "a") intTy)

    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackWrongBoundUseCaseExpr)
      `shouldBe` Left (BackendVariableTypeMismatch "n" (BTVar "a") boolTy)

    validateBackendProgram boundedListPackCaseProgram
      `shouldBe` Right ()

    validateBackendProgram boundedListPackWrongBoundUseCaseProgram
      `shouldBe` Left (BackendVariableTypeMismatch "n" (listTy (BTVar "a")) (listTy boolTy))

    validateBackendProgram (programWithDataAndMainExpr [dependentBoundedPackData] dependentBoundedPackIntExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [dependentBoundedPackData] dependentBoundedPackBoolExpr)
      `shouldBe` Left (BackendConstructorArgumentMismatch "DependentBoundedPack" 0 intTy boolTy)

    validateBackendProgram dependentActualBoundPackProgram
      `shouldBe` Right ()

    validateBackendProgram dependentActualBoundPackWrongProgram
      `shouldBe` Left (BackendConstructorArgumentMismatch "DependentActualBoundPack" 0 (listTy intTy) (BTVar "b"))

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
                  [ BackendData "LeftBox" [] [BackendConstructor "Box" [] [intTy] boxTy],
                    BackendData "RightBox" [] [BackendConstructor "Box" [] [intTy] boxTy]
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
      backendDataConstructors = [BackendConstructor "Box" [] [intTy] boxTy]
    }

optionData :: BackendData
optionData =
  BackendData
    { backendDataName = "Option",
      backendDataParameters = ["a"],
      backendDataConstructors = [BackendConstructor "Some" [] [BTVar "a"] (optionTy (BTVar "a"))]
    }

boxFData :: BackendData
boxFData =
  BackendData
    { backendDataName = "BoxF",
      backendDataParameters = ["a"],
      backendDataConstructors = [BackendConstructor "BoxF" [] [BTVar "a"] (boxFTy (BTVar "a"))]
    }

maybeFData :: BackendData
maybeFData =
  BackendData
    { backendDataName = "MaybeF",
      backendDataParameters = ["f", "a"],
      backendDataConstructors =
        [ BackendConstructor "NothingF" [] [] (maybeFTy (BTVar "f") (BTVar "a")),
          BackendConstructor "JustF" [] [BTVarApp "f" (BTVar "a" :| [])] (maybeFTy (BTVar "f") (BTVar "a"))
        ]
    }

packData :: BackendData
packData =
  BackendData
    { backendDataName = "Pack",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "Pack" [BackendTypeBinder "a" Nothing] [BTVar "a"] packTy]
    }

boundedPackData :: BackendData
boundedPackData =
  BackendData
    { backendDataName = "BoundedPack",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "BoundedPack" [BackendTypeBinder "a" (Just intTy)] [BTVar "a"] boundedPackTy]
    }

boundedListPackData :: BackendData
boundedListPackData =
  BackendData
    { backendDataName = "BoundedListPack",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "BoundedListPack" [BackendTypeBinder "a" (Just intTy)] [listTy (BTVar "a")] boundedListPackTy]
    }

dependentBoundedPackData :: BackendData
dependentBoundedPackData =
  BackendData
    { backendDataName = "DependentBoundedPack",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            "DependentBoundedPack"
            [BackendTypeBinder "z" (Just intTy), BackendTypeBinder "a" (Just (BTVar "z"))]
            [BTVar "a"]
            dependentBoundedPackTy
        ]
    }

dependentActualBoundPackData :: BackendData
dependentActualBoundPackData =
  BackendData
    { backendDataName = "DependentActualBoundPack",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            "DependentActualBoundPack"
            [BackendTypeBinder "a" (Just (listTy intTy))]
            [BTVar "a"]
            dependentActualBoundPackTy
        ]
    }

captureForallData :: BackendData
captureForallData =
  BackendData
    { backendDataName = "CaptureForall",
      backendDataParameters = ["p"],
      backendDataConstructors =
        [ BackendConstructor
            "CaptureForall"
            []
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
            []
            [BTMu "a" (BTVar "p")]
            (captureTy "CaptureMu" (BTVar "p"))
        ]
    }

captureCaseData :: BackendData
captureCaseData =
  BackendData
    { backendDataName = "CaptureCase",
      backendDataParameters = ["p"],
      backendDataConstructors = [BackendConstructor "CaptureCase" [] [] captureCaseTemplateTy]
    }

outOfOrderStructuralData :: BackendData
outOfOrderStructuralData =
  BackendData
    { backendDataName = "OutOfOrder",
      backendDataParameters = ["a", "b"],
      backendDataConstructors =
        [ BackendConstructor
            "OutOfOrder"
            []
            [BTVar "b", BTVar "a"]
            outOfOrderStructuralTy
        ]
    }

vacuousRecursiveBoxData :: BackendData
vacuousRecursiveBoxData =
  BackendData
    { backendDataName = "VacuousMuBox",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "VacuousMuBox" [] [vacuousRecursiveIntTy] vacuousRecursiveBoxTy]
    }

boxCaseExpr :: BackendExpr
boxCaseExpr =
  boxCaseExprWith
    (BackendConstruct boxTy "Box" [intLit 1])
    (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendVar intTy "n") :| [])

someIntExpr :: BackendExpr
someIntExpr =
  BackendConstruct (optionTy intTy) "Some" [intLit 1]

someIntAsOptionVarExpr :: BackendExpr
someIntAsOptionVarExpr =
  BackendConstruct (optionTy (BTVar "a")) "Some" [intLit 1]

someBoolAsOptionIntExpr :: BackendExpr
someBoolAsOptionIntExpr =
  BackendConstruct (optionTy intTy) "Some" [boolLit True]

malformedStructuralBoxTy :: BackendType
malformedStructuralBoxTy =
  BTMu "$Box_self" BTBottom

malformedStructuralBoxConstructExpr :: BackendExpr
malformedStructuralBoxConstructExpr =
  BackendConstruct malformedStructuralBoxTy "Box" [intLit 1]

structuralBoxTy :: BackendType
structuralBoxTy =
  BTMu "$Box_self" (singleFieldStructuralBody intTy)

structuralBoxConstructExpr :: BackendExpr
structuralBoxConstructExpr =
  BackendConstruct structuralBoxTy "Box" [intLit 1]

mismatchedStructuralBoxTy :: BackendType
mismatchedStructuralBoxTy =
  BTMu "$Box_self" (singleFieldStructuralBody boolTy)

mismatchedStructuralBoxConstructExpr :: BackendExpr
mismatchedStructuralBoxConstructExpr =
  BackendConstruct mismatchedStructuralBoxTy "Box" [intLit 1]

mismatchedStructuralBoxCaseProgram :: BackendProgram
mismatchedStructuralBoxCaseProgram =
  programWithDataAndBindings
    [boxData]
    [ binding "badBox" mismatchedStructuralBoxTy (BackendVar mismatchedStructuralBoxTy "badBox"),
      mainBinding
        ( boxCaseExprWith
            (BackendVar mismatchedStructuralBoxTy "badBox")
            (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendVar intTy "n") :| [])
        )
    ]

justFBoxBoolExpr :: BackendExpr
justFBoxBoolExpr =
  BackendConstruct (maybeFTy (BTBase (BaseTy "BoxF")) boolTy) "JustF" [BackendConstruct (boxFTy boolTy) "BoxF" [boolLit True]]

maybeFCaseExpr :: BackendExpr
maybeFCaseExpr =
  BackendCase
    { backendExprType = boolTy,
      backendScrutinee = justFBoxBoolExpr,
      backendAlternatives =
        BackendAlternative (BackendConstructorPattern "NothingF" []) (boolLit False)
          :| [BackendAlternative (BackendConstructorPattern "JustF" ["box"]) (boolLit True)]
    }

packIntExpr :: BackendExpr
packIntExpr =
  BackendConstruct packTy "Pack" [intLit 1]

boundedPackIntExpr :: BackendExpr
boundedPackIntExpr =
  BackendConstruct boundedPackTy "BoundedPack" [intLit 1]

boundedPackBoolExpr :: BackendExpr
boundedPackBoolExpr =
  BackendConstruct boundedPackTy "BoundedPack" [boolLit True]

dependentBoundedPackIntExpr :: BackendExpr
dependentBoundedPackIntExpr =
  BackendConstruct dependentBoundedPackTy "DependentBoundedPack" [intLit 1]

dependentBoundedPackBoolExpr :: BackendExpr
dependentBoundedPackBoolExpr =
  BackendConstruct dependentBoundedPackTy "DependentBoundedPack" [boolLit True]

dependentActualBoundPackProgram :: BackendProgram
dependentActualBoundPackProgram =
  programWithDataAndMainExpr [dependentActualBoundPackData] (dependentActualBoundPackWrapper intTy)

dependentActualBoundPackWrongProgram :: BackendProgram
dependentActualBoundPackWrongProgram =
  programWithDataAndMainExpr [dependentActualBoundPackData] (dependentActualBoundPackWrapper boolTy)

dependentActualBoundPackWrapper :: BackendType -> BackendExpr
dependentActualBoundPackWrapper zBound =
  BackendTyAbs
    { backendExprType = dependentActualBoundPackWrapperTy zBound,
      backendTyParamName = "z",
      backendTyParamBound = Just zBound,
      backendTyAbsBody =
        BackendTyAbs
          { backendExprType = dependentActualBoundPackInnerTy,
            backendTyParamName = "b",
            backendTyParamBound = Just (listTy (BTVar "z")),
            backendTyAbsBody =
              BackendLam
                { backendExprType = BTArrow (BTVar "b") dependentActualBoundPackTy,
                  backendParamName = "x",
                  backendParamType = BTVar "b",
                  backendBody =
                    BackendConstruct
                      dependentActualBoundPackTy
                      "DependentActualBoundPack"
                      [BackendVar (BTVar "b") "x"]
                }
          }
    }

dependentActualBoundPackWrapperTy :: BackendType -> BackendType
dependentActualBoundPackWrapperTy zBound =
  BTForall "z" (Just zBound) dependentActualBoundPackInnerTy

dependentActualBoundPackInnerTy :: BackendType
dependentActualBoundPackInnerTy =
  BTForall "b" (Just (listTy (BTVar "z"))) (BTArrow (BTVar "b") dependentActualBoundPackTy)

boundedPackCaseExpr :: BackendExpr
boundedPackCaseExpr =
  BackendCase
    { backendExprType = intTy,
      backendScrutinee = boundedPackIntExpr,
      backendAlternatives = BackendAlternative (BackendConstructorPattern "BoundedPack" ["n"]) (BackendVar intTy "n") :| []
    }

boundedPackRepackCaseExpr :: BackendExpr
boundedPackRepackCaseExpr =
  BackendCase
    { backendExprType = boundedPackTy,
      backendScrutinee = boundedPackIntExpr,
      backendAlternatives =
        BackendAlternative
          (BackendConstructorPattern "BoundedPack" ["n"])
          (BackendConstruct boundedPackTy "BoundedPack" [BackendVar (BTVar "a") "n"])
          :| []
    }

boundedPackOuterNameCollisionCaseProgram :: BackendProgram
boundedPackOuterNameCollisionCaseProgram =
  boundedPackOuterNameCollisionCaseWith (BackendVar intTy "n")

boundedPackOuterNameCollisionWrongOuterUseProgram :: BackendProgram
boundedPackOuterNameCollisionWrongOuterUseProgram =
  boundedPackOuterNameCollisionCaseWith (BackendVar intTy "outer")

boundedPackOuterNameCollisionCaseWith :: BackendExpr -> BackendProgram
boundedPackOuterNameCollisionCaseWith branchBody =
  programWithDataAndMainExpr
    [boundedPackData]
    ( BackendTyAbs
        { backendExprType = BTForall "a" (Just boolTy) (BTArrow (BTVar "a") intTy),
          backendTyParamName = "a",
          backendTyParamBound = Just boolTy,
          backendTyAbsBody =
            BackendLam
              { backendExprType = BTArrow (BTVar "a") intTy,
                backendParamName = "outer",
                backendParamType = BTVar "a",
                backendBody =
                  BackendCase
                    { backendExprType = intTy,
                      backendScrutinee = boundedPackIntExpr,
                      backendAlternatives =
                        BackendAlternative
                          (BackendConstructorPattern "BoundedPack" ["n"])
                          branchBody
                          :| []
                    }
              }
        }
    )

boundedPackWrongBoundUseCaseExpr :: BackendExpr
boundedPackWrongBoundUseCaseExpr =
  BackendCase
    { backendExprType = boolTy,
      backendScrutinee = boundedPackIntExpr,
      backendAlternatives =
        BackendAlternative
          (BackendConstructorPattern "BoundedPack" ["n"])
          (BackendVar boolTy "n")
          :| []
    }

boundedListPackCaseProgram :: BackendProgram
boundedListPackCaseProgram =
  programWithDataAndBindings
    [boundedListPackData]
    [ mainBinding
        BackendCase
          { backendExprType = listTy intTy,
            backendScrutinee = boundedListPackListIntExpr,
            backendAlternatives =
              BackendAlternative
                (BackendConstructorPattern "BoundedListPack" ["n"])
                (BackendVar (listTy intTy) "n")
                :| []
          },
      listArgBinding
    ]

boundedListPackWrongBoundUseCaseProgram :: BackendProgram
boundedListPackWrongBoundUseCaseProgram =
  programWithDataAndBindings
    [boundedListPackData]
    [ mainBinding
        BackendCase
          { backendExprType = listTy boolTy,
            backendScrutinee = boundedListPackListIntExpr,
            backendAlternatives =
              BackendAlternative
                (BackendConstructorPattern "BoundedListPack" ["n"])
                (BackendVar (listTy boolTy) "n")
                :| []
          },
      listArgBinding
    ]

boundedListPackListIntExpr :: BackendExpr
boundedListPackListIntExpr =
  BackendConstruct boundedListPackTy "BoundedListPack" [BackendVar (listTy intTy) "listArg"]

listArgBinding :: BackendBinding
listArgBinding =
  binding "listArg" (listTy intTy) (BackendVar (listTy intTy) "listArg")

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

vacuousRecursiveVariableMismatchProgram :: BackendProgram
vacuousRecursiveVariableMismatchProgram =
  programWithBindings
    [ mainBinding
        BackendLam
          { backendExprType = BTArrow vacuousRecursiveIntTy vacuousRecursiveBoolTy,
            backendParamName = "x",
            backendParamType = vacuousRecursiveIntTy,
            backendBody = BackendVar vacuousRecursiveBoolTy "x"
          }
    ]

oneSidedVacuousRecursiveMismatchProgram :: BackendProgram
oneSidedVacuousRecursiveMismatchProgram =
  programWithBindings
    [ mainBinding
        BackendLam
          { backendExprType = BTArrow recursiveArrowIntTy vacuousRecursiveBoolTy,
            backendParamName = "x",
            backendParamType = recursiveArrowIntTy,
            backendBody = BackendVar vacuousRecursiveBoolTy "x"
          }
    ]

vacuousRecursiveConstructorMismatchProgram :: BackendProgram
vacuousRecursiveConstructorMismatchProgram =
  programWithDataAndBindings
    [vacuousRecursiveBoxData]
    [ mainBinding (BackendConstruct vacuousRecursiveBoxTy "VacuousMuBox" [BackendVar vacuousRecursiveBoolTy "muBoolArg"]),
      binding "muBoolArg" vacuousRecursiveBoolTy (BackendVar vacuousRecursiveBoolTy "muBoolArg")
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

nullaryStructuralBody :: BackendType
nullaryStructuralBody =
  BTForall "r" Nothing (BTArrow (BTVar "r") (BTVar "r"))

singleFieldStructuralBody :: BackendType -> BackendType
singleFieldStructuralBody fieldTy =
  BTForall "r" Nothing (BTArrow (BTArrow fieldTy (BTVar "r")) (BTVar "r"))

boxTy :: BackendType
boxTy =
  BTBase (BaseTy "Box")

packTy :: BackendType
packTy =
  BTBase (BaseTy "Pack")

boundedPackTy :: BackendType
boundedPackTy =
  BTBase (BaseTy "BoundedPack")

boundedListPackTy :: BackendType
boundedListPackTy =
  BTBase (BaseTy "BoundedListPack")

dependentBoundedPackTy :: BackendType
dependentBoundedPackTy =
  BTBase (BaseTy "DependentBoundedPack")

dependentActualBoundPackTy :: BackendType
dependentActualBoundPackTy =
  BTBase (BaseTy "DependentActualBoundPack")

vacuousRecursiveBoxTy :: BackendType
vacuousRecursiveBoxTy =
  BTBase (BaseTy "VacuousMuBox")

listTy :: BackendType -> BackendType
listTy ty =
  BTCon (BaseTy "List") (ty :| [])

optionTy :: BackendType -> BackendType
optionTy ty =
  BTCon (BaseTy "Option") (ty :| [])

boxFTy :: BackendType -> BackendType
boxFTy ty =
  BTCon (BaseTy "BoxF") (ty :| [])

maybeFTy :: BackendType -> BackendType -> BackendType
maybeFTy fTy argTy =
  BTCon (BaseTy "MaybeF") (fTy :| [argTy])

pairTy :: BackendType -> BackendType -> BackendType
pairTy left right =
  BTCon (BaseTy "Pair") (left :| [right])

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

vacuousRecursiveIntTy :: BackendType
vacuousRecursiveIntTy =
  BTMu "a" intTy

vacuousRecursiveBoolTy :: BackendType
vacuousRecursiveBoolTy =
  BTMu "b" boolTy

recursiveArrowIntTy :: BackendType
recursiveArrowIntTy =
  BTMu "self" (BTArrow (BTVar "self") intTy)

captureCaseTemplateTy :: BackendType
captureCaseTemplateTy =
  BTCon (BaseTy "CaptureCase") (BTVar "p" :| [BTForall "a" Nothing (BTVar "p")])

captureCaseScrutineeTy :: BackendType
captureCaseScrutineeTy =
  BTCon (BaseTy "CaptureCase") (BTVar "a1" :| [captureForallActualTy])

outOfOrderStructuralConstructExpr :: BackendExpr
outOfOrderStructuralConstructExpr =
  BackendConstruct
    (outOfOrderTy intTy boolTy)
    "OutOfOrder"
    [boolLit True, intLit 1]

outOfOrderTy :: BackendType -> BackendType -> BackendType
outOfOrderTy aTy bTy =
  BTCon (BaseTy "OutOfOrder") (aTy :| [bTy])

outOfOrderStructuralTy :: BackendType
outOfOrderStructuralTy =
  BTMu "$OutOfOrder_self" outOfOrderStructuralBody

outOfOrderStructuralBody :: BackendType
outOfOrderStructuralBody =
  BTForall
    "r"
    Nothing
    (BTArrow (BTArrow (BTVar "b") (BTArrow (BTVar "a") (BTVar "r"))) (BTVar "r"))
