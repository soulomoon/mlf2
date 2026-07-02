{-# LANGUAGE GADTs #-}

module BackendConvertSpec (spec) where

import Control.Applicative ((<|>))
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import ElabTermTestSupport
  ( generatedResolvedLocal,
    generatedResolvedLocalForName,
    mkTestDeferredVar,
    mkTestLocalLam,
    mkTestLocalLet,
    mkTestRecursiveLocalLet,
    mkTestTyAbs,
    testTForall,
    testTMu,
    testTVar,
  )
import MLF.API (parseRawProgram, renderProgramParseError)
import MLF.Backend.Convert
import MLF.Backend.IR
import qualified MLF.Backend.LLVM.Lower as Lower
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId (..))
import MLF.Elab.Types (schemeFromType)
import qualified MLF.Types.Elab as Elab
import MLF.Frontend.Program.Builtins (builtinTypeIdentity)
import MLF.Frontend.Program.Prelude (withPrelude)
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), symbolDefiningName, symbolIdentityFromParts, symbolIdentityStableName, symbolUniqueIdentity)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ClassInfo (..),
    DeferredBindingMode (..),
    DeferredConstructorCall (..),
    DeferredProgramObligation (..),
    TypeView (..),
    ValueInfo (..),
    checkedBindingName,
    checkedProgramMain,
    classInfoSymbolIdentity,
    ctorForalls,
    ctorArgs,
    constructorRefFromInfo,
    ConstructorInfo (..),
    DataInfo (..),
    dataInfoIdentityQualifiedName,
    emptyTypeBinderSubst,
    IdDetails (..),
    instanceHeadIdentityTypes,
    instanceInfoClassSymbolIdentity,
    lookupInstanceMethod,
    methodName,
    mkTypeView,
    typeHeadNamesSrcType,
  )
import MLF.Frontend.Syntax (Lit (..), SrcBound (..), SrcTy (..), SrcType)
import MLF.Frontend.Syntax.Program (Program)
import MLF.Pipeline (checkProgram)
import MLF.Types.Identity (deferredRefFromIdentity, deferredRefName, UniqueIdentity (..), typeBinderIdentityFromUnique, typeBinderIdentityStableName)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import Test.Hspec

spec :: Spec
spec = describe "MLF.Backend.Convert" $ do
  it "converts a checked function program to validated backend IR" $ do
    checked <- requireChecked simpleFunctionProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendProgramMain backend `shouldBe` "Main__main"

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding `shouldBe` intTy
    backendBindingExportedAsMain mainBinding `shouldBe` True
    case backendBindingExpr mainBinding of
      BackendApp
        { backendExprType = resultTy,
          backendFunction = BackendVarWithIdentity {backendVarName = "Main__id"},
          backendArgument = BackendLit {backendLit = LInt 1}
        } ->
          resultTy `shouldBe` intTy
      other -> expectationFailure ("expected backend application, got " ++ show other)

  it "builds test local terms with resolved local occurrences" $ do
    let lamTerm = mkTestLocalLam "x" intElabTy (mkTestDeferredVar "x")
        letTerm =
          mkTestLocalLet
            "x"
            (schemeFromType intElabTy)
            (mkTestDeferredVar "x")
            (mkTestDeferredVar "x")
    case lamTerm of
      Elab.ELam binder (Elab.EVarNode occurrence) ->
        occurrence `shouldSatisfy` Elab.resolvedVarSameIdentity binder
      other -> expectationFailure ("expected resolved local lambda occurrence, got " ++ show other)
    case letTerm of
      Elab.ELet binder _ (Elab.EVarNode rhs) (Elab.EVarNode body) -> do
        fmap deferredRefName (Elab.deferredResolvedVarRef rhs) `shouldBe` Just "x"
        body `shouldSatisfy` Elab.resolvedVarSameIdentity binder
      other -> expectationFailure ("expected resolved local let body occurrence, got " ++ show other)

  it "accepts backend conversion when pure bindings reference opaque Prelude helpers" $ do
    program <-
      requireParsed $
        unlines
          [ "module Main export (main) {",
            "  import Prelude exposing (Unit(..), IO, pure);",
            "  def discard : IO Unit -> Unit = λ(_action : IO Unit) Unit;",
            "  def main : Unit = discard (pure Unit);",
            "}"
          ]
    checked <- requireRight (checkProgram (withPrelude program))
    backend <- requireRight (convertCheckedProgram checked)
    validateBackendProgram backend `shouldBe` Right ()

  it "accepts backend conversion when pure bindings reference IO primitives" $ do
    program <-
      requireParsed $
        unlines
          [ "module Main export (main) {",
            "  import Prelude exposing (Unit(..), IO);",
            "  def main : Unit = (λ(_action : IO Unit) Unit) (__io_pure Unit);",
            "}"
          ]
    checked <- requireRight (checkProgram (withPrelude program))
    backend <- requireRight (convertCheckedProgram checked)
    validateBackendProgram backend `shouldBe` Right ()

  it "accepts backend conversion when pure bindings reference IORef primitives" $ do
    program <-
      requireParsed $
        unlines
          [ "module Main export (main) {",
            "  import Prelude exposing (Unit(..), IO);",
            "  def main : Unit = (λ(_action : IO (IORef Unit)) Unit) (__io_newIORef Unit);",
            "}"
          ]
    checked <- requireRight (checkProgram (withPrelude program))
    backend <- requireRight (convertCheckedProgram checked)
    validateBackendProgram backend `shouldBe` Right ()

  it "preserves source data identity when a primitive result is structurally compatible" $ do
    program <-
      requireParsed $
        unlines
          [ "module Main export (main) {",
            "  import Prelude exposing (Option(..), stringCharAtOption);",
            "  def main : Option Char = stringCharAtOption \"abc\" 0;",
            "}"
          ]
    checked <- requireRight (checkProgram (withPrelude program))
    backend <- requireRight (convertCheckedProgram checked)
    validateBackendProgram backend `shouldBe` Right ()

    optionData <- requireBackendData "Prelude.Option" backend
    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingType mainBinding of
      BTConWithIdentity (Just identity) (BaseTy "Prelude.Option") (_ :| []) ->
        Just identity `shouldBe` backendDataIdentity optionData
      other ->
        expectationFailure ("expected identity-bearing Prelude.Option result, got " ++ show other)

  it "converts backend modules from checked module identity when module names are stale" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked = renameCheckedModuleName "Main" "$stale_Main" checked0
    backend <- requireRight (convertCheckedProgram checked)

    case backendProgramModules backend of
      [backendModule] -> do
        fmap symbolDefiningName (backendModuleIdentity backendModule) `shouldBe` Just "Main"
        backendModuleName backendModule `shouldBe` "Main"
      modules0 ->
        expectationFailure ("expected one backend module, got " ++ show (length modules0))
    validateBackendProgram backend `shouldBe` Right ()

  it "resolves backend main by checked main identity when the main runtime name is stale" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked = renameCheckedProgramMainRuntimeName "$stale_Main__main" checked0
    backend <- requireRight (convertCheckedProgram checked)

    backendProgramMain backend `shouldBe` "Main__main"
    fmap symbolDefiningName (backendProgramMainIdentity backend) `shouldBe` Just "main"
    validateBackendProgram backend `shouldBe` Right ()

  it "matches the checked backend IR snapshot for a primitive function program" $ do
    checked <- requireChecked simpleFunctionProgram
    backend <- requireRight (convertCheckedProgram checked)

    backendIRGolden "test/golden/backend-ir-simple-function.golden" backend

  it "recovers explicit backend constructors and cases from checked ADT paths" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendDataNames backend `shouldSatisfy` (not . null)
    backendConstructorNames backend `shouldSatisfy` (not . null)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase
    collectConstructNames (backendBindingExpr mainBinding) `shouldSatisfy` (not . null)

  it "matches the checked backend IR snapshot for a simple ADT case program" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)

    backendIRGolden "test/golden/backend-ir-adt-case.golden" backend

  it "recovers backend cases when the result type differs from the scrutinee ADT" $ do
    checked <- requireChecked intCaseProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case findBackendCase (backendBindingExpr mainBinding) of
      Just BackendCase {backendExprType = resultTy, backendScrutinee = scrutinee} -> do
        resultTy `shouldBe` intTy
        backendExprType scrutinee `shouldSatisfy` (/= intTy)
      Just other -> expectationFailure ("expected backend case, got " ++ show other)
      Nothing -> expectationFailure "expected backend case"

  it "applies over-applied case-shaped terms through the closure-call path" $ do
    checked0 <- requireChecked functionCaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = intElabTy,
                    checkedBindingTerm = Elab.EApp (checkedBindingTerm binding) (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingExpr mainBinding of
      BackendClosureCall
        { backendExprType = resultTy,
          backendClosureFunction = fun,
          backendClosureArguments = [BackendLit {backendLit = LInt 1}]
        } -> do
          resultTy `shouldBe` intTy
          fun `shouldSatisfy` containsBackendCase
      other -> expectationFailure ("expected backend closure call of recovered case, got " ++ show other)

  it "preserves resolved locals while inferring partial application heads" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let binder = resolvedLocal "$x#0" "runtime-x" intElabTy
        occurrence = resolvedLocal "$x#0" "different-runtime" intElabTy
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      Elab.EApp
                        (Elab.ELam binder (Elab.EVarNode occurrence))
                        (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendApp

  it "recovers backend cases with type-wrapped handler lambdas" $ do
    checked0 <- requireChecked intCaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding {checkedBindingTerm = wrapCaseHandlersWithTypeWrappers (checkedBindingTerm binding)}
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase

  it "keeps type wrappers that belong to case handler bodies" $ do
    checked0 <- requireChecked functionCaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding {checkedBindingTerm = replaceCaseHandlerBodiesAfterLams 1 instantiatedIntIdentity (checkedBindingTerm binding)}
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendTyApp

  it "accepts alpha-equivalent case handler result types" $ do
    checked0 <- requireChecked functionCaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = polymorphicIdentityElabTy,
                    checkedBindingTerm = replaceCaseHandlerBodiesAfterLams 1 alphaEquivalentIdentityInstId (checkedBindingTerm binding)
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "preserves expected checked binding types through ordinary fallback" $ do
    checked <- requireChecked =<< readFile "test/programs/recursive-adt/abstract-module-use.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "validates, converts, and lowers the same recursive ADT through public backend paths" $ do
    checked <- requireChecked =<< readFile "test/programs/recursive-adt/plain-recursive-nat.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    _ <- requireRight (Lower.lowerBackendProgram backend)
    pure ()

  it "renames expected forall bodies to actual type abstraction binders" $ do
    checked <- requireChecked =<< readFile "test/programs/unified/first-class-polymorphism.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "uses canonical expected type abstraction names for same-spelled refs" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = mkTypeView sameNamedTypeAbsSourceTy sameNamedTypeAbsSourceTy,
                    checkedBindingType = sameNamedTypeAbsElabTy,
                    checkedBindingTerm = sameNamedTypeAbsTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    let (canonicalOuterRef, gen1) =
          Elab.freshTypeBinderRef "a" (Elab.identityGeneratorAfterType sameNamedTypeAbsElabTy)
        (canonicalInnerRef, _) =
          Elab.freshTypeBinderRef "a" gen1
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity
        (Just (Elab.typeBinderRefIdentity canonicalOuterRef))
        "a"
        Nothing
        ( BTForallWithIdentity
            (Just (Elab.typeBinderRefIdentity canonicalInnerRef))
            "a1"
            Nothing
            intTy
        )
    case backendBindingType mainBinding of
      BTForallWithIdentity (Just outerTypeIdentity) "a" Nothing (BTForallWithIdentity (Just innerTypeIdentity) "a1" Nothing _) -> do
        outerTypeIdentity `shouldBe` Elab.typeBinderRefIdentity canonicalOuterRef
        innerTypeIdentity `shouldBe` Elab.typeBinderRefIdentity canonicalInnerRef
      other ->
        expectationFailure ("expected identity-backed backend forall type, got " ++ show other)
    case backendBindingExpr mainBinding of
      BackendTyAbsWithIdentity
        { backendTyParamIdentity = Just outerIdentity,
          backendTyParamName = outerName,
          backendTyAbsBody =
            BackendTyAbsWithIdentity
              { backendTyParamIdentity = Just innerIdentity,
                backendTyParamName = innerName,
                backendTyAbsBody = BackendLit {backendLit = LInt 1}
              }
        } -> do
          outerName `shouldBe` "a"
          innerName `shouldBe` "a1"
          outerIdentity `shouldBe` Elab.typeBinderRefIdentity canonicalOuterRef
          innerIdentity `shouldBe` Elab.typeBinderRefIdentity canonicalInnerRef
      other ->
        expectationFailure ("expected nested backend type abstraction, got " ++ show other)

  it "rejects identity-less backend-to-elab free type variables" $ do
    let dataIdentity =
          symbolIdentityFromParts (UniqueIdentity 0) SymbolType "Main" "Box" Nothing
        backendTy =
          BTArrow
            (BTBaseWithIdentity (Just dataIdentity) (BaseTy "Box"))
            (BTVarWithIdentity Nothing "fresh")

    backendTypeToElabType backendTy `shouldBe` Nothing

  it "resolves backend-to-elab type binders by identity before display name" $ do
    let identity = typeBinderIdentityFromUnique (UniqueIdentity 7)
        backendTy =
          BTForallWithIdentity
            (Just identity)
            "canonical"
            Nothing
            (BTVarWithIdentity (Just identity) "stale")

    case backendTypeToElabType backendTy of
      Just (Elab.TForallRef binderRef Nothing (Elab.TVarRef occurrenceRef)) -> do
        Elab.typeBinderRefIdentity binderRef `shouldBe` identity
        Elab.typeBinderRefName binderRef `shouldBe` "canonical"
        Elab.typeBinderRefIdentity occurrenceRef `shouldBe` identity
        Elab.typeBinderRefName occurrenceRef `shouldBe` "canonical"
      other ->
        expectationFailure ("expected identity-keyed backend type conversion, got " ++ show other)

  it "rejects name-only backend-to-elab variables under identity binders" $ do
    let identity = typeBinderIdentityFromUnique (UniqueIdentity 8)
        backendTy =
          BTForallWithIdentity
            (Just identity)
            "a"
            Nothing
            (BTVar "a")

    backendTypeToElabType backendTy `shouldBe` Nothing

  it "seeds checked source type binders after source type head identities" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let typeIdentity =
          symbolIdentityFromParts (UniqueIdentity 0) SymbolType "Main" "Box" Nothing
        boxElabTy = Elab.TBaseWithIdentity (Just typeIdentity) (BaseTy "Box")
        sourceTy =
          STForall
            "a"
            (Just (SrcBound (STBase "Box")))
            (STForall "a" Nothing (STBase "Int"))
        sourceView =
          (mkTypeView sourceTy sourceTy)
            { typeViewHeadIdentities = Map.singleton "Box" typeIdentity
            }
        checkedTy =
          Elab.TForallRef
            sameNamedOuterTypeRef
            (Just boxElabTy)
            (Elab.TForallRef sameNamedInnerTypeRef Nothing intElabTy)
        checkedTerm =
          Elab.ETyAbsRef
            sameNamedOuterTypeRef
            (Just boxElabTy)
            (Elab.ETyAbsRef sameNamedInnerTypeRef Nothing (Elab.ELit (LInt 1)))
        expectedOuterIdentity = typeBinderIdentityFromUnique (UniqueIdentity 1)
        expectedInnerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 2)
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = sourceView,
                    checkedBindingType = checkedTy,
                    checkedBindingTerm = checkedTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity
        (Just expectedOuterIdentity)
        "a"
        (Just (BTBaseWithIdentity (Just typeIdentity) (BaseTy "Box")))
        (BTForallWithIdentity (Just expectedInnerIdentity) "a1" Nothing intTy)

  it "generates source type binder identities while canonicalizing stable-looking checked source types" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let stableName = "$typevar#991605"
        (canonicalOuterRef, gen1) =
          Elab.freshTypeBinderRef stableName (Elab.identityGeneratorAfterType sameNamedTypeAbsElabTy)
        (canonicalInnerRef, _) =
          Elab.freshTypeBinderRef "a" gen1
        expectedOuterIdentity = Elab.typeBinderRefIdentity canonicalOuterRef
        expectedInnerIdentity = Elab.typeBinderRefIdentity canonicalInnerRef
        sourceTy = STForall stableName Nothing (STForall "a" Nothing (STBase "Int"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = mkTypeView sourceTy sourceTy,
                    checkedBindingType = sameNamedTypeAbsElabTy,
                    checkedBindingTerm = sameNamedTypeAbsTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity
        (Just expectedOuterIdentity)
        stableName
        Nothing
        (BTForallWithIdentity (Just expectedInnerIdentity) "a" Nothing intTy)
    case backendBindingExpr mainBinding of
      BackendTyAbsWithIdentity
        { backendTyParamIdentity = Just outerIdentity,
          backendTyAbsBody =
            BackendTyAbsWithIdentity
              { backendTyParamIdentity = Just innerIdentity
              }
        } -> do
          outerIdentity `shouldBe` expectedOuterIdentity
          innerIdentity `shouldBe` expectedInnerIdentity
      other ->
        expectationFailure ("expected fresh backend type abstraction identities, got " ++ show other)

  it "uses checked source type binder identities while canonicalizing checked source types" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991606)
        sourceStableName = typeBinderIdentityStableName sourceIdentity
        displayTy = STForall "a" Nothing (STBase "Int")
        identityTy = STForall sourceStableName Nothing (STBase "Int")
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      (mkTypeView displayTy identityTy)
                        { typeViewBinderIdentities =
                            Map.singleton "a" sourceIdentity
                        },
                    checkedBindingType = Elab.TForallRef sameNamedOuterTypeRef Nothing intElabTy,
                    checkedBindingTerm = Elab.ETyAbsRef sameNamedOuterTypeRef Nothing (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity (Just sourceIdentity) "a" Nothing intTy
    case backendBindingExpr mainBinding of
      BackendTyAbsWithIdentity {backendTyParamIdentity = Just identity, backendTyParamName = name} -> do
        identity `shouldBe` sourceIdentity
        name `shouldBe` "a"
      other ->
        expectationFailure ("expected source identity-backed backend type abstraction, got " ++ show other)

  it "uses checked source type binder identities when identity names are stale" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991608)
        displayTy = STForall "a" Nothing (STBase "Int")
        identityTy = STForall "$stale_a" Nothing (STBase "Int")
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      (mkTypeView displayTy identityTy)
                        { typeViewBinderIdentities =
                            Map.singleton "a" sourceIdentity
                        },
                    checkedBindingType = Elab.TForallRef sameNamedOuterTypeRef Nothing intElabTy,
                    checkedBindingTerm = Elab.ETyAbsRef sameNamedOuterTypeRef Nothing (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity (Just sourceIdentity) "a" Nothing intTy

  it "does not reuse an outer display binder identity for a same-named missing inner binder" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let outerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991632)
        displayTy = STForall "a" Nothing (STForall "a" Nothing (STBase "Int"))
        identityTy = STForall "$outer_a" Nothing (STForall "$missing_inner_a" Nothing (STBase "Int"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      (mkTypeView displayTy identityTy)
                        { typeViewBinderIdentities =
                            Map.singleton "a" outerIdentity
                        },
                    checkedBindingType = sameNamedTypeAbsElabTy,
                    checkedBindingTerm = sameNamedTypeAbsTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingType mainBinding of
      BTForallWithIdentity (Just outerBackendIdentity) _ Nothing (BTForallWithIdentity (Just innerBackendIdentity) _ Nothing _) -> do
        outerBackendIdentity `shouldBe` outerIdentity
        innerBackendIdentity `shouldNotBe` outerIdentity
      other ->
        expectationFailure ("expected distinct identity-backed backend foralls, got " ++ show other)

  it "does not attach display binder identity to unrelated identity occurrences" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991609)
        checkedIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991610)
        checkedRef = Elab.typeBinderRefFromIdentity checkedIdentity "a"
        displayTy = STForall "a" Nothing (STArrow (STVar "a") (STBase "Int"))
        identityTy = STForall "$stale_a" Nothing (STArrow (STVar "$other") (STBase "Int"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      (mkTypeView displayTy identityTy)
                        { typeViewBinderIdentities =
                            Map.singleton "a" sourceIdentity
                        },
                    checkedBindingType = Elab.TForallRef checkedRef Nothing (Elab.TArrow (Elab.TVarRef checkedRef) intElabTy),
                    checkedBindingTerm = Elab.ETyAbsRef checkedRef Nothing (mkTestLocalLam "x" (Elab.TVarRef checkedRef) (Elab.ELit (LInt 1)))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity (Just checkedIdentity) "a" Nothing (BTArrow (BTVarWithIdentity (Just checkedIdentity) "a") intTy)

  it "keeps checked source type binder identities through source type view fallback" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991607)
        displayTy = STForall "a" Nothing (STBase "Int")
        identityTy = STBase "Int"
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      (mkTypeView displayTy identityTy)
                        { typeViewBinderIdentities =
                            Map.singleton "a" sourceIdentity
                        },
                    checkedBindingType = Elab.TForallRef sameNamedOuterTypeRef Nothing intElabTy,
                    checkedBindingTerm = Elab.ETyAbsRef sameNamedOuterTypeRef Nothing (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity (Just sourceIdentity) "a" Nothing intTy

  it "does not reuse an outer display binder identity in source type view fallback" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let outerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991635)
        displayTy = STForall "a" Nothing (STForall "a" Nothing (STBase "Int"))
        identityTy = STBase "Int"
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      (mkTypeView displayTy identityTy)
                        { typeViewBinderIdentities =
                            Map.singleton "a" outerIdentity
                        },
                    checkedBindingType = sameNamedTypeAbsElabTy,
                    checkedBindingTerm = sameNamedTypeAbsTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingType mainBinding of
      BTForallWithIdentity (Just outerBackendIdentity) _ Nothing (BTForallWithIdentity innerBackendIdentity _ Nothing _) -> do
        outerBackendIdentity `shouldBe` outerIdentity
        innerBackendIdentity `shouldNotBe` Just outerIdentity
      other ->
        expectationFailure ("expected distinct source fallback backend foralls, got " ++ show other)

  it "synthesizes constructor bindings for checked GADT and existential programs" $ do
    mapM_
      ( \path -> do
          checked <- requireChecked =<< readFile path
          backend <- requireRight (convertCheckedProgram checked)
          validateBackendProgram backend `shouldBe` Right ()
          let generatedConstructorArgBinders =
                [ identity
                | binding <- backendBindings backend,
                  (name, identity) <- backendExprBinderRefs (backendBindingExpr binding),
                  "$" `isPrefixOf` name,
                  "_arg" `isInfixOf` name
                ]
          generatedConstructorArgBinders `shouldSatisfy` (not . null)
          generatedConstructorArgBinders `shouldSatisfy` all (/= Nothing)
      )
      [ "test/programs/recursive-adt/recursive-gadt.mlfp",
        "test/programs/recursive-adt/recursive-existential.mlfp"
      ]

  it "instantiates parameterized constructor fields from the result type" $ do
    checked <- requireChecked parameterizedConstructorProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Some"]

  it "accepts checked source type identities for binding data hints" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case checkedDataInfos checked0 of
      dataInfo : _ -> do
        let identityDataHead = dataInfoIdentityQualifiedName dataInfo
        let checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView =
                          ( mkTypeView
                              (STCon "$stale_source_option" (STBase "Int" :| []))
                              (STCon identityDataHead (STBase "Int" :| []))
                          )
                            { typeViewHeadIdentities = Map.singleton identityDataHead (dataInfoSymbol dataInfo)
                            },
                        checkedBindingType =
                          Elab.TConWithIdentity
                            Nothing
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int") :| [])
                      }
                )
                checked0
        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
        backendDataNames backend `shouldContain` ["Main.Option"]
      [] -> expectationFailure "expected checked data info"

  it "uses checked source type head identity maps for binding data hints by identity aliases" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case checkedDataInfos checked0 of
      dataInfo : _ -> do
        let staleIdentityDataHead = "$stale_identity_option"
            stableIdentityDataHead = symbolIdentityStableName (dataInfoSymbol dataInfo)
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView =
                          ( mkTypeView
                              (STCon "$stale_source_option" (STBase "Int" :| []))
                              (STCon staleIdentityDataHead (STBase "Int" :| []))
                          )
                            { typeViewHeadIdentities = Map.singleton staleIdentityDataHead (dataInfoSymbol dataInfo)
                            },
                        checkedBindingType =
                          Elab.TConWithIdentity
                            Nothing
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int") :| [])
                      }
                )
                checked0
            checkedByStableHead =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView =
                          ( mkTypeView
                              (STCon "$stale_source_option" (STBase "Int" :| []))
                              (STCon stableIdentityDataHead (STBase "Int" :| []))
                          )
                            { typeViewHeadIdentities = Map.singleton (dataInfoIdentityQualifiedName dataInfo) (dataInfoSymbol dataInfo)
                            },
                        checkedBindingType =
                          Elab.TConWithIdentity
                            Nothing
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int") :| [])
                      }
                )
                checked0
        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
        backendDataNames backend `shouldContain` ["Main.Option"]
        stableBackend <- requireRight (convertCheckedProgram checkedByStableHead)
        validateBackendProgram stableBackend `shouldBe` Right ()
        backendDataNames stableBackend `shouldContain` ["Main.Option"]
      [] -> expectationFailure "expected checked data info"

  it "uses checked source type display head identity maps for binding data hints when identity names are stale" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case checkedDataInfos checked0 of
      dataInfo : _ -> do
        let displayDataHead = symbolDefiningName (dataInfoSymbol dataInfo)
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView =
                          ( mkTypeView
                              (STCon displayDataHead (STBase "Int" :| []))
                              (STCon "$stale_identity_option" (STBase "Int" :| []))
                          )
                            { typeViewHeadIdentities = Map.singleton displayDataHead (dataInfoSymbol dataInfo)
                            },
                        checkedBindingType =
                          Elab.TConWithIdentity
                            Nothing
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int") :| [])
                      }
                )
                checked0
        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
        backendDataNames backend `shouldContain` ["Main.Option"]
      [] -> expectationFailure "expected checked data info"

  it "does not recover source data hints through names when head identity metadata misses" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case checkedDataInfos checked0 of
      dataInfo : _ -> do
        let staleIdentityDataHead = dataInfoIdentityQualifiedName dataInfo
            fakeOptionIdentity =
              symbolIdentityFromParts (UniqueIdentity 991420) SymbolType "Other" "Option" Nothing
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView =
                          ( mkTypeView
                              (STCon "$stale_source_option" (STBase "Int" :| []))
                              (STCon staleIdentityDataHead (STBase "Int" :| []))
                          )
                            { typeViewHeadIdentities = Map.singleton staleIdentityDataHead fakeOptionIdentity
                            },
                        checkedBindingType =
                          Elab.TConWithIdentity
                            Nothing
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int") :| [])
                      }
                )
                checked0
        convertCheckedProgram checked `shouldSatisfy` isLeft
      [] -> expectationFailure "expected checked data info"

  it "does not recover source data hints from identity names without head metadata" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case checkedDataInfos checked0 of
      dataInfo : _ -> do
        let identityDataHead = dataInfoIdentityQualifiedName dataInfo
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView =
                          mkTypeView
                            (STCon "$stale_source_option" (STBase "Int" :| []))
                            (STCon identityDataHead (STBase "Int" :| [])),
                        checkedBindingType =
                          Elab.TConWithIdentity
                            Nothing
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int") :| [])
                      }
                )
                checked0
        convertCheckedProgram checked `shouldSatisfy` isLeft
      [] -> expectationFailure "expected checked data info"

  it "converts constructor metadata by identity type when display type names are stale" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    originalBackend <- requireRight (convertCheckedProgram checked0)
    originalConstructor <- requireConstructor "Main__Some" originalBackend
    let staleCtorType =
          STArrow (STVar "a") (STCon "$stale_option" (STVar "a" :| []))
        checked =
          withConstructorDisplayType "Main__Some" staleCtorType checked0
    backend <- requireRight (convertCheckedProgram checked)
    validateBackendProgram backend `shouldBe` Right ()
    constructor <- requireConstructor "Main__Some" backend
    backendConstructorResult constructor `shouldBe` backendConstructorResult originalConstructor

  it "records constructor type head identities while checking local data declarations" $ do
    checked <- requireChecked parameterizedConstructorProgram
    case find ((== "Main.Option") . dataInfoIdentityQualifiedName) (checkedDataInfos checked) of
      Just dataInfo ->
        case find ((== "Main__Some") . ctorRuntimeName) (dataConstructors dataInfo) of
          Just constructorInfo ->
            dataInfoSymbol dataInfo
              `shouldSatisfy` (`elem` Map.elems (typeViewHeadIdentities (ctorTypeView constructorInfo)))
          Nothing ->
            expectationFailure "expected Main__Some constructor"
      Nothing ->
        expectationFailure "expected Main.Option data info"

  it "converts constructor metadata by head identity maps when identity names are stale" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case find ((== "Main.Option") . dataInfoIdentityQualifiedName) (checkedDataInfos checked0) of
      Just dataInfo -> do
        let staleDisplayHead = "$stale_display_option"
            staleIdentityHead = "$stale_identity_option"
            staleCtorType =
              ( mkTypeView
                  (STArrow (STVar "a") (STCon staleDisplayHead (STVar "a" :| [])))
                  (STArrow (STVar "a") (STCon staleIdentityHead (STVar "a" :| [])))
              )
                { typeViewHeadIdentities = Map.singleton staleIdentityHead (dataInfoSymbol dataInfo)
                }
            checked =
              withConstructorTypeView "Main__Some" staleCtorType checked0
        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
        constructor <- requireConstructor "Main__Some" backend
        case backendConstructorResult constructor of
          BTConWithIdentity (Just identity) (BaseTy "Main.Option") (_ :| []) ->
            identity `shouldBe` dataInfoSymbol dataInfo
          other ->
            expectationFailure ("expected identity-bearing Main.Option result, got " ++ show other)
      Nothing ->
        expectationFailure "expected Main.Option data info"

  it "promotes constructor field local builtin names before checking metadata" $ do
    checked0 <- requireChecked constructorFieldLetProgram
    let nameOnlyIntTy = Elab.TBaseWithIdentity Nothing (BaseTy "Int")
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      rewriteFirstLetBindingType nameOnlyIntTy (checkedBindingTerm binding)
                  }
            )
            checked0

    backend <- requireRight (convertCheckedProgram checked)
    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingExpr mainBinding of
      BackendLetWithIdentity {backendLetType = ty} ->
        ty `shouldBe` intTy
      other ->
        expectationFailure ("expected backend let, got " ++ show other)

  it "recovers case data from local scrutinee types by data identity" $ do
    checked0 <- requireChecked identityAliasLetCaseProgram
    case checkedDataInfos checked0 of
      dataInfo : _ -> do
        let stableTy = Elab.TBaseWithIdentity (Just (dataInfoSymbol dataInfo)) (BaseTy "$stale_scrutinee_name")
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingTerm =
                          rewriteFirstLetBindingType stableTy (checkedBindingTerm binding)
                      }
                )
                checked0
        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
        mainBinding <- requireBinding (backendProgramMain backend) backend
        case findBackendCase (backendBindingExpr mainBinding) of
          Just BackendCase {backendScrutinee = scrutinee} -> do
            show (backendExprType scrutinee) `shouldNotSatisfy` isInfixOf "$stale_scrutinee_name"
            show (backendExprType scrutinee) `shouldSatisfy` isInfixOf "Main.Flag"
          other -> expectationFailure ("expected backend case, got " ++ show other)
      [] -> expectationFailure "expected checked data info"

  it "does not keep stable structural binder identity text as data identity" $ do
    checked0 <- requireChecked identityAliasLambdaCaseProgram
    case checkedDataInfos checked0 of
      dataInfo : _ -> do
        let stableSelfName = symbolIdentityStableName (dataInfoSymbol dataInfo) ++ "_self"
            stableStructuralTy =
              testTMu
                stableSelfName
                ( testTForall
                    "$T_result"
                    Nothing
                    (Elab.TArrow (testTVar "$T_result") (testTVar "$T_result"))
                )
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingType =
                          replaceFunctionDomain stableStructuralTy (checkedBindingType binding),
                        checkedBindingTerm =
                          rewriteFirstLamBindingType stableStructuralTy (checkedBindingTerm binding)
                      }
                )
                checked0
        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
        mainBinding <- requireBinding (backendProgramMain backend) backend
        case findBackendCase (backendBindingExpr mainBinding) of
          Just BackendCase {backendScrutinee = scrutinee} -> do
            show (backendExprType scrutinee) `shouldNotSatisfy` isInfixOf stableSelfName
            show (backendExprType scrutinee) `shouldSatisfy` isInfixOf "Main.Flag"
          other -> expectationFailure ("expected backend case, got " ++ show other)
      [] -> expectationFailure "expected checked data info"

  it "preserves resolved constructor identities in backend metadata" $ do
    checked <- requireChecked parameterizedConstructorProgram
    backend <- requireRight (convertCheckedProgram checked)
    let checkedConstructors =
          [ (ctorRuntimeName ctor, ctorInfoSymbol ctor)
          | dataInfo <- checkedDataInfos checked,
            ctor <- dataConstructors dataInfo
          ]
    mapM_
      ( \(name, symbol) -> do
          constructor <- requireConstructor name backend
          backendConstructorIdentity constructor `shouldBe` Just symbol
      )
      checkedConstructors

  it "preserves resolved data identities in backend metadata" $ do
    checked <- requireChecked parameterizedConstructorProgram
    backend <- requireRight (convertCheckedProgram checked)
    let checkedData =
          [ (dataInfoIdentityQualifiedName dataInfo, dataInfoSymbol dataInfo)
          | dataInfo <- checkedDataInfos checked
          ]
    mapM_
      ( \(name, symbol) -> do
          dataDecl <- requireBackendData name backend
          backendDataIdentity dataDecl `shouldBe` Just symbol
      )
      checkedData

  it "preserves resolved constructor identities on backend constructor applications" $ do
    checked <- requireChecked parameterizedConstructorProgram
    backend <- requireRight (convertCheckedProgram checked)
    someConstructor <- requireCheckedConstructor "Main__Some" checked

    mainBinding <- requireBinding (backendProgramMain backend) backend
    lookup "Main__Some" (collectConstructIdentities (backendBindingExpr mainBinding))
      `shouldBe` Just (Just (ctorInfoSymbol someConstructor))

  it "preserves resolved constructor identities on backend case patterns" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)
    succConstructor <- requireCheckedConstructor "Main__Succ" checked

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case findBackendCase (backendBindingExpr mainBinding) of
      Just BackendCase {backendAlternatives = alternatives} ->
        lookup "Main__Succ" (collectPatternIdentities alternatives)
          `shouldBe` Just (Just (ctorInfoSymbol succConstructor))
      Just other -> expectationFailure ("expected backend case, got " ++ show other)
      Nothing -> expectationFailure "expected backend case"

  it "validates and lowers backend constructor applications by resolved identity when node names are stale" $ do
    checked <- requireChecked parameterizedConstructorProgram
    backend <- requireRight (convertCheckedProgram checked)
    let staleBackend =
          mapBackendMainBinding
            ( \binding ->
                binding
                  { backendBindingExpr =
                      renameBackendConstructorReferences True False (== "Main__Some") "$stale_some_node" (backendBindingExpr binding)
                  }
            )
            backend

    validateBackendProgram staleBackend `shouldBe` Right ()
    _ <- requireRight (Lower.lowerBackendProgram staleBackend)
    pure ()

  it "validates and lowers backend case patterns by resolved identity when pattern names are stale" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)
    let staleBackend =
          mapBackendMainBinding
            ( \binding ->
                binding
                  { backendBindingExpr =
                      renameBackendConstructorReferences False True (== "Main__Succ") "$stale_succ_pattern" (backendBindingExpr binding)
                  }
            )
            backend

    validateBackendProgram staleBackend `shouldBe` Right ()
    _ <- requireRight (Lower.lowerBackendProgram staleBackend)
    pure ()

  it "validates and lowers backend global variables by resolved identity when node names are stale" $ do
    checked <- requireChecked simpleFunctionProgram
    backend <- requireRight (convertCheckedProgram checked)
    let staleBackend =
          mapBackendMainBinding
            ( \binding ->
                binding
                  { backendBindingExpr =
                      renameBackendVarReferences (== "Main__id") "$stale_id_node" (backendBindingExpr binding)
                  }
            )
            backend

    validateBackendProgram staleBackend `shouldBe` Right ()
    _ <- requireRight (Lower.lowerBackendProgram staleBackend)
    pure ()

  it "looks up direct constructor applications by resolved identity instead of constructor runtime name" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    let checked =
          renameCheckedConstructorRuntimeNamesWhere
            (== "Main__Some")
            "$stale_some_backend_name"
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["$stale_some_backend_name"]

  it "recovers higher-kinded structural constructors as backend constructors" $ do
    checked <- requireChecked higherKindedConstructorProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    let constructNames = collectConstructNames (backendBindingExpr mainBinding)
    constructNames `shouldContain` ["Main__Wrap", "Main__Box"]
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase

  it "recovers structural constructors by resolved local identity when occurrence names are stale" $ do
    checked0 <- requireChecked higherKindedConstructorProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      staleLocalOccurrenceRuntimes "$stale_local_occurrence" (checkedBindingTerm binding)
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Wrap", "Main__Box"]

  it "recovers hidden-owner value-only constructor imports" $ do
    checked <- requireChecked hiddenOwnerConstructorImportProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Core__NothingF"]

  it "rejects stale structural constructor head type instantiations" $ do
    checked0 <- requireChecked hiddenOwnerConstructorImportProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      addStructuralConstructorHeadInstantiation
                        boolElabTy
                        (checkedBindingTerm binding)
                  }
            )
            checked0

    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) ->
        message `shouldSatisfy` isInfixOf "constructor"
      Left err ->
        expectationFailure ("expected structural constructor type application mismatch, got " ++ show err)
      Right backend ->
        expectationFailure ("expected structural constructor type application rejection, got " ++ show backend)

  it "preserves constructor type applications when checking constructor fields" $ do
    checked <- requireChecked constructorForallApplicationProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Pack"]

  it "maps constructor type applications in backend data parameter order" $ do
    checked0 <- requireChecked dataParameterOrderConstructorProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding {checkedBindingTerm = dataParameterOrderConstructorTerm checked0}
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Mk"]

  it "preserves bounded constructor foralls in backend metadata" $ do
    checked <- requireChecked boundedConstructorForallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    constructor <- requireConstructor "Main__Pack" backend
    map backendTypeBinderName (backendConstructorForalls constructor) `shouldBe` ["a"]
    map backendTypeBinderBound (backendConstructorForalls constructor)
      `shouldBe` [Just (BTBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int"))]
    map backendTypeBinderIdentity (backendConstructorForalls constructor)
      `shouldSatisfy` all (/= Nothing)
    case backendConstructorForalls constructor of
      [BackendTypeBinderWithIdentity (Just binderIdentity) "a" (Just boundTy)] -> do
        boundTy `shouldBe` BTBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int")
        backendConstructorFields constructor `shouldBe` [BTVarWithIdentity (Just binderIdentity) "a"]
      other ->
        expectationFailure ("expected one identity-bearing constructor forall, got " ++ show other)

    let corruptedExpr =
              BackendConstructWithIdentity
                { backendExprType = backendConstructorResult constructor,
                  backendConstructIdentity = backendConstructorIdentity constructor,
                  backendConstructName = backendConstructorName constructor,
                  backendConstructArgs = [BackendLit boolTy (LBool True)]
                }
        corruptedBackend =
          mapBackendMainBinding
            ( \binding ->
                binding
                  { backendBindingExpr = corruptedExpr,
                    backendBindingType = backendConstructorResult constructor
                  }
            )
            backend

    validateBackendProgram corruptedBackend
      `shouldBe` Left
        ( BackendConstructorArgumentMismatch
            "Main__Pack"
            0
            (BTBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int"))
            boolTy
        )

  it "matches bounded constructor foralls against type variables with equivalent bounds" $ do
    checked0 <- requireChecked boundedConstructorForallProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = boundedWrapElabTy (checkedBindingType binding),
                    checkedBindingTerm = boundedWrapTerm checked0
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding
      `shouldSatisfy` containsConstructArgTypeVar "Main__Pack" "b"

  it "keeps same-spelled type env bounds under canonical backend names" $ do
    checked0 <- requireChecked boundedConstructorForallProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = mkTypeView sameNamedBoundedWrapSourceTy sameNamedBoundedWrapSourceTy,
                    checkedBindingType = sameNamedBoundedWrapElabTy (checkedBindingType binding),
                    checkedBindingTerm = sameNamedBoundedWrapTerm checked0
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingType mainBinding of
      BTForall "a" Nothing (BTForall "a1" (Just boundTy) (BTArrow (BTVar "a1") _)) ->
        boundTy `shouldBe` intTy
      other ->
        expectationFailure ("expected canonical same-spelled bounded foralls, got " ++ show other)
    case backendBindingExpr mainBinding of
      BackendTyAbsWithIdentity
        { backendTyParamName = "a",
          backendTyAbsBody =
            BackendTyAbsWithIdentity
              { backendTyParamName = "a1",
                backendTyParamBound = Just boundTy,
                backendTyAbsBody = BackendLamWithIdentity {backendParamType = BTVar "a1", backendBody = BackendConstructWithIdentity {backendConstructName = "Main__Pack"}}
              }
        } ->
          boundTy `shouldBe` intTy
      other ->
        expectationFailure ("expected canonical same-spelled bounded type abstraction, got " ++ show other)

  it "matches bounded constructor foralls through dependent type-variable bounds" $ do
    checked0 <- requireChecked dependentBoundedConstructorForallProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = dependentBoundedWrapElabTy (checkedBindingType binding),
                    checkedBindingTerm = dependentBoundedWrapTerm checked0
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding
      `shouldSatisfy` containsConstructArgTypeVar "Main__Pack" "b"

  it "converts nested constructor arguments under expected constructor field types" $ do
    checked <- requireChecked =<< readFile "test/programs/recursive-adt/typeclass-integration.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "converts hidden Eq evidence for constrained helpers" $ do
    checked <- requireChecked hiddenEqEvidenceProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    sameBinding <- requireBinding "Main__same" backend
    backendBindingExpr sameBinding `shouldNotSatisfy` containsBackendClosureCall

  it "converts constrained parameterized Eq evidence without ambiguous ADT recovery" $ do
    checked <- requireChecked parameterizedEqEvidenceProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    case [dataInfoSymbol info | info <- checkedDataInfos checked, not (null (dataTypeParams info))] of
      optionIdentity : _ ->
        case parameterizedInstanceMethodRuntimeNames optionIdentity checked of
          methodRuntimeName : _ ->
            map backendBindingName (backendBindings backend)
              `shouldSatisfy` elem methodRuntimeName
          [] -> expectationFailure "expected parameterized instance method info"
      [] -> expectationFailure "expected parameterized data info"

  it "lifts recursive parameterized deriving Eq helpers with captured evidence" $ do
    checked <- requireChecked recursiveListDerivingEqProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    map backendBindingName (backendBindings backend) `shouldSatisfy` any (isInfixOf "$letrec$")

  it "closure-converts top-level recursive higher-order function parameters" $ do
    checked <- requireChecked topLevelRecursiveHigherOrderProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    loopBinding <- requireBinding "Main__loop" backend
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr loopBinding `shouldSatisfy` containsBackendClosureCallFunction "f"
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure

  it "lifts closed local recursive higher-order helpers with closure-demanded arguments" $ do
    checked <- requireChecked localRecursiveHigherOrderProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType helper `shouldSatisfy` isBackendFunctionType
    backendBindingExpr helper `shouldSatisfy` containsBackendClosureCallFunction "f"
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure

  it "rejects constructor head type instantiations with no matching constructor parameter" $ do
    checked0 <- requireChecked constructorForallApplicationProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      addStaleConstructorHeadInstantiation
                        "Main__Pack"
                        boolElabTy
                        (checkedBindingTerm binding)
                  }
            )
            checked0

    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) ->
        message `shouldSatisfy` isInfixOf "constructor type application arity mismatch"
      Left err ->
        expectationFailure ("expected constructor type application arity mismatch, got " ++ show err)
      Right backend ->
        expectationFailure ("expected constructor type application rejection, got " ++ show backend)

  it "rejects constructor head type instantiations that conflict with the expected result" $ do
    checked0 <- requireChecked gadtResultConstructorProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      replaceConstructorHeadInstantiation
                        "Main__Box"
                        boolElabTy
                        (checkedBindingTerm binding)
                  }
            )
            checked0

    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) ->
        message `shouldSatisfy` isInfixOf "constructor"
      Left (BackendValidationFailed (BackendConstructorArgumentMismatch name _ _ _)) ->
        name `shouldBe` "Main__Box"
      Left err ->
        expectationFailure ("expected constructor type application mismatch, got " ++ show err)
      Right backend ->
        expectationFailure ("expected constructor type application rejection, got " ++ show backend)

  it "rejects constructor head type instantiations that would fall back to same-named type variables" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = mkTypeView polymorphicOptionSourceTy polymorphicOptionSourceTy,
                    checkedBindingType = polymorphicOptionElabTy,
                    checkedBindingTerm = staleSomeInPolymorphicOptionTerm checked0
                  }
            )
            checked0

    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) ->
        message `shouldSatisfy` isInfixOf "constructor result type does not match expected result"
      Left err ->
        expectationFailure ("expected constructor result type mismatch, got " ++ show err)
      Right backend ->
        expectationFailure ("expected constructor type application rejection, got " ++ show backend)

  it "accepts constructor result placeholders by identity when same-named type binders differ" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case find ((== "Main.Option") . dataInfoIdentityQualifiedName) (checkedDataInfos checked0) of
      Just dataInfo -> do
        let checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView = polymorphicOptionSourceView dataInfo identityPlaceholderExpectedRef,
                        checkedBindingType = identityPlaceholderPolymorphicOptionElabTy,
                        checkedBindingTerm = identityPlaceholderSomeTerm checked0
                      }
                )
                checked0

        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
      Nothing -> expectationFailure "expected Main.Option data info"

  it "matches repeated constructor parameters modulo alpha-equivalence" $ do
    checked0 <- requireChecked repeatedPolymorphicParameterProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = boolElabTy,
                    checkedBindingTerm = repeatedPolymorphicParameterCaseTerm checked0
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "rejects mismatched vacuous recursive constructor fallback results during conversion" $ do
    checked0 <- requireChecked vacuousRecursiveConstructorFallbackProgram
    let checked =
          withConstructorResult "Main__MkBox" (STMu "a" (STBase "Int")) $
            mapMainBinding
              ( \binding ->
                  binding {checkedBindingType = testTMu "b" boolElabTy}
              )
              checked0

    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) ->
        message `shouldSatisfy` isInfixOf "constructor result type does not match expected result"
      Left err ->
        expectationFailure ("expected constructor shape rejection, got " ++ show err)
      Right backend ->
        expectationFailure ("expected constructor shape rejection, got backend:\n" ++ show backend)

  it "rejects duplicate checked data identities before building backend context maps" $ do
    checked0 <- requireChecked duplicateDataNameProgram
    case checkedDataInfos checked0 of
      firstData : secondData : _ -> do
        let duplicateIdentity = dataInfoSymbol firstData
            checked =
              replaceDataInfoSymbol
                (dataInfoSymbol secondData)
                duplicateIdentity
                checked0
        convertCheckedProgram checked
          `shouldBe` Left (BackendValidationFailed (BackendDuplicateData (symbolIdentityStableName duplicateIdentity)))
      _ ->
        expectationFailure "expected at least two checked data infos"

  it "rejects duplicate checked constructor identities before building backend context maps" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case [ctors | dataInfo <- checkedDataInfos checked0, let ctors = dataConstructors dataInfo, length ctors >= 2] of
      (firstCtor : secondCtor : _) : _ -> do
        let duplicateIdentity = ctorInfoSymbol firstCtor
            checked =
              replaceConstructorInfoSymbol
                (ctorInfoSymbol secondCtor)
                duplicateIdentity
                checked0
        convertCheckedProgram checked
          `shouldBe` Left (BackendValidationFailed (BackendDuplicateConstructor (symbolIdentityStableName duplicateIdentity)))
      _ ->
        expectationFailure "expected data info with at least two constructors"

  it "keeps same-name data declarations module-scoped during type lowering" $ do
    checked <- requireChecked duplicateDataNameProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendDataNames backend `shouldContain` ["A.T", "B.T"]

    aBinding <- requireBinding "A__A" backend
    aConstructor <- requireConstructor "A__A" backend
    backendConstructorResult aConstructor `shouldBe` backendBindingType aBinding

  it "canonicalizes same-name data heads when module names sort after type names" $ do
    checked <- requireChecked qualifiedAliasOrderingProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendDataNames backend `shouldContain` ["Y.T", "Z.T"]

    yConstructor <- requireConstructor "Y__YValue" backend
    zConstructor <- requireConstructor "Z__ZValue" backend
    backendConstructorResult yConstructor `shouldSatisfy` (/= backendConstructorResult zConstructor)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase

  it "scopes unqualified structural owner recovery to the current module" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = mkTypeView (STBase "Main.T") (STBase "Main.T"),
                    checkedBindingType = Elab.TBase (BaseTy "Main.T"),
                    checkedBindingTerm = unqualifiedStructuralNullaryConstructorTerm
                  }
            )
            checked0

    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__T"]
    collectConstructNames (backendBindingExpr mainBinding) `shouldNotContain` ["Core__External"]

  it "recovers structural constructor owners by result type identity when display head is stale" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    case find ((== "Main.T") . dataInfoIdentityQualifiedName) (checkedDataInfos checked0) of
      Just dataInfo -> do
        let staleResultTy =
              Elab.TBaseWithIdentity
                (Just (dataInfoSymbol dataInfo))
                (BaseTy "$stale_structural_result")
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView = mkTypeView (STBase "Main.T") (STBase "Main.T"),
                        checkedBindingType = Elab.TBase (BaseTy "Main.T"),
                        checkedBindingTerm = structuralNullaryConstructorTermWithResult staleResultTy
                      }
                )
                checked0

        backend <- requireRight (convertCheckedProgram checked)

        validateBackendProgram backend `shouldBe` Right ()
        mainBinding <- requireBinding (backendProgramMain backend) backend
        collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__T"]
        collectConstructNames (backendBindingExpr mainBinding) `shouldNotContain` ["Core__External"]
      Nothing -> expectationFailure "missing Main.T data info"

  it "does not recover structural constructors by name when result self identity is not structural" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = mkTypeView (STBase "Main.T") (STBase "Main.T"),
                    checkedBindingType = Elab.TBase (BaseTy "Main.T"),
                    checkedBindingTerm = structuralNullaryConstructorTermWithResult nonStructuralSelfIdentityTElabTy
                  }
            )
            checked0
    case convertCheckedProgram checked of
      Left (BackendValidationFailed (BackendBindingTypeMismatch "Main__main" _ (BTMuWithIdentity (Just identity) name _))) -> do
        identity `shouldBe` Elab.typeBinderRefIdentity (backendFixtureTypeRef 9110 "$T_self")
        name `shouldBe` "$T_self"
      other ->
        expectationFailure ("expected non-recovered structural roll rejection, got " ++ show other)

  it "treats stale app-like instantiations on non-forall terms as no-ops" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm = Elab.ETyInst (Elab.ELit (LInt 1)) (Elab.InstApp intElabTy),
                    checkedBindingType = intElabTy
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldBe` BackendLit intTy (LInt 1)

  it "promotes closed recursive local lets to backend helper bindings" $ do
    checked <- requireChecked =<< readFile "test/programs/unified/authoritative-recursive-let.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    helper <-
      case filter (isInfixOf "$letrec$" . backendBindingName) (backendBindings backend) of
        [helper] -> pure helper
        helpers -> expectationFailure ("expected one lifted helper, got " ++ show (map backendBindingName helpers)) >> fail "helper mismatch"
    backendBindingType helper `shouldSatisfy` isBackendFunctionType
    backendBindingExpr helper `shouldSatisfy` containsBackendCase
    backendBindingExpr helper `shouldSatisfy` containsBackendVar (backendBindingName helper)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendVar (backendBindingName helper)

  it "generates unique lifted helper identities across checked bindings" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let rewrite binding =
          binding
            { checkedBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int"),
              checkedBindingType = intElabTy,
              checkedBindingTerm = recursiveIntLiftTerm
            }
        checked =
          mapBinding "Main__id" rewrite $
            mapMainBinding rewrite checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    let helperNames = filter (isInfixOf "$letrec$") (map backendBindingName (backendBindings backend))
    length helperNames `shouldBe` 2
    helperNames `shouldBe` nub helperNames

  it "seeds lifted helper identities from checked metadata identities" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let reservedUnique = UniqueIdentity 991999
        reservedDataIdentity =
          symbolIdentityFromParts reservedUnique SymbolType "Main" "Reserved" Nothing
        reservedDataInfo =
          DataInfo
            { dataInfoSymbol = reservedDataIdentity,
              dataTypeParams = [],
              dataConstructors = []
            }
        rewrite binding =
          binding
            { checkedBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int"),
              checkedBindingType = intElabTy,
              checkedBindingTerm = recursiveIntLiftTerm
            }
        checked =
          addDataInfo reservedDataInfo $
            mapMainBinding rewrite checked0
    backend <- requireRight (convertCheckedProgram checked)

    case
      [ symbolUniqueIdentity identity
      | binding <- backendBindings backend,
        "$letrec$" `isInfixOf` backendBindingName binding,
        Just identity <- [backendBindingIdentity binding]
      ]
      of
      [UniqueIdentity helperUnique] ->
        helperUnique `shouldSatisfy` (> 991999)
      helperUniques ->
        expectationFailure ("expected one lifted helper identity, got " ++ show helperUniques)

  it "seeds lifted helper identities from checked source type view metadata identities" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let reservedUnique = UniqueIdentity 2000000000
        reservedSourceIdentity = typeBinderIdentityFromUnique reservedUnique
        sourceView =
          (mkTypeView (STBase "Int") (STBase "Int"))
            { typeViewBinderIdentities = Map.singleton "reserved" reservedSourceIdentity
            }
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = sourceView,
                    checkedBindingType = intElabTy,
                    checkedBindingTerm = recursiveIntLiftTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    case
      [ symbolUniqueIdentity identity
      | binding <- backendBindings backend,
        "$letrec$" `isInfixOf` backendBindingName binding,
        Just identity <- [backendBindingIdentity binding]
      ]
      of
      [UniqueIdentity helperUnique] ->
        helperUnique `shouldSatisfy` (> 2000000000)
      helperUniques ->
        expectationFailure ("expected one lifted helper identity, got " ++ show helperUniques)

  it "seeds lifted helper identities from deferred constructor type head identities" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    ctorInfo <- requireCheckedConstructor "Main__Some" checked0
    let reservedUnique = UniqueIdentity 2000000010
        reservedHeadIdentity =
          symbolIdentityFromParts reservedUnique SymbolType "Main" "ReservedHead" Nothing
        deferredRef = deferredRefFromIdentity (UniqueIdentity 2000000000) "$deferred"
        deferredConstructor =
          DeferredConstructorCall
            { deferredConstructorRef = deferredRef,
              deferredConstructorInfo = ctorInfo,
              deferredConstructorArgCount = 0,
              deferredConstructorSourceType = STBase "Main.Option",
              deferredConstructorOccurrenceType = STBase "Main.Option",
              deferredConstructorTypeHeadIdentities = Map.singleton "ReservedHead" reservedHeadIdentity,
              deferredConstructorInstBinders = [],
              deferredConstructorInitialSubst = emptyTypeBinderSubst,
              deferredConstructorBindingMode = DeferredBindingMonomorphic
            }
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingDeferredObligations =
                      Map.insert deferredRef (DeferredConstructor deferredConstructor) (checkedBindingDeferredObligations binding),
                    checkedBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int"),
                    checkedBindingType = intElabTy,
                    checkedBindingTerm = recursiveIntLiftTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    case
      [ symbolUniqueIdentity identity
      | binding <- backendBindings backend,
        "$letrec$" `isInfixOf` backendBindingName binding,
        Just identity <- [backendBindingIdentity binding]
      ]
      of
      [UniqueIdentity helperUnique] ->
        helperUnique `shouldSatisfy` (> 2000000010)
      helperUniques ->
        expectationFailure ("expected one lifted helper identity, got " ++ show helperUniques)

  it "renames binders that would capture recursive helper evidence" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveCaptureAvoidingElabTy,
                    checkedBindingTerm = recursiveCaptureAvoidingTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    let evidenceBinders =
          filter ((== "$evidence_E") . fst) (backendExprBinderRefs (backendBindingExpr helper))
    case map snd evidenceBinders of
      [Just outerEvidence, Just shadowingEvidence] ->
        outerEvidence `shouldNotBe` shadowingEvidence
      other ->
        expectationFailure ("expected two identity-distinct evidence binders, got " ++ show other)

  it "classifies declared evidence by resolved identity instead of evidence prefix" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveCaptureAvoidingElabTy,
                    checkedBindingTerm = recursiveCaptureAvoidingTermWith "hiddenEvidence"
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    _ <- requireSingleLiftedHelper backend
    pure ()

  it "keeps renamed let binders out of their own right-hand sides" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveLetRhsRenameElabTy,
                    checkedBindingTerm = recursiveLetRhsRenameTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingExpr helper `shouldNotSatisfy` containsSelfReferentialLetRhs

  it "captures type variables used only by recursive RHS instantiations" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveTypeCaptureElabTy,
                    checkedBindingTerm = recursiveTypeCaptureTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    case backendBindingType helper of
      BTForallWithIdentity (Just typeIdentity) "a" Nothing bodyTy -> do
        bodyTy `shouldBe` unaryIntBackendTy
        backendBindingExpr helper `shouldSatisfy` containsBackendTyAppArgument (BTVarWithIdentity (Just typeIdentity) "a")
      other ->
        expectationFailure ("expected identity-bearing helper type capture, got " ++ show other)

  it "captures recursive helper type refs by identity when binders share spelling" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveSameNamedTypeCaptureElabTy,
                    checkedBindingTerm = recursiveSameNamedTypeCaptureTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingType helper
      `shouldBe` BTForallWithIdentity
        (Just (Elab.typeBinderRefIdentity sameNamedInnerTypeRef))
        "a"
        Nothing
        unaryIntBackendTy

  it "captures recursive helper term refs by identity when binders share spelling" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveSameNamedTermCaptureElabTy,
                    checkedBindingTerm = recursiveSameNamedTermCaptureTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingType helper `shouldBe` BTArrow unaryIntBackendTy unaryIntBackendTy

  it "keeps type abstraction bounds outside freshened binder scope" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveTypeBoundScopeElabTy,
                    checkedBindingTerm = recursiveTypeBoundScopeTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingType helper
      `shouldBe` BTForallWithIdentity
        (Just (Elab.typeBinderRefIdentity recursiveTypeBoundScopeOuterRef))
        "a"
        Nothing
        unaryIntBackendTy
    backendBindingExpr helper `shouldSatisfy` containsFreshenedTypeAbsWithOuterBound

  it "renames shadowing type abstraction bounds that refer to outer binders" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveNestedTypeBoundScopeElabTy,
                    checkedBindingTerm = recursiveNestedTypeBoundScopeTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingType helper
      `shouldBe` BTForallWithIdentity
        (Just (Elab.typeBinderRefIdentity recursiveNestedTypeBoundScopeOuterRef))
        "a"
        Nothing
        unaryIntBackendTy

  it "lifts recursive lets that shadow outer term binders" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveShadowedLetElabTy,
                    checkedBindingTerm = recursiveShadowedLetTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    backendBindingType helper `shouldBe` unaryIntBackendTy
    backendBindingExpr helper `shouldSatisfy` containsBackendVar (backendBindingName helper)

  it "preserves lexical type binder order when lifting recursive helper captures" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveLexicalTypeOrderElabTy,
                    checkedBindingTerm = recursiveLexicalTypeOrderTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    case backendBindingType helper of
      BTForallWithIdentity (Just {}) "z" Nothing (BTForallWithIdentity (Just {}) "a" Nothing bodyTy) ->
        bodyTy `shouldBe` unaryIntBackendTy
      other ->
        expectationFailure ("expected identity-bearing helper captures in lexical order, got " ++ show other)

  it "rejects recursive local functions that capture lexical values" $ do
    checked <- requireChecked recursiveLetCaptureProgram

    case convertCheckedProgram checked of
      Left (BackendUnsupportedRecursiveLet detail) ->
        detail `shouldSatisfy` isInfixOf "captures lexical bindings"
      other ->
        expectationFailure ("expected recursive-let capture rejection, got " ++ show other)

  it "rejects nested recursive local functions that capture outer recursive functions" $ do
    checked <- requireChecked nestedRecursiveLetCaptureProgram

    case convertCheckedProgram checked of
      Left (BackendUnsupportedRecursiveLet detail) -> do
        detail `shouldSatisfy` isInfixOf "captures lexical bindings"
        detail `shouldSatisfy` isInfixOf "peel"
      other ->
        expectationFailure ("expected nested recursive-let capture rejection, got " ++ show other)

  it "converts source type applications into backend applied type variables" $ do
    let intWithIdentity = BTBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int")
    convertSourceType unsupportedVariableHeadType
      `shouldBe` Right (BTVarApp "f" (intWithIdentity :| []))

  it "keeps stable source type binder names metadata-free in backend source conversion" $ do
    let stableName = "$typevar#991604"

    convertSourceType (STForall stableName Nothing (STVarApp stableName (STVar stableName :| [])))
      `shouldBe` Right
        ( BTForallWithIdentity
            Nothing
            stableName
            Nothing
            (BTVarAppWithIdentity Nothing stableName (BTVarWithIdentity Nothing stableName :| []))
        )

  it "preserves builtin source type identities in backend source conversion" $ do
    convertSourceType (STBase "Int")
      `shouldBe` Right (BTBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int"))

  it "closure-converts a returned local function value" $ do
    checked <- requireChecked returnedClosureProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure

  it "seeds fresh closure entry names from the identity generator" $ do
    checked <- requireChecked returnedClosureProgram
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case collectBackendClosureEntryRefs (backendBindingExpr mainBinding) of
      (Just entryIdentity, entryName) : _ -> do
        entryName `shouldSatisfy` isPrefixOf "__mlfp_closure$Main__main$"
        closureNameUniqueSuffix entryName `shouldBe` Just (uniqueIdentityValue entryIdentity)
      (Nothing, _) : _ ->
        expectationFailure "expected generated closure entry identity"
      [] ->
        expectationFailure "expected converted closure entry"

  it "closure-converts local function aliases that cross let boundaries" $ do
    checked <- requireChecked closureAliasCallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCall

  it "closure-converts captured lambdas called through let aliases" $ do
    checked <- requireChecked capturedClosureCallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCapture "captured"
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCall

  it "closure-converts closure-valued function parameters at call sites" $ do
    checked <- requireChecked functionParameterClosureCallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    useBinding <- requireBinding "Main__use" backend
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr useBinding `shouldSatisfy` containsBackendClosureCall
    backendBindingExpr useBinding `shouldNotSatisfy` containsBackendApp
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure

  it "closure-converts calls to closure-valued top-level bindings" $ do
    checked <- requireChecked topLevelClosureCallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    makerBinding <- requireBinding "Main__maker" backend
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr makerBinding `shouldSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCall
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendApp

  it "uses resolved identity for top-level closure heads with stale runtime spelling" $ do
    checked0 <- requireChecked topLevelClosureCallProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      staleTopLevelOccurrenceRuntime
                        "Main__maker"
                        "$stale_maker_runtime"
                        (checkedBindingTerm binding)
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCall
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendApp

  it "looks up top-level closure demands by resolved identity instead of runtime spelling" $ do
    checked0 <- requireChecked localDirectAliasPartialApplicationBaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      staleTopLevelOccurrenceRuntime
                        "Main__apply"
                        "$stale_apply_runtime"
                        (checkedBindingTerm binding)
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure

  it "closure-converts calls to type-abstracted closure-valued top-level bindings" $ do
    checked <- requireChecked polymorphicTopLevelClosureCallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    makerBinding <- requireBinding "Main__maker" backend
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr makerBinding `shouldSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCall
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendApp

  it "closure-converts closure-valued function parameters through nested let aliases" $ do
    checked <- requireChecked functionParameterNestedClosureAliasCallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    useBinding <- requireBinding "Main__use" backend
    backendBindingExpr useBinding `shouldSatisfy` containsBackendClosureCall
    backendBindingExpr useBinding `shouldNotSatisfy` containsBackendApp

  it "uses resolved local alias identity for closure-demand heads with stale runtime spelling" $ do
    checked0 <- requireChecked functionParameterNestedClosureAliasCallProgram
    let fBinder = resolvedLocal "$f#0" "f" unaryIntElabTy
        fOccurrence = resolvedLocal "$f#0" "stale-f" unaryIntElabTy
        gBinder = resolvedLocal "$g#0" "g" unaryIntElabTy
        gOccurrence = resolvedLocal "$g#0" "stale-g" unaryIntElabTy
        checked =
          mapBinding
            "Main__use"
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      Elab.ELam fBinder $
                        Elab.ELet
                          gBinder
                          (schemeFromType unaryIntElabTy)
                          (Elab.EVarNode fOccurrence)
                          (Elab.EApp (Elab.EVarNode gOccurrence) (Elab.ELit (LInt 1)))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    useBinding <- requireBinding "Main__use" backend
    backendBindingExpr useBinding `shouldSatisfy` containsBackendClosureCall
    backendBindingExpr useBinding `shouldNotSatisfy` containsBackendApp

  it "unfolds resolved let aliases by identity for closure-demand heads" $ do
    checked0 <- requireChecked functionParameterNestedClosureAliasCallProgram
    let fBinder = resolvedLocal "$f#0" "f-runtime" unaryIntElabTy
        fOccurrence = resolvedLocal "$f#0" "f-use-runtime" unaryIntElabTy
        gBinder = resolvedLocal "$g#0" "g-runtime" unaryIntElabTy
        gOccurrence = resolvedLocal "$g#0" "g-use-runtime" unaryIntElabTy
        hBinder = resolvedLocal "$h#0" "h-runtime" unaryIntElabTy
        hOccurrence = resolvedLocal "$h#0" "h-use-runtime" unaryIntElabTy
        checked =
          mapBinding
            "Main__use"
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      Elab.ELam fBinder $
                        Elab.ELet
                          gBinder
                          (schemeFromType unaryIntElabTy)
                          ( Elab.ELet
                              hBinder
                              (schemeFromType unaryIntElabTy)
                              (Elab.EVarNode fOccurrence)
                              (Elab.EVarNode hOccurrence)
                          )
                          (Elab.EApp (Elab.EVarNode gOccurrence) (Elab.ELit (LInt 1)))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    useBinding <- requireBinding "Main__use" backend
    backendBindingExpr useBinding `shouldSatisfy` containsBackendClosureCall
    backendBindingExpr useBinding `shouldNotSatisfy` containsBackendApp

  it "clears shadowed closure locals when classifying let RHS values" $ do
    checked <- requireChecked shadowedClosureLocalProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    useBinding <- requireBinding "Main__use" backend
    backendBindingExpr useBinding `shouldSatisfy` containsBackendApp
    backendBindingExpr useBinding `shouldNotSatisfy` containsBackendClosureCall

  it "does not classify same-named lambda heads as outer closure demands" $ do
    checked <- requireChecked shadowedFunctionHeadDemandProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendApp
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendClosure

  it "classifies function-valued case pattern fields as closure locals" $ do
    checked <- requireChecked shadowedCaseClosureLocalProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    useBinding <- requireBinding "Main__use" backend
    backendBindingExpr useBinding `shouldSatisfy` containsBackendCase
    backendBindingExpr useBinding `shouldSatisfy` containsBackendClosure
    backendBindingExpr useBinding `shouldSatisfy` containsBackendClosureCall
    backendBindingExpr useBinding `shouldNotSatisfy` containsBackendCaseHeadedApp

  it "lets function-valued case fields shadow same-named closure globals" $ do
    checked <- requireChecked shadowedGlobalClosureHeadProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCall

  it "stores and projects closure-valued constructor fields as closure values" $ do
    checked <- requireChecked closureValuedConstructorFieldProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCapture "captured"
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCall

  it "collects closure parameters through lets before returned lambdas" $ do
    checked <- requireChecked returnedLetLambdaClosureProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    closureParamCounts (backendBindingExpr mainBinding) `shouldSatisfy` elem 2

  it "alpha-renames returned lambda parameters hoisted across shadowing lets" $ do
    checked0 <- requireChecked returnedLetLambdaClosureProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding {checkedBindingTerm = returnedLetLambdaShadowingXmlfTerm}
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsAlphaRenamedShadowingClosure

  it "keeps direct first-order local calls on the direct application path" $ do
    checked <- requireChecked directLocalCallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendApp
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendClosureCall

  it "captures local direct callees when packaging partial applications" $ do
    checked0 <- requireChecked localDirectAliasPartialApplicationBaseProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding {checkedBindingTerm = localDirectAliasPartialApplicationTerm checked0}
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCapture "keep"

  it "captures local direct callees by identity when their reference spelling matches a global" $ do
    checked0 <- requireChecked localDirectAliasPartialApplicationBaseProgram
    let keepBinder = generatedResolvedLocal 0 "Main__keepLeft" "stale-local-keep" binaryIntElabTy
        keepOccurrence = generatedResolvedLocal 0 "Main__keepLeft" "different-local-keep" binaryIntElabTy
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      Elab.ELet
                        keepBinder
                        (schemeFromType binaryIntElabTy)
                        (mkTestLocalLam "x" intElabTy (mkTestLocalLam "y" intElabTy (mkTestDeferredVar "x")))
                        ( Elab.EApp
                            (resolvedBindingTerm checked0 "Main__apply")
                            (Elab.EApp (Elab.EVarNode keepOccurrence) (Elab.ELit (LInt 1)))
                        )
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosureCapture "Main__keepLeft"

  it "does not capture same-named local callees with different identities" $ do
    checked0 <- requireChecked localDirectAliasPartialApplicationBaseProgram
    let keepBinder = generatedResolvedLocal 0 "keep" "keep" binaryIntElabTy
        staleKeepOccurrence = generatedResolvedLocal 1 "keep" "keep" binaryIntElabTy
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      Elab.ELet
                        keepBinder
                        (schemeFromType binaryIntElabTy)
                        (resolvedBindingTerm checked0 "Main__keepLeft")
                        ( Elab.EApp
                            (resolvedBindingTerm checked0 "Main__apply")
                            (Elab.EApp (Elab.EVarNode staleKeepOccurrence) (Elab.ELit (LInt 1)))
                        )
                  }
            )
            checked0
    case convertCheckedProgram checked of
      Left (BackendTypeCheckFailed _ err) ->
        show err `shouldSatisfy` isInfixOf "TCUnboundVar"
      other ->
        expectationFailure ("expected stale local identity rejection, got " ++ show other)

  it "beta-reduces resolved identity lambdas before packaging partial applications" $ do
    checked0 <- requireChecked localDirectAliasPartialApplicationBaseProgram
    let binder = resolvedLocal "$f#0" "runtime-f" unaryIntElabTy
        occurrence = resolvedLocal "$f#0" "different-runtime" unaryIntElabTy
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = unaryIntElabTy,
                    checkedBindingSourceTypeView = mkTypeView (STArrow (STBase "Int") (STBase "Int")) (STArrow (STBase "Int") (STBase "Int")),
                    checkedBindingTerm =
                      Elab.EApp
                        (Elab.ELam binder (Elab.EVarNode occurrence))
                        (Elab.EApp (resolvedBindingTerm checked0 "Main__keepLeft") (Elab.ELit (LInt 1)))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendClosure

  it "shares closure entry names across lifted recursive helper conversion" $ do
    checked0 <- requireChecked liftedRecursiveHelpersClosureNameProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = intElabTy,
                    checkedBindingSourceTypeView = mkTypeView (STBase "Int") (STBase "Int"),
                    checkedBindingTerm = liftedRecursiveHelpersClosureNameTerm checked0
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    let helperBindings = filter (isInfixOf "$letrec$" . backendBindingName) (backendBindings backend)
        closureEntryNames = concatMap (collectBackendClosureEntryNames . backendBindingExpr) helperBindings
    length helperBindings `shouldBe` 2
    length closureEntryNames `shouldBe` 2
    closureEntryNames `shouldBe` nub closureEntryNames

simpleFunctionProgram :: String
simpleFunctionProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : Int -> Int = λx x;",
      "  def main : Int = id 1;",
      "}"
    ]

recursiveLetCaptureProgram :: String
recursiveLetCaptureProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Nat -> Nat = λ(seed : Nat)",
      "    let peel : Nat -> Nat = λ(n : Nat) case n of {",
      "      Zero -> seed;",
      "      Succ inner -> peel inner",
      "    } in peel seed;",
      "}"
    ]

nestedRecursiveLetCaptureProgram :: String
nestedRecursiveLetCaptureProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Nat -> Nat =",
      "    let peel : Nat -> Nat = λ(n : Nat)",
      "      let bounce : Nat -> Nat = λ(m : Nat) case m of {",
      "        Zero -> peel Zero;",
      "        Succ inner -> bounce inner",
      "      } in case n of {",
      "        Zero -> Zero;",
      "        Succ inner -> bounce inner",
      "      }",
      "    in peel;",
      "}"
    ]

adtCaseProgram :: String
adtCaseProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Nat = case Succ Zero of {",
      "    Zero -> Zero;",
      "    Succ n -> n",
      "  };",
      "}"
    ]

intCaseProgram :: String
intCaseProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Int = case Succ Zero of {",
      "    Zero -> 0;",
      "    Succ n -> 1",
      "  };",
      "}"
    ]

identityAliasLetCaseProgram :: String
identityAliasLetCaseProgram =
  unlines
    [ "module Main export (Flag(..), main) {",
      "  data Flag =",
      "      Off : Flag",
      "    | On : Flag;",
      "",
      "  def main : Flag =",
      "    let scrutinee : Flag = On in case scrutinee of {",
      "      Off -> Off;",
      "      On -> On",
      "    };",
      "}"
    ]

identityAliasLambdaCaseProgram :: String
identityAliasLambdaCaseProgram =
  unlines
    [ "module Main export (Flag(..), main) {",
      "  data Flag =",
      "      Off : Flag",
      "    | On : Flag;",
      "",
      "  def main : Flag -> Flag = λ(scrutinee : Flag) case scrutinee of {",
      "    Off -> Off;",
      "    On -> On",
      "  };",
      "}"
    ]

functionCaseProgram :: String
functionCaseProgram =
  unlines
    [ "module Main export (Box(..), main) {",
      "  data Box =",
      "      Box : Int -> Box;",
      "",
      "  def main : Int -> Int = case Box 0 of {",
      "    Box n -> λx x",
      "  };",
      "}"
    ]

constructorFieldLetProgram :: String
constructorFieldLetProgram =
  unlines
    [ "module Main export (Box(..), main) {",
      "  data Box =",
      "      Box : Int -> Box;",
      "",
      "  def main : Box = let x : Int = 1 in Box x;",
      "}"
    ]

parameterizedConstructorProgram :: String
parameterizedConstructorProgram =
  unlines
    [ "module Main export (Option(..), main) {",
      "  data Option a =",
      "      None : Option a",
      "    | Some : a -> Option a;",
      "",
      "  def main : Option Int = Some 1;",
      "}"
    ]

gadtResultConstructorProgram :: String
gadtResultConstructorProgram =
  unlines
    [ "module Main export (Box(..), main) {",
      "  data Box a =",
      "      Box : ∀ (b ⩾ Int). b -> Box b;",
      "",
      "  def main : Box Int = Box 1;",
      "}"
    ]

higherKindedConstructorProgram :: String
higherKindedConstructorProgram =
  unlines
    [ "module Main export (Box(..), Wrap(..), main) {",
      "  data Box a =",
      "      Box : a -> Box a;",
      "",
      "  data Wrap (f :: * -> *) a =",
      "      Wrap : f a -> Wrap f a;",
      "",
      "  def main : Bool = case Wrap (Box false) of {",
      "    Wrap box -> true",
      "  };",
      "}"
    ]

hiddenOwnerConstructorImportProgram :: String
hiddenOwnerConstructorImportProgram =
  unlines
    [ "module Core export (Box(..), NothingF, accept) {",
      "  data Box a =",
      "      Box : a -> Box a;",
      "",
      "  data MaybeF (f :: * -> *) a =",
      "      NothingF : MaybeF f a",
      "    | JustF : f a -> MaybeF f a;",
      "",
      "  def accept : MaybeF Box Bool -> Bool = λvalue case value of {",
      "    NothingF -> true;",
      "    JustF box -> true",
      "  };",
      "}",
      "",
      "module Main export (main) {",
      "  import Core exposing (NothingF, accept);",
      "  def id : ∀ a. a -> a = λx x;",
      "  def main : Bool = accept (id NothingF);",
      "}"
    ]

hiddenEqEvidenceProgram :: String
hiddenEqEvidenceProgram =
  unlines
    [ "module Main export (Eq, Nat(..), eq, same, main) {",
      "  class Eq a {",
      "    eq : a -> a -> Bool;",
      "  }",
      "",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat",
      "    deriving Eq;",
      "",
      "  def same : Eq a => a -> a -> Bool = λx λy eq x y;",
      "  def main : Bool = same Zero Zero;",
      "}"
    ]

parameterizedEqEvidenceProgram :: String
parameterizedEqEvidenceProgram =
  unlines
    [ "module Main export (Eq, Nat(..), Option(..), eq, main) {",
      "  class Eq a {",
      "    eq : a -> a -> Bool;",
      "  }",
      "",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat",
      "    deriving Eq;",
      "",
      "  data Option a =",
      "      None : Option a",
      "    | Some : a -> Option a;",
      "",
      "  instance Eq a => Eq (Option a) {",
      "    eq = λleft λright case left of {",
      "      None -> case right of {",
      "        None -> true;",
      "        Some _ -> false",
      "      };",
      "      Some l -> case right of {",
      "        None -> false;",
      "        Some r -> eq l r",
      "      }",
      "    };",
      "  }",
      "",
      "  def main : Bool = eq (Some (Some Zero)) (Some (Some Zero));",
      "}"
    ]

recursiveListDerivingEqProgram :: String
recursiveListDerivingEqProgram =
  unlines
    [ "module Main export (Eq, Nat(..), List(..), eq, main) {",
      "  class Eq a {",
      "    eq : a -> a -> Bool;",
      "  }",
      "",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat",
      "    deriving Eq;",
      "",
      "  data List a =",
      "      Nil : List a",
      "    | Cons : a -> List a -> List a",
      "    deriving Eq;",
      "",
      "  def main : Bool = eq (Cons Zero Nil) (Cons Zero Nil);",
      "}"
    ]

topLevelRecursiveHigherOrderProgram :: String
topLevelRecursiveHigherOrderProgram =
  unlines
    [ "module Main export (Nat(..), loop, idInt, main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def idInt : Int -> Int = λ(x : Int) x;",
      "  def loop : (Int -> Int) -> Nat -> Int = λ(f : Int -> Int) λ(n : Nat) case n of {",
      "    Zero -> f 1;",
      "    Succ inner -> loop f inner",
      "  };",
      "  def main : Int = loop idInt (Succ Zero);",
      "}"
    ]

localRecursiveHigherOrderProgram :: String
localRecursiveHigherOrderProgram =
  unlines
    [ "module Main export (Nat(..), main) {",
      "  data Nat =",
      "      Zero : Nat",
      "    | Succ : Nat -> Nat;",
      "",
      "  def main : Int =",
      "    let idInt : Int -> Int = λ(x : Int) x in",
      "    let loop : (Int -> Int) -> Nat -> Int = λ(f : Int -> Int) λ(n : Nat) case n of {",
      "      Zero -> f 1;",
      "      Succ inner -> loop f inner",
      "    } in",
      "    loop idInt (Succ Zero);",
      "}"
    ]

constructorForallApplicationProgram :: String
constructorForallApplicationProgram =
  unlines
    [ "module Main export (Pack(..), main) {",
      "  data Pack =",
      "      Pack : ∀ a. a -> Pack;",
      "",
      "  def main : Pack = Pack 1;",
      "}"
    ]

dataParameterOrderConstructorProgram :: String
dataParameterOrderConstructorProgram =
  unlines
    [ "module Main export (T(..), main) {",
      "  data T z a =",
      "      Mk : z -> a -> T z a;",
      "",
      "  def main : T Bool Int = Mk true 1;",
      "}"
    ]

dataParameterOrderConstructorTerm :: CheckedProgram -> Elab.XmlfTerm
dataParameterOrderConstructorTerm checked =
  Elab.EApp
    ( Elab.EApp
        ( Elab.ETyInst
            (Elab.ETyInst (resolvedConstructorTerm checked "Main__Mk") (Elab.InstApp boolElabTy))
            (Elab.InstApp intElabTy)
        )
        (Elab.ELit (LBool True))
    )
    (Elab.ELit (LInt 1))

boundedConstructorForallProgram :: String
boundedConstructorForallProgram =
  unlines
    [ "module Main export (Pack(..), main) {",
      "  data Pack =",
      "      Pack : ∀ (a ⩾ Int). a -> Pack;",
      "",
      "  def main : Pack = Pack 1;",
      "}"
    ]

dependentBoundedConstructorForallProgram :: String
dependentBoundedConstructorForallProgram =
  unlines
    [ "module Main export (Pack(..), main) {",
      "  data Pack =",
      "      Pack : ∀ (a ⩾ Int -> Int). a -> Pack;",
      "",
      "  def id : Int -> Int = λx x;",
      "  def main : Pack = Pack id;",
      "}"
    ]

repeatedPolymorphicParameterProgram :: String
repeatedPolymorphicParameterProgram =
  unlines
    [ "module Main export (Pair(..), main) {",
      "  data Pair a =",
      "      Pair : a -> a -> Pair a;",
      "",
      "  def main : Bool = true;",
      "}"
    ]

duplicateDataNameProgram :: String
duplicateDataNameProgram =
  unlines
    [ "module A export (T(..), make) {",
      "  data T =",
      "      A : T;",
      "",
      "  def make : T = A;",
      "}",
      "",
      "module B export (T(..)) {",
      "  data T =",
      "      B : Int -> T;",
      "}",
      "",
      "module Main export (main) {",
      "  import A as A;",
      "",
      "  def main : Bool = case A.make of {",
      "    A.A -> true",
      "  };",
      "}"
    ]

vacuousRecursiveConstructorFallbackProgram :: String
vacuousRecursiveConstructorFallbackProgram =
  unlines
    [ "module Main export (Box(..), main) {",
      "  data Box = MkBox : Box;",
      "",
      "  def main : Box = MkBox;",
      "}"
    ]

qualifiedAliasOrderingProgram :: String
qualifiedAliasOrderingProgram =
  unlines
    [ "module Y export (T(..)) {",
      "  data T =",
      "      YValue : T;",
      "}",
      "",
      "module Z export (T(..), make) {",
      "  data T =",
      "      ZValue : T;",
      "",
      "  def make : T = ZValue;",
      "}",
      "",
      "module Main export (main) {",
      "  import Z as Z;",
      "",
      "  def main : Bool = case Z.make of {",
      "    Z.ZValue -> true",
      "  };",
      "}"
    ]

sameNameUnqualifiedStructuralOwnerProgram :: String
sameNameUnqualifiedStructuralOwnerProgram =
  unlines
    [ "module Core export (T(..)) {",
      "  data T =",
      "      External : T;",
      "}",
      "",
      "module Main export (T(..), main) {",
      "  import Core as C;",
      "",
      "  data T =",
      "      T : T;",
      "",
      "  def main : T = T;",
      "}"
    ]

unsupportedVariableHeadType :: SrcType
unsupportedVariableHeadType =
  STVarApp "f" (STBase "Int" :| [])

returnedClosureProgram :: String
returnedClosureProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int -> Int =",
      "    let captured : Int = 41 in λ(x : Int) captured;",
      "}"
    ]

closureAliasCallProgram :: String
closureAliasCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    let f : Int -> Int = λ(x : Int) x in",
      "    let g : Int -> Int = f in",
      "    g 7;",
      "}"
    ]

capturedClosureCallProgram :: String
capturedClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    let g : Int -> Int = f in",
      "    g 0;",
      "}"
    ]

functionParameterClosureCallProgram :: String
functionParameterClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int) f 1;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    use f;",
      "}"
    ]

topLevelClosureCallProgram :: String
topLevelClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def maker : Int -> Int = let captured : Int = 41 in λ(x : Int) captured;",
      "  def main : Int = maker 0;",
      "}"
    ]

polymorphicTopLevelClosureCallProgram :: String
polymorphicTopLevelClosureCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def maker : ∀ a. a -> Int = let captured : Int = 41 in λ(x : a) captured;",
      "  def main : Int = maker 0;",
      "}"
    ]

functionParameterNestedClosureAliasCallProgram :: String
functionParameterNestedClosureAliasCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int)",
      "    let g : Int -> Int = (let h : Int -> Int = f in h) in",
      "    g 1;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    use f;",
      "}"
    ]

shadowedClosureLocalProgram :: String
shadowedClosureLocalProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : Int -> Int = λ(x : Int) x;",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int)",
      "    let h : Int -> Int = let f : Int -> Int = id in f in",
      "    h 0;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    use f;",
      "}"
    ]

shadowedFunctionHeadDemandProgram :: String
shadowedFunctionHeadDemandProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λ(x : Int) λ(y : Int) x;",
      "  def main : Int =",
      "    let f : Int -> Int -> Int = λ(x : Int) λ(y : Int) x in",
      "    (λ(f : Int -> Int -> Int) f 1 2) keepLeft;",
      "}"
    ]

shadowedCaseClosureLocalProgram :: String
shadowedCaseClosureLocalProgram =
  unlines
    [ "module Main export (FnBox(..), main) {",
      "  data FnBox = FnBox : (Int -> Int) -> FnBox;",
      "  def id : Int -> Int = λ(x : Int) x;",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int)",
      "    let g : Int -> Int = case FnBox id of { FnBox f -> f } in",
      "    g 0;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    use f;",
      "}"
    ]

shadowedGlobalClosureHeadProgram :: String
shadowedGlobalClosureHeadProgram =
  unlines
    [ "module Main export (FnBox(..), main) {",
      "  data FnBox = FnBox : (Int -> Int) -> FnBox;",
      "  def f : Int -> Int = let captured : Int = 41 in λ(x : Int) captured;",
      "  def id : Int -> Int = λ(x : Int) x;",
      "  def main : Int = case FnBox id of { FnBox f -> f 0 };",
      "}"
    ]

closureValuedConstructorFieldProgram :: String
closureValuedConstructorFieldProgram =
  unlines
    [ "module Main export (FnBox(..), main) {",
      "  data FnBox = FnBox : (Int -> Int) -> FnBox;",
      "  def main : Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int = λ(x : Int) captured in",
      "    case FnBox f of { FnBox g -> g 0 };",
      "}"
    ]

returnedLetLambdaClosureProgram :: String
returnedLetLambdaClosureProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int -> Int -> Int =",
      "    let captured : Int = 41 in",
      "    let f : Int -> Int -> Int = λ(x : Int) let y : Int = captured in λ(z : Int) y in",
      "    f;",
      "}"
    ]

returnedLetLambdaShadowingXmlfTerm :: Elab.XmlfTerm
returnedLetLambdaShadowingXmlfTerm =
  mkTestLocalLet
    "captured"
    (schemeFromType intElabTy)
    (Elab.ELit (LInt 41))
    ( mkTestLocalLet
        "f"
        (schemeFromType (Elab.TArrow intElabTy (Elab.TArrow intElabTy intElabTy)))
        ( mkTestLocalLam
            "x"
            intElabTy
            ( mkTestLocalLet
                "y"
                (schemeFromType intElabTy)
                (Elab.ELit (LInt 1))
                (mkTestLocalLam "y" intElabTy (mkTestDeferredVar "y"))
            )
        )
        (mkTestDeferredVar "f")
    )

directLocalCallProgram :: String
directLocalCallProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Int =",
      "    let f : Int -> Int = λ(x : Int) x in f 7;",
      "}"
    ]

localDirectAliasPartialApplicationBaseProgram :: String
localDirectAliasPartialApplicationBaseProgram =
  unlines
    [ "module Main export (main) {",
      "  def keepLeft : Int -> Int -> Int = λx λy x;",
      "  def apply : (Int -> Int) -> Int = λf f 2;",
      "  def main : Int = apply (keepLeft 1);",
      "}"
    ]

localDirectAliasPartialApplicationTerm :: CheckedProgram -> Elab.XmlfTerm
localDirectAliasPartialApplicationTerm checked =
  mkTestLocalLet
    "keep"
    (schemeFromType binaryIntElabTy)
    (resolvedBindingTerm checked "Main__keepLeft")
    ( Elab.EApp
        (resolvedBindingTerm checked "Main__apply")
        (Elab.EApp (mkTestDeferredVar "keep") (Elab.ELit (LInt 1)))
    )

liftedRecursiveHelpersClosureNameProgram :: String
liftedRecursiveHelpersClosureNameProgram =
  unlines
    [ "module Main export (main) {",
      "  def use : (Int -> Int) -> Int = λ(f : Int -> Int) f 1;",
      "  def main : Int = 0;",
      "}"
    ]

requireChecked :: String -> IO CheckedProgram
requireChecked input = do
  program <- requireParsed input
  requireRight (checkProgram program)

requireParsed :: String -> IO Program
requireParsed input =
  case parseRawProgram input of
    Left err -> expectationFailure (renderProgramParseError err) >> fail "parse failed"
    Right program -> pure program

requireRight :: (Show err) => Either err a -> IO a
requireRight result =
  case result of
    Left err -> expectationFailure (show err) >> fail "unexpected Left"
    Right value -> pure value

backendIRGolden :: FilePath -> BackendProgram -> Expectation
backendIRGolden goldenPath backend = do
  validateBackendProgram backend `shouldBe` Right ()
  goldenText goldenPath (renderBackendIRSnapshot backend)

goldenText :: FilePath -> String -> Expectation
goldenText goldenPath actual = do
  accept <- lookupEnv "GOLDEN_ACCEPT"
  case accept of
    Just "1" -> do
      createDirectoryIfMissing True (takeDirectory goldenPath)
      writeFile goldenPath actual
    _ -> do
      expected <- readFile goldenPath
      length expected `seq` actual `shouldBe` expected

renderBackendIRSnapshot :: BackendProgram -> String
renderBackendIRSnapshot backend =
  unlines $
    [ "backend-program",
      "  main: " ++ backendProgramMain backend,
      "  modules:"
    ]
      ++ concatMap renderBackendIRModule (backendProgramModules backend)

renderBackendIRModule :: BackendModule -> [String]
renderBackendIRModule backendModule =
  [ indent 4 ("module " ++ backendModuleName backendModule),
    indent 6 "data:"
  ]
    ++ renderListOrEmpty 8 renderBackendIRData (backendModuleData backendModule)
    ++ [indent 6 "bindings:"]
    ++ renderListOrEmpty 8 renderBackendIRBinding (backendModuleBindings backendModule)

renderBackendIRData :: BackendData -> [String]
renderBackendIRData backendData =
  [ indent 8 ("data " ++ backendDataName backendData ++ renderPlainTypeParameters (backendDataParameters backendData)),
    indent 10 "constructors:"
  ]
    ++ renderListOrEmpty 12 renderBackendIRConstructor (backendDataConstructors backendData)

renderBackendIRConstructor :: BackendConstructor -> [String]
renderBackendIRConstructor constructor =
  [ indent 12 ("ctor " ++ backendConstructorName constructor ++ renderBackendTypeBinders (backendConstructorForalls constructor)),
    indent 14 ("fields: " ++ renderTypeList (backendConstructorFields constructor)),
    indent 14 ("result: " ++ renderBackendIRType (backendConstructorResult constructor))
  ]

renderBackendIRBinding :: BackendBinding -> [String]
renderBackendIRBinding binding =
  [ indent 8 ("binding " ++ backendBindingName binding ++ " : " ++ renderBackendIRType (backendBindingType binding)),
    indent 10 ("exported-main: " ++ renderBool (backendBindingExportedAsMain binding)),
    indent 10 "expr:"
  ]
    ++ renderBackendIRExpr 12 (backendBindingExpr binding)

renderBackendIRExpr :: Int -> BackendExpr -> [String]
renderBackendIRExpr level expr =
  case expr of
    BackendVar resultTy name ->
      [indent level ("var " ++ name ++ " : " ++ renderBackendIRType resultTy)]
    BackendLit resultTy lit ->
      [indent level ("lit " ++ renderLit lit ++ " : " ++ renderBackendIRType resultTy)]
    BackendLam resultTy name paramTy body ->
      [ indent level ("lam " ++ name ++ " : " ++ renderBackendIRType paramTy ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "body:"
      ]
        ++ renderBackendIRExpr (level + 4) body
    BackendLamWithIdentity resultTy _ name paramTy body ->
      [ indent level ("lam " ++ name ++ " : " ++ renderBackendIRType paramTy ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "body:"
      ]
        ++ renderBackendIRExpr (level + 4) body
    BackendApp resultTy fun arg ->
      [ indent level ("app : " ++ renderBackendIRType resultTy),
        indent (level + 2) "function:"
      ]
        ++ renderBackendIRExpr (level + 4) fun
        ++ [indent (level + 2) "argument:"]
        ++ renderBackendIRExpr (level + 4) arg
    BackendLet resultTy name bindingTy rhs body ->
      [ indent level ("let " ++ name ++ " : " ++ renderBackendIRType bindingTy ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "rhs:"
      ]
        ++ renderBackendIRExpr (level + 4) rhs
        ++ [indent (level + 2) "body:"]
        ++ renderBackendIRExpr (level + 4) body
    BackendLetWithIdentity resultTy _ name bindingTy rhs body ->
      [ indent level ("let " ++ name ++ " : " ++ renderBackendIRType bindingTy ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "rhs:"
      ]
        ++ renderBackendIRExpr (level + 4) rhs
        ++ [indent (level + 2) "body:"]
        ++ renderBackendIRExpr (level + 4) body
    BackendTyAbs resultTy name mbBound body ->
      [ indent level ("type-lam " ++ renderTypeBinder name mbBound ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "body:"
      ]
        ++ renderBackendIRExpr (level + 4) body
    BackendTyApp resultTy fun tyArg ->
      [ indent level ("type-app [" ++ renderBackendIRType tyArg ++ "] : " ++ renderBackendIRType resultTy),
        indent (level + 2) "function:"
      ]
        ++ renderBackendIRExpr (level + 4) fun
    BackendConstruct resultTy name args ->
      [ indent level ("construct " ++ name ++ " : " ++ renderBackendIRType resultTy),
        indent (level + 2) "args:"
      ]
        ++ renderExprList (level + 4) args
    BackendCase resultTy scrutinee alternatives ->
      [ indent level ("case : " ++ renderBackendIRType resultTy),
        indent (level + 2) "scrutinee:"
      ]
        ++ renderBackendIRExpr (level + 4) scrutinee
        ++ [indent (level + 2) "alternatives:"]
        ++ concatMap (renderBackendIRAlternative (level + 4)) (toList alternatives)
    BackendRoll resultTy payload ->
      [ indent level ("roll : " ++ renderBackendIRType resultTy),
        indent (level + 2) "payload:"
      ]
        ++ renderBackendIRExpr (level + 4) payload
    BackendUnroll resultTy payload ->
      [ indent level ("unroll : " ++ renderBackendIRType resultTy),
        indent (level + 2) "payload:"
      ]
        ++ renderBackendIRExpr (level + 4) payload
    BackendClosure resultTy entryName captures params body ->
      [ indent level ("closure " ++ entryName ++ " : " ++ renderBackendIRType resultTy),
        indent (level + 2) ("params: " ++ renderNamedTypeList params),
        indent (level + 2) "captures:"
      ]
        ++ renderListOrEmpty (level + 4) (renderBackendIRCapture (level + 4)) captures
        ++ [indent (level + 2) "body:"]
        ++ renderBackendIRExpr (level + 4) body
    BackendClosureCall resultTy fun args ->
      [ indent level ("closure-call : " ++ renderBackendIRType resultTy),
        indent (level + 2) "function:"
      ]
        ++ renderBackendIRExpr (level + 4) fun
        ++ [indent (level + 2) "arguments:"]
        ++ renderExprList (level + 4) args

renderBackendIRCapture :: Int -> BackendClosureCapture -> [String]
renderBackendIRCapture level capture =
  [ indent level (backendClosureCaptureName capture ++ " : " ++ renderBackendIRType (backendClosureCaptureType capture)),
    indent (level + 2) "expr:"
  ]
    ++ renderBackendIRExpr (level + 4) (backendClosureCaptureExpr capture)

renderBackendIRAlternative :: Int -> BackendAlternative -> [String]
renderBackendIRAlternative level alternative =
  [ indent level ("alternative " ++ renderBackendIRPattern (backendAltPattern alternative)),
    indent (level + 2) "body:"
  ]
    ++ renderBackendIRExpr (level + 4) (backendAltBody alternative)

renderBackendIRPattern :: BackendPattern -> String
renderBackendIRPattern pattern0 =
  case pattern0 of
    BackendDefaultPattern ->
      "default"
    BackendConstructorPattern name binders ->
      name ++ "(" ++ intercalate ", " binders ++ ")"

renderExprList :: Int -> [BackendExpr] -> [String]
renderExprList level exprs =
  renderListOrEmpty level renderArg (zip [0 :: Int ..] exprs)
  where
    renderArg (ix, expr) =
      indent level ("arg " ++ show ix ++ ":") : renderBackendIRExpr (level + 2) expr

renderBackendIRType :: BackendType -> String
renderBackendIRType backendTy =
  case backendTy of
    BTVar name -> "$" ++ name
    BTArrow dom cod -> "(" ++ renderBackendIRType dom ++ " -> " ++ renderBackendIRType cod ++ ")"
    BTBase (BaseTy name) -> name
    BTCon (BaseTy name) args -> name ++ "<" ++ intercalate ", " (map renderBackendIRType (toList args)) ++ ">"
    BTVarApp name args -> "$" ++ name ++ "<" ++ intercalate ", " (map renderBackendIRType (toList args)) ++ ">"
    BTForall name mbBound body -> "forall " ++ renderTypeBinder name mbBound ++ ". " ++ renderBackendIRType body
    BTMu name body -> "mu " ++ name ++ ". " ++ renderBackendIRType body
    BTBottom -> "bottom"

renderBackendTypeBinders :: [BackendTypeBinder] -> String
renderBackendTypeBinders binders =
  case binders of
    [] -> ""
    _ -> "<" ++ intercalate ", " [renderTypeBinder name mbBound | BackendTypeBinder name mbBound <- binders] ++ ">"

renderTypeBinder :: String -> Maybe BackendType -> String
renderTypeBinder name mbBound =
  "$" ++ name ++ maybe "" ((" >= " ++) . renderBackendIRType) mbBound

renderPlainTypeParameters :: [String] -> String
renderPlainTypeParameters params =
  case params of
    [] -> ""
    _ -> "<" ++ intercalate ", " (map ("$" ++) params) ++ ">"

renderTypeList :: [BackendType] -> String
renderTypeList types0 =
  "[" ++ intercalate ", " (map renderBackendIRType types0) ++ "]"

renderNamedTypeList :: [(String, BackendType)] -> String
renderNamedTypeList params =
  "[" ++ intercalate ", " [name ++ " : " ++ renderBackendIRType ty | (name, ty) <- params] ++ "]"

renderLit :: Lit -> String
renderLit lit =
  case lit of
    LInt n -> show n
    LBool True -> "true"
    LBool False -> "false"
    LChar value -> show value
    LString value -> show value

renderBool :: Bool -> String
renderBool value =
  case value of
    True -> "true"
    False -> "false"

renderListOrEmpty :: Int -> (a -> [String]) -> [a] -> [String]
renderListOrEmpty level render items =
  case items of
    [] -> [indent level "<none>"]
    _ -> concatMap render items

indent :: Int -> String -> String
indent level line =
  replicate level ' ' ++ line

requireBinding :: String -> BackendProgram -> IO BackendBinding
requireBinding name backend =
  case find ((== name) . backendBindingName) (backendBindings backend) of
    Just binding -> pure binding
    Nothing -> expectationFailure ("missing backend binding " ++ show name) >> fail "missing binding"

requireSingleLiftedHelper :: BackendProgram -> IO BackendBinding
requireSingleLiftedHelper backend =
  case filter (isInfixOf "$letrec$" . backendBindingName) (backendBindings backend) of
    [helper] -> pure helper
    helpers -> expectationFailure ("expected one lifted helper, got " ++ show (map backendBindingName helpers)) >> fail "helper mismatch"

requireConstructor :: String -> BackendProgram -> IO BackendConstructor
requireConstructor name backend =
  case find ((== name) . backendConstructorName) (backendConstructors backend) of
    Just constructor -> pure constructor
    Nothing -> expectationFailure ("missing backend constructor " ++ show name) >> fail "missing constructor"

requireCheckedConstructor :: String -> CheckedProgram -> IO ConstructorInfo
requireCheckedConstructor name checked =
  case find ((== name) . ctorRuntimeName) constructors of
    Just constructor -> pure constructor
    Nothing -> expectationFailure ("missing checked constructor " ++ show name) >> fail "missing checked constructor"
  where
    constructors =
      concatMap dataConstructors (checkedDataInfos checked)

backendBindings :: BackendProgram -> [BackendBinding]
backendBindings =
  concatMap backendModuleBindings . backendProgramModules

backendConstructors :: BackendProgram -> [BackendConstructor]
backendConstructors =
  concatMap (concatMap backendDataConstructors . backendModuleData) . backendProgramModules

backendDataNames :: BackendProgram -> [String]
backendDataNames =
  concatMap (map backendDataName . backendModuleData) . backendProgramModules

requireBackendData :: String -> BackendProgram -> IO BackendData
requireBackendData name backend =
  case find ((== name) . backendDataName) (concatMap backendModuleData (backendProgramModules backend)) of
    Just dataDecl -> pure dataDecl
    Nothing -> expectationFailure ("missing backend data " ++ show name) >> fail "missing backend data"

checkedDataInfos :: CheckedProgram -> [DataInfo]
checkedDataInfos checked =
  concatMap (toList . checkedModuleData) (checkedProgramModules checked)

parameterizedInstanceMethodRuntimeNames :: SymbolIdentity -> CheckedProgram -> [String]
parameterizedInstanceMethodRuntimeNames dataIdentity checked =
  [ runtimeName
  | checkedModule <- checkedProgramModules checked,
    classInfo <- toList (checkedModuleClasses checkedModule),
    methodInfo <- Map.elems (classMethodsByIdentity classInfo),
    methodName methodInfo == "eq",
    instanceInfo <- checkedModuleInstances checkedModule,
    instanceInfoClassSymbolIdentity instanceInfo == classInfoSymbolIdentity classInfo,
    any (srcTypeMentionsDataIdentity dataIdentity) (toList (instanceHeadIdentityTypes instanceInfo)),
    Just valueInfo <- [lookupInstanceMethod methodInfo instanceInfo],
    Just runtimeName <- [valueInfoRuntimeName valueInfo]
  ]

valueInfoRuntimeName :: ValueInfo -> Maybe String
valueInfoRuntimeName valueInfo =
  case valueInfo of
    OrdinaryValue {valueRuntimeName = runtimeName} -> Just runtimeName
    ConstructorValue {valueRuntimeName = runtimeName} -> Just runtimeName
    OverloadedMethod {} -> Nothing

srcTypeMentionsDataIdentity :: SymbolIdentity -> SrcType -> Bool
srcTypeMentionsDataIdentity identity ty =
  symbolIdentityStableName identity `elem` typeHeadNamesSrcType ty

backendConstructorNames :: BackendProgram -> [String]
backendConstructorNames =
  concatMap (concatMap (map backendConstructorName . backendDataConstructors) . backendModuleData) . backendProgramModules

containsBackendCase :: BackendExpr -> Bool
containsBackendCase expr =
  case expr of
    BackendCase {} -> True
    BackendLamWithIdentity {backendBody = body} -> containsBackendCase body
    BackendApp {backendFunction = fun, backendArgument = arg} -> containsBackendCase fun || containsBackendCase arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} -> containsBackendCase rhs || containsBackendCase body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendCase body
    BackendTyApp {backendTyFunction = fun} -> containsBackendCase fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any containsBackendCase args
    BackendRoll {backendRollPayload = body} -> containsBackendCase body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendCase body
    _ -> False

containsBackendTyApp :: BackendExpr -> Bool
containsBackendTyApp expr =
  case expr of
    BackendTyApp {} -> True
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendTyApp scrutinee || any (containsBackendTyApp . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> containsBackendTyApp body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendTyApp fun || containsBackendTyApp arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendTyApp rhs || containsBackendTyApp body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendTyApp body
    BackendConstructWithIdentity {backendConstructArgs = args} -> any containsBackendTyApp args
    BackendRoll {backendRollPayload = body} -> containsBackendTyApp body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendTyApp body
    _ -> False

containsBackendApp :: BackendExpr -> Bool
containsBackendApp expr =
  case expr of
    BackendApp {} -> True
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendApp scrutinee || any (containsBackendApp . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> containsBackendApp body
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendApp rhs || containsBackendApp body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendApp body
    BackendTyApp {backendTyFunction = fun} -> containsBackendApp fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any containsBackendApp args
    BackendRoll {backendRollPayload = body} -> containsBackendApp body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendApp body
    BackendClosure _ _ captures _ body ->
      any (containsBackendApp . backendClosureCaptureExpr) captures || containsBackendApp body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      containsBackendApp fun || any containsBackendApp args
    _ -> False

containsBackendCaseHeadedApp :: BackendExpr -> Bool
containsBackendCaseHeadedApp expr =
  case expr of
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      isCaseHead fun || containsBackendCaseHeadedApp fun || containsBackendCaseHeadedApp arg
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendCaseHeadedApp scrutinee || any (containsBackendCaseHeadedApp . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> containsBackendCaseHeadedApp body
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendCaseHeadedApp rhs || containsBackendCaseHeadedApp body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendCaseHeadedApp body
    BackendTyApp {backendTyFunction = fun} -> containsBackendCaseHeadedApp fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any containsBackendCaseHeadedApp args
    BackendRoll {backendRollPayload = body} -> containsBackendCaseHeadedApp body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendCaseHeadedApp body
    BackendClosure _ _ captures _ body ->
      any (containsBackendCaseHeadedApp . backendClosureCaptureExpr) captures || containsBackendCaseHeadedApp body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      containsBackendCaseHeadedApp fun || any containsBackendCaseHeadedApp args
    _ -> False
  where
    isCaseHead headExpr =
      case stripTyApps headExpr of
        BackendCase {} -> True
        _ -> False

    stripTyApps headExpr =
      case headExpr of
        BackendTyApp {backendTyFunction = fun} -> stripTyApps fun
        other -> other

containsBackendClosure :: BackendExpr -> Bool
containsBackendClosure expr =
  case expr of
    BackendClosure _ _ _ _ _ -> True
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendClosure scrutinee || any (containsBackendClosure . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> containsBackendClosure body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendClosure fun || containsBackendClosure arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendClosure rhs || containsBackendClosure body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendClosure body
    BackendTyApp {backendTyFunction = fun} -> containsBackendClosure fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any containsBackendClosure args
    BackendRoll {backendRollPayload = body} -> containsBackendClosure body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendClosure body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      containsBackendClosure fun || any containsBackendClosure args
    _ -> False

collectBackendClosureEntryNames :: BackendExpr -> [String]
collectBackendClosureEntryNames expr =
  map snd (collectBackendClosureEntryRefs expr)

collectBackendClosureEntryRefs :: BackendExpr -> [(Maybe UniqueIdentity, String)]
collectBackendClosureEntryRefs expr =
  case expr of
    BackendClosureWithParamIdentities {backendClosureEntryIdentity = entryIdentity, backendClosureEntryName = entryName, backendClosureCaptures = captures, backendClosureBody = body} ->
      (entryIdentity, entryName) : concatMap (collectBackendClosureEntryRefs . backendClosureCaptureExpr) captures ++ collectBackendClosureEntryRefs body
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      collectBackendClosureEntryRefs scrutinee ++ concatMap (collectBackendClosureEntryRefs . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> collectBackendClosureEntryRefs body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      collectBackendClosureEntryRefs fun ++ collectBackendClosureEntryRefs arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      collectBackendClosureEntryRefs rhs ++ collectBackendClosureEntryRefs body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> collectBackendClosureEntryRefs body
    BackendTyApp {backendTyFunction = fun} -> collectBackendClosureEntryRefs fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> concatMap collectBackendClosureEntryRefs args
    BackendRoll {backendRollPayload = body} -> collectBackendClosureEntryRefs body
    BackendUnroll {backendUnrollPayload = body} -> collectBackendClosureEntryRefs body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      collectBackendClosureEntryRefs fun ++ concatMap collectBackendClosureEntryRefs args
    _ -> []

closureNameUniqueSuffix :: String -> Maybe Int
closureNameUniqueSuffix name =
  case reads (reverse (takeWhile (/= '$') (reverse name))) of
    [(value, "")] -> Just value
    _ -> Nothing

containsBackendClosureCall :: BackendExpr -> Bool
containsBackendClosureCall expr =
  case expr of
    BackendClosureCall {} -> True
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendClosureCall scrutinee || any (containsBackendClosureCall . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> containsBackendClosureCall body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendClosureCall fun || containsBackendClosureCall arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendClosureCall rhs || containsBackendClosureCall body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendClosureCall body
    BackendTyApp {backendTyFunction = fun} -> containsBackendClosureCall fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any containsBackendClosureCall args
    BackendRoll {backendRollPayload = body} -> containsBackendClosureCall body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendClosureCall body
    BackendClosure _ _ captures _ body ->
      any (containsBackendClosureCall . backendClosureCaptureExpr) captures || containsBackendClosureCall body
    _ -> False

containsBackendClosureCallFunction :: String -> BackendExpr -> Bool
containsBackendClosureCallFunction expected expr =
  case expr of
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      closureFunctionMatches fun || any (containsBackendClosureCallFunction expected) args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendClosureCallFunction expected scrutinee
        || any (containsBackendClosureCallFunction expected . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} ->
      containsBackendClosureCallFunction expected body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendClosureCallFunction expected fun || containsBackendClosureCallFunction expected arg
    BackendLetWithIdentity {backendLetName = name, backendLetRhs = rhs, backendLetBody = body}
      | name == expected -> containsBackendClosureCallFunction expected rhs
      | otherwise ->
          containsBackendClosureCallFunction expected rhs || containsBackendClosureCallFunction expected body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendClosureCallFunction expected body
    BackendTyApp {backendTyFunction = fun} -> containsBackendClosureCallFunction expected fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any (containsBackendClosureCallFunction expected) args
    BackendRoll {backendRollPayload = body} -> containsBackendClosureCallFunction expected body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendClosureCallFunction expected body
    BackendClosure _ _ captures params body
      | expected `elem` map fst params || expected `elem` map backendClosureCaptureName captures ->
          any (containsBackendClosureCallFunction expected . backendClosureCaptureExpr) captures
      | otherwise ->
          any (containsBackendClosureCallFunction expected . backendClosureCaptureExpr) captures
            || containsBackendClosureCallFunction expected body
    _ -> False
  where
    closureFunctionMatches fun =
      case fun of
        BackendVarWithIdentity {backendVarName = name} -> generatedBackendNameMatches expected name
        BackendTyApp {backendTyFunction = inner} -> closureFunctionMatches inner
        _ -> containsBackendClosureCallFunction expected fun

    generatedBackendNameMatches plain name =
      name == plain || ("$" ++ plain ++ "#") `isPrefixOf` name

containsAlphaRenamedShadowingClosure :: BackendExpr -> Bool
containsAlphaRenamedShadowingClosure expr =
  case expr of
    BackendClosure
      _
      _
      captures
      params
      ( BackendLetWithIdentity
          { backendLetName = "y",
            backendLetBody = BackendVarWithIdentity {backendVarName = bodyName}
          }
        ) ->
        (bodyName /= "y" && bodyName `elem` map fst params)
          || any (containsAlphaRenamedShadowingClosure . backendClosureCaptureExpr) captures
    BackendClosure _ _ captures _ body ->
      any (containsAlphaRenamedShadowingClosure . backendClosureCaptureExpr) captures
        || containsAlphaRenamedShadowingClosure body
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsAlphaRenamedShadowingClosure scrutinee
        || any (containsAlphaRenamedShadowingClosure . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> containsAlphaRenamedShadowingClosure body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsAlphaRenamedShadowingClosure fun || containsAlphaRenamedShadowingClosure arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      containsAlphaRenamedShadowingClosure rhs || containsAlphaRenamedShadowingClosure body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsAlphaRenamedShadowingClosure body
    BackendTyApp {backendTyFunction = fun} -> containsAlphaRenamedShadowingClosure fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any containsAlphaRenamedShadowingClosure args
    BackendRoll {backendRollPayload = body} -> containsAlphaRenamedShadowingClosure body
    BackendUnroll {backendUnrollPayload = body} -> containsAlphaRenamedShadowingClosure body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      containsAlphaRenamedShadowingClosure fun || any containsAlphaRenamedShadowingClosure args
    _ -> False

closureParamCounts :: BackendExpr -> [Int]
closureParamCounts expr =
  case expr of
    BackendClosure _ _ captures params body ->
      length params : concatMap (closureParamCounts . backendClosureCaptureExpr) captures ++ closureParamCounts body
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      closureParamCounts scrutinee ++ concatMap (closureParamCounts . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> closureParamCounts body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      closureParamCounts fun ++ closureParamCounts arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      closureParamCounts rhs ++ closureParamCounts body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> closureParamCounts body
    BackendTyApp {backendTyFunction = fun} -> closureParamCounts fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> concatMap closureParamCounts args
    BackendRoll {backendRollPayload = body} -> closureParamCounts body
    BackendUnroll {backendUnrollPayload = body} -> closureParamCounts body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      closureParamCounts fun ++ concatMap closureParamCounts args
    _ -> []

containsBackendClosureCapture :: String -> BackendExpr -> Bool
containsBackendClosureCapture expected expr =
  case expr of
    BackendClosure _ _ captures _ body ->
      any (captureNameMatches expected . backendClosureCaptureName) captures
        || any (containsBackendClosureCapture expected . backendClosureCaptureExpr) captures
        || containsBackendClosureCapture expected body
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendClosureCapture expected scrutinee
        || any (containsBackendClosureCapture expected . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> containsBackendClosureCapture expected body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendClosureCapture expected fun || containsBackendClosureCapture expected arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendClosureCapture expected rhs || containsBackendClosureCapture expected body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendClosureCapture expected body
    BackendTyApp {backendTyFunction = fun} -> containsBackendClosureCapture expected fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any (containsBackendClosureCapture expected) args
    BackendRoll {backendRollPayload = body} -> containsBackendClosureCapture expected body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendClosureCapture expected body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      containsBackendClosureCapture expected fun || any (containsBackendClosureCapture expected) args
    _ -> False
  where
    captureNameMatches expectedName actualName =
      expectedName == actualName || expectedName `isInfixOf` actualName

containsBackendTyAppArgument :: BackendType -> BackendExpr -> Bool
containsBackendTyAppArgument expected expr =
  case expr of
    BackendTyApp {backendTyArgument = ty, backendTyFunction = fun} ->
      alphaEqBackendType ty expected || containsBackendTyAppArgument expected fun
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendTyAppArgument expected scrutinee || any (containsBackendTyAppArgument expected . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> containsBackendTyAppArgument expected body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendTyAppArgument expected fun || containsBackendTyAppArgument expected arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendTyAppArgument expected rhs || containsBackendTyAppArgument expected body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendTyAppArgument expected body
    BackendConstructWithIdentity {backendConstructArgs = args} -> any (containsBackendTyAppArgument expected) args
    BackendRoll {backendRollPayload = body} -> containsBackendTyAppArgument expected body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendTyAppArgument expected body
    _ -> False

containsFreshenedTypeAbsWithOuterBound :: BackendExpr -> Bool
containsFreshenedTypeAbsWithOuterBound expr =
  case expr of
    BackendTyAbsWithIdentity {backendTyParamName = name, backendTyParamBound = Just (BTArrow (BTVar boundDom) (BTVar boundCod)), backendTyAbsBody = body} ->
      (name /= "a" && boundDom == "a" && boundCod == "a") || containsFreshenedTypeAbsWithOuterBound body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} ->
      containsFreshenedTypeAbsWithOuterBound body
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsFreshenedTypeAbsWithOuterBound scrutinee || any (containsFreshenedTypeAbsWithOuterBound . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> containsFreshenedTypeAbsWithOuterBound body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsFreshenedTypeAbsWithOuterBound fun || containsFreshenedTypeAbsWithOuterBound arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      containsFreshenedTypeAbsWithOuterBound rhs || containsFreshenedTypeAbsWithOuterBound body
    BackendTyApp {backendTyFunction = fun} -> containsFreshenedTypeAbsWithOuterBound fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any containsFreshenedTypeAbsWithOuterBound args
    BackendRoll {backendRollPayload = body} -> containsFreshenedTypeAbsWithOuterBound body
    BackendUnroll {backendUnrollPayload = body} -> containsFreshenedTypeAbsWithOuterBound body
    _ -> False

containsBackendVar :: String -> BackendExpr -> Bool
containsBackendVar expected expr =
  case expr of
    BackendVarWithIdentity {backendVarName = name} -> name == expected
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendVar expected scrutinee || any (containsBackendVar expected . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendParamName = name, backendBody = body}
      | name == expected -> False
      | otherwise -> containsBackendVar expected body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendVar expected fun || containsBackendVar expected arg
    BackendLetWithIdentity {backendLetName = name, backendLetRhs = rhs, backendLetBody = body}
      | name == expected -> containsBackendVar expected rhs
      | otherwise -> containsBackendVar expected rhs || containsBackendVar expected body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsBackendVar expected body
    BackendTyApp {backendTyFunction = fun} -> containsBackendVar expected fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any (containsBackendVar expected) args
    BackendRoll {backendRollPayload = body} -> containsBackendVar expected body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendVar expected body
    _ -> False

backendExprBinderRefs :: BackendExpr -> [(String, Maybe IdDetails)]
backendExprBinderRefs expr =
  case expr of
    BackendVarWithIdentity {} -> []
    BackendLit {} -> []
    BackendLamWithIdentity {backendParamName = name, backendParamIdentity = identity, backendBody = body} ->
      (name, identity) : backendExprBinderRefs body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      backendExprBinderRefs fun ++ backendExprBinderRefs arg
    BackendLetWithIdentity {backendLetName = name, backendLetIdentity = identity, backendLetRhs = rhs, backendLetBody = body} ->
      (name, identity) : backendExprBinderRefs rhs ++ backendExprBinderRefs body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> backendExprBinderRefs body
    BackendTyApp {backendTyFunction = fun} -> backendExprBinderRefs fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> concatMap backendExprBinderRefs args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      backendExprBinderRefs scrutinee ++ concatMap alternativeBinderRefs (toList alternatives)
    BackendRoll {backendRollPayload = body} -> backendExprBinderRefs body
    BackendUnroll {backendUnrollPayload = body} -> backendExprBinderRefs body
    BackendClosureWithParamIdentities {backendClosureCaptures = captures, backendClosureParamsWithIdentities = params, backendClosureBody = body} ->
      concatMap (backendExprBinderRefs . backendClosureCaptureExpr) captures
        ++ [(backendClosureCaptureName capture, backendClosureCaptureIdentity capture) | capture <- captures]
        ++ [(backendClosureParamName param, backendClosureParamIdentity param) | param <- params]
        ++ backendExprBinderRefs body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      backendExprBinderRefs fun ++ concatMap backendExprBinderRefs args
  where
    alternativeBinderRefs (BackendAlternative pattern0 body) =
      patternBackendBinderRefs pattern0 ++ backendExprBinderRefs body

    patternBackendBinderRefs BackendDefaultPattern = []
    patternBackendBinderRefs (BackendConstructorPatternWithBinderIdentities _ _ binders) =
      [(backendPatternBinderName binder, backendPatternBinderIdentity binder) | binder <- binders]

containsSelfReferentialLetRhs :: BackendExpr -> Bool
containsSelfReferentialLetRhs expr =
  case expr of
    BackendVarWithIdentity {} -> False
    BackendLit {} -> False
    BackendLamWithIdentity {backendBody = body} -> containsSelfReferentialLetRhs body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsSelfReferentialLetRhs fun || containsSelfReferentialLetRhs arg
    BackendLetWithIdentity {backendLetIdentity = identity, backendLetName = name, backendLetRhs = rhs, backendLetBody = body} ->
      rhsIsSelfReference (identity, name) rhs || containsSelfReferentialLetRhs rhs || containsSelfReferentialLetRhs body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> containsSelfReferentialLetRhs body
    BackendTyApp {backendTyFunction = fun} -> containsSelfReferentialLetRhs fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> any containsSelfReferentialLetRhs args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsSelfReferentialLetRhs scrutinee || any (containsSelfReferentialLetRhs . backendAltBody) (toList alternatives)
    BackendRoll {backendRollPayload = body} -> containsSelfReferentialLetRhs body
    BackendUnroll {backendUnrollPayload = body} -> containsSelfReferentialLetRhs body
    BackendClosureWithParamIdentities {backendClosureCaptures = captures, backendClosureBody = body} ->
      any (containsSelfReferentialLetRhs . backendClosureCaptureExpr) captures || containsSelfReferentialLetRhs body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      containsSelfReferentialLetRhs fun || any containsSelfReferentialLetRhs args
  where
    rhsIsSelfReference binder rhs =
      case rhs of
        BackendVarWithIdentity {backendVarIdentity = rhsIdentity, backendVarName = rhsName} ->
          backendBinderRefMatches binder (rhsIdentity, rhsName)
        _ -> False

    backendBinderRefMatches (Just left, _) (Just right, _) = left == right
    backendBinderRefMatches (Nothing, leftName) (Nothing, rightName) = leftName == rightName
    backendBinderRefMatches _ _ = False

isBackendFunctionType :: BackendType -> Bool
isBackendFunctionType ty =
  case ty of
    BTArrow {} -> True
    _ -> False

containsConstructArgTypeVar :: String -> String -> BackendExpr -> Bool
containsConstructArgTypeVar constructorName argName =
  go Map.empty
  where
    go scope expr =
      case expr of
        BackendConstructWithIdentity {backendConstructName = name, backendConstructArgs = args} ->
          (name == constructorName && any (matchesArgType scope . backendExprType) args)
            || any (go scope) args
        BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
          go scope scrutinee
            || any (go scope . backendAltBody) (toList alternatives)
        BackendLamWithIdentity {backendBody = body} -> go scope body
        BackendApp {backendFunction = fun, backendArgument = arg} ->
          go scope fun || go scope arg
        BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
          go scope rhs || go scope body
        BackendTyAbsWithIdentity {backendTyParamIdentity = mbIdentity, backendTyParamName = paramName, backendTyAbsBody = body} ->
          go (maybe scope (\identity -> Map.insert paramName identity scope) mbIdentity) body
        BackendTyApp {backendTyFunction = fun} -> go scope fun
        BackendRoll {backendRollPayload = body} -> go scope body
        BackendUnroll {backendUnrollPayload = body} -> go scope body
        _ -> False

    matchesArgType scope ty =
      case ty of
        BTVarWithIdentity (Just identity) name ->
          name == argName && Map.lookup argName scope == Just identity
        _ ->
          False

findBackendCase :: BackendExpr -> Maybe BackendExpr
findBackendCase expr =
  case expr of
    BackendCase {} -> Just expr
    BackendLamWithIdentity {backendBody = body} -> findBackendCase body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      findBackendCase fun <|> findBackendCase arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      findBackendCase rhs <|> findBackendCase body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> findBackendCase body
    BackendTyApp {backendTyFunction = fun} -> findBackendCase fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> firstJust (map findBackendCase args)
    BackendRoll {backendRollPayload = body} -> findBackendCase body
    BackendUnroll {backendUnrollPayload = body} -> findBackendCase body
    _ -> Nothing

collectConstructNames :: BackendExpr -> [String]
collectConstructNames expr =
  case expr of
    BackendConstructWithIdentity {backendConstructName = name, backendConstructArgs = args} ->
      name : concatMap collectConstructNames args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      collectConstructNames scrutinee ++ concatMap (collectConstructNames . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> collectConstructNames body
    BackendApp {backendFunction = fun, backendArgument = arg} -> collectConstructNames fun ++ collectConstructNames arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} -> collectConstructNames rhs ++ collectConstructNames body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> collectConstructNames body
    BackendTyApp {backendTyFunction = fun} -> collectConstructNames fun
    BackendRoll {backendRollPayload = body} -> collectConstructNames body
    BackendUnroll {backendUnrollPayload = body} -> collectConstructNames body
    _ -> []

collectConstructIdentities :: BackendExpr -> [(String, Maybe SymbolIdentity)]
collectConstructIdentities expr =
  case expr of
    BackendConstructWithIdentity {backendConstructIdentity = identity, backendConstructName = name, backendConstructArgs = args} ->
      (name, identity) : concatMap collectConstructIdentities args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      collectConstructIdentities scrutinee ++ concatMap (collectConstructIdentities . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> collectConstructIdentities body
    BackendApp {backendFunction = fun, backendArgument = arg} -> collectConstructIdentities fun ++ collectConstructIdentities arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} -> collectConstructIdentities rhs ++ collectConstructIdentities body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> collectConstructIdentities body
    BackendTyApp {backendTyFunction = fun} -> collectConstructIdentities fun
    BackendRoll {backendRollPayload = body} -> collectConstructIdentities body
    BackendUnroll {backendUnrollPayload = body} -> collectConstructIdentities body
    _ -> []

collectPatternIdentities :: NonEmpty BackendAlternative -> [(String, Maybe SymbolIdentity)]
collectPatternIdentities alternatives =
  [ (name, identity)
  | BackendAlternative (BackendConstructorPatternWithBinderIdentities identity name _) _ <- toList alternatives
  ]

renameBackendConstructorReferences :: Bool -> Bool -> (String -> Bool) -> String -> BackendExpr -> BackendExpr
renameBackendConstructorReferences renameConstructs renamePatterns predicate replacement =
  go
  where
    renameName enabled name
      | enabled && predicate name = replacement
      | otherwise = name

    go expr =
      case expr of
        BackendVarWithIdentity {} -> expr
        BackendLit {} -> expr
        BackendLam resultTy name paramTy body ->
          BackendLam resultTy name paramTy (go body)
        BackendApp resultTy fun arg ->
          BackendApp resultTy (go fun) (go arg)
        BackendLet resultTy name bindingTy rhs body ->
          BackendLet resultTy name bindingTy (go rhs) (go body)
        BackendTyAbs resultTy name mbBound body ->
          BackendTyAbs resultTy name mbBound (go body)
        BackendTyApp resultTy fun tyArg ->
          BackendTyApp resultTy (go fun) tyArg
        BackendRoll resultTy payload ->
          BackendRoll resultTy (go payload)
        BackendUnroll resultTy payload ->
          BackendUnroll resultTy (go payload)
        BackendClosure resultTy entryName captures params body ->
          BackendClosure resultTy entryName (map renameCapture captures) params (go body)
        BackendClosureCall resultTy fun args ->
          BackendClosureCall resultTy (go fun) (map go args)
        BackendConstructWithIdentity resultTy identity name args ->
          BackendConstructWithIdentity resultTy identity (renameName renameConstructs name) (map go args)
        BackendCase resultTy scrutinee alternatives ->
          BackendCase resultTy (go scrutinee) (fmap renameAlternative alternatives)

    renameAlternative (BackendAlternative pattern0 body) =
      BackendAlternative (renamePattern pattern0) (go body)

    renamePattern pattern0 =
      case pattern0 of
        BackendDefaultPattern ->
          BackendDefaultPattern
        BackendConstructorPatternWithBinderIdentities identity name binders ->
          BackendConstructorPatternWithBinderIdentities identity (renameName renamePatterns name) binders

    renameCapture capture =
      capture {backendClosureCaptureExpr = go (backendClosureCaptureExpr capture)}

renameBackendVarReferences :: (String -> Bool) -> String -> BackendExpr -> BackendExpr
renameBackendVarReferences predicate replacement =
  go
  where
    renameName name
      | predicate name = replacement
      | otherwise = name

    go expr =
      case expr of
        BackendVarWithIdentity resultTy identity name ->
          BackendVarWithIdentity resultTy identity (renameName name)
        BackendLit {} -> expr
        BackendLam resultTy name paramTy body ->
          BackendLam resultTy name paramTy (go body)
        BackendApp resultTy fun arg ->
          BackendApp resultTy (go fun) (go arg)
        BackendLet resultTy name bindingTy rhs body ->
          BackendLet resultTy name bindingTy (go rhs) (go body)
        BackendTyAbs resultTy name mbBound body ->
          BackendTyAbs resultTy name mbBound (go body)
        BackendTyApp resultTy fun tyArg ->
          BackendTyApp resultTy (go fun) tyArg
        BackendRoll resultTy payload ->
          BackendRoll resultTy (go payload)
        BackendUnroll resultTy payload ->
          BackendUnroll resultTy (go payload)
        BackendClosure resultTy entryName captures params body ->
          BackendClosure resultTy entryName (map renameCapture captures) params (go body)
        BackendClosureCall resultTy fun args ->
          BackendClosureCall resultTy (go fun) (map go args)
        BackendConstructWithIdentity resultTy identity name args ->
          BackendConstructWithIdentity resultTy identity name (map go args)
        BackendCase resultTy scrutinee alternatives ->
          BackendCase resultTy (go scrutinee) (fmap renameAlternative alternatives)

    renameAlternative (BackendAlternative pattern0 body) =
      BackendAlternative pattern0 (go body)

    renameCapture capture =
      capture {backendClosureCaptureExpr = go (backendClosureCaptureExpr capture)}

intTy :: BackendType
intTy =
  BTBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int")

boolTy :: BackendType
boolTy =
  BTBaseWithIdentity (Just (builtinTypeIdentity "Bool")) (BaseTy "Bool")

unaryIntBackendTy :: BackendType
unaryIntBackendTy =
  BTArrow intTy intTy

intElabTy :: Elab.ElabType
intElabTy =
  Elab.TBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int")

resolvedLocal :: String -> String -> Elab.ElabType -> Elab.ResolvedVar
resolvedLocal ref runtime ty =
  generatedResolvedLocalForName ref runtime ty

boolElabTy :: Elab.ElabType
boolElabTy =
  Elab.TBaseWithIdentity (Just (builtinTypeIdentity "Bool")) (BaseTy "Bool")

polymorphicOptionSourceTy :: SrcType
polymorphicOptionSourceTy =
  STForall
    "a"
    Nothing
    (STArrow (STVar "a") (STCon "Main.Option" (STVar "a" :| [])))

polymorphicOptionSourceView :: DataInfo -> Elab.TypeBinderRef -> TypeView
polymorphicOptionSourceView dataInfo ref =
  (mkTypeView polymorphicOptionSourceTy polymorphicOptionSourceTy)
    { typeViewHeadIdentities = Map.singleton "Main.Option" (dataInfoSymbol dataInfo),
      typeViewBinderIdentities = Map.singleton "a" (Elab.typeBinderRefIdentity ref)
    }

polymorphicOptionElabTy :: Elab.ElabType
polymorphicOptionElabTy =
  testTForall
    "a"
    Nothing
    ( Elab.TArrow
        (testTVar "a")
        (Elab.TCon (BaseTy "Main.Option") (testTVar "a" :| []))
    )

staleSomeInPolymorphicOptionTerm :: CheckedProgram -> Elab.XmlfTerm
staleSomeInPolymorphicOptionTerm checked =
  mkTestTyAbs "a"
    Nothing
    ( mkTestLocalLam
        "x"
        (testTVar "a")
        ( Elab.EApp
            (Elab.ETyInst (resolvedConstructorTerm checked "Main__Some") (Elab.InstApp boolElabTy))
            (mkTestDeferredVar "x")
        )
    )

identityPlaceholderExpectedRef :: Elab.TypeBinderRef
identityPlaceholderExpectedRef =
  backendFixtureTypeRef 9100 "a"

identityPlaceholderTermRef :: Elab.TypeBinderRef
identityPlaceholderTermRef =
  backendFixtureTypeRef 9101 "a"

identityPlaceholderPolymorphicOptionElabTy :: Elab.ElabType
identityPlaceholderPolymorphicOptionElabTy =
  Elab.TForallRef
    identityPlaceholderExpectedRef
    Nothing
    ( Elab.TArrow
        (Elab.TVarRef identityPlaceholderExpectedRef)
        (Elab.TCon (BaseTy "Main.Option") (Elab.TVarRef identityPlaceholderExpectedRef :| []))
    )

identityPlaceholderSomeTerm :: CheckedProgram -> Elab.XmlfTerm
identityPlaceholderSomeTerm checked =
  Elab.ETyAbsRef
    identityPlaceholderTermRef
    Nothing
    ( mkTestLocalLam
        "x"
        (Elab.TVarRef identityPlaceholderTermRef)
        ( Elab.EApp
            (resolvedConstructorTerm checked "Main__Some")
            (mkTestDeferredVar "x")
        )
    )

unaryIntElabTy :: Elab.ElabType
unaryIntElabTy =
  Elab.TArrow intElabTy intElabTy

binaryIntElabTy :: Elab.ElabType
binaryIntElabTy =
  Elab.TArrow intElabTy (Elab.TArrow intElabTy intElabTy)

recursiveCaptureAvoidingElabTy :: Elab.ElabType
recursiveCaptureAvoidingElabTy =
  Elab.TArrow unaryIntElabTy intElabTy

recursiveCaptureAvoidingTerm :: Elab.XmlfTerm
recursiveCaptureAvoidingTerm =
  recursiveCaptureAvoidingTermWith "$evidence_E"

recursiveCaptureAvoidingTermWith :: String -> Elab.XmlfTerm
recursiveCaptureAvoidingTermWith evidenceName =
  mkTestLocalLam
    evidenceName
    unaryIntElabTy
    ( mkTestRecursiveLocalLet
        "loop"
        (schemeFromType unaryIntElabTy)
        (recursiveCaptureAvoidingRhsWith evidenceName)
        (Elab.EApp (mkTestDeferredVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveCaptureAvoidingRhsWith :: String -> Elab.XmlfTerm
recursiveCaptureAvoidingRhsWith evidenceName =
  mkTestLocalLam
    "n"
    intElabTy
    ( mkTestLocalLet
        "before"
        (schemeFromType intElabTy)
        (Elab.EApp (mkTestDeferredVar evidenceName) (mkTestDeferredVar "n"))
        ( mkTestLocalLet
            evidenceName
            (schemeFromType unaryIntElabTy)
            intIdentityXmlfTerm
            (Elab.EApp (mkTestDeferredVar "loop") (mkTestDeferredVar "before"))
        )
    )

recursiveLetRhsRenameElabTy :: Elab.ElabType
recursiveLetRhsRenameElabTy =
  Elab.TArrow unaryIntElabTy intElabTy

recursiveIntLiftTerm :: Elab.XmlfTerm
recursiveIntLiftTerm =
  mkTestRecursiveLocalLet
    "loop"
    (schemeFromType unaryIntElabTy)
    (mkTestLocalLam "n" intElabTy (Elab.EApp (mkTestDeferredVar "loop") (mkTestDeferredVar "n")))
    (Elab.ELit (LInt 0))

recursiveLetRhsRenameTerm :: Elab.XmlfTerm
recursiveLetRhsRenameTerm =
  mkTestLocalLam
    "$evidence_E"
    unaryIntElabTy
    ( mkTestRecursiveLocalLet
        "loop"
        (schemeFromType unaryIntElabTy)
        recursiveLetRhsRenameRhs
        (Elab.EApp (mkTestDeferredVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveLetRhsRenameRhs :: Elab.XmlfTerm
recursiveLetRhsRenameRhs =
  mkTestLocalLam
    "n"
    intElabTy
    ( mkTestLocalLet
        "before"
        (schemeFromType intElabTy)
        (Elab.EApp (mkTestDeferredVar "$evidence_E") (mkTestDeferredVar "n"))
        ( mkTestLocalLet
            "$evidence_E"
            (schemeFromType unaryIntElabTy)
            (mkTestDeferredVar "$evidence_E")
            (Elab.EApp (mkTestDeferredVar "loop") (mkTestDeferredVar "before"))
        )
    )

recursiveTypeCaptureElabTy :: Elab.ElabType
recursiveTypeCaptureElabTy =
  testTForall "a" Nothing intElabTy

recursiveTypeCaptureTerm :: Elab.XmlfTerm
recursiveTypeCaptureTerm =
  mkTestTyAbs "a"
    Nothing
    ( mkTestRecursiveLocalLet
        "loop"
        (schemeFromType unaryIntElabTy)
        recursiveTypeCaptureRhs
        (Elab.EApp (mkTestDeferredVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveTypeCaptureRhs :: Elab.XmlfTerm
recursiveTypeCaptureRhs =
  mkTestLocalLam
    "n"
    intElabTy
    ( mkTestLocalLet
        "ignored"
        (schemeFromType intElabTy)
        recursiveTypeOnlyInstantiation
        (Elab.EApp (mkTestDeferredVar "loop") (mkTestDeferredVar "n"))
    )

recursiveTypeOnlyInstantiation :: Elab.XmlfTerm
recursiveTypeOnlyInstantiation =
  Elab.ETyInst
    (mkTestTyAbs "b" Nothing (Elab.ELit (LInt 0)))
    (Elab.InstApp (testTVar "a"))

recursiveSameNamedTypeCaptureElabTy :: Elab.ElabType
recursiveSameNamedTypeCaptureElabTy =
  Elab.TForallRef
    sameNamedOuterTypeRef
    Nothing
    (Elab.TForallRef sameNamedInnerTypeRef Nothing intElabTy)

recursiveSameNamedTypeCaptureTerm :: Elab.XmlfTerm
recursiveSameNamedTypeCaptureTerm =
  Elab.ETyAbsRef
    sameNamedOuterTypeRef
    Nothing
    ( Elab.ETyAbsRef
        sameNamedInnerTypeRef
        Nothing
        ( mkTestRecursiveLocalLet
            "loop"
            (schemeFromType unaryIntElabTy)
            recursiveSameNamedTypeCaptureRhs
            (Elab.EApp (mkTestDeferredVar "loop") (Elab.ELit (LInt 0)))
        )
    )

recursiveSameNamedTypeCaptureRhs :: Elab.XmlfTerm
recursiveSameNamedTypeCaptureRhs =
  mkTestLocalLam
    "n"
    intElabTy
    ( mkTestLocalLet
        "ignored"
        (schemeFromType intElabTy)
        recursiveSameNamedTypeOnlyInstantiation
        (Elab.EApp (mkTestDeferredVar "loop") (mkTestDeferredVar "n"))
    )

recursiveSameNamedTypeOnlyInstantiation :: Elab.XmlfTerm
recursiveSameNamedTypeOnlyInstantiation =
  Elab.ETyInst
    (mkTestTyAbs "b" Nothing (Elab.ELit (LInt 0)))
    (Elab.InstApp (Elab.TVarRef sameNamedInnerTypeRef))

recursiveSameNamedTermCaptureElabTy :: Elab.ElabType
recursiveSameNamedTermCaptureElabTy =
  Elab.TArrow unaryIntElabTy intElabTy

recursiveSameNamedTermCaptureTerm :: Elab.XmlfTerm
recursiveSameNamedTermCaptureTerm =
  let outerEvidence = generatedResolvedLocal 900 "$evidence_E" "$evidence_E" unaryIntElabTy
      loop = generatedResolvedLocal 901 "$evidence_E" "$evidence_E" unaryIntElabTy
      n = generatedResolvedLocal 902 "n" "n" intElabTy
      rhs =
        Elab.ELam
          n
          ( Elab.EApp
              (Elab.EVarNode loop)
              (Elab.EApp (Elab.EVarNode outerEvidence) (Elab.EVarNode n))
          )
   in Elab.ELam
        outerEvidence
        ( Elab.ELet
            loop
            (schemeFromType unaryIntElabTy)
            rhs
            (Elab.EApp (Elab.EVarNode loop) (Elab.ELit (LInt 0)))
        )

sameNamedTypeAbsSourceTy :: SrcType
sameNamedTypeAbsSourceTy =
  STForall "a" Nothing (STForall "a" Nothing (STBase "Int"))

sameNamedTypeAbsElabTy :: Elab.ElabType
sameNamedTypeAbsElabTy =
  Elab.TForallRef
    sameNamedOuterTypeRef
    Nothing
    (Elab.TForallRef sameNamedInnerTypeRef Nothing intElabTy)

sameNamedTypeAbsTerm :: Elab.XmlfTerm
sameNamedTypeAbsTerm =
  Elab.ETyAbsRef
    sameNamedOuterTypeRef
    Nothing
    (Elab.ETyAbsRef sameNamedInnerTypeRef Nothing (Elab.ELit (LInt 1)))

sameNamedBoundedWrapSourceTy :: SrcType
sameNamedBoundedWrapSourceTy =
  STForall "a" Nothing (STForall "a" Nothing (STArrow (STVar "a") (STBase "Pack")))

sameNamedBoundedWrapElabTy :: Elab.ElabType -> Elab.ElabType
sameNamedBoundedWrapElabTy resultTy =
  Elab.TForallRef
    sameNamedOuterTypeRef
    Nothing
    ( Elab.TForallRef
        sameNamedInnerTypeRef
        (Just intElabBoundTy)
        (Elab.TArrow (Elab.TVarRef sameNamedInnerTypeRef) resultTy)
    )

sameNamedBoundedWrapTerm :: CheckedProgram -> Elab.XmlfTerm
sameNamedBoundedWrapTerm checked =
  Elab.ETyAbsRef
    sameNamedOuterTypeRef
    Nothing
    ( Elab.ETyAbsRef
        sameNamedInnerTypeRef
        (Just intElabBoundTy)
        ( mkTestLocalLam
            "x"
            (Elab.TVarRef sameNamedInnerTypeRef)
            (Elab.EApp (resolvedConstructorTerm checked "Main__Pack") (mkTestDeferredVar "x"))
        )
    )

sameNamedOuterTypeRef :: Elab.TypeBinderRef
sameNamedOuterTypeRef =
  sameNamedTypeRef 9000

sameNamedInnerTypeRef :: Elab.TypeBinderRef
sameNamedInnerTypeRef =
  sameNamedTypeRef 9001

sameNamedTypeRef :: Int -> Elab.TypeBinderRef
sameNamedTypeRef key =
  backendFixtureTypeRef key "a"

backendFixtureTypeRef :: Int -> String -> Elab.TypeBinderRef
backendFixtureTypeRef key name =
  Elab.typeBinderRefFromIdentity (Elab.typeBinderIdentityFromNode (NodeId key)) name

recursiveTypeBoundScopeElabTy :: Elab.ElabType
recursiveTypeBoundScopeElabTy =
  Elab.TForallRef recursiveTypeBoundScopeOuterRef Nothing intElabTy

recursiveTypeBoundScopeTerm :: Elab.XmlfTerm
recursiveTypeBoundScopeTerm =
  Elab.ETyAbsRef
    recursiveTypeBoundScopeOuterRef
    Nothing
    ( mkTestRecursiveLocalLet
        "loop"
        (schemeFromType unaryIntElabTy)
        recursiveTypeBoundScopeRhs
        (Elab.EApp (mkTestDeferredVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveTypeBoundScopeRhs :: Elab.XmlfTerm
recursiveTypeBoundScopeRhs =
  mkTestLocalLam
    "n"
    intElabTy
    ( Elab.ETyInst
        ( Elab.ETyAbsRef
            recursiveTypeBoundScopeInnerRef
            (Just (dependentArrowElabBoundTy (Elab.TVarRef recursiveTypeBoundScopeOuterRef)))
            (Elab.EApp (mkTestDeferredVar "loop") (mkTestDeferredVar "n"))
        )
        (Elab.InstApp (dependentArrowElabTy (Elab.TVarRef recursiveTypeBoundScopeOuterRef)))
    )

recursiveTypeBoundScopeOuterRef :: Elab.TypeBinderRef
recursiveTypeBoundScopeOuterRef =
  backendFixtureTypeRef 9010 "a"

recursiveTypeBoundScopeInnerRef :: Elab.TypeBinderRef
recursiveTypeBoundScopeInnerRef =
  backendFixtureTypeRef 9011 "a"

recursiveNestedTypeBoundScopeElabTy :: Elab.ElabType
recursiveNestedTypeBoundScopeElabTy =
  Elab.TForallRef recursiveNestedTypeBoundScopeOuterRef Nothing intElabTy

recursiveNestedTypeBoundScopeTerm :: Elab.XmlfTerm
recursiveNestedTypeBoundScopeTerm =
  Elab.ETyAbsRef
    recursiveNestedTypeBoundScopeOuterRef
    Nothing
    ( mkTestRecursiveLocalLet
        "loop"
        (schemeFromType unaryIntElabTy)
        recursiveNestedTypeBoundScopeRhs
        (Elab.EApp (mkTestDeferredVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveNestedTypeBoundScopeRhs :: Elab.XmlfTerm
recursiveNestedTypeBoundScopeRhs =
  mkTestLocalLam
    "n"
    intElabTy
    ( Elab.ETyInst
        ( Elab.ETyAbsRef
            recursiveNestedTypeBoundScopeFirstInnerRef
            (Just (dependentArrowElabBoundTy (Elab.TVarRef recursiveNestedTypeBoundScopeOuterRef)))
            ( Elab.ETyInst
                ( Elab.ETyAbsRef
                    recursiveNestedTypeBoundScopeSecondInnerRef
                    (Just (dependentArrowElabBoundTy (Elab.TVarRef recursiveNestedTypeBoundScopeOuterRef)))
                    (Elab.EApp (mkTestDeferredVar "loop") (mkTestDeferredVar "n"))
                )
                (Elab.InstApp (dependentArrowElabTy (Elab.TVarRef recursiveNestedTypeBoundScopeOuterRef)))
            )
        )
        (Elab.InstApp (dependentArrowElabTy (Elab.TVarRef recursiveNestedTypeBoundScopeOuterRef)))
    )

recursiveNestedTypeBoundScopeOuterRef :: Elab.TypeBinderRef
recursiveNestedTypeBoundScopeOuterRef =
  backendFixtureTypeRef 9020 "a"

recursiveNestedTypeBoundScopeFirstInnerRef :: Elab.TypeBinderRef
recursiveNestedTypeBoundScopeFirstInnerRef =
  backendFixtureTypeRef 9021 "a"

recursiveNestedTypeBoundScopeSecondInnerRef :: Elab.TypeBinderRef
recursiveNestedTypeBoundScopeSecondInnerRef =
  backendFixtureTypeRef 9022 "a"

recursiveShadowedLetElabTy :: Elab.ElabType
recursiveShadowedLetElabTy =
  intElabTy

recursiveShadowedLetTerm :: Elab.XmlfTerm
recursiveShadowedLetTerm =
  mkTestLocalLet
    "f"
    (schemeFromType unaryIntElabTy)
    intIdentityXmlfTerm
    ( mkTestRecursiveLocalLet
        "f"
        (schemeFromType unaryIntElabTy)
        ( mkTestLocalLam
            "n"
            intElabTy
            (Elab.EApp (mkTestDeferredVar "f") (mkTestDeferredVar "n"))
        )
        (Elab.EApp (mkTestDeferredVar "f") (Elab.ELit (LInt 0)))
    )

recursiveLexicalTypeOrderElabTy :: Elab.ElabType
recursiveLexicalTypeOrderElabTy =
  testTForall
    "z"
    Nothing
    ( testTForall
        "a"
        Nothing
        intElabTy
    )

recursiveLexicalTypeOrderTerm :: Elab.XmlfTerm
recursiveLexicalTypeOrderTerm =
  mkTestTyAbs "z"
    Nothing
    ( mkTestTyAbs "a"
        Nothing
        ( mkTestRecursiveLocalLet
            "loop"
            (schemeFromType unaryIntElabTy)
            recursiveLexicalTypeOrderRhs
            (Elab.EApp (mkTestDeferredVar "loop") (Elab.ELit (LInt 0)))
        )
    )

recursiveLexicalTypeOrderRhs :: Elab.XmlfTerm
recursiveLexicalTypeOrderRhs =
  mkTestLocalLam
    "n"
    intElabTy
    ( mkTestLocalLet
        "captureA"
        (schemeFromType intElabTy)
        ( Elab.ETyInst
            (mkTestTyAbs "b" Nothing (Elab.ELit (LInt 0)))
            (Elab.InstApp (testTVar "a"))
        )
        ( mkTestLocalLet
            "captureZ"
            (schemeFromType intElabTy)
            ( Elab.ETyInst
                (mkTestTyAbs "b" Nothing (Elab.ELit (LInt 0)))
                (Elab.InstApp (testTVar "z"))
            )
            (Elab.EApp (mkTestDeferredVar "loop") (mkTestDeferredVar "n"))
        )
    )

liftedRecursiveHelpersClosureNameTerm :: CheckedProgram -> Elab.XmlfTerm
liftedRecursiveHelpersClosureNameTerm checked =
  mkTestRecursiveLocalLet
    "left"
    (schemeFromType unaryIntElabTy)
    (liftedRecursiveHelpersClosureNameRhs checked "left")
    ( mkTestRecursiveLocalLet
        "right"
        (schemeFromType unaryIntElabTy)
        (liftedRecursiveHelpersClosureNameRhs checked "right")
        (Elab.EApp (mkTestDeferredVar "right") (Elab.ELit (LInt 0)))
    )

liftedRecursiveHelpersClosureNameRhs :: CheckedProgram -> String -> Elab.XmlfTerm
liftedRecursiveHelpersClosureNameRhs checked selfName =
  mkTestLocalLam
    "n"
    intElabTy
    ( mkTestLocalLet
        "f"
        (schemeFromType unaryIntElabTy)
        (mkTestLocalLam "x" intElabTy (mkTestDeferredVar "x"))
        ( mkTestLocalLet
            "ignored"
            (schemeFromType intElabTy)
            (Elab.EApp (mkTestDeferredVar selfName) (mkTestDeferredVar "n"))
            (Elab.EApp (resolvedBindingTerm checked "Main__use") (mkTestDeferredVar "f"))
        )
    )

intIdentityXmlfTerm :: Elab.XmlfTerm
intIdentityXmlfTerm =
  mkTestLocalLam "m" intElabTy (mkTestDeferredVar "m")

intElabBoundTy :: Elab.BoundType
intElabBoundTy =
  Elab.TBase (BaseTy "Int")

boundedWrapElabTy :: Elab.ElabType -> Elab.ElabType
boundedWrapElabTy resultTy =
  testTForall "b" (Just intElabBoundTy) (Elab.TArrow (testTVar "b") resultTy)

boundedWrapTerm :: CheckedProgram -> Elab.XmlfTerm
boundedWrapTerm checked =
  mkTestTyAbs "b"
    (Just intElabBoundTy)
    ( mkTestLocalLam
        "x"
        (testTVar "b")
        (Elab.EApp (resolvedConstructorTerm checked "Main__Pack") (mkTestDeferredVar "x"))
    )

dependentBoundedWrapElabTy :: Elab.ElabType -> Elab.ElabType
dependentBoundedWrapElabTy resultTy =
  testTForall
    "z"
    (Just intElabBoundTy)
    ( testTForall
        "b"
        (Just (dependentArrowElabBoundTy (testTVar "z")))
        (Elab.TArrow (testTVar "b") resultTy)
    )

dependentBoundedWrapTerm :: CheckedProgram -> Elab.XmlfTerm
dependentBoundedWrapTerm checked =
  mkTestTyAbs "z"
    (Just intElabBoundTy)
    ( mkTestTyAbs "b"
        (Just (dependentArrowElabBoundTy (testTVar "z")))
        ( mkTestLocalLam
            "x"
            (testTVar "b")
            (Elab.EApp (resolvedConstructorTerm checked "Main__Pack") (mkTestDeferredVar "x"))
        )
    )

dependentArrowElabBoundTy :: Elab.ElabType -> Elab.BoundType
dependentArrowElabBoundTy ty =
  Elab.TArrow ty ty

dependentArrowElabTy :: Elab.ElabType -> Elab.ElabType
dependentArrowElabTy ty =
  Elab.TArrow ty ty

polymorphicIdentityElabTy :: Elab.ElabType
polymorphicIdentityElabTy =
  testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a"))

alphaEquivalentIdentityElabTy :: Elab.ElabType
alphaEquivalentIdentityElabTy =
  testTForall "b" Nothing (Elab.TArrow (testTVar "b") (testTVar "b"))

repeatedPolymorphicParameterCaseTerm :: CheckedProgram -> Elab.XmlfTerm
repeatedPolymorphicParameterCaseTerm checked =
  Elab.EApp
    (Elab.ETyInst (Elab.EUnroll pairScrutinee) (Elab.InstApp boolElabTy))
    ( mkTestLocalLam
        "$pair_f"
        polymorphicIdentityElabTy
        (mkTestLocalLam "$pair_g" alphaEquivalentIdentityElabTy (Elab.ELit (LBool True)))
    )
  where
    pairScrutinee =
      Elab.EApp
        (Elab.EApp (resolvedConstructorTerm checked "Main__Pair") polymorphicIdentityTerm)
        (Elab.ETyInst alphaEquivalentIdentityTerm Elab.InstId)

polymorphicIdentityTerm :: Elab.XmlfTerm
polymorphicIdentityTerm =
  mkTestTyAbs "a"
    Nothing
    (mkTestLocalLam "$poly_id_a" (testTVar "a") (mkTestDeferredVar "$poly_id_a"))

alphaEquivalentIdentityTerm :: Elab.XmlfTerm
alphaEquivalentIdentityTerm =
  mkTestTyAbs "b"
    Nothing
    (mkTestLocalLam "$poly_id_b" (testTVar "b") (mkTestDeferredVar "$poly_id_b"))

unqualifiedStructuralNullaryConstructorTerm :: Elab.XmlfTerm
unqualifiedStructuralNullaryConstructorTerm =
  structuralNullaryConstructorTermWithResult unqualifiedStructuralTElabTy

structuralNullaryConstructorTermWithResult :: Elab.ElabType -> Elab.XmlfTerm
structuralNullaryConstructorTermWithResult resultTy =
  Elab.ERoll
    resultTy
    ( mkTestTyAbs "$T_result"
        Nothing
        (mkTestLocalLam "$T_handler" (testTVar "$T_result") (mkTestDeferredVar "$T_handler"))
    )

unqualifiedStructuralTElabTy :: Elab.ElabType
unqualifiedStructuralTElabTy =
  testTMu
    "$T_self"
    ( testTForall
        "$T_result"
        Nothing
        (Elab.TArrow (testTVar "$T_result") (testTVar "$T_result"))
    )

nonStructuralSelfIdentityTElabTy :: Elab.ElabType
nonStructuralSelfIdentityTElabTy =
  Elab.tMuWithRef
    (backendFixtureTypeRef 9110 "$T_self")
    ( testTForall
        "$T_result"
        Nothing
        (Elab.TArrow (testTVar "$T_result") (testTVar "$T_result"))
    )

mapMainBinding :: (CheckedBinding -> CheckedBinding) -> CheckedProgram -> CheckedProgram
mapMainBinding f checked =
  mapBinding (checkedProgramMain checked) f checked

renameCheckedProgramMainRuntimeName :: String -> CheckedProgram -> CheckedProgram
renameCheckedProgramMainRuntimeName replacement checked =
  checked
    { checkedProgramMainResolvedVar =
        (checkedProgramMainResolvedVar checked)
          { Elab.resolvedVarRuntimeName = replacement
          }
    }

addDataInfo :: DataInfo -> CheckedProgram -> CheckedProgram
addDataInfo dataInfo checked =
  checked
    { checkedProgramModules =
        case checkedProgramModules checked of
          [] -> []
          checkedModule : rest ->
            checkedModule
              { checkedModuleData = Map.insert (dataInfoSymbol dataInfo) dataInfo (checkedModuleData checkedModule)
              }
              : rest
    }

replaceDataInfoSymbol :: SymbolIdentity -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceDataInfoSymbol target replacement checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleData = fmap updateDataInfo (checkedModuleData checkedModule)
        }

    updateDataInfo dataInfo
      | dataInfoSymbol dataInfo == target =
          dataInfo {dataInfoSymbol = replacement}
      | otherwise =
          dataInfo

replaceConstructorInfoSymbol :: SymbolIdentity -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceConstructorInfoSymbol target replacement checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleData = fmap updateDataInfo (checkedModuleData checkedModule)
        }

    updateDataInfo dataInfo =
      dataInfo {dataConstructors = map updateConstructorInfo (dataConstructors dataInfo)}

    updateConstructorInfo constructorInfo
      | ctorInfoSymbol constructorInfo == target =
          constructorInfo {ctorInfoSymbol = replacement}
      | otherwise =
          constructorInfo

mapBinding :: String -> (CheckedBinding -> CheckedBinding) -> CheckedProgram -> CheckedProgram
mapBinding target f checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleBindings =
            map updateBinding (checkedModuleBindings checkedModule)
        }

    updateBinding binding
      | checkedBindingName binding == target = f binding
      | otherwise = binding

renameCheckedModuleName :: String -> String -> CheckedProgram -> CheckedProgram
renameCheckedModuleName oldName newName checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule
      | checkedModuleName checkedModule == oldName =
          checkedModule {checkedModuleName = newName}
      | otherwise =
          checkedModule

resolvedConstructorTerm :: CheckedProgram -> String -> Elab.XmlfTerm
resolvedConstructorTerm checked runtimeName =
  case findConstructorInfo runtimeName checked of
    Just ctorInfo ->
      Elab.EVarNode
        Elab.ResolvedVar
          { Elab.resolvedVarRuntimeName = ctorRuntimeName ctorInfo,
            Elab.resolvedVarType = Elab.TBottom,
            Elab.resolvedVarDetails = ConstructorId (constructorRefFromInfo ctorInfo)
          }
    Nothing ->
      error ("missing checked constructor metadata for " ++ show runtimeName)

resolvedBindingTerm :: CheckedProgram -> String -> Elab.XmlfTerm
resolvedBindingTerm checked bindingName =
  case findCheckedBinding bindingName checked of
    Just binding ->
      Elab.EVarNode (checkedBindingResolvedVar binding)
    Nothing ->
      error ("missing checked binding metadata for " ++ show bindingName)

findCheckedBinding :: String -> CheckedProgram -> Maybe CheckedBinding
findCheckedBinding bindingName checked =
  find
    ((== bindingName) . checkedBindingName)
    [ binding
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule
    ]

findConstructorInfo :: String -> CheckedProgram -> Maybe ConstructorInfo
findConstructorInfo runtimeName checked =
  find
    ((== runtimeName) . ctorRuntimeName)
    [ ctorInfo
      | checkedModule <- checkedProgramModules checked,
        dataInfo <- toList (checkedModuleData checkedModule),
        ctorInfo <- dataConstructors dataInfo
    ]

staleTopLevelOccurrenceRuntime :: String -> String -> Elab.XmlfTerm -> Elab.XmlfTerm
staleTopLevelOccurrenceRuntime target replacement =
  go
  where
    go term =
      case term of
        Elab.EVarNode resolved
          | Elab.resolvedVarReferenceName resolved == target ->
              Elab.EVarNode (resolved {Elab.resolvedVarRuntimeName = replacement})
        Elab.ELam resolved body ->
          Elab.ELam resolved (go body)
        Elab.EApp fun arg ->
          Elab.EApp (go fun) (go arg)
        Elab.ELet resolved scheme rhs body ->
          Elab.ELet resolved scheme (go rhs) (go body)
        Elab.ETyAbsRef ref mbBound body ->
          Elab.ETyAbsRef ref mbBound (go body)
        Elab.ETyInst inner inst ->
          Elab.ETyInst (go inner) inst
        Elab.ERoll ty body ->
          Elab.ERoll ty (go body)
        Elab.EUnroll body ->
          Elab.EUnroll (go body)
        _ ->
          term

staleLocalOccurrenceRuntimes :: String -> Elab.XmlfTerm -> Elab.XmlfTerm
staleLocalOccurrenceRuntimes replacement =
  go
  where
    go term =
      case term of
        Elab.EVarNode resolved
          | Elab.resolvedVarIsLocal resolved ->
              Elab.EVarNode (resolved {Elab.resolvedVarRuntimeName = replacement})
        Elab.ELam resolved body ->
          Elab.ELam resolved (go body)
        Elab.EApp fun arg ->
          Elab.EApp (go fun) (go arg)
        Elab.ELet resolved scheme rhs body ->
          Elab.ELet resolved scheme (go rhs) (go body)
        Elab.ETyAbsRef ref mbBound body ->
          Elab.ETyAbsRef ref mbBound (go body)
        Elab.ETyInst inner inst ->
          Elab.ETyInst (go inner) inst
        Elab.ERoll ty body ->
          Elab.ERoll ty (go body)
        Elab.EUnroll body ->
          Elab.EUnroll (go body)
        _ ->
          term

rewriteFirstLetBindingType :: Elab.ElabType -> Elab.XmlfTerm -> Elab.XmlfTerm
rewriteFirstLetBindingType replacementTy =
  go
  where
    go term =
      case term of
        Elab.ELet resolved _ rhs body ->
          Elab.ELet
            (Elab.mapResolvedVarType (const replacementTy) resolved)
            (schemeFromType replacementTy)
            rhs
            body
        Elab.ELam resolved body ->
          Elab.ELam resolved (go body)
        Elab.EApp fun arg ->
          Elab.EApp (go fun) (go arg)
        Elab.ETyAbsRef ref mbBound body ->
          Elab.ETyAbsRef ref mbBound (go body)
        Elab.ETyInst inner inst ->
          Elab.ETyInst (go inner) inst
        Elab.ERoll ty body ->
          Elab.ERoll ty (go body)
        Elab.EUnroll body ->
          Elab.EUnroll (go body)
        _ ->
          term

rewriteFirstLamBindingType :: Elab.ElabType -> Elab.XmlfTerm -> Elab.XmlfTerm
rewriteFirstLamBindingType replacementTy term =
  case term of
    Elab.ELam resolved body ->
      Elab.ELam (Elab.mapResolvedVarType (const replacementTy) resolved) body
    _ ->
      term

replaceFunctionDomain :: Elab.ElabType -> Elab.ElabType -> Elab.ElabType
replaceFunctionDomain replacementDomain ty =
  case ty of
    Elab.TArrow _ cod -> Elab.TArrow replacementDomain cod
    _ -> ty

withConstructorResult :: String -> SrcType -> CheckedProgram -> CheckedProgram
withConstructorResult runtimeName resultTy checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleData = fmap updateDataInfo (checkedModuleData checkedModule)
        }

    updateDataInfo dataInfo =
      dataInfo {dataConstructors = map updateConstructorInfo (dataConstructors dataInfo)}

    updateConstructorInfo constructorInfo
      | ctorRuntimeName constructorInfo == runtimeName =
          constructorInfo
            { ctorTypeView =
                (ctorTypeView constructorInfo)
                  { typeViewDisplay =
                      foldr
                        (\(name, mbBound) body -> STForall name (fmap SrcBound mbBound) body)
                        (foldr STArrow resultTy (ctorArgs constructorInfo))
                        (ctorForalls constructorInfo)
                  }
            }
      | otherwise =
          constructorInfo

withConstructorTypeView :: String -> TypeView -> CheckedProgram -> CheckedProgram
withConstructorTypeView runtimeName view checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleData = fmap updateDataInfo (checkedModuleData checkedModule)
        }

    updateDataInfo dataInfo =
      dataInfo {dataConstructors = map updateConstructorInfo (dataConstructors dataInfo)}

    updateConstructorInfo constructorInfo
      | ctorRuntimeName constructorInfo == runtimeName =
          constructorInfo {ctorTypeView = view}
      | otherwise =
          constructorInfo

withConstructorDisplayType :: String -> SrcType -> CheckedProgram -> CheckedProgram
withConstructorDisplayType runtimeName displayTy checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleData = fmap updateDataInfo (checkedModuleData checkedModule)
        }

    updateDataInfo dataInfo =
      dataInfo {dataConstructors = map updateConstructorInfo (dataConstructors dataInfo)}

    updateConstructorInfo constructorInfo
      | ctorRuntimeName constructorInfo == runtimeName =
          constructorInfo
            { ctorTypeView =
                (ctorTypeView constructorInfo)
                  { typeViewDisplay = displayTy
                  }
            }
      | otherwise =
          constructorInfo

renameCheckedConstructorRuntimeNamesWhere :: (String -> Bool) -> String -> CheckedProgram -> CheckedProgram
renameCheckedConstructorRuntimeNamesWhere predicate replacement checked =
  checked
    { checkedProgramModules =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleData = fmap updateDataInfo (checkedModuleData checkedModule)
        }

    updateDataInfo dataInfo =
      dataInfo {dataConstructors = map updateConstructorInfo (dataConstructors dataInfo)}

    updateConstructorInfo constructorInfo
      | predicate (ctorRuntimeName constructorInfo) =
          constructorInfo {ctorRuntimeName = replacement}
      | otherwise =
          constructorInfo

mapBackendMainBinding :: (BackendBinding -> BackendBinding) -> BackendProgram -> BackendProgram
mapBackendMainBinding f backend =
  backend
    { backendProgramModules =
        map updateModule (backendProgramModules backend)
    }
  where
    updateModule backendModule =
      backendModule
        { backendModuleBindings =
            map updateBinding (backendModuleBindings backendModule)
        }

    updateBinding binding
      | backendBindingName binding == backendProgramMain backend = f binding
      | otherwise = binding

addStaleConstructorHeadInstantiation :: String -> Elab.ElabType -> Elab.XmlfTerm -> Elab.XmlfTerm
addStaleConstructorHeadInstantiation target staleTy =
  go
  where
    go term =
      case collectAppsElab term of
        (headTerm, args)
          | isTargetConstructorHead headTerm ->
              rebuildAppsElab (Elab.ETyInst headTerm (Elab.InstApp staleTy)) args
        _ ->
          case term of
            Elab.ELam resolved body ->
              Elab.ELam resolved (go body)
            Elab.EApp fun arg ->
              Elab.EApp (go fun) (go arg)
            Elab.ELet resolved scheme rhs body ->
              Elab.ELet resolved scheme (go rhs) (go body)
            Elab.ETyAbsRef ref mbBound body ->
              Elab.ETyAbsRef ref mbBound (go body)
            Elab.ETyInst inner inst ->
              Elab.ETyInst (go inner) inst
            Elab.ERoll ty body ->
              Elab.ERoll ty (go body)
            Elab.EUnroll body ->
              Elab.EUnroll (go body)
            _ ->
              term

    isTargetConstructorHead headTerm =
      case stripElabTypeInsts headTerm of
        Elab.EVarNode resolved -> Elab.resolvedVarReferenceName resolved == target
        _ -> False

replaceConstructorHeadInstantiation :: String -> Elab.ElabType -> Elab.XmlfTerm -> Elab.XmlfTerm
replaceConstructorHeadInstantiation target replacementTy =
  go
  where
    go term =
      case collectAppsElab term of
        (headTerm, args)
          | isTargetConstructorHead headTerm ->
              rebuildAppsElab (Elab.ETyInst (stripElabTypeInsts headTerm) (Elab.InstApp replacementTy)) args
        _ ->
          case term of
            Elab.ELam resolved body ->
              Elab.ELam resolved (go body)
            Elab.EApp fun arg ->
              Elab.EApp (go fun) (go arg)
            Elab.ELet resolved scheme rhs body ->
              Elab.ELet resolved scheme (go rhs) (go body)
            Elab.ETyAbsRef ref mbBound body ->
              Elab.ETyAbsRef ref mbBound (go body)
            Elab.ETyInst inner inst ->
              Elab.ETyInst (go inner) inst
            Elab.ERoll ty body ->
              Elab.ERoll ty (go body)
            Elab.EUnroll body ->
              Elab.EUnroll (go body)
            _ ->
              term

    isTargetConstructorHead headTerm =
      case stripElabTypeInsts headTerm of
        Elab.EVarNode resolved -> Elab.resolvedVarReferenceName resolved == target
        _ -> False

addStructuralConstructorHeadInstantiation :: Elab.ElabType -> Elab.XmlfTerm -> Elab.XmlfTerm
addStructuralConstructorHeadInstantiation staleTy =
  go
  where
    go term =
      case collectAppsElab term of
        (headTerm@Elab.ERoll {}, args) ->
          rebuildAppsElab (Elab.ETyInst headTerm (Elab.InstApp staleTy)) args
        _ ->
          case term of
            Elab.ELam resolved body ->
              Elab.ELam resolved (go body)
            Elab.EApp fun arg ->
              Elab.EApp (go fun) (go arg)
            Elab.ELet resolved scheme rhs body ->
              Elab.ELet resolved scheme (go rhs) (go body)
            Elab.ETyAbsRef ref mbBound body ->
              Elab.ETyAbsRef ref mbBound (go body)
            Elab.ETyInst inner inst ->
              Elab.ETyInst (go inner) inst
            Elab.ERoll ty body ->
              Elab.ERoll ty (go body)
            Elab.EUnroll body ->
              Elab.EUnroll (go body)
            _ ->
              term

stripElabTypeInsts :: Elab.XmlfTerm -> Elab.XmlfTerm
stripElabTypeInsts term =
  case term of
    Elab.ETyInst inner _ -> stripElabTypeInsts inner
    other -> other

wrapCaseHandlersWithTypeWrappers :: Elab.XmlfTerm -> Elab.XmlfTerm
wrapCaseHandlersWithTypeWrappers term =
  case collectAppsElab term of
    (headTerm@(Elab.ETyInst (Elab.EUnroll _) _), handlers@(_ : _)) ->
      rebuildAppsElab headTerm (map wrapHandler handlers)
    _ ->
      case term of
        Elab.ELam resolved body ->
          Elab.ELam resolved (wrapCaseHandlersWithTypeWrappers body)
        Elab.EApp fun arg ->
          Elab.EApp (wrapCaseHandlersWithTypeWrappers fun) (wrapCaseHandlersWithTypeWrappers arg)
        Elab.ELet resolved scheme rhs body ->
          Elab.ELet resolved scheme (wrapCaseHandlersWithTypeWrappers rhs) (wrapCaseHandlersWithTypeWrappers body)
        Elab.ETyAbsRef ref mbBound body ->
          Elab.ETyAbsRef ref mbBound (wrapCaseHandlersWithTypeWrappers body)
        Elab.ETyInst inner inst ->
          Elab.ETyInst (wrapCaseHandlersWithTypeWrappers inner) inst
        Elab.ERoll ty body ->
          Elab.ERoll ty (wrapCaseHandlersWithTypeWrappers body)
        Elab.EUnroll body ->
          Elab.EUnroll (wrapCaseHandlersWithTypeWrappers body)
        _ ->
          term
  where
    wrapHandler handler =
      mkTestTyAbs "$case_handler_a" Nothing (Elab.ETyInst handler (Elab.InstApp intElabTy))

replaceCaseHandlerBodiesAfterLams :: Int -> Elab.XmlfTerm -> Elab.XmlfTerm -> Elab.XmlfTerm
replaceCaseHandlerBodiesAfterLams lamCount replacement term =
  case collectAppsElab term of
    (headTerm@(Elab.ETyInst (Elab.EUnroll _) _), handlers@(_ : _)) ->
      rebuildAppsElab headTerm (map (replaceHandlerBody lamCount) handlers)
    _ ->
      case term of
        Elab.ELam resolved body ->
          Elab.ELam resolved (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.EApp fun arg ->
          Elab.EApp (replaceCaseHandlerBodiesAfterLams lamCount replacement fun) (replaceCaseHandlerBodiesAfterLams lamCount replacement arg)
        Elab.ELet resolved scheme rhs body ->
          Elab.ELet resolved scheme (replaceCaseHandlerBodiesAfterLams lamCount replacement rhs) (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.ETyAbsRef ref mbBound body ->
          Elab.ETyAbsRef ref mbBound (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.ETyInst inner inst ->
          Elab.ETyInst (replaceCaseHandlerBodiesAfterLams lamCount replacement inner) inst
        Elab.ERoll ty body ->
          Elab.ERoll ty (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.EUnroll body ->
          Elab.EUnroll (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        _ ->
          term
  where
    replaceHandlerBody remaining handler
      | remaining <= 0 = replacement
      | otherwise =
          case handler of
            Elab.ELam resolved body ->
              Elab.ELam resolved (replaceHandlerBody (remaining - 1) body)
            _ ->
              handler

instantiatedIntIdentity :: Elab.XmlfTerm
instantiatedIntIdentity =
  Elab.ETyInst
    ( mkTestTyAbs "$case_body_a"
        Nothing
        (mkTestLocalLam "$case_body_x" (testTVar "$case_body_a") (mkTestDeferredVar "$case_body_x"))
    )
    (Elab.InstApp intElabTy)

alphaEquivalentIdentityInstId :: Elab.XmlfTerm
alphaEquivalentIdentityInstId =
  Elab.ETyInst
    ( mkTestTyAbs "$case_body_b"
        Nothing
        (mkTestLocalLam "$case_body_y" (testTVar "$case_body_b") (mkTestDeferredVar "$case_body_y"))
    )
    Elab.InstId

collectAppsElab :: Elab.XmlfTerm -> (Elab.XmlfTerm, [Elab.XmlfTerm])
collectAppsElab =
  go []
  where
    go args term =
      case term of
        Elab.EApp fun arg -> go (arg : args) fun
        other -> (other, args)

rebuildAppsElab :: Elab.XmlfTerm -> [Elab.XmlfTerm] -> Elab.XmlfTerm
rebuildAppsElab =
  foldl Elab.EApp

firstJust :: [Maybe a] -> Maybe a
firstJust [] =
  Nothing
firstJust (value : rest) =
  case value of
    Just _ -> value
    Nothing -> firstJust rest
