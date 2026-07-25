{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module BackendConvertSpec (spec) where

import BackendIRTestSupport
import qualified ElabTypeTestSupport as TestElab
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
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
import MLF.Backend.Convert hiding (convertCheckedProgram)
import qualified MLF.Backend.Convert as Convert
import MLF.Backend.IR hiding
  ( BTBase,
    BTCon,
    BTForall,
    BTMu,
    BTVar,
    BTVarApp,
    BackendBinding,
    backendBindingExpr,
    backendBindingExportedAsMain,
    backendBindingName,
    backendBindingType,
    BackendClosure,
    backendClosureParams,
    BackendConstruct,
    BackendConstructor,
    backendConstructorFields,
    backendConstructorForalls,
    backendConstructorName,
    BackendConstructorPattern,
    backendConstructorResult,
    BackendData,
    backendDataConstructors,
    backendDataName,
    backendDataParameters,
    BackendLam,
    BackendLet,
    BackendModule,
    backendModuleBindings,
    backendModuleData,
    backendModuleName,
    BackendProgram,
    backendProgramMain,
    backendProgramModules,
    BackendTyAbs,
    BackendTypeBinder,
    BackendVar
  )
import MLF.Backend.IR
  ( type BackendBinding,
    type BackendConstructor,
    type BackendData,
    type BackendModule,
    type BackendProgram,
    type BackendTypeBinder,
  )
import MLF.Backend.IR.Production.Internal (productionBackendProgramIR)
import qualified MLF.Backend.LLVM.Lower as Lower
import MLF.Backend.LLVM.Syntax (LLVMModule)
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId (..))
import qualified MLF.Elab.Pipeline as ElabPipeline
import MLF.Elab.Types (TypeCheckError (..), mapBoundType, schemeFromType)
import qualified MLF.Types.Elab as Elab
import MLF.Frontend.Program.Builtins (builtinTypeHeadIdentity, builtinTypeIdentity, builtinValueIdentity)
import MLF.Frontend.Program.Checked
  ( checkedProgramMain,
    checkedProgramModules,
    checkedProgramResolved,
  )
import MLF.Frontend.Program.Checked.Internal (CheckedProgram (..))
import MLF.Frontend.Program.Prelude (withPreludePackage)
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), SymbolOrigin (..), renameSymbolDefiningName, sameSymbolIdentity, symbolDefiningName, symbolIdentityFromParts, symbolIdentityStableName, symbolUniqueIdentity)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedTypeParam (..),
    CheckedModule (..),
    ClassInfo (..),
    ConstructorForallBinder (..),
    DeferredBindingMode (..),
    DeferredConstructorCall (..),
    DeferredProgramObligation (..),
    ResolvedLocalSymbols (..),
    ResolvedModule (..),
    ResolvedProgram (..),
    ResolvedSemanticModule (..),
    TypeView,
    typeViewBinderIdentities,
    typeViewDisplay,
    typeViewHeadIdentities,
    typeViewIdentity,
    typeViewFromSourceType,
    ValueInfo (..),
    checkedBindingName,
    checkedTypeParamIdentity,
    classInfoSymbolIdentity,
    ctorForalls,
    ctorArgs,
    constructorRefFromInfo,
    ctorForallBinderInfo,
    ConstructorInfo (..),
    DataInfo (..),
    dataInfoIdentityQualifiedName,
    dataParamBinders,
    emptyTypeBinderSubst,
    IdDetails (..),
    instanceHeadIdentityTypes,
    instanceInfoClassSymbolIdentity,
    lookupInstanceMethod,
    methodName,
    resolvedValueInfoSymbol,
    splitArrows,
    splitForalls,
    typeHeadNamesSrcType,
    valueInfoRuntimeName,
  )
import MLF.Frontend.Syntax (Lit (..), SrcBound (..), SrcTy (..), SrcType, resolvedTypeBinderRefFromIdentity)
import MLF.Frontend.Syntax.Program (Program)
import MLF.Pipeline
  ( checkProgram
  , checkProgramPackage
  , trivialProgramPackage
  )
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Reify.TypeOps (freeTypeVarRefsType)
import MLF.Types.Identity (constructorRefSymbol, deferredRefFromIdentity, deferredRefName, idDetailsIdentityKey, idDetailsStableName, LocalIdentity (..), localRefFromIdentity, ResolvedTermIdentityKey, UniqueIdentity (..), StructuralTypeBinderRole (..), TypeBinderIdentity, typeBinderIdentityFromStructural, typeBinderIdentityFromUnique, typeBinderIdentityGeneratedUnique, typeBinderIdentityStableName, typeBinderIdentityStructural)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import Test.Hspec
import TypeViewTestSupport
  ( fixtureTypeView,
    mkTypeView,
    setTypeViewBinderIdentities,
    setTypeViewDisplay,
    setTypeViewHeadIdentities,
    setTypeViewTypes,
  )

convertCheckedProgram :: CheckedProgram -> Either BackendConversionError BackendProgram
convertCheckedProgram =
  fmap productionBackendProgramIR . Convert.convertCheckedProgram

lowerTestBackendProgram :: BackendProgram -> Either Lower.BackendLLVMError LLVMModule
lowerTestBackendProgram program =
  case mkProductionBackendProgram program of
    Left err -> Left (Lower.BackendLLVMValidationFailed err)
    Right productionProgram -> Lower.lowerBackendProgram productionProgram

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

  it "lowers inside-bound instantiation before eliminating a bounded forall" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = transformedBoundSourceTypeView,
                    checkedBindingType = transformedBoundResultType,
                    checkedBindingTerm = transformedBoundTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding
      `shouldSatisfy` containsScopedTransformedBoundTypeApplication

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
    checked <- requireCheckedWithPrelude program
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
    checked <- requireCheckedWithPrelude program
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
    checked <- requireCheckedWithPrelude program
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
    checked <- requireCheckedWithPrelude program
    backend <- requireRight (convertCheckedProgram checked)
    validateBackendProgram backend `shouldBe` Right ()

    optionData <- requireBackendData "Prelude.Option" backend
    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingType mainBinding of
      BTConWithIdentity (identity) (BaseTy "Prelude.Option") (_ :| []) ->
        identity `shouldBe` backendDataIdentity optionData
      other ->
        expectationFailure ("expected identity-bearing Prelude.Option result, got " ++ show other)

  it "converts backend modules from checked module identity when module names are stale" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked = renameCheckedModuleName "Main" "$stale_Main" checked0
    backend <- requireRight (convertCheckedProgram checked)

    case backendProgramModules backend of
      [backendModule] -> do
        symbolDefiningName (backendModuleIdentity backendModule) `shouldBe` "Main"
        backendModuleName backendModule `shouldBe` "Main"
      modules0 ->
        expectationFailure ("expected one backend module, got " ++ show (length modules0))
    validateBackendProgram backend `shouldBe` Right ()

  it "resolves backend main by checked main identity when the main runtime name is stale" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked = renameCheckedProgramMainRuntimeName "$stale_Main__main" checked0
    backend <- requireRight (convertCheckedProgram checked)

    backendProgramMain backend `shouldBe` "Main__main"
    symbolDefiningName (backendProgramMainIdentity backend) `shouldBe` "main"
    validateBackendProgram backend `shouldBe` Right ()

  it "matches the checked backend IR snapshot for a primitive function program" $ do
    checked <- requireChecked simpleFunctionProgram
    backend <- requireRight (convertCheckedProgram checked)

    backendIRGolden "test/golden/backend-ir-simple-function.golden" backend

  it "recovers explicit backend constructors and cases from checked ADT paths" $ do
    checked <- requireChecked adtCaseProgram
    checkedMainBinding <- requireCheckedBinding "Main__main" checked
    let checkedMainTerm = checkedBindingTerm checkedMainBinding
        recursiveTypes =
          concatMap recursiveTypesInType (elabTypesInTerm checkedMainTerm)
    case checkedMainTerm of
      Elab.ETyAbsRef ref _ _ ->
        expectationFailure
          ( "recursive structural binder escaped as a root type abstraction: "
              ++ show ref
          )
      _ -> pure ()
    recursiveTypes `shouldSatisfy` (not . null)
    recursiveTypes
      `shouldSatisfy` all
        ( \case
            Elab.TMuRef ref _ ->
              isJust (typeBinderIdentityStructural (Elab.typeBinderRefIdentity ref))
            _ -> False
        )
    recursiveTypes `shouldSatisfy` all (not . containsBottomType)
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
                    checkedBindingTerm =
                      Elab.EApp
                        ( Elab.ETyInst
                            (checkedBindingTerm binding)
                            Elab.InstId
                        )
                        (Elab.ELit (LInt 1))
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
    originalMain <- requireCheckedBinding "Main__main" checked0
    rewrittenMain <- requireCheckedBinding "Main__main" checked
    checkedBindingTerm rewrittenMain `shouldNotBe` checkedBindingTerm originalMain
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
    originalMain <- requireCheckedBinding "Main__main" checked0
    rewrittenMain <- requireCheckedBinding "Main__main" checked
    checkedBindingTerm rewrittenMain `shouldNotBe` checkedBindingTerm originalMain
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
                    checkedBindingTerm =
                      setCaseResultInstantiation
                        polymorphicIdentityElabTy
                        ( replaceCaseHandlerBodiesAfterLams
                            1
                            alphaEquivalentIdentityInstId
                            (checkedBindingTerm binding)
                        )
                  }
            )
            checked0
    originalMain <- requireCheckedBinding "Main__main" checked0
    rewrittenMain <- requireCheckedBinding "Main__main" checked
    checkedBindingTerm rewrittenMain `shouldNotBe` checkedBindingTerm originalMain
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
    _ <- requireRight (lowerTestBackendProgram backend)
    pure ()

  it "renames expected forall bodies to actual type abstraction binders" $ do
    checked <- requireChecked =<< readFile "test/programs/unified/first-class-polymorphism.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "keeps local polymorphic function arguments on the static backend lane" $ do
    program <- requireParsed localFirstClassPolymorphismProgram
    checked <- requireCheckedWithPrelude program
    backend <- requireRight (convertCheckedProgram checked)
    mainBinding <- requireBinding (backendProgramMain backend) backend

    validateBackendProgram backend `shouldBe` Right ()
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendClosure

  it "preserves an already-polymorphic let instead of synthesizing expected type abstractions" $ do
    checked <-
      requireChecked $
        unlines
          [ "module Main export (Pick, pick, main) {",
            "  class Pick a {",
            "    pick : ∀ ghost. ∀ b. a -> b -> b;",
            "  }",
            "  instance Pick Bool {",
            "    pick = let impl : ∀ ghost. ∀ b. Bool -> b -> b = λflag λvalue value in impl;",
            "  }",
            "  def main : Bool = pick true false;",
            "}"
          ]
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    case
        [ binding
        | binding <- backendBindings backend
        , BackendLetWithIdentity {} <- [backendBindingExpr binding]
        ]
      of
        [methodBinding] ->
          case (backendBindingType methodBinding, backendBindingExpr methodBinding) of
            ( BTForallWithIdentity _ "ghost" Nothing (BTForallWithIdentity _ "b" Nothing _),
              BackendLetWithIdentity
                { backendExprType = letResultTy,
                  backendLetType = localTy,
                  backendLetBody = BackendVarWithIdentity {backendExprType = bodyTy}
                }
              ) -> do
                alphaEqBackendType letResultTy (backendBindingType methodBinding) `shouldBe` True
                alphaEqBackendType bodyTy localTy `shouldBe` True
            other ->
              expectationFailure ("expected a polymorphic backend let producer, got " ++ show other)
        bindings ->
          expectationFailure ("expected one root backend let binding, got " ++ show (map backendBindingName bindings))

  it "uses canonical expected type abstraction names for same-spelled refs" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let (canonicalOuterRef, gen1) =
          Elab.freshTypeBinderRef "a" (Elab.identityGeneratorAfterType sameNamedTypeAbsElabTy)
        (canonicalInnerRef, _) =
          Elab.freshTypeBinderRef "a" gen1
        sourceOuterIdentity = Elab.typeBinderRefIdentity canonicalOuterRef
        sourceInnerIdentity = Elab.typeBinderRefIdentity canonicalInnerRef
        identityTy =
          STForall
            (typeBinderIdentityStableName sourceOuterIdentity)
            Nothing
            (STForall (typeBinderIdentityStableName sourceInnerIdentity) Nothing (STBase "Int"))
        sourceView =
          fixtureTypeView
            sameNamedTypeAbsSourceTy
            identityTy
            Map.empty
            ( Map.fromList
                [ (typeBinderIdentityStableName sourceOuterIdentity, sourceOuterIdentity),
                  (typeBinderIdentityStableName sourceInnerIdentity, sourceInnerIdentity)
                ]
            )
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = sourceView,
                    checkedBindingType = sameNamedTypeAbsElabTy,
                    checkedBindingTerm = sameNamedTypeAbsTerm
                  }
            )
            checked0
    Map.lookup (typeBinderIdentityStableName sourceOuterIdentity) (typeViewBinderIdentities sourceView)
      `shouldBe` Just sourceOuterIdentity
    Map.lookup (typeBinderIdentityStableName sourceInnerIdentity) (typeViewBinderIdentities sourceView)
      `shouldBe` Just sourceInnerIdentity
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity
        ((Elab.typeBinderRefIdentity canonicalOuterRef))
        "a"
        Nothing
        ( BTForallWithIdentity
            ((Elab.typeBinderRefIdentity canonicalInnerRef))
            "a1"
            Nothing
            intTy
        )
    case backendBindingType mainBinding of
      BTForallWithIdentity (outerTypeIdentity) "a" Nothing (BTForallWithIdentity (innerTypeIdentity) "a1" Nothing _) -> do
        outerTypeIdentity `shouldBe` Elab.typeBinderRefIdentity canonicalOuterRef
        innerTypeIdentity `shouldBe` Elab.typeBinderRefIdentity canonicalInnerRef
      other ->
        expectationFailure ("expected identity-backed backend forall type, got " ++ show other)
    case backendBindingExpr mainBinding of
      BackendTyAbsWithIdentity
        { backendTyParamIdentity = actualOuterIdentity,
          backendTyParamName = outerName,
          backendTyAbsBody =
            BackendTyAbsWithIdentity
              { backendTyParamIdentity = actualInnerIdentity,
                backendTyParamName = innerName,
                backendTyAbsBody = BackendLit {backendLit = LInt 1}
              }
        } -> do
          outerName `shouldBe` "a"
          innerName `shouldBe` "a1"
          actualOuterIdentity `shouldBe` Elab.typeBinderRefIdentity canonicalOuterRef
          actualInnerIdentity `shouldBe` Elab.typeBinderRefIdentity canonicalInnerRef
      other ->
        expectationFailure ("expected nested backend type abstraction, got " ++ show other)

  it "converts identity-bearing backend free type variables" $ do
    let dataIdentity =
          symbolIdentityFromParts (UniqueIdentity 0) SymbolType "Main" "Box" Nothing
        variableIdentity = typeBinderIdentityFromUnique (UniqueIdentity 772024)
        backendTy =
          BTArrow
            (BTBaseWithIdentity (dataIdentity) (BaseTy "Box"))
            (BTVarWithIdentity variableIdentity "fresh")

    case backendTypeToElabType backendTy of
      Just (Elab.TArrow _ (Elab.TVarRef ref)) ->
        Elab.typeBinderRefIdentity ref `shouldBe` variableIdentity
      other -> expectationFailure ("expected identity-bearing free variable conversion, got " ++ show other)

  it "resolves backend-to-elab type binders by identity before display name" $ do
    let identity = typeBinderIdentityFromUnique (UniqueIdentity 7)
        backendTy =
          BTForallWithIdentity
            (identity)
            "canonical"
            Nothing
            (BTVarWithIdentity (identity) "stale")

    case backendTypeToElabType backendTy of
      Just (Elab.TForallRef binderRef Nothing (Elab.TVarRef occurrenceRef)) -> do
        Elab.typeBinderRefIdentity binderRef `shouldBe` identity
        Elab.typeBinderRefName binderRef `shouldBe` "canonical"
        Elab.typeBinderRefIdentity occurrenceRef `shouldBe` identity
        Elab.typeBinderRefName occurrenceRef `shouldBe` "canonical"
      other ->
        expectationFailure ("expected identity-keyed backend type conversion, got " ++ show other)

  it "keeps same-named backend variables distinct when their identities differ" $ do
    let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 8)
        occurrenceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 9)
        backendTy =
          BTForallWithIdentity
            binderIdentity
            "a"
            Nothing
            (BTVarWithIdentity occurrenceIdentity "a")

    case backendTypeToElabType backendTy of
      Just (Elab.TForallRef binderRef Nothing (Elab.TVarRef occurrenceRef)) -> do
        Elab.typeBinderRefIdentity binderRef `shouldBe` binderIdentity
        Elab.typeBinderRefIdentity occurrenceRef `shouldBe` occurrenceIdentity
      other -> expectationFailure ("expected distinct identity-bearing backend variables, got " ++ show other)

  it "preserves checked source type binder identities after source type head identities" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let typeIdentity =
          symbolIdentityFromParts (UniqueIdentity 0) SymbolType "Main" "Box" Nothing
        boxElabTy = Elab.TBaseWithIdentity typeIdentity (BaseTy "Box")
        sourceTy =
          STForall
            "a"
            (Just (SrcBound (STBase "Box")))
            (STForall "a" Nothing (STBase "Int"))
        sourceIdentityTy =
          STForall
            (typeBinderIdentityStableName expectedOuterIdentity)
            (Just (SrcBound (STBase "Box")))
            (STForall (typeBinderIdentityStableName expectedInnerIdentity) Nothing (STBase "Int"))
        sourceView =
          fixtureTypeView
            sourceTy
            sourceIdentityTy
            (Map.singleton "Box" typeIdentity)
            ( Map.fromList
                [ (typeBinderIdentityStableName expectedOuterIdentity, expectedOuterIdentity),
                  (typeBinderIdentityStableName expectedInnerIdentity, expectedInnerIdentity)
                ]
            )
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
        (expectedOuterIdentity)
        "a"
        (Just (BTBaseWithIdentity (typeIdentity) (BaseTy "Box")))
        (BTForallWithIdentity (expectedInnerIdentity) "a1" Nothing intTy)

  it "uses explicit source type binder identities while canonicalizing stable-looking checked source types" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let stableName = "$typevar#991605"
        (canonicalOuterRef, gen1) =
          Elab.freshTypeBinderRef stableName (Elab.identityGeneratorAfterType sameNamedTypeAbsElabTy)
        (canonicalInnerRef, _) =
          Elab.freshTypeBinderRef "a" gen1
        expectedOuterIdentity = Elab.typeBinderRefIdentity canonicalOuterRef
        expectedInnerIdentity = Elab.typeBinderRefIdentity canonicalInnerRef
        sourceTy = STForall stableName Nothing (STForall "a" Nothing (STBase "Int"))
        sourceIdentityTy =
          STForall
            (typeBinderIdentityStableName expectedOuterIdentity)
            Nothing
            (STForall (typeBinderIdentityStableName expectedInnerIdentity) Nothing (STBase "Int"))
        sourceView =
          fixtureTypeView
            sourceTy
            sourceIdentityTy
            Map.empty
            ( Map.fromList
                [ (typeBinderIdentityStableName expectedOuterIdentity, expectedOuterIdentity),
                  (typeBinderIdentityStableName expectedInnerIdentity, expectedInnerIdentity)
                ]
            )
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = sourceView,
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
        (expectedOuterIdentity)
        stableName
        Nothing
        (BTForallWithIdentity (expectedInnerIdentity) "a" Nothing intTy)
    case backendBindingExpr mainBinding of
      BackendTyAbsWithIdentity
        { backendTyParamIdentity = outerIdentity,
          backendTyAbsBody =
            BackendTyAbsWithIdentity
              { backendTyParamIdentity = innerIdentity
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
                      fixtureTypeView
                        displayTy
                        identityTy
                        Map.empty
                        (Map.fromList [("a", sourceIdentity), (sourceStableName, sourceIdentity)]),
                    checkedBindingType = Elab.TForallRef sameNamedOuterTypeRef Nothing intElabTy,
                    checkedBindingTerm = Elab.ETyAbsRef sameNamedOuterTypeRef Nothing (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity (sourceIdentity) "a" Nothing intTy
    case backendBindingExpr mainBinding of
      BackendTyAbsWithIdentity {backendTyParamIdentity = identity, backendTyParamName = name} -> do
        identity `shouldBe` sourceIdentity
        name `shouldBe` "a"
      other ->
        expectationFailure ("expected source identity-backed backend type abstraction, got " ++ show other)

  it "resolves checked source type binder occurrences through stable aliases under stale declaration names" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991636)
        sourceStableName = typeBinderIdentityStableName sourceIdentity
        displayTy = STForall "a" Nothing (STArrow (STVar "a") (STBase "Int"))
        identityTy = STForall "$stale_a" Nothing (STArrow (STVar sourceStableName) (STBase "Int"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      fixtureTypeView
                        displayTy
                        identityTy
                        Map.empty
                        (Map.fromList [("a", sourceIdentity), ("$stale_a", sourceIdentity), (sourceStableName, sourceIdentity)]),
                    checkedBindingType = Elab.TForallRef sameNamedOuterTypeRef Nothing (Elab.TArrow (Elab.TVarRef sameNamedOuterTypeRef) intElabTy),
                    checkedBindingTerm = Elab.ETyAbsRef sameNamedOuterTypeRef Nothing (mkTestLocalLam "x" (Elab.TVarRef sameNamedOuterTypeRef) (Elab.ELit (LInt 1)))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity
        (sourceIdentity)
        "a"
        Nothing
        (BTArrow (BTVarWithIdentity (sourceIdentity) "a") intTy)

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
                      fixtureTypeView
                        displayTy
                        identityTy
                        Map.empty
                        (Map.fromList [("a", sourceIdentity), ("$stale_a", sourceIdentity)]),
                    checkedBindingType = Elab.TForallRef sameNamedOuterTypeRef Nothing intElabTy,
                    checkedBindingTerm = Elab.ETyAbsRef sameNamedOuterTypeRef Nothing (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity (sourceIdentity) "a" Nothing intTy

  it "canonicalizes checked binder identities to the identity-bearing source view" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991609)
        checkedIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991610)
        checkedRef = Elab.typeBinderRefFromIdentity checkedIdentity "a"
        displayTy = STForall "a" Nothing (STArrow (STVar "a") (STBase "Int"))
        identityTy = STForall "$stale_a" Nothing (STArrow (STVar "$stale_a") (STBase "Int"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      fixtureTypeView
                        displayTy
                        identityTy
                        Map.empty
                        ( Map.fromList
                            [ ("a", sourceIdentity),
                              ("$stale_a", sourceIdentity)
                            ]
                        ),
                    checkedBindingType = Elab.TForallRef checkedRef Nothing (Elab.TArrow (Elab.TVarRef checkedRef) intElabTy),
                    checkedBindingTerm = Elab.ETyAbsRef checkedRef Nothing (mkTestLocalLam "x" (Elab.TVarRef checkedRef) (Elab.ELit (LInt 1)))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity (sourceIdentity) "a" Nothing (BTArrow (BTVarWithIdentity (sourceIdentity) "a") intTy)

  it "keeps checked source type binder identities on the identity-bearing source view" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991607)
        sourceStableName = typeBinderIdentityStableName sourceIdentity
        displayTy = STForall "a" Nothing (STBase "Int")
        identityTy = STForall sourceStableName Nothing (STBase "Int")
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      fixtureTypeView
                        displayTy
                        identityTy
                        Map.empty
                        (Map.fromList [("a", sourceIdentity), (sourceStableName, sourceIdentity)]),
                    checkedBindingType = Elab.TForallRef sameNamedOuterTypeRef Nothing intElabTy,
                    checkedBindingTerm = Elab.ETyAbsRef sameNamedOuterTypeRef Nothing (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding
      `shouldBe` BTForallWithIdentity (sourceIdentity) "a" Nothing intTy

  it "preserves explicit source type binders after source view metadata identities" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let reservedUnique = UniqueIdentity 2000000040
        reservedHeadIdentity =
          symbolIdentityFromParts reservedUnique SymbolType "Main" "ReservedSourceMeta" Nothing
        sourceIdentity = typeBinderIdentityFromUnique (UniqueIdentity 2000000041)
        sourceTy = STForall "a" Nothing (STBase "Int")
        sourceIdentityTy =
          STForall (typeBinderIdentityStableName sourceIdentity) Nothing (STBase "Int")
        sourceView =
          fixtureTypeView
            sourceTy
            sourceIdentityTy
            (Map.singleton "ReservedSourceMeta" reservedHeadIdentity)
            (Map.singleton (typeBinderIdentityStableName sourceIdentity) sourceIdentity)
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = sourceView,
                    checkedBindingType = Elab.TForallRef sameNamedOuterTypeRef Nothing intElabTy,
                    checkedBindingTerm = Elab.ETyAbsRef sameNamedOuterTypeRef Nothing (Elab.ELit (LInt 1))
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingType mainBinding of
      BTForallWithIdentity (identity) "a" Nothing _ ->
        typeBinderIdentityGeneratedUnique identity `shouldBe` Just (UniqueIdentity 2000000041)
      other ->
        expectationFailure ("expected generated source fallback binder, got " ++ show other)

  it "preserves distinct identities for same-spelled source binders" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let outerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991635)
        innerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991637)
        outerStableName = typeBinderIdentityStableName outerIdentity
        innerStableName = typeBinderIdentityStableName innerIdentity
        displayTy = STForall "a" Nothing (STForall "a" Nothing (STBase "Int"))
        identityTy = STForall outerStableName Nothing (STForall innerStableName Nothing (STBase "Int"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView =
                      fixtureTypeView
                        displayTy
                        identityTy
                        Map.empty
                        ( Map.fromList
                            [ (outerStableName, outerIdentity),
                              (innerStableName, innerIdentity)
                            ]
                        ),
                    checkedBindingType = sameNamedTypeAbsElabTy,
                    checkedBindingTerm = sameNamedTypeAbsTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case backendBindingType mainBinding of
      BTForallWithIdentity (outerBackendIdentity) _ Nothing (BTForallWithIdentity innerBackendIdentity _ Nothing _) -> do
        outerBackendIdentity `shouldBe` outerIdentity
        innerBackendIdentity `shouldBe` innerIdentity
      other ->
        expectationFailure ("expected distinct source fallback backend foralls, got " ++ show other)

  it "preserves GADT indices through generated type applications" $ do
    checked <-
      requireChecked
        =<< readFile "test/programs/recursive-adt/recursive-gadt.mlfp"
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

  it "synthesizes constructor bindings for a checked existential program" $ do
    existentialChecked <-
      requireChecked
        =<< readFile "test/programs/recursive-adt/recursive-existential.mlfp"
    backend <- requireRight (convertCheckedProgram existentialChecked)

    validateBackendProgram backend `shouldBe` Right ()
    let generatedConstructorArgBinders =
          [ identity
          | binding <- backendBindings backend,
            (name, identity) <- backendExprBinderRefs (backendBindingExpr binding),
            "$" `isPrefixOf` name,
            "_arg" `isInfixOf` name
          ]
    generatedConstructorArgBinders `shouldSatisfy` (not . null)

    unwrapSome <-
      requireCheckedBinding
        "RecursiveExistential__unwrapSome"
        existentialChecked
    someExpr <-
      requireCheckedConstructor
        "RecursiveExistential__SomeExpr"
        existentialChecked
    case ctorForallBinderInfo someExpr of
      [existentialBinder] ->
        caseHandlerTypeAbstractionIdentities (checkedBindingTerm unwrapSome)
          `shouldContain` [constructorForallIdentity existentialBinder]
      binders ->
        expectationFailure
          ("expected one SomeExpr existential binder, got " ++ show binders)
    checkedBindingTerm unwrapSome `shouldSatisfy` (not . hasBottomTypedConstructorRef)

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
                          identityCompleteFixtureTypeView
                            []
                            []
                            ( setTypeViewHeadIdentities
                                (Map.singleton identityDataHead (dataInfoSymbol dataInfo))
                                ( mkTypeView
                                    (STCon "$stale_source_option" (STBase "Int" :| []))
                                    (STCon identityDataHead (STBase "Int" :| []))
                                )
                            ),
                        checkedBindingType =
                          Elab.TConWithIdentity
                            (dataInfoSymbol dataInfo)
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int") :| [])
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
                          identityCompleteFixtureTypeView
                            []
                            []
                            ( setTypeViewHeadIdentities
                                (Map.singleton staleIdentityDataHead (dataInfoSymbol dataInfo))
                                ( mkTypeView
                                    (STCon "$stale_source_option" (STBase "Int" :| []))
                                    (STCon staleIdentityDataHead (STBase "Int" :| []))
                                )
                            ),
                        checkedBindingType =
                          Elab.TConWithIdentity
                            (dataInfoSymbol dataInfo)
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int") :| [])
                      }
                )
                checked0
            checkedByStableHead =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView =
                          identityCompleteFixtureTypeView
                            []
                            []
                            ( setTypeViewHeadIdentities
                                (Map.singleton (dataInfoIdentityQualifiedName dataInfo) (dataInfoSymbol dataInfo))
                                ( mkTypeView
                                    (STCon "$stale_source_option" (STBase "Int" :| []))
                                    (STCon stableIdentityDataHead (STBase "Int" :| []))
                                )
                            ),
                        checkedBindingType =
                          Elab.TConWithIdentity
                            (dataInfoSymbol dataInfo)
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int") :| [])
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
                          identityCompleteFixtureTypeView
                            []
                            []
                            ( setTypeViewHeadIdentities
                                (Map.singleton displayDataHead (dataInfoSymbol dataInfo))
                                ( mkTypeView
                                    (STCon displayDataHead (STBase "Int" :| []))
                                    (STCon "$stale_identity_option" (STBase "Int" :| []))
                                )
                            ),
                        checkedBindingType =
                          Elab.TConWithIdentity
                            (dataInfoSymbol dataInfo)
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int") :| [])
                      }
                )
                checked0
        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
        backendDataNames backend `shouldContain` ["Main.Option"]
      [] -> expectationFailure "expected checked data info"

  it "does not recover source data hints through a same-spelled different identity" $ do
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
                      fixtureTypeView
                        (STCon "$stale_source_option" (STBase "Int" :| []))
                        (STCon staleIdentityDataHead (STBase "Int" :| []))
                        (Map.singleton staleIdentityDataHead fakeOptionIdentity)
                        Map.empty,
                        checkedBindingType =
                          Elab.TConWithIdentity
                            fakeOptionIdentity
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int") :| [])
                      }
                )
                checked0
        convertCheckedProgram checked `shouldSatisfy` isLeft
      [] -> expectationFailure "expected checked data info"

  it "does not recover source data hints when head identity payloads conflict" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case checkedDataInfos checked0 of
      dataInfo : _ -> do
        let staleIdentityDataHead = dataInfoIdentityQualifiedName dataInfo
            conflictingOptionIdentity =
              renameSymbolDefiningName "$stale_Option" (dataInfoSymbol dataInfo)
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                  { checkedBindingSourceTypeView =
                      setTypeViewHeadIdentities
                        (Map.singleton staleIdentityDataHead conflictingOptionIdentity)
                        ( mkTypeView
                            (STCon "$stale_source_option" (STBase "Int" :| []))
                            (STCon staleIdentityDataHead (STBase "Int" :| []))
                        ),
                        checkedBindingType =
                          Elab.TConWithIdentity
                            conflictingOptionIdentity
                            (BaseTy "$stale_elab_option")
                            (Elab.TBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int") :| [])
                      }
                )
                checked0
        convertCheckedProgram checked `shouldSatisfy` isLeft
      [] -> expectationFailure "expected checked data info"

  it "cannot construct a TypeView from identity-shaped names without identity payloads" $ do
    typeViewFromSourceType
      Map.empty
      Map.empty
      (STCon "$identity_shaped_option" (STBase "Int" :| []))
      `shouldSatisfy` isLeft

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
              fixtureTypeView
                (STArrow (STVar "a") (STCon staleDisplayHead (STVar "a" :| [])))
                (STArrow (STVar "a") (STCon staleIdentityHead (STVar "a" :| [])))
                (Map.singleton staleIdentityHead (dataInfoSymbol dataInfo))
                (Map.fromList (dataParamBinders dataInfo))
            checked =
              withConstructorTypeView "Main__Some" staleCtorType checked0
        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
        constructor <- requireConstructor "Main__Some" backend
        case backendConstructorResult constructor of
          BTConWithIdentity (identity) (BaseTy "Main.Option") (_ :| []) ->
            identity `shouldBe` dataInfoSymbol dataInfo
          other ->
            expectationFailure ("expected identity-bearing Main.Option result, got " ++ show other)
      Nothing ->
        expectationFailure "expected Main.Option data info"

  it "converts constructor metadata by display head identity maps when identity names are stale" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    case find ((== "Main.Option") . dataInfoIdentityQualifiedName) (checkedDataInfos checked0) of
      Just dataInfo -> do
        let displayHeadAlias = "$display_alias_option"
            staleIdentityHead = "$stale_identity_option"
            staleCtorType =
              fixtureTypeView
                (STArrow (STVar "a") (STCon displayHeadAlias (STVar "a" :| [])))
                (STArrow (STVar "a") (STCon staleIdentityHead (STVar "a" :| [])))
                (Map.singleton displayHeadAlias (dataInfoSymbol dataInfo))
                (Map.fromList (dataParamBinders dataInfo))
            checked =
              withConstructorTypeView "Main__Some" staleCtorType checked0
        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
        constructor <- requireConstructor "Main__Some" backend
        case backendConstructorResult constructor of
          BTConWithIdentity (identity) (BaseTy "Main.Option") (_ :| []) ->
            identity `shouldBe` dataInfoSymbol dataInfo
          other ->
            expectationFailure ("expected identity-bearing Main.Option result, got " ++ show other)
      Nothing ->
        expectationFailure "expected Main.Option data info"

  it "preserves identity-bearing constructor field local builtins" $ do
    checked0 <- requireChecked constructorFieldLetProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      rewriteFirstLetBindingType intElabTy (checkedBindingTerm binding)
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
      _dataInfo : _ -> do
        let checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingTerm =
                          rewriteFirstLetBindingTypeWith
                            (renameElabTypeBinderDisplays "$stale_scrutinee_name")
                            (checkedBindingTerm binding)
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

  it "rejects stable structural binder text as data identity" $ do
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
        case convertCheckedProgram checked of
          Left (BackendTypeCheckFailed _ (TCResolvedVarTypeMismatch _ _ _)) ->
            pure ()
          other ->
            expectationFailure
              ("expected identity-mismatched checked IR to be rejected, got " ++ show other)
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
          backendConstructorIdentity constructor `shouldBe` symbol
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
          backendDataIdentity dataDecl `shouldBe` symbol
      )
      checkedData

  it "preserves resolved constructor identities on backend constructor applications" $ do
    checked <- requireChecked parameterizedConstructorProgram
    backend <- requireRight (convertCheckedProgram checked)
    someConstructor <- requireCheckedConstructor "Main__Some" checked

    mainBinding <- requireBinding (backendProgramMain backend) backend
    lookup "Main__Some" (collectConstructIdentities (backendBindingExpr mainBinding))
      `shouldBe` Just (ctorInfoSymbol someConstructor)

  it "preserves resolved constructor identities on backend case patterns" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)
    succConstructor <- requireCheckedConstructor "Main__Succ" checked

    mainBinding <- requireBinding (backendProgramMain backend) backend
    case findBackendCase (backendBindingExpr mainBinding) of
      Just BackendCase {backendAlternatives = alternatives} ->
        lookup "Main__Succ" (collectPatternIdentities alternatives)
          `shouldBe` Just (ctorInfoSymbol succConstructor)
      Just other -> expectationFailure ("expected backend case, got " ++ show other)
      Nothing -> expectationFailure "expected backend case"

  it "validates and lowers backend constructor applications by resolved identity when node names are stale" $ do
    checked <- requireChecked parameterizedConstructorProgram
    backend <- requireRight (convertCheckedProgram checked)
    let staleBackend =
          mapBackendMainBinding
            ( \binding ->
                binding
                  { backendBindingExprWithMetadata =
                      renameBackendConstructorReferences True False (== "Main__Some") "$stale_some_node" (backendBindingExpr binding)
                  }
            )
            backend

    validateBackendProgram staleBackend `shouldBe` Right ()
    _ <- requireRight (lowerTestBackendProgram staleBackend)
    pure ()

  it "validates and lowers backend case patterns by resolved identity when pattern names are stale" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)
    let staleBackend =
          mapBackendMainBinding
            ( \binding ->
                binding
                  { backendBindingExprWithMetadata =
                      renameBackendConstructorReferences False True (== "Main__Succ") "$stale_succ_pattern" (backendBindingExpr binding)
                  }
            )
            backend

    validateBackendProgram staleBackend `shouldBe` Right ()
    _ <- requireRight (lowerTestBackendProgram staleBackend)
    pure ()

  it "validates and lowers backend global variables by resolved identity when node names are stale" $ do
    checked <- requireChecked simpleFunctionProgram
    backend <- requireRight (convertCheckedProgram checked)
    let staleBackend =
          mapBackendMainBinding
            ( \binding ->
                binding
                  { backendBindingExprWithMetadata =
                      renameBackendVarReferences (== "Main__id") "$stale_id_node" (backendBindingExpr binding)
                  }
            )
            backend

    validateBackendProgram staleBackend `shouldBe` Right ()
    _ <- requireRight (lowerTestBackendProgram staleBackend)
    pure ()

  it "emits direct constructor applications from resolved identity instead of constructor runtime name" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    let checked =
          renameCheckedConstructorRuntimeNamesWhere
            (== "Main__Some")
            "$stale_some_backend_name"
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Some"]
    collectConstructNames (backendBindingExpr mainBinding) `shouldNotContain` ["$stale_some_backend_name"]

  it "recovers higher-kinded structural constructors as backend constructors" $ do
    checked <- requireChecked higherKindedConstructorProgram
    wrapBinding <- requireCheckedBinding "Main__Wrap" checked
    freeTypeVarRefsType (checkedBindingType wrapBinding) `shouldBe` []
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
      Left (BackendTypeCheckFailed _ (TCInstantiationError _ _ message)) ->
        message `shouldSatisfy` isInfixOf "InstApp expects forall"
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

  it "maps constructor type application heads by binder identity when display names are stale" $ do
    checked0 <- requireChecked higherKindedConstructorProgram
    wrapData <- requireCheckedData "Main.Wrap" checked0
    wrapConstructor <- requireCheckedConstructor "Main__Wrap" checked0
    let checked =
          withConstructorTypeView
            "Main__Wrap"
            (staleTypeViewBinderDisplay "f" "$stale_f" (ctorTypeView wrapConstructor))
            checked0

    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    constructor <- requireConstructor "Main__Wrap" backend
    case (dataParamBinders wrapData, backendConstructorFields constructor) of
      ( [("f", fIdentity), ("a", aIdentity)],
        [BTVarAppWithIdentity (actualFIdentity) "f" (BTVarWithIdentity (actualAIdentity) "a" :| [])]
        ) -> do
          actualFIdentity `shouldBe` fIdentity
          actualAIdentity `shouldBe` aIdentity
      other ->
        expectationFailure ("expected identity-mapped higher-kinded constructor field, got " ++ show other)

  it "does not pick an arbitrary constructor binder identity when data parameter displays collide" $ do
    dataChecked0 <- requireChecked dataParameterOrderConstructorProgram
    mainChecked <- requireChecked simpleFunctionProgram
    dataInfo <- requireCheckedData "Main.T" dataChecked0
    case dataParamBinders dataInfo of
      [("z", zIdentity), ("a", aIdentity)] -> do
        let zStableName = typeBinderIdentityStableName zIdentity
            aStableName = typeBinderIdentityStableName aIdentity
            displayTy =
              STArrow
                (STVar "a")
                (STArrow (STVar "a") (STCon "T" (STVar "a" :| [STVar "a"])))
            identityTy =
              STArrow
                (STVar zStableName)
                (STArrow (STVar aStableName) (STCon (dataInfoIdentityQualifiedName dataInfo) (STVar zStableName :| [STVar aStableName])))
            constructorView =
              fixtureTypeView
                displayTy
                identityTy
                (Map.singleton (dataInfoIdentityQualifiedName dataInfo) (dataInfoSymbol dataInfo))
                ( Map.fromList
                    [ (zStableName, zIdentity),
                      (aStableName, aIdentity)
                    ]
                )
            dataChecked =
              withConstructorTypeView "Main__Mk" constructorView $
                renameDataParamDisplays "Main.T" ["a", "a"] dataChecked0
        mutatedDataInfo <- requireCheckedData "Main.T" dataChecked
        let checked = addDataInfo mutatedDataInfo mainChecked
        backend <- requireRight (convertCheckedProgram checked)

        validateBackendProgram backend `shouldBe` Right ()
        constructor <- requireConstructor "Main__Mk" backend
        backendConstructorFields constructor
          `shouldBe` [ BTVarWithIdentity (zIdentity) "a",
                       BTVarWithIdentity (aIdentity) "a"
                     ]
      other ->
        expectationFailure ("expected two data parameters, got " ++ show other)

  it "preserves bounded constructor foralls in backend metadata" $ do
    checked <- requireChecked boundedConstructorForallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    constructor <- requireConstructor "Main__Pack" backend
    map backendTypeBinderName (backendConstructorForalls constructor) `shouldBe` ["a"]
    map backendTypeBinderBound (backendConstructorForalls constructor)
      `shouldBe` [Just (BTBaseWithIdentity ((builtinTypeIdentity "Int")) (BaseTy "Int"))]
    map backendTypeBinderIdentity (backendConstructorForalls constructor)
      `shouldSatisfy` (not . null)
    case backendConstructorForalls constructor of
      [BackendTypeBinderWithIdentity (binderIdentity) "a" (Just boundTy)] -> do
        boundTy `shouldBe` BTBaseWithIdentity ((builtinTypeIdentity "Int")) (BaseTy "Int")
        backendConstructorFields constructor `shouldBe` [BTVarWithIdentity (binderIdentity) "a"]
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
                  { backendBindingExprWithMetadata = corruptedExpr,
                    backendBindingTypeWithMetadata = backendConstructorResult constructor
                  }
            )
            backend

    validateBackendProgram corruptedBackend
      `shouldBe` Left
        ( BackendConstructorArgumentMismatch
            "Main__Pack"
            0
            (BTBaseWithIdentity ((builtinTypeIdentity "Int")) (BaseTy "Int"))
            boolTy
        )

  it "matches bounded constructor foralls against type variables with equivalent bounds" $ do
    checked0 <- requireChecked boundedConstructorForallProgram
    packConstructor <- requireCheckedConstructor "Main__Pack" checked0
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
      `shouldSatisfy`
        containsConstructArgTypeVar
          (ctorInfoSymbol packConstructor)
          (Elab.typeBinderRefIdentity boundedWrapTypeRef)

  it "keeps same-spelled type env bounds under canonical backend names" $ do
    checked0 <- requireChecked boundedConstructorForallProgram
    packInfo <- requireCheckedData "Main.Pack" checked0
    packConstructor <- requireCheckedConstructor "Main__Pack" checked0
    let outerIdentity = Elab.typeBinderRefIdentity sameNamedOuterTypeRef
        innerIdentity = Elab.typeBinderRefIdentity sameNamedInnerTypeRef
        sourceIdentityTy =
          STForall
            (typeBinderIdentityStableName outerIdentity)
            Nothing
            ( STForall
                (typeBinderIdentityStableName innerIdentity)
                Nothing
                ( STArrow
                    (STVar (typeBinderIdentityStableName innerIdentity))
                    (STBase "Main.Pack")
                )
            )
        sourceView =
          identityCompleteFixtureTypeView
            [("Main.Pack", dataInfoSymbol packInfo)]
            [ (typeBinderIdentityStableName outerIdentity, outerIdentity),
              (typeBinderIdentityStableName innerIdentity, innerIdentity)
            ]
            (mkTypeView sameNamedBoundedWrapSourceTy sourceIdentityTy)
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = sourceView,
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
                backendTyAbsBody = BackendLamWithIdentity {backendParamType = BTVar "a1", backendBody = body}
              }
        } -> do
          boundTy `shouldBe` intTy
          body
            `shouldSatisfy`
              containsConstructArgTypeVar
                (ctorInfoSymbol packConstructor)
                innerIdentity
      other ->
        expectationFailure ("expected canonical same-spelled bounded type abstraction, got " ++ show other)

  it "matches bounded constructor foralls through dependent type-variable bounds" $ do
    checked0 <- requireChecked dependentBoundedConstructorForallProgram
    packConstructor <- requireCheckedConstructor "Main__Pack" checked0
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
      `shouldSatisfy`
        containsConstructArgTypeVar
          (ctorInfoSymbol packConstructor)
          (Elab.typeBinderRefIdentity dependentBoundedWrapInnerRef)

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
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingType mainBinding `shouldBe` boolTy
    backendExprType (backendBindingExpr mainBinding) `shouldBe` boolTy
    _ <- requireRight (lowerTestBackendProgram backend)
    case [dataInfoSymbol info | info <- checkedDataInfos checked, not (null (dataTypeParams info))] of
      optionIdentity : _ ->
        case parameterizedInstanceMethodRuntimeNames optionIdentity checked of
          methodRuntimeName : _ ->
            map backendBindingName (backendBindings backend)
              `shouldSatisfy` elem methodRuntimeName
          [] -> expectationFailure "expected parameterized instance method info"
      [] -> expectationFailure "expected parameterized data info"

  it "lowers structural Option unroll to an identity-owned nominal case" $ do
    checked <- requireChecked parameterizedEqEvidenceProgram
    backend <- requireRight (convertCheckedProgram checked)
    optionInfo <- requireCheckedData "Main.Option" checked
    validateBackendProgram backend `shouldBe` Right ()
    case parameterizedInstanceMethodRuntimeNames (dataInfoSymbol optionInfo) checked of
      methodRuntimeName : _ -> do
        methodBinding <- requireBinding methodRuntimeName backend
        case findBackendCase (backendBindingExpr methodBinding) of
          Just BackendCase {backendScrutinee = scrutinee} ->
            case backendExprType scrutinee of
              BTConWithIdentity identity _ (_ :| _) ->
                identity `shouldBe` dataInfoSymbol optionInfo
              other ->
                expectationFailure
                  ("expected identity-owned nominal Option scrutinee, got " ++ show other)
          other -> expectationFailure ("expected nominal Option case, got " ++ show other)
      [] -> expectationFailure "expected parameterized Option instance method"
    _ <- requireRight (lowerTestBackendProgram backend)
    pure ()

  it "rejects an ill-typed roll payload at the checked backend boundary" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    let recursiveIntTy = testTMu "$bad_roll_self" intElabTy
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      mkTestLocalLet
                        "$bad_roll_payload"
                        (schemeFromType boolElabTy)
                        (Elab.ELit (LBool True))
                        ( Elab.ERoll
                            recursiveIntTy
                            (mkTestDeferredVar "$bad_roll_payload")
                        )
                  }
            )
            checked0

    case convertCheckedProgram checked of
      Left (BackendTypeCheckFailed _ (TCRollBodyMismatch expectedTy actualTy)) -> do
        expectedTy `shouldBe` intElabTy
        actualTy `shouldBe` boolElabTy
      other ->
        expectationFailure ("expected roll payload rejection, got " ++ show other)

  it "rejects a mismatched checked unroll type at the backend boundary" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    let boolSourceView =
          identityCompleteFixtureTypeView
            []
            []
            (mkTypeView (STBase "Bool") (STBase "Bool"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = boolSourceView,
                    checkedBindingType = boolElabTy,
                    checkedBindingTerm =
                      Elab.EUnroll unqualifiedStructuralNullaryConstructorTerm
                  }
            )
            checked0

    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) ->
        message
          `shouldSatisfy` isInfixOf "backend boundary term does not have its checked binding type"
      other ->
        expectationFailure ("expected unroll result rejection, got " ++ show other)

  it "lifts recursive parameterized deriving Eq helpers with captured evidence" $ do
    checked <- requireChecked recursiveListDerivingEqProgram
    mainBinding <- requireCheckedBinding "Main__main" checked
    let checkedEnv =
          ElabPipeline.mkTypeCheckEnvWithResolvedTerms
            [ (checkedBindingResolvedVar binding, checkedBindingType binding)
            | checkedModule <- checkedProgramModules checked,
              binding <- checkedModuleBindings checkedModule
            ]
            Map.empty
    backend <- requireRight (convertCheckedProgram checked)

    ElabPipeline.typeCheckWithEnv checkedEnv (checkedBindingTerm mainBinding)
      `shouldBe` Right (checkedBindingType mainBinding)
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

  it "rejects excess constructor head instantiations at the checked backend boundary" $ do
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
      Left (BackendTypeCheckFailed _ (TCInstantiationError _ _ message)) ->
        message `shouldSatisfy` isInfixOf "InstApp expects forall"
      Left err ->
        expectationFailure ("expected excess constructor type application rejection, got " ++ show err)
      Right backend ->
        expectationFailure ("expected constructor type application rejection, got " ++ show backend)

  it "rejects bounded constructor instantiation conflicts at the checked backend boundary" $ do
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
      Left (BackendTypeCheckFailed _ (TCInstantiationError _ _ message)) ->
        message `shouldSatisfy` isInfixOf "InstBot expects TBottom"
      Left err ->
        expectationFailure ("expected constructor type application mismatch, got " ++ show err)
      Right backend ->
        expectationFailure ("expected constructor type application rejection, got " ++ show backend)

  it "rejects same-named constructor instantiation mismatches by identity at the checked boundary" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    dataInfo <- requireCheckedData "Main.Option" checked0
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = polymorphicOptionSourceView dataInfo identityPlaceholderExpectedRef,
                    checkedBindingType = polymorphicOptionElabTy dataInfo,
                    checkedBindingTerm = staleSomeInPolymorphicOptionTerm checked0
                  }
            )
            checked0

    case convertCheckedProgram checked of
      Left (BackendTypeCheckFailed _ (TCArgumentMismatch expectedTy actualTy)) -> do
        expectedTy `shouldBe` boolElabTy
        actualTy `shouldSatisfy` \case
          Elab.TVarRef _ -> True
          _ -> False
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
                        checkedBindingType = identityPlaceholderPolymorphicOptionElabTy dataInfo,
                        checkedBindingTerm = identityPlaceholderSomeTerm checked0
                      }
                )
                checked0

        backend <- requireRight (convertCheckedProgram checked)
        validateBackendProgram backend `shouldBe` Right ()
      Nothing -> expectationFailure "expected Main.Option data info"

  it "keeps the local constructor field owner over a conflicting TypeView identity" $ do
    checked0 <- requireChecked constructorFieldHeadIdentityProgram
    carrierData <- requireCheckedData "Main.Carrier" checked0
    packConstructor <- requireCheckedConstructor "Main__Pack" checked0
    let expectedIdentity = symbolIdentityFromParts (UniqueIdentity 2000000100) SymbolType "Fixture" "Carrier" Nothing
        actualIdentity = dataInfoSymbol carrierData
        checked =
          withConstructorTypeView
            "Main__Pack"
            (constructorFieldHeadView expectedIdentity "Main.Carrier" packConstructor)
            checked0
    backend <- requireRight (convertCheckedProgram checked)
    constructor <- requireConstructor "Main__Pack" backend
    case backendConstructorFields constructor of
      [BTConWithIdentity fieldIdentity _ _] -> do
        fieldIdentity `shouldBe` actualIdentity
        fieldIdentity `shouldNotBe` expectedIdentity
      fields ->
        expectationFailure ("expected one identity-bearing constructor field, got " ++ show fields)

  it "accepts stale constructor field head displays when identities match" $ do
    checked0 <- requireChecked constructorFieldHeadIdentityProgram
    carrierData <- requireCheckedData "Main.Carrier" checked0
    packConstructor <- requireCheckedConstructor "Main__Pack" checked0
    let carrierIdentity = dataInfoSymbol carrierData
        checked =
          withConstructorTypeView
            "Main__Pack"
            (constructorFieldHeadView carrierIdentity "$stale_carrier" packConstructor)
            checked0
    backend <- requireRight (convertCheckedProgram checked)
    validateBackendProgram backend `shouldBe` Right ()

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

  it "rejects a vacuous recursive checked type that mismatches its constructor result" $ do
    checked0 <- requireChecked vacuousRecursiveConstructorFallbackProgram
    let checked =
          withConstructorResult "Main__MkBox" (STBase "Int") $
            mapMainBinding
              ( \binding ->
                  binding {checkedBindingType = testTMu "b" boolElabTy}
              )
              checked0

    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) ->
        message `shouldSatisfy` isInfixOf "backend boundary term does not have its checked binding type"
      Left err ->
        expectationFailure ("expected constructor shape rejection, got " ++ show err)
      Right backend ->
        expectationFailure ("expected constructor shape rejection, got backend:\n" ++ show backend)

  it "rejects duplicate checked module identities before building backend context maps" $ do
    checked0 <- requireChecked duplicateDataNameProgram
    case checkedProgramModules checked0 of
      firstModule : secondModule : _ -> do
        let duplicateIdentity = checkedModuleIdentity firstModule
            checked =
              replaceCheckedModuleIdentity
                (checkedModuleName secondModule)
                duplicateIdentity
                checked0
        convertCheckedProgram checked
          `shouldBe` Left (BackendValidationFailed (BackendDuplicateModule (symbolIdentityStableName duplicateIdentity)))
      _ ->
        expectationFailure "expected at least two checked modules"

  it "rejects duplicate embedded resolved module identities before building backend context maps" $ do
    checked0 <- requireChecked duplicateDataNameProgram
    case resolvedProgramModules (checkedProgramResolved checked0) of
      firstModule : secondModule : _ -> do
        let duplicateIdentity = resolvedSemanticModuleIdentity (resolvedModuleSemantic firstModule)
            checked =
              replaceResolvedModuleIdentity
                (resolvedSemanticModuleName (resolvedModuleSemantic secondModule))
                duplicateIdentity
                checked0
        convertCheckedProgram checked
          `shouldBe` Left (BackendValidationFailed (BackendDuplicateModule (symbolIdentityStableName duplicateIdentity)))
      _ ->
        expectationFailure "expected at least two resolved modules"

  it "rejects embedded resolved module identity payload conflicts before building backend context maps" $ do
    checked0 <- requireChecked duplicateDataNameProgram
    case resolvedProgramModules (checkedProgramResolved checked0) of
      firstModule : secondModule : _ -> do
        let duplicateIdentity = resolvedSemanticModuleIdentity (resolvedModuleSemantic firstModule)
            conflictingIdentity = renameSymbolDefiningName "$stale_resolved_module" duplicateIdentity
            checked =
              replaceResolvedModuleIdentity
                (resolvedSemanticModuleName (resolvedModuleSemantic secondModule))
                conflictingIdentity
                checked0
        convertCheckedProgram checked
          `shouldBe` Left (BackendValidationFailed (BackendConflictingIdentityPayload "resolved module" (symbolIdentityStableName duplicateIdentity)))
      _ ->
        expectationFailure "expected at least two resolved modules"

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

  it "rejects duplicate checked binding identities before building backend context maps" $ do
    checked0 <- requireChecked duplicateBindingIdentityProgram
    mainBinding <- requireCheckedBinding "Main__main" checked0
    duplicateIdentity <- requireTopLevelBindingIdentity mainBinding
    let checked =
          replaceBindingTopLevelIdentity
            "Main__helper"
            duplicateIdentity
            checked0
    convertCheckedProgram checked
      `shouldBe` Left (BackendValidationFailed (BackendDuplicateBinding (symbolIdentityStableName duplicateIdentity)))

  it "rejects checked binding identity payload conflicts before building backend context maps" $ do
    checked0 <- requireChecked duplicateBindingIdentityProgram
    mainBinding <- requireCheckedBinding "Main__main" checked0
    duplicateIdentity <- requireTopLevelBindingIdentity mainBinding
    let conflictingIdentity = renameSymbolDefiningName "$stale_main" duplicateIdentity
        checked =
          replaceBindingTopLevelIdentity
            "Main__helper"
            conflictingIdentity
            checked0
    convertCheckedProgram checked
      `shouldBe` Left (BackendValidationFailed (BackendConflictingIdentityPayload "binding" (symbolIdentityStableName duplicateIdentity)))

  it "rejects checked bindings with no symbol identity before building backend context maps" $ do
    checked0 <- requireChecked duplicateBindingIdentityProgram
    let checked =
          replaceBindingDetails
            "Main__helper"
            (LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991753)) "helper"))
            checked0
    convertCheckedProgram checked
      `shouldSatisfy` isLeft

  it "rejects checked binding identities that collide with primitive identities" $ do
    checked0 <- requireChecked duplicateBindingIdentityProgram
    let primitiveIdentity = builtinValueIdentity PrimitiveInventory.stringFromIntPrimitiveName
        checked =
          replaceBindingTopLevelIdentity
            "Main__helper"
            primitiveIdentity
            checked0
    convertCheckedProgram checked
      `shouldBe` Left (BackendValidationFailed (BackendDuplicateBinding (symbolIdentityStableName primitiveIdentity)))

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

  it "does not recover generated structural self binders by unqualified name" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    dataInfo <- requireCheckedData "Main.T" checked0
    let sourceView =
          identityCompleteFixtureTypeView
            [("Main.T", dataInfoSymbol dataInfo)]
            []
            (mkTypeView (STBase "Main.T") (STBase "Main.T"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = sourceView,
                    checkedBindingType = dataInfoElabBase dataInfo "Main.T",
                    checkedBindingTerm = unqualifiedStructuralNullaryConstructorTerm
                  }
            )
            checked0

    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) -> do
        message `shouldSatisfy` isInfixOf "backend boundary term does not have its checked binding type"
        message `shouldSatisfy` isInfixOf "$T_self"
      other ->
        expectationFailure ("expected generated structural self binder rejection, got " ++ show other)

  it "recovers structural constructor owners by result type identity when display head is stale" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    case find ((== "Main.T") . dataInfoIdentityQualifiedName) (checkedDataInfos checked0) of
      Just dataInfo -> do
        let staleResultTy =
              dataOwnedStructuralNullaryElabTy
                dataInfo
                "$stale_structural_self"
                "$stale_structural_result"
            sourceView =
              identityCompleteFixtureTypeView
                [("Main.T", dataInfoSymbol dataInfo)]
                []
                (mkTypeView (STBase "Main.T") (STBase "Main.T"))
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView = sourceView,
                        checkedBindingType = dataInfoElabBase dataInfo "Main.T",
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

  it "does not recover structural constructor owners from conflicting result type identity payloads" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    case find ((== "Main.T") . dataInfoIdentityQualifiedName) (checkedDataInfos checked0) of
      Just dataInfo -> do
        let conflictingIdentity = renameSymbolDefiningName "$stale_Main_T" (dataInfoSymbol dataInfo)
            staleResultTy =
              Elab.TBaseWithIdentity
                conflictingIdentity
                (BaseTy "$stale_structural_result")
            sourceView =
              identityCompleteFixtureTypeView
                [("Main.T", dataInfoSymbol dataInfo)]
                []
                (mkTypeView (STBase "Main.T") (STBase "Main.T"))
            checked =
              mapMainBinding
                ( \binding ->
                    binding
                      { checkedBindingSourceTypeView = sourceView,
                        checkedBindingType = dataInfoElabBase dataInfo "Main.T",
                        checkedBindingTerm = structuralNullaryConstructorTermWithResult staleResultTy
                      }
                )
                checked0

        convertCheckedProgram checked `shouldSatisfy` isLeft
      Nothing -> expectationFailure "missing Main.T data info"

  it "rejects nominal result heads carrying a different identity from their source view" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    dataInfo <- requireCheckedData "Main.T" checked0
    let sourceView =
          identityCompleteFixtureTypeView
            [("Main.T", dataInfoSymbol dataInfo)]
            []
            (mkTypeView (STBase "Main.T") (STBase "Main.T"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = sourceView,
                    checkedBindingType = dataInfoElabBase dataInfo "Main.T",
                    checkedBindingTerm = structuralNullaryConstructorTermWithResult (TestElab.tBase (BaseTy "Main.T"))
                  }
            )
            checked0

    convertCheckedProgram checked `shouldSatisfy` isLeft

  it "does not recover structural constructors by name when result self identity is not structural" $ do
    checked0 <- requireChecked sameNameUnqualifiedStructuralOwnerProgram
    dataInfo <- requireCheckedData "Main.T" checked0
    let sourceView =
          identityCompleteFixtureTypeView
            [("Main.T", dataInfoSymbol dataInfo)]
            []
            (mkTypeView (STBase "Main.T") (STBase "Main.T"))
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = sourceView,
                    checkedBindingType = dataInfoElabBase dataInfo "Main.T",
                    checkedBindingTerm = structuralNullaryConstructorTermWithResult nonStructuralSelfIdentityTElabTy
                  }
            )
            checked0
    case convertCheckedProgram checked of
      Left (BackendUnsupportedCaseShape message) -> do
        message `shouldSatisfy` isInfixOf "backend boundary term does not have its checked binding type"
        message `shouldSatisfy` isInfixOf "NodeId {getNodeId = 9110}"
        message `shouldSatisfy` isInfixOf "$T_self"
      other ->
        expectationFailure ("expected non-recovered structural roll rejection, got " ++ show other)

  it "rejects app-like instantiations on non-forall checked terms" $ do
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
    case convertCheckedProgram checked of
      Left (BackendTypeCheckFailed _ (TCInstantiationError _ ty message)) -> do
        ty `shouldBe` intElabTy
        message `shouldSatisfy` isInfixOf "InstApp expects forall"
      other ->
        expectationFailure
          ("expected non-forall instantiation rejection, got " ++ show other)

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
            { checkedBindingSourceTypeView = intSourceTypeView,
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
            { checkedBindingSourceTypeView = intSourceTypeView,
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
        let identity = backendBindingIdentity binding
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
          setTypeViewBinderIdentities
            (Map.singleton "reserved" reservedSourceIdentity)
            intSourceTypeView
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
        let identity = backendBindingIdentity binding
      ]
      of
      [UniqueIdentity helperUnique] ->
        helperUnique `shouldSatisfy` (> 2000000000)
      helperUniques ->
        expectationFailure ("expected one lifted helper identity, got " ++ show helperUniques)

  it "seeds lifted helper identities from checked constructor type view metadata identities" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    ctorInfo <- requireCheckedConstructor "Main__Some" checked0
    let reservedUnique = UniqueIdentity 2000000005
        reservedHeadIdentity =
          symbolIdentityFromParts reservedUnique SymbolType "Main" "ReservedCtorHead" Nothing
        ctorView =
          setTypeViewHeadIdentities
            (Map.insert "ReservedCtorHead" reservedHeadIdentity (typeViewHeadIdentities (ctorTypeView ctorInfo)))
            (ctorTypeView ctorInfo)
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingSourceTypeView = intSourceTypeView,
                    checkedBindingType = intElabTy,
                    checkedBindingTerm = recursiveIntLiftTerm
                  }
            )
            (withConstructorTypeView "Main__Some" ctorView checked0)
    backend <- requireRight (convertCheckedProgram checked)

    case
      [ symbolUniqueIdentity identity
      | binding <- backendBindings backend,
        "$letrec$" `isInfixOf` backendBindingName binding,
        let identity = backendBindingIdentity binding
      ]
      of
      [UniqueIdentity helperUnique] ->
        helperUnique `shouldSatisfy` (> 2000000005)
      helperUniques ->
        expectationFailure ("expected one lifted helper identity, got " ++ show helperUniques)

  it "seeds lifted helper identities from deferred constructor type head identities" $ do
    checked0 <- requireChecked parameterizedConstructorProgram
    ctorInfo <- requireCheckedConstructor "Main__Some" checked0
    let reservedUnique = UniqueIdentity 2000000010
        reservedHeadIdentity =
          symbolIdentityFromParts reservedUnique SymbolType "Main" "ReservedHead" Nothing
        ownerIdentity = ctorOwningTypeIdentity ctorInfo
        ownerStableName = symbolIdentityStableName ownerIdentity
        deferredTypeView =
          setTypeViewHeadIdentities
            ( Map.fromList
                [ ("Main.Option", ownerIdentity),
                  (ownerStableName, ownerIdentity),
                  ("ReservedHead", reservedHeadIdentity)
                ]
            )
            (mkTypeView (STBase "Main.Option") (STBase ownerStableName))
        deferredRef = deferredRefFromIdentity (UniqueIdentity 2000000000) "$deferred"
        deferredConstructor =
          DeferredConstructorCall
            { deferredConstructorRef = deferredRef,
              deferredConstructorInfo = ctorInfo,
              deferredConstructorArgCount = 0,
              deferredConstructorSourceTypeView = deferredTypeView,
              deferredConstructorOccurrenceTypeView = deferredTypeView,
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
                    checkedBindingSourceTypeView = intSourceTypeView,
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
        let identity = backendBindingIdentity binding
      ]
      of
      [UniqueIdentity helperUnique] ->
        helperUnique `shouldSatisfy` (> 2000000010)
      helperUniques ->
        expectationFailure ("expected one lifted helper identity, got " ++ show helperUniques)

  it "seeds lifted helper identities from resolved artifact identities" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let reservedUnique = UniqueIdentity 2000000020
        checked =
          injectResolvedLocalValueIdentity reservedUnique $
            mapMainBinding
              ( \binding ->
                  binding
                    { checkedBindingSourceTypeView = intSourceTypeView,
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
        let identity = backendBindingIdentity binding
      ]
      of
      [UniqueIdentity helperUnique] ->
        helperUnique `shouldSatisfy` (> 2000000020)
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
    let binderRefs = backendExprBinderRefs (backendBindingExpr helper)
        helperEvidenceBinders =
          [(name, details) | (name, details@EvidenceId {}) <- binderRefs]
        shadowingEvidenceBinders =
          [details | ("$evidence_E", details@LocalId {}) <- binderRefs]
    case (helperEvidenceBinders, shadowingEvidenceBinders) of
      ([(helperEvidenceName, helperEvidence)], [shadowingEvidence]) -> do
        helperEvidenceName `shouldNotBe` "$evidence_E"
        helperEvidence `shouldNotBe` shadowingEvidence
        backendBindingEvidenceParamIndices helper `shouldBe` Set.singleton 0
      other ->
        expectationFailure ("expected helper-owned and shadowing evidence binders, got " ++ show other)

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
    helper <- requireSingleLiftedHelper backend
    let helperEvidenceBinders =
          [ details
          | (_, details@EvidenceId {}) <- backendExprBinderRefs (backendBindingExpr helper)
          ]
    length helperEvidenceBinders `shouldBe` 1
    backendBindingEvidenceParamIndices helper `shouldBe` Set.singleton 0

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
      BTForallWithIdentity (typeIdentity) "a" Nothing bodyTy -> do
        bodyTy `shouldBe` unaryIntBackendTy
        backendBindingExpr helper `shouldSatisfy` containsBackendTyAppArgument (BTVarWithIdentity (typeIdentity) "a")
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
        ((Elab.typeBinderRefIdentity sameNamedInnerTypeRef))
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
        ((Elab.typeBinderRefIdentity recursiveTypeBoundScopeOuterRef))
        "a"
        Nothing
        unaryIntBackendTy
    backendBindingExpr helper `shouldSatisfy` containsFreshenedTypeAbsWithOuterBound

  it "freshens recursive helper type-abstraction identity during substitution" $ do
    checked0 <- requireChecked simpleFunctionProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = recursiveTypeIdentityCaptureElabTy,
                    checkedBindingTerm = recursiveTypeIdentityCaptureTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    helper <- requireSingleLiftedHelper backend
    let capturedIdentity = Elab.typeBinderRefIdentity recursiveTypeIdentityCaptureRef
        typeAbsIdentities = map snd (backendTypeAbsBinders (backendBindingExpr helper))
    length (filter (== capturedIdentity) typeAbsIdentities) `shouldBe` 1
    typeAbsIdentities `shouldSatisfy` any (/= capturedIdentity)

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
        ((Elab.typeBinderRefIdentity recursiveNestedTypeBoundScopeOuterRef))
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
      BTForallWithIdentity _ "z" Nothing (BTForallWithIdentity _ "a" Nothing bodyTy) ->
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
      (entryIdentity, entryName) : _ -> do
        entryName `shouldSatisfy` isPrefixOf "__mlfp_closure$Main__main$"
        closureNameUniqueSuffix entryName `shouldBe` Just (uniqueIdentityValue entryIdentity)
      [] ->
        expectationFailure "expected converted closure entry"

  it "keeps saturated local function aliases on the direct call path" $ do
    checked <- requireChecked closureAliasCallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendApp
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendClosureCall

  it "keeps saturated captured local aliases on the direct call path" $ do
    checked <- requireChecked capturedClosureCallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendApp
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendClosure
    backendBindingExpr mainBinding `shouldNotSatisfy` containsBackendClosureCall

  it "closure-converts closure-valued function parameters at call sites" $ do
    checked <- requireChecked functionParameterClosureCallProgram
    checkedUse <- requireCheckedBinding "Main__use" checked
    let checkedEnv =
          ElabPipeline.mkTypeCheckEnvWithResolvedTerms
            [ (checkedBindingResolvedVar binding, checkedBindingType binding)
            | checkedModule <- checkedProgramModules checked,
              binding <- checkedModuleBindings checkedModule
            ]
            Map.empty
    backend <- requireRight (convertCheckedProgram checked)

    ElabPipeline.typeCheckWithEnv checkedEnv (checkedBindingTerm checkedUse)
      `shouldBe` Right (checkedBindingType checkedUse)
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

  it "does not classify top-level closure heads through stale identity payloads" $ do
    checked0 <- requireChecked topLevelClosureCallProgram
    makerBinding <- requireCheckedBinding "Main__maker" checked0
    mainBinding <- requireCheckedBinding (checkedProgramMain checked0) checked0
    makerIdentity <- requireTopLevelBindingIdentity makerBinding
    let staleMakerIdentity = renameSymbolDefiningName "$stale_maker" makerIdentity
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      poisonTopLevelTermIdentity
                        makerIdentity
                        staleMakerIdentity
                        "$stale_maker"
                        (checkedBindingTerm mainBinding)
                  }
            )
            checked0
    case convertCheckedProgram checked of
      Left (BackendTypeCheckFailed _ (TCUnboundVar name)) ->
        name `shouldBe` "Main__$stale_maker"
      other ->
        expectationFailure ("expected stale top-level identity rejection, got " ++ show other)

  it "rejects stale top-level identity payloads" $ do
    checked0 <- requireChecked topLevelClosureCallProgram
    makerBinding <- requireCheckedBinding "Main__maker" checked0
    mainBinding <- requireCheckedBinding (checkedProgramMain checked0) checked0
    makerIdentity <- requireTopLevelBindingIdentity makerBinding
    let staleMakerIdentity = renameSymbolDefiningName "$stale_maker" makerIdentity
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      poisonTopLevelTermIdentity
                        makerIdentity
                        staleMakerIdentity
                        "Main__maker"
                        (checkedBindingTerm mainBinding)
                  }
            )
            checked0
    case convertCheckedProgram checked of
      Left (BackendTypeCheckFailed _ (TCUnboundVar name)) ->
        name `shouldBe` "Main__$stale_maker"
      other ->
        expectationFailure ("expected stale top-level identity rejection, got " ++ show other)

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
    closureParamCounts (backendBindingExpr mainBinding) `shouldBe` [2]

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

  it "freshens returned closure parameters away from resolved identity stable aliases" $ do
    checked0 <- requireChecked returnedLetLambdaClosureProgram
    let firstParam = generatedResolvedLocal 9191 "x" "runtime-x" intElabTy
        stableAlias = idDetailsStableName (Elab.resolvedVarDetails firstParam)
        secondParam = generatedResolvedLocal 9192 stableAlias "runtime-stable-alias" intElabTy
        fBinder = generatedResolvedLocal 9193 "f" "runtime-f" binaryIntElabTy
        checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingTerm =
                      Elab.ELet
                        fBinder
                        (schemeFromType binaryIntElabTy)
                        (Elab.ELam firstParam (Elab.ELam secondParam (Elab.EVarNode secondParam)))
                        (Elab.EVarNode fBinder)
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    mainBinding <- requireBinding (backendProgramMain backend) backend
    case filter ((== 2) . length) (closureParamNameGroups (backendBindingExpr mainBinding)) of
      [[_, secondName]] ->
        secondName `shouldNotBe` stableAlias
      groups ->
        expectationFailure ("expected one two-parameter closure, got " ++ show groups)

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
                    checkedBindingSourceTypeView =
                      identityCompleteFixtureTypeView
                        []
                        []
                        (mkTypeView (STArrow (STBase "Int") (STBase "Int")) (STArrow (STBase "Int") (STBase "Int"))),
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
                    checkedBindingSourceTypeView = intSourceTypeView,
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

transformedBoundSourceTypeView :: TypeView
transformedBoundSourceTypeView =
  identityCompleteFixtureTypeView
    []
    [("alpha", Elab.typeBinderRefIdentity transformedBoundOuterRef)]
    (mkTypeView sourceTy sourceTy)
  where
    sourceTy =
      STForall
        "alpha"
        Nothing
        (STArrow (STVar "alpha") (STVar "alpha"))

transformedBoundResultType :: Elab.ElabType
transformedBoundResultType =
  Elab.TForallRef
    transformedBoundOuterRef
    Nothing
    ( Elab.TArrow
        (Elab.TVarRef transformedBoundOuterRef)
        (Elab.TVarRef transformedBoundOuterRef)
    )

-- This fixture directly constructs the xMLF computation
-- @Inside (App alpha); N@.  The producer's leading binder is bounded by
-- @forall a. a -> a@, so transforming that bound at @alpha@ must make the
-- backend type application argument @alpha -> alpha@ while @alpha@ is in
-- lexical scope.  The paper's @g g@ instead applies @g@ to the complete
-- @sigma-id@ bound and therefore cannot witness this conversion branch.
transformedBoundTerm :: Elab.XmlfTerm
transformedBoundTerm =
  Elab.ETyAbsRef transformedBoundOuterRef Nothing
    ( Elab.ETyInst
        transformedBoundProducer
        ( Elab.InstSeq
            ( Elab.InstInside
                (Elab.InstApp (Elab.TVarRef transformedBoundOuterRef))
            )
            Elab.InstElim
        )
    )

transformedBoundProducer :: Elab.XmlfTerm
transformedBoundProducer =
  Elab.ETyAbsRef transformedBoundResultRef
    (Just transformedIdentityBound)
    ( Elab.ETyInst
        transformedIdentityTerm
        (Elab.InstAbstrRef transformedBoundResultRef)
    )

transformedIdentityTerm :: Elab.XmlfTerm
transformedIdentityTerm =
  Elab.ETyAbsRef transformedIdentityRef Nothing
    ( mkTestLocalLam
        "transformed-bound-x"
        (Elab.TVarRef transformedIdentityRef)
        (mkTestDeferredVar "transformed-bound-x")
    )

transformedIdentityBound :: Elab.BoundType
transformedIdentityBound =
  Elab.TForallRef transformedIdentityRef Nothing
    ( Elab.TArrow
        (Elab.TVarRef transformedIdentityRef)
        (Elab.TVarRef transformedIdentityRef)
    )

transformedBoundOuterRef :: Elab.TypeBinderRef
transformedBoundOuterRef = backendFixtureTypeRef 9040 "alpha"

transformedBoundResultRef :: Elab.TypeBinderRef
transformedBoundResultRef = backendFixtureTypeRef 9041 "result"

transformedIdentityRef :: Elab.TypeBinderRef
transformedIdentityRef = backendFixtureTypeRef 9042 "a"

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

localFirstClassPolymorphismProgram :: String
localFirstClassPolymorphismProgram =
  unlines
    [ "module Main export (main) {",
      "  def main : Bool =",
      "    let usePoly : (∀ a. a -> a) -> Bool =",
      "      λ(poly : ∀ a. a -> a) let keepInt = poly 1 in poly true",
      "    in let id : ∀ a. a -> a = λx x in usePoly id;",
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

constructorFieldHeadIdentityProgram :: String
constructorFieldHeadIdentityProgram =
  unlines
    [ "module Main export (Carrier(..), Pack(..), main) {",
      "  data Carrier a =",
      "      Carrier : a -> Carrier a;",
      "",
      "  data Pack =",
      "      Pack : Carrier Int -> Pack;",
      "",
      "  def main : Pack = Pack (Carrier 1);",
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

duplicateBindingIdentityProgram :: String
duplicateBindingIdentityProgram =
  unlines
    [ "module Main export (main) {",
      "  def helper : Int = 0;",
      "  def main : Int = helper;",
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

requireCheckedWithPrelude :: Program -> IO CheckedProgram
requireCheckedWithPrelude =
  requireRight
    . checkProgramPackage
    . withPreludePackage
    . trivialProgramPackage

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
  let termOrdinals = backendSnapshotTermOrdinals backend
   in unlines $
        [ "backend-program",
          "  main: " ++ backendProgramMain backend,
          "  modules:"
        ]
          ++ concatMap (renderBackendIRModule termOrdinals) (backendProgramModules backend)

renderBackendIRModule :: Map.Map ResolvedTermIdentityKey Int -> BackendModule -> [String]
renderBackendIRModule termOrdinals backendModule =
  [ indent 4 ("module " ++ backendModuleName backendModule),
    indent 6 "data:"
  ]
    ++ renderListOrEmpty 8 renderBackendIRData (backendModuleData backendModule)
    ++ [indent 6 "bindings:"]
    ++ renderListOrEmpty 8 (renderBackendIRBinding termOrdinals) (backendModuleBindings backendModule)

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

renderBackendIRBinding :: Map.Map ResolvedTermIdentityKey Int -> BackendBinding -> [String]
renderBackendIRBinding termOrdinals binding =
  [ indent 8 ("binding " ++ backendBindingName binding ++ " : " ++ renderBackendIRType (backendBindingType binding)),
    indent 10 ("exported-main: " ++ renderBool (backendBindingExportedAsMain binding)),
    indent 10 "expr:"
  ]
    ++ renderBackendIRExpr termOrdinals 12 (backendBindingExpr binding)

renderBackendIRExpr :: Map.Map ResolvedTermIdentityKey Int -> Int -> BackendExpr -> [String]
renderBackendIRExpr termOrdinals level expr =
  case expr of
    BackendVarWithIdentity resultTy identity name ->
      [indent level ("var " ++ renderBackendSnapshotTermName termOrdinals identity name ++ " : " ++ renderBackendIRType resultTy)]
    BackendLit resultTy lit ->
      [indent level ("lit " ++ renderLit lit ++ " : " ++ renderBackendIRType resultTy)]
    BackendLamWithIdentity resultTy identity name paramTy body ->
      [ indent level ("lam " ++ renderBackendSnapshotTermName termOrdinals identity name ++ " : " ++ renderBackendIRType paramTy ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "body:"
      ]
        ++ renderBackendIRExpr termOrdinals (level + 4) body
    BackendApp resultTy fun arg ->
      [ indent level ("app : " ++ renderBackendIRType resultTy),
        indent (level + 2) "function:"
      ]
        ++ renderBackendIRExpr termOrdinals (level + 4) fun
        ++ [indent (level + 2) "argument:"]
        ++ renderBackendIRExpr termOrdinals (level + 4) arg
    BackendLetWithIdentity resultTy identity name bindingTy rhs body ->
      [ indent level ("let " ++ renderBackendSnapshotTermName termOrdinals identity name ++ " : " ++ renderBackendIRType bindingTy ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "rhs:"
      ]
        ++ renderBackendIRExpr termOrdinals (level + 4) rhs
        ++ [indent (level + 2) "body:"]
        ++ renderBackendIRExpr termOrdinals (level + 4) body
    BackendTyAbsWithIdentity resultTy _ name mbBound body ->
      [ indent level ("type-lam " ++ renderTypeBinder name mbBound ++ " -> " ++ renderBackendIRType resultTy),
        indent (level + 2) "body:"
      ]
        ++ renderBackendIRExpr termOrdinals (level + 4) body
    BackendTyApp resultTy fun tyArg ->
      [ indent level ("type-app [" ++ renderBackendIRType tyArg ++ "] : " ++ renderBackendIRType resultTy),
        indent (level + 2) "function:"
      ]
        ++ renderBackendIRExpr termOrdinals (level + 4) fun
    BackendConstructWithIdentity resultTy _ name args ->
      [ indent level ("construct " ++ name ++ " : " ++ renderBackendIRType resultTy),
        indent (level + 2) "args:"
      ]
        ++ renderExprList termOrdinals (level + 4) args
    BackendCase resultTy scrutinee alternatives ->
      [ indent level ("case : " ++ renderBackendIRType resultTy),
        indent (level + 2) "scrutinee:"
      ]
        ++ renderBackendIRExpr termOrdinals (level + 4) scrutinee
        ++ [indent (level + 2) "alternatives:"]
        ++ concatMap (renderBackendIRAlternative termOrdinals (level + 4)) (toList alternatives)
    BackendRoll resultTy payload ->
      [ indent level ("roll : " ++ renderBackendIRType resultTy),
        indent (level + 2) "payload:"
      ]
        ++ renderBackendIRExpr termOrdinals (level + 4) payload
    BackendUnroll resultTy payload ->
      [ indent level ("unroll : " ++ renderBackendIRType resultTy),
        indent (level + 2) "payload:"
      ]
        ++ renderBackendIRExpr termOrdinals (level + 4) payload
    BackendClosureWithParamIdentities resultTy _ entryName captures params body ->
      [ indent level ("closure " ++ entryName ++ " : " ++ renderBackendIRType resultTy),
        indent
          (level + 2)
          ( "params: "
              ++ renderNamedTypeList
                [ ( renderBackendSnapshotTermName termOrdinals (backendClosureParamIdentity param) (backendClosureParamName param),
                    backendClosureParamType param
                  )
                | param <- params
                ]
          ),
        indent (level + 2) "captures:"
      ]
        ++ renderListOrEmpty (level + 4) (renderBackendIRCapture termOrdinals (level + 4)) captures
        ++ [indent (level + 2) "body:"]
        ++ renderBackendIRExpr termOrdinals (level + 4) body
    BackendClosureCall resultTy fun args ->
      [ indent level ("closure-call : " ++ renderBackendIRType resultTy),
        indent (level + 2) "function:"
      ]
        ++ renderBackendIRExpr termOrdinals (level + 4) fun
        ++ [indent (level + 2) "arguments:"]
        ++ renderExprList termOrdinals (level + 4) args

renderBackendIRCapture :: Map.Map ResolvedTermIdentityKey Int -> Int -> BackendClosureCapture -> [String]
renderBackendIRCapture termOrdinals level capture =
  [ indent level (renderBackendSnapshotTermName termOrdinals (backendClosureCaptureIdentity capture) (backendClosureCaptureName capture) ++ " : " ++ renderBackendIRType (backendClosureCaptureType capture)),
    indent (level + 2) "expr:"
  ]
    ++ renderBackendIRExpr termOrdinals (level + 4) (backendClosureCaptureExpr capture)

renderBackendIRAlternative :: Map.Map ResolvedTermIdentityKey Int -> Int -> BackendAlternative -> [String]
renderBackendIRAlternative termOrdinals level alternative =
  [ indent level ("alternative " ++ renderBackendIRPattern termOrdinals (backendAltPattern alternative)),
    indent (level + 2) "body:"
  ]
    ++ renderBackendIRExpr termOrdinals (level + 4) (backendAltBody alternative)

renderBackendIRPattern :: Map.Map ResolvedTermIdentityKey Int -> BackendPattern -> String
renderBackendIRPattern termOrdinals pattern0 =
  case pattern0 of
    BackendDefaultPattern ->
      "default"
    BackendConstructorPatternWithBinderIdentities _ name binders ->
      name
        ++ "("
        ++ intercalate
          ", "
          [ renderBackendSnapshotTermName termOrdinals (backendPatternBinderIdentity binder) (backendPatternBinderName binder)
          | binder <- binders
          ]
        ++ ")"

renderExprList :: Map.Map ResolvedTermIdentityKey Int -> Int -> [BackendExpr] -> [String]
renderExprList termOrdinals level exprs =
  renderListOrEmpty level renderArg (zip [0 :: Int ..] exprs)
  where
    renderArg (ix, expr) =
      indent level ("arg " ++ show ix ++ ":") : renderBackendIRExpr termOrdinals (level + 2) expr

-- Snapshot-local ordinals deliberately ignore the process-wide identity
-- generator position. The map is keyed by semantic identity, so a reference
-- to the wrong binder still renders with the wrong ordinal and fails the
-- golden comparison; only the unstable numeric display suffix is normalized.
backendSnapshotTermOrdinals :: BackendProgram -> Map.Map ResolvedTermIdentityKey Int
backendSnapshotTermOrdinals backend =
  Map.fromListWith
    min
    [ (idDetailsIdentityKey identity, ordinal)
    | (ordinal, (identity, _)) <-
        zip
          [0 :: Int ..]
          [ binder
          | binder@(_, name) <- concatMap moduleBinders (backendProgramModules backend)
          , isJust (generatedLocalNameStem name)
          ]
    ]
  where
    moduleBinders =
      concatMap (exprBinders . backendBindingExpr) . backendModuleBindings

    exprBinders expr =
      case expr of
        BackendVarWithIdentity {} -> []
        BackendLit {} -> []
        BackendLamWithIdentity _ identity name _ body ->
          (identity, name) : exprBinders body
        BackendApp _ fun arg ->
          exprBinders fun ++ exprBinders arg
        BackendLetWithIdentity _ identity name _ rhs body ->
          (identity, name) : exprBinders rhs ++ exprBinders body
        BackendTyAbsWithIdentity _ _ _ _ body ->
          exprBinders body
        BackendTyApp _ fun _ ->
          exprBinders fun
        BackendRoll _ payload ->
          exprBinders payload
        BackendUnroll _ payload ->
          exprBinders payload
        BackendClosureWithParamIdentities _ _ _ captures params body ->
          [ (backendClosureParamIdentity param, backendClosureParamName param)
          | param <- params
          ]
            ++ concatMap captureBinders captures
            ++ exprBinders body
        BackendClosureCall _ fun args ->
          exprBinders fun ++ concatMap exprBinders args
        BackendConstructWithIdentity _ _ _ args ->
          concatMap exprBinders args
        BackendCase _ scrutinee alternatives ->
          exprBinders scrutinee ++ concatMap alternativeBinders (toList alternatives)

    captureBinders capture =
      (backendClosureCaptureIdentity capture, backendClosureCaptureName capture)
        : exprBinders (backendClosureCaptureExpr capture)

    alternativeBinders alternative =
      patternBinders (backendAltPattern alternative) ++ exprBinders (backendAltBody alternative)

    patternBinders pattern0 =
      case pattern0 of
        BackendDefaultPattern -> []
        BackendConstructorPatternWithBinderIdentities _ _ binders ->
          [ (backendPatternBinderIdentity binder, backendPatternBinderName binder)
          | binder <- binders
          ]

renderBackendSnapshotTermName :: Map.Map ResolvedTermIdentityKey Int -> IdDetails -> String -> String
renderBackendSnapshotTermName termOrdinals identity name =
  case (Map.lookup (idDetailsIdentityKey identity) termOrdinals, generatedLocalNameStem name) of
    (Just ordinal, Just stem) -> stem ++ "#" ++ show ordinal
    _ -> name

generatedLocalNameStem :: String -> Maybe String
generatedLocalNameStem name
  | "$" `isPrefixOf` name =
      case span isDigit (reverse name) of
        (digits, '#' : stemReversed)
          | not (null digits) -> Just (reverse stemReversed)
        _ -> Nothing
  | otherwise = Nothing

renderBackendIRType :: BackendType -> String
renderBackendIRType backendTy =
  case backendTy of
    BTVarWithIdentity _ name -> "$" ++ name
    BTArrow dom cod -> "(" ++ renderBackendIRType dom ++ " -> " ++ renderBackendIRType cod ++ ")"
    BTBaseWithIdentity _ (BaseTy name) -> name
    BTConWithIdentity _ (BaseTy name) args -> name ++ "<" ++ intercalate ", " (map renderBackendIRType (toList args)) ++ ">"
    BTVarAppWithIdentity _ name args -> "$" ++ name ++ "<" ++ intercalate ", " (map renderBackendIRType (toList args)) ++ ">"
    BTForallWithIdentity _ name mbBound body -> "forall " ++ renderTypeBinder name mbBound ++ ". " ++ renderBackendIRType body
    BTMuWithIdentity _ name body -> "mu " ++ name ++ ". " ++ renderBackendIRType body
    BTBottom -> "bottom"

renderBackendTypeBinders :: [BackendTypeBinder] -> String
renderBackendTypeBinders binders =
  case binders of
    [] -> ""
    _ -> "<" ++ intercalate ", " [renderTypeBinder name mbBound | BackendTypeBinderWithIdentity _ name mbBound <- binders] ++ ">"

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

requireCheckedData :: String -> CheckedProgram -> IO DataInfo
requireCheckedData name checked =
  case find ((== name) . dataInfoIdentityQualifiedName) (checkedDataInfos checked) of
    Just dataInfo -> pure dataInfo
    Nothing -> expectationFailure ("missing checked data " ++ show name) >> fail "missing checked data"

staleTypeViewBinderDisplay :: String -> String -> TypeView -> TypeView
staleTypeViewBinderDisplay oldName newName view =
  setTypeViewBinderIdentities
    ( maybe
        (typeViewBinderIdentities view)
        (\identity -> Map.insert newName identity (typeViewBinderIdentities view))
        (Map.lookup oldName (typeViewBinderIdentities view))
    )
    (setTypeViewDisplay (renameSourceTypeBinderDisplay oldName newName (typeViewDisplay view)) view)

renameSourceTypeBinderDisplay :: String -> String -> SrcType -> SrcType
renameSourceTypeBinderDisplay oldName newName =
  go
  where
    rename name
      | name == oldName = newName
      | otherwise = name

    go =
      \case
        STVar name -> STVar (rename name)
        STArrow dom cod -> STArrow (go dom) (go cod)
        STBase name -> STBase name
        STCon name args -> STCon name (fmap go args)
        STVarApp name args -> STVarApp (rename name) (fmap go args)
        STTyLam name body -> STTyLam (rename name) (go body)
        STTyApp fun arg -> STTyApp (go fun) (go arg)
        STForall name mbBound body -> STForall (rename name) (fmap (SrcBound . go . unSrcBound) mbBound) (go body)
        STMu name body -> STMu (rename name) (go body)
        STBottom -> STBottom

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
    Just valueInfo@OrdinaryValue {} <- [lookupInstanceMethod methodInfo instanceInfo],
    let runtimeName = valueInfoRuntimeName valueInfo
  ]

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

collectBackendClosureEntryRefs :: BackendExpr -> [(UniqueIdentity, String)]
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

closureParamNameGroups :: BackendExpr -> [[String]]
closureParamNameGroups expr =
  case expr of
    BackendClosure _ _ captures params body ->
      map fst params : concatMap (closureParamNameGroups . backendClosureCaptureExpr) captures ++ closureParamNameGroups body
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      closureParamNameGroups scrutinee ++ concatMap (closureParamNameGroups . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> closureParamNameGroups body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      closureParamNameGroups fun ++ closureParamNameGroups arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      closureParamNameGroups rhs ++ closureParamNameGroups body
    BackendTyAbsWithIdentity {backendTyAbsBody = body} -> closureParamNameGroups body
    BackendTyApp {backendTyFunction = fun} -> closureParamNameGroups fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> concatMap closureParamNameGroups args
    BackendRoll {backendRollPayload = body} -> closureParamNameGroups body
    BackendUnroll {backendUnrollPayload = body} -> closureParamNameGroups body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      closureParamNameGroups fun ++ concatMap closureParamNameGroups args
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

containsScopedTransformedBoundTypeApplication :: BackendExpr -> Bool
containsScopedTransformedBoundTypeApplication =
  go Set.empty
  where
    go boundIdentities expr =
      case expr of
        BackendTyAbsWithIdentity
          { backendTyParamIdentity = identity,
            backendTyAbsBody = body
          } ->
            go (Set.insert identity boundIdentities) body
        BackendTyApp
          { backendTyArgument = argumentTy,
            backendTyFunction = fun
          } ->
            isScopedIdentityArrow boundIdentities argumentTy
              || go boundIdentities fun
        BackendCase
          { backendScrutinee = scrutinee,
            backendAlternatives = alternatives
          } ->
            go boundIdentities scrutinee
              || any (go boundIdentities . backendAltBody) (toList alternatives)
        BackendLamWithIdentity {backendBody = body} ->
          go boundIdentities body
        BackendApp {backendFunction = fun, backendArgument = arg} ->
          go boundIdentities fun || go boundIdentities arg
        BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
          go boundIdentities rhs || go boundIdentities body
        BackendClosure _ _ captures _ body ->
          any (go boundIdentities . backendClosureCaptureExpr) captures
            || go boundIdentities body
        BackendClosureCall
          { backendClosureFunction = fun,
            backendClosureArguments = args
          } ->
            go boundIdentities fun || any (go boundIdentities) args
        BackendConstructWithIdentity {backendConstructArgs = args} ->
          any (go boundIdentities) args
        BackendRoll {backendRollPayload = body} ->
          go boundIdentities body
        BackendUnroll {backendUnrollPayload = body} ->
          go boundIdentities body
        _ -> False

    isScopedIdentityArrow boundIdentities argumentTy =
      case argumentTy of
        BTArrow
          (BTVarWithIdentity domainIdentity _)
          (BTVarWithIdentity codomainIdentity _) ->
            domainIdentity == codomainIdentity
              && Set.member domainIdentity boundIdentities
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

backendTypeAbsBinders :: BackendExpr -> [(String, TypeBinderIdentity)]
backendTypeAbsBinders expr =
  case expr of
    BackendTyAbsWithIdentity {backendTyParamName = name, backendTyParamIdentity = identity, backendTyAbsBody = body} ->
      (name, identity) : backendTypeAbsBinders body
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      backendTypeAbsBinders scrutinee ++ concatMap (backendTypeAbsBinders . backendAltBody) (toList alternatives)
    BackendLamWithIdentity {backendBody = body} -> backendTypeAbsBinders body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      backendTypeAbsBinders fun ++ backendTypeAbsBinders arg
    BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
      backendTypeAbsBinders rhs ++ backendTypeAbsBinders body
    BackendTyApp {backendTyFunction = fun} -> backendTypeAbsBinders fun
    BackendConstructWithIdentity {backendConstructArgs = args} -> concatMap backendTypeAbsBinders args
    BackendRoll {backendRollPayload = body} -> backendTypeAbsBinders body
    BackendUnroll {backendUnrollPayload = body} -> backendTypeAbsBinders body
    BackendClosureWithParamIdentities {backendClosureCaptures = captures, backendClosureBody = body} ->
      concatMap (backendTypeAbsBinders . backendClosureCaptureExpr) captures ++ backendTypeAbsBinders body
    BackendClosureCall {backendClosureFunction = fun, backendClosureArguments = args} ->
      backendTypeAbsBinders fun ++ concatMap backendTypeAbsBinders args
    _ -> []

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

backendExprBinderRefs :: BackendExpr -> [(String, IdDetails)]
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

    backendBinderRefMatches (left, _) (right, _) = left == right

isBackendFunctionType :: BackendType -> Bool
isBackendFunctionType ty =
  case ty of
    BTArrow {} -> True
    _ -> False

containsConstructArgTypeVar :: SymbolIdentity -> TypeBinderIdentity -> BackendExpr -> Bool
containsConstructArgTypeVar constructorIdentity argumentTypeIdentity =
  go
  where
    go expr =
      case expr of
        BackendConstructWithIdentity {backendConstructIdentity = identity, backendConstructArgs = args} ->
          (identity == constructorIdentity && any (matchesArgType . backendExprType) args)
            || any go args
        BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
          go scrutinee
            || any (go . backendAltBody) (toList alternatives)
        BackendLamWithIdentity {backendBody = body} -> go body
        BackendApp {backendFunction = fun, backendArgument = arg} ->
          (constructorHeadIdentity fun == Just constructorIdentity && matchesArgType (backendExprType arg))
            || go fun
            || go arg
        BackendLetWithIdentity {backendLetRhs = rhs, backendLetBody = body} ->
          go rhs || go body
        BackendTyAbsWithIdentity {backendTyAbsBody = body} -> go body
        BackendTyApp {backendTyFunction = fun} -> go fun
        BackendRoll {backendRollPayload = body} -> go body
        BackendUnroll {backendUnrollPayload = body} -> go body
        _ -> False

    matchesArgType ty =
      case ty of
        BTVarWithIdentity identity _ ->
          identity == argumentTypeIdentity
        _ ->
          False

    constructorHeadIdentity expr =
      case expr of
        BackendVarWithIdentity {backendVarIdentity = ConstructorId ref} ->
          Just (constructorRefSymbol ref)
        BackendTyApp {backendTyFunction = fun} ->
          constructorHeadIdentity fun
        _ ->
          Nothing

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

collectConstructIdentities :: BackendExpr -> [(String, SymbolIdentity)]
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

collectPatternIdentities :: NonEmpty BackendAlternative -> [(String, SymbolIdentity)]
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
        BackendLamWithIdentity resultTy identity name paramTy body ->
          BackendLamWithIdentity resultTy identity name paramTy (go body)
        BackendApp resultTy fun arg ->
          BackendApp resultTy (go fun) (go arg)
        BackendLetWithIdentity resultTy identity name bindingTy rhs body ->
          BackendLetWithIdentity resultTy identity name bindingTy (go rhs) (go body)
        BackendTyAbsWithIdentity resultTy identity name mbBound body ->
          BackendTyAbsWithIdentity resultTy identity name mbBound (go body)
        BackendTyApp resultTy fun tyArg ->
          BackendTyApp resultTy (go fun) tyArg
        BackendRoll resultTy payload ->
          BackendRoll resultTy (go payload)
        BackendUnroll resultTy payload ->
          BackendUnroll resultTy (go payload)
        BackendClosureWithParamIdentities resultTy identity entryName captures params body ->
          BackendClosureWithParamIdentities resultTy identity entryName (map renameCapture captures) params (go body)
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
        BackendLamWithIdentity resultTy identity name paramTy body ->
          BackendLamWithIdentity resultTy identity name paramTy (go body)
        BackendApp resultTy fun arg ->
          BackendApp resultTy (go fun) (go arg)
        BackendLetWithIdentity resultTy identity name bindingTy rhs body ->
          BackendLetWithIdentity resultTy identity name bindingTy (go rhs) (go body)
        BackendTyAbsWithIdentity resultTy identity name mbBound body ->
          BackendTyAbsWithIdentity resultTy identity name mbBound (go body)
        BackendTyApp resultTy fun tyArg ->
          BackendTyApp resultTy (go fun) tyArg
        BackendRoll resultTy payload ->
          BackendRoll resultTy (go payload)
        BackendUnroll resultTy payload ->
          BackendUnroll resultTy (go payload)
        BackendClosureWithParamIdentities resultTy identity entryName captures params body ->
          BackendClosureWithParamIdentities resultTy identity entryName (map renameCapture captures) params (go body)
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
  BTBaseWithIdentity ((builtinTypeIdentity "Int")) (BaseTy "Int")

identityCompleteFixtureTypeView :: [(String, SymbolIdentity)] -> [(String, TypeBinderIdentity)] -> TypeView -> TypeView
identityCompleteFixtureTypeView headEntries binderEntries view =
  setTypeViewBinderIdentities
    ( Map.unions
        [ Map.fromList (concatMap binderAliases binderEntries),
          typeViewBinderIdentities view
        ]
    )
    ( setTypeViewHeadIdentities
        ( Map.unions
            [ Map.fromList (concatMap headAliases headEntries),
              builtinHeadIdentities,
              typeViewHeadIdentities view
            ]
        )
        view
    )
  where
    builtinHeadIdentities =
      Map.fromList
        [ (name, identity)
        | name <- toList (typeHeadNamesSrcType (typeViewIdentity view)),
          Just identity <- [builtinTypeHeadIdentity name]
        ]

    headAliases (name, identity) =
      [ (name, identity),
        (symbolIdentityStableName identity, identity)
      ]

    binderAliases (name, identity) =
      [ (name, identity),
        (typeBinderIdentityStableName identity, identity)
      ]

intSourceTypeView :: TypeView
intSourceTypeView =
  identityCompleteFixtureTypeView [] [] (mkTypeView (STBase "Int") (STBase "Int"))

dataInfoElabBase :: DataInfo -> String -> Elab.ElabType
dataInfoElabBase dataInfo name =
  Elab.TBaseWithIdentity (dataInfoSymbol dataInfo) (BaseTy name)

boolTy :: BackendType
boolTy =
  BTBaseWithIdentity ((builtinTypeIdentity "Bool")) (BaseTy "Bool")

unaryIntBackendTy :: BackendType
unaryIntBackendTy =
  BTArrow intTy intTy

intElabTy :: Elab.ElabType
intElabTy =
  Elab.TBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int")

resolvedLocal :: String -> String -> Elab.ElabType -> Elab.ResolvedVar
resolvedLocal ref runtime ty =
  generatedResolvedLocalForName ref runtime ty

boolElabTy :: Elab.ElabType
boolElabTy =
  Elab.TBaseWithIdentity (builtinTypeIdentity "Bool") (BaseTy "Bool")

polymorphicOptionSourceTy :: SrcType
polymorphicOptionSourceTy =
  STForall
    "a"
    Nothing
    (STArrow (STVar "a") (STCon "Main.Option" (STVar "a" :| [])))

polymorphicOptionSourceView :: DataInfo -> Elab.TypeBinderRef -> TypeView
polymorphicOptionSourceView dataInfo ref =
  fixtureTypeView
    polymorphicOptionSourceTy
    polymorphicOptionSourceTy
    (Map.singleton "Main.Option" (dataInfoSymbol dataInfo))
    (Map.singleton "a" (Elab.typeBinderRefIdentity ref))

polymorphicOptionElabTy :: DataInfo -> Elab.ElabType
polymorphicOptionElabTy dataInfo =
  testTForall
    "a"
    Nothing
    ( Elab.TArrow
        (testTVar "a")
        (Elab.TConWithIdentity (dataInfoSymbol dataInfo) (BaseTy "Main.Option") (testTVar "a" :| []))
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

identityPlaceholderPolymorphicOptionElabTy :: DataInfo -> Elab.ElabType
identityPlaceholderPolymorphicOptionElabTy dataInfo =
  Elab.TForallRef
    identityPlaceholderExpectedRef
    Nothing
    ( Elab.TArrow
        (Elab.TVarRef identityPlaceholderExpectedRef)
        (Elab.TConWithIdentity (dataInfoSymbol dataInfo) (BaseTy "Main.Option") (Elab.TVarRef identityPlaceholderExpectedRef :| []))
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
            ( Elab.ETyInst
                (resolvedConstructorTerm checked "Main__Some")
                (Elab.InstApp (Elab.TVarRef identityPlaceholderTermRef))
            )
            (mkTestDeferredVar "x")
        )
    )

constructorFieldHeadView :: SymbolIdentity -> String -> ConstructorInfo -> TypeView
constructorFieldHeadView identity name constructor =
  identityCompleteFixtureTypeView
    [(name, identity)]
    []
    ( setTypeViewHeadIdentities
        (Map.insert name identity (typeViewHeadIdentities originalView))
        ( setTypeViewTypes
            (renameFirstConstructorFieldHead name (typeViewDisplay originalView))
            (renameFirstConstructorFieldHead name (typeViewIdentity originalView))
            originalView
        )
    )
  where
    originalView =
      ctorTypeView constructor

renameFirstConstructorFieldHead :: String -> SrcType -> SrcType
renameFirstConstructorFieldHead name =
  \case
    STForall binder mbBound body ->
      STForall binder mbBound (renameFirstConstructorFieldHead name body)
    STArrow (STCon _ args) resultTy ->
      STArrow (STCon name args) resultTy
    ty ->
      ty

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
            ( Elab.EApp
                ( Elab.ETyInst
                    (resolvedConstructorTerm checked "Main__Pack")
                    (instantiateLeadingForallWithRef sameNamedInnerTypeRef)
                )
                (mkTestDeferredVar "x")
            )
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

recursiveTypeIdentityCaptureElabTy :: Elab.ElabType
recursiveTypeIdentityCaptureElabTy =
  Elab.TForallRef recursiveTypeIdentityCaptureRef Nothing intElabTy

recursiveTypeIdentityCaptureTerm :: Elab.XmlfTerm
recursiveTypeIdentityCaptureTerm =
  Elab.ETyAbsRef
    recursiveTypeIdentityCaptureRef
    Nothing
    ( mkTestRecursiveLocalLet
        "loop"
        (schemeFromType unaryIntElabTy)
        recursiveTypeIdentityCaptureRhs
        (Elab.EApp (mkTestDeferredVar "loop") (Elab.ELit (LInt 0)))
    )

recursiveTypeIdentityCaptureRhs :: Elab.XmlfTerm
recursiveTypeIdentityCaptureRhs =
  mkTestLocalLam
    "n"
    intElabTy
    ( Elab.ETyInst
        ( Elab.ETyAbsRef
            recursiveTypeIdentityCaptureRef
            Nothing
            (Elab.EApp (mkTestDeferredVar "loop") (mkTestDeferredVar "n"))
        )
        (Elab.InstApp (Elab.TVarRef recursiveTypeIdentityCaptureRef))
    )

recursiveTypeIdentityCaptureRef :: Elab.TypeBinderRef
recursiveTypeIdentityCaptureRef =
  backendFixtureTypeRef 9012 "a"

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
  TestElab.tBase (BaseTy "Int")

boundedWrapElabTy :: Elab.ElabType -> Elab.ElabType
boundedWrapElabTy resultTy =
  Elab.TForallRef
    boundedWrapTypeRef
    (Just intElabBoundTy)
    (Elab.TArrow (Elab.TVarRef boundedWrapTypeRef) resultTy)

boundedWrapTerm :: CheckedProgram -> Elab.XmlfTerm
boundedWrapTerm checked =
  Elab.ETyAbsRef boundedWrapTypeRef
    (Just intElabBoundTy)
    ( mkTestLocalLam
        "x"
        (Elab.TVarRef boundedWrapTypeRef)
        ( Elab.EApp
            ( Elab.ETyInst
                (resolvedConstructorTerm checked "Main__Pack")
                (instantiateLeadingForallWithRef boundedWrapTypeRef)
            )
            (mkTestDeferredVar "x")
        )
    )

boundedWrapTypeRef :: Elab.TypeBinderRef
boundedWrapTypeRef =
  backendFixtureTypeRef 9030 "b"

dependentBoundedWrapElabTy :: Elab.ElabType -> Elab.ElabType
dependentBoundedWrapElabTy resultTy =
  Elab.TForallRef
    dependentBoundedWrapOuterRef
    (Just intElabBoundTy)
    ( Elab.TForallRef
        dependentBoundedWrapInnerRef
        (Just (dependentArrowElabBoundTy (Elab.TVarRef dependentBoundedWrapOuterRef)))
        (Elab.TArrow (Elab.TVarRef dependentBoundedWrapInnerRef) resultTy)
    )

dependentBoundedWrapTerm :: CheckedProgram -> Elab.XmlfTerm
dependentBoundedWrapTerm checked =
  Elab.ETyAbsRef dependentBoundedWrapOuterRef
    (Just intElabBoundTy)
    ( Elab.ETyAbsRef dependentBoundedWrapInnerRef
        (Just (dependentArrowElabBoundTy (Elab.TVarRef dependentBoundedWrapOuterRef)))
        ( mkTestLocalLam
            "x"
            (Elab.TVarRef dependentBoundedWrapInnerRef)
            ( Elab.EApp
                ( Elab.ETyInst
                    (resolvedConstructorTerm checked "Main__Pack")
                    (instantiateLeadingForallWithRef dependentBoundedWrapInnerRef)
                )
                (mkTestDeferredVar "x")
            )
        )
    )

dependentBoundedWrapOuterRef :: Elab.TypeBinderRef
dependentBoundedWrapOuterRef =
  backendFixtureTypeRef 9031 "z"

dependentBoundedWrapInnerRef :: Elab.TypeBinderRef
dependentBoundedWrapInnerRef =
  backendFixtureTypeRef 9032 "b"

instantiateLeadingForallWithRef :: Elab.TypeBinderRef -> Elab.Instantiation
instantiateLeadingForallWithRef ref =
  Elab.InstSeq
    (Elab.InstInside (Elab.InstAbstrRef ref))
    Elab.InstElim

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
        ( Elab.EApp
            ( Elab.ETyInst
                (resolvedConstructorTerm checked "Main__Pair")
                (Elab.InstApp polymorphicIdentityElabTy)
            )
            polymorphicIdentityTerm
        )
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

dataOwnedStructuralNullaryElabTy :: DataInfo -> String -> String -> Elab.ElabType
dataOwnedStructuralNullaryElabTy dataInfo selfName resultName =
  Elab.TMuRef
    selfRef
    ( Elab.TForallRef
        resultRef
        Nothing
        (Elab.TArrow (Elab.TVarRef resultRef) (Elab.TVarRef resultRef))
    )
  where
    ownerUnique = symbolUniqueIdentity (dataInfoSymbol dataInfo)
    selfRef =
      Elab.typeBinderRefFromIdentity
        (typeBinderIdentityFromStructural ownerUnique StructuralSelfBinder)
        selfName
    resultRef =
      Elab.typeBinderRefFromIdentity
        (typeBinderIdentityFromStructural ownerUnique StructuralResultBinder)
        resultName

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
renameCheckedProgramMainRuntimeName _replacement checked =
  checked

addDataInfo :: DataInfo -> CheckedProgram -> CheckedProgram
addDataInfo dataInfo checked =
  checked
    { checkedProgramModulesInternal =
        case checkedProgramModules checked of
          [] -> []
          checkedModule : rest ->
            checkedModule
              { checkedModuleData = Map.insert (dataInfoSymbol dataInfo) dataInfo (checkedModuleData checkedModule)
              }
              : rest
    }

injectResolvedLocalValueIdentity :: UniqueIdentity -> CheckedProgram -> CheckedProgram
injectResolvedLocalValueIdentity reservedUnique checked =
  checked
    { checkedProgramResolvedInternal =
        injectProgram (checkedProgramResolved checked)
    }
  where
    reservedSymbol =
      symbolIdentityFromParts reservedUnique SymbolValue "Main" "resolvedReserved" Nothing
    reservedValue =
      OrdinaryValue
        { valueInfoSymbol = reservedSymbol,
          valueRuntimeName = "Main__resolvedReserved",
          valueTypeView = mkTypeView (STBase "Int") (STBase "Int"),
          valueConstraintInfos = []
        }
    reservedResolvedSymbol =
      resolvedValueInfoSymbol (SymbolLocal "Main") "resolvedReserved" reservedValue

    injectProgram (ResolvedProgram modules0) =
      ResolvedProgram $
        case modules0 of
          [] -> []
          resolvedModule : rest -> injectModule resolvedModule : rest

    injectModule resolvedModule =
      resolvedModule
        { resolvedModuleSemantic = injectSemantic (resolvedModuleSemantic resolvedModule)
        }

    injectSemantic semantic =
      semantic
        { resolvedSemanticModuleLocalSymbols =
            injectLocalSymbols (resolvedSemanticModuleLocalSymbols semantic)
        }

    injectLocalSymbols localSymbols =
      localSymbols
        { resolvedLocalValues =
            Map.insertWith
              (++)
              "resolvedReserved"
              [reservedResolvedSymbol]
              (resolvedLocalValues localSymbols)
        }

replaceDataInfoSymbol :: SymbolIdentity -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceDataInfoSymbol target replacement checked =
  checked
    { checkedProgramModulesInternal =
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
    { checkedProgramModulesInternal =
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
    { checkedProgramModulesInternal =
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

requireCheckedBinding :: String -> CheckedProgram -> IO CheckedBinding
requireCheckedBinding bindingName checked =
  case findCheckedBinding bindingName checked of
    Just binding -> pure binding
    Nothing -> expectationFailure ("missing checked binding " ++ show bindingName) >> fail "missing checked binding"

requireTopLevelBindingIdentity :: CheckedBinding -> IO SymbolIdentity
requireTopLevelBindingIdentity binding =
  case Elab.resolvedVarDetails (checkedBindingResolvedVar binding) of
    TopLevelId identity -> pure identity
    other -> expectationFailure ("expected top-level binding identity, got " ++ show other) >> fail "missing top-level identity"

replaceBindingTopLevelIdentity :: String -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceBindingTopLevelIdentity bindingName replacement =
  mapBinding bindingName $ \binding ->
    binding
      { checkedBindingResolvedVar =
          (checkedBindingResolvedVar binding)
            { Elab.resolvedVarDetails = TopLevelId replacement
            }
      }

replaceBindingDetails :: String -> IdDetails -> CheckedProgram -> CheckedProgram
replaceBindingDetails bindingName replacement =
  mapBinding bindingName $ \binding ->
    binding
      { checkedBindingResolvedVar =
          (checkedBindingResolvedVar binding)
            { Elab.resolvedVarDetails = replacement
            }
      }

replaceCheckedModuleIdentity :: String -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceCheckedModuleIdentity moduleName0 replacement checked =
  checked
    { checkedProgramModulesInternal =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule
      | checkedModuleName checkedModule == moduleName0 =
          checkedModule {checkedModuleIdentity = replacement}
      | otherwise =
          checkedModule

replaceResolvedModuleIdentity :: String -> SymbolIdentity -> CheckedProgram -> CheckedProgram
replaceResolvedModuleIdentity moduleName0 replacement checked =
  checked
    { checkedProgramResolvedInternal =
        updateProgram (checkedProgramResolved checked)
    }
  where
    updateProgram (ResolvedProgram modules0) =
      ResolvedProgram (map updateModule modules0)

    updateModule resolvedModule
      | resolvedSemanticModuleName semantic == moduleName0 =
          resolvedModule
            { resolvedModuleSemantic =
                semantic
                  { resolvedSemanticModuleIdentity = replacement
                  }
            }
      | otherwise =
          resolvedModule
      where
        semantic =
          resolvedModuleSemantic resolvedModule

renameCheckedModuleName :: String -> String -> CheckedProgram -> CheckedProgram
renameCheckedModuleName oldName newName checked =
  checked
    { checkedProgramModulesInternal =
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
          {
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
staleTopLevelOccurrenceRuntime _target _replacement =
  id

poisonTopLevelTermIdentity :: SymbolIdentity -> SymbolIdentity -> String -> Elab.XmlfTerm -> Elab.XmlfTerm
poisonTopLevelTermIdentity target replacement _replacementName =
  go
  where
    go term =
      case term of
        Elab.EVarNode resolved@Elab.ResolvedVar {Elab.resolvedVarDetails = TopLevelId identity}
          | sameSymbolIdentity identity target ->
              Elab.EVarNode
                resolved
                  {
                    Elab.resolvedVarDetails = TopLevelId replacement
                  }
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
staleLocalOccurrenceRuntimes _replacement =
  id

rewriteFirstLetBindingType :: Elab.ElabType -> Elab.XmlfTerm -> Elab.XmlfTerm
rewriteFirstLetBindingType replacementTy =
  rewriteFirstLetBindingTypeWith (const replacementTy)

rewriteFirstLetBindingTypeWith :: (Elab.ElabType -> Elab.ElabType) -> Elab.XmlfTerm -> Elab.XmlfTerm
rewriteFirstLetBindingTypeWith rewriteTy =
  go
  where
    go term =
      case term of
        Elab.ELet resolved scheme rhs body ->
          let replacementTy = rewriteTy (testSchemeToType scheme)
           in Elab.ELet
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

testSchemeToType :: Elab.ElabScheme -> Elab.ElabType
testSchemeToType scheme =
  foldr
    (\(ref, mbBound) body -> Elab.TForallRef ref mbBound body)
    (Elab.schemeBody scheme)
    (Elab.schemeBinderRefs scheme)

renameElabTypeBinderDisplays :: String -> Elab.ElabType -> Elab.ElabType
renameElabTypeBinderDisplays displayName =
  go
  where
    renameRef = Elab.renameTypeBinderRef displayName

    go ty =
      case ty of
        Elab.TVarRef ref -> Elab.TVarRef (renameRef ref)
        Elab.TArrow domain codomain ->
          Elab.TArrow (go domain) (go codomain)
        Elab.TConWithIdentity identity name args ->
          Elab.TConWithIdentity identity name (fmap go args)
        Elab.TVarAppRef ref args ->
          Elab.TVarAppRef (renameRef ref) (fmap go args)
        Elab.TBaseWithIdentity identity name ->
          Elab.TBaseWithIdentity identity name
        Elab.TBottom -> Elab.TBottom
        Elab.TForallRef ref mbBound body ->
          Elab.TForallRef
            (renameRef ref)
            (fmap (mapBoundType go) mbBound)
            (go body)
        Elab.TMuRef ref body ->
          Elab.TMuRef (renameRef ref) (go body)

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
    { checkedProgramModulesInternal =
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
                setTypeViewHeadIdentities
                  ( Map.union
                      (typeViewHeadIdentities (ctorTypeView constructorInfo))
                      resultHeadIdentities
                  )
                  ( setTypeViewTypes
                      ( foldr
                          (\(name, mbBound) body -> STForall name (fmap SrcBound mbBound) body)
                          (foldr STArrow resultTy (ctorArgs constructorInfo))
                          (ctorForalls constructorInfo)
                      )
                      ( foldr
                          (\(name, mbBound) body -> STForall name (fmap SrcBound mbBound) body)
                          (foldr STArrow resultTy identityArgs)
                          identityForalls
                      )
                      (ctorTypeView constructorInfo)
                  )
            }
      | otherwise =
          constructorInfo
      where
        (identityForalls, identityBody) = splitForalls (typeViewIdentity (ctorTypeView constructorInfo))
        (identityArgs, _) = splitArrows identityBody

    resultHeadIdentities =
      Map.fromList
        [ (name, identity)
        | name <- toList (typeHeadNamesSrcType resultTy),
          Just identity <- [builtinTypeHeadIdentity name]
        ]

withConstructorTypeView :: String -> TypeView -> CheckedProgram -> CheckedProgram
withConstructorTypeView runtimeName view checked =
  checked
    { checkedProgramModulesInternal =
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

renameDataParamDisplays :: String -> [String] -> CheckedProgram -> CheckedProgram
renameDataParamDisplays dataName names checked =
  checked
    { checkedProgramModulesInternal =
        map updateModule (checkedProgramModules checked)
    }
  where
    updateModule checkedModule =
      checkedModule
        { checkedModuleData = fmap updateDataInfo (checkedModuleData checkedModule)
        }

    updateDataInfo dataInfo
      | dataInfoIdentityQualifiedName dataInfo == dataName =
          dataInfo {dataTypeParams = zipWith renameTypeParam names (dataTypeParams dataInfo)}
      | otherwise =
          dataInfo

    renameTypeParam name param =
      CheckedTypeParam
        (resolvedTypeBinderRefFromIdentity (checkedTypeParamIdentity param) name)
        (checkedTypeParamKind param)

withConstructorDisplayType :: String -> SrcType -> CheckedProgram -> CheckedProgram
withConstructorDisplayType runtimeName displayTy checked =
  checked
    { checkedProgramModulesInternal =
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
                setTypeViewDisplay displayTy (ctorTypeView constructorInfo)
            }
      | otherwise =
          constructorInfo

renameCheckedConstructorRuntimeNamesWhere :: (String -> Bool) -> String -> CheckedProgram -> CheckedProgram
renameCheckedConstructorRuntimeNamesWhere predicate replacement checked =
  checked
    { checkedProgramModulesInternal =
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
    { backendProgramModulesWithIdentity =
        map updateModule (backendProgramModulesWithIdentity backend)
    }
  where
    updateModule backendModule =
      backendModule
        { backendModuleBindingsWithIdentity =
            map updateBinding (backendModuleBindingsWithIdentity backendModule)
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
  case exposeCaseApplicationSpine term of
    Just (headTerm, handlers@(_ : _)) ->
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
      Elab.ETyInst
        (mkTestTyAbs "$case_handler_a" Nothing handler)
        (Elab.InstApp intElabTy)

replaceCaseHandlerBodiesAfterLams :: Int -> Elab.XmlfTerm -> Elab.XmlfTerm -> Elab.XmlfTerm
replaceCaseHandlerBodiesAfterLams lamCount replacement term =
  case exposeCaseApplicationSpine term of
    Just (headTerm, handlers@(_ : _)) ->
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
            Elab.ETyAbsRef ref mbBound body ->
              Elab.ETyAbsRef ref mbBound (replaceHandlerBody remaining body)
            Elab.ETyInst inner inst ->
              Elab.ETyInst (replaceHandlerBody remaining inner) inst
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

caseHandlerTypeAbstractionIdentities :: Elab.XmlfTerm -> [TypeBinderIdentity]
caseHandlerTypeAbstractionIdentities term =
  case exposeCaseApplicationSpine term of
    Just (Elab.ETyInst (Elab.EUnroll scrutinee) _, handlers@(_ : _)) ->
      [ Elab.typeBinderRefIdentity ref
      | Elab.ETyAbsRef ref _ _ <- handlers
      ]
        ++ caseHandlerTypeAbstractionIdentities scrutinee
        ++ concatMap caseHandlerTypeAbstractionIdentities handlers
    _ ->
      case term of
        Elab.EVarNode _ -> []
        Elab.ELit _ -> []
        Elab.ELam _ body ->
          caseHandlerTypeAbstractionIdentities body
        Elab.EApp fun arg ->
          caseHandlerTypeAbstractionIdentities fun
            ++ caseHandlerTypeAbstractionIdentities arg
        Elab.ELet _ _ rhs body ->
          caseHandlerTypeAbstractionIdentities rhs
            ++ caseHandlerTypeAbstractionIdentities body
        Elab.ETyAbsRef _ _ body ->
          caseHandlerTypeAbstractionIdentities body
        Elab.ETyInst inner _ ->
          caseHandlerTypeAbstractionIdentities inner
        Elab.ERoll _ body ->
          caseHandlerTypeAbstractionIdentities body
        Elab.EUnroll body ->
          caseHandlerTypeAbstractionIdentities body

-- Checked term closure may place a type abstraction/application around the
-- head and some already-applied handlers of a source case.  Expose only that
-- leading type computation so fixture rewrites still address the complete
-- case spine; value applications remain untouched.
exposeCaseApplicationSpine :: Elab.XmlfTerm -> Maybe (Elab.XmlfTerm, [Elab.XmlfTerm])
exposeCaseApplicationSpine term =
  let (rawHead, outerArgs) = collectAppsElab term
      exposedHead = reduceLeadingTypeInstantiation rawHead
      (caseHead, enclosedArgs) = collectAppsElab exposedHead
   in case caseHead of
        Elab.ETyInst (Elab.EUnroll _) _ ->
          Just (caseHead, enclosedArgs ++ outerArgs)
        _ -> Nothing

setCaseResultInstantiation :: Elab.ElabType -> Elab.XmlfTerm -> Elab.XmlfTerm
setCaseResultInstantiation resultTy term =
  case exposeCaseApplicationSpine term of
    Just (Elab.ETyInst (Elab.EUnroll scrutinee) _, handlers) ->
      rebuildAppsElab
        (Elab.ETyInst (Elab.EUnroll scrutinee) (Elab.InstApp resultTy))
        handlers
    _ -> term

reduceLeadingTypeInstantiation :: Elab.XmlfTerm -> Elab.XmlfTerm
reduceLeadingTypeInstantiation term@(Elab.ETyInst inner _)
  | termHasLeadingTypeAbstraction inner =
      case ElabPipeline.step term of
        Just reduced -> reduceLeadingTypeInstantiation reduced
        Nothing -> term
reduceLeadingTypeInstantiation term = term

termHasLeadingTypeAbstraction :: Elab.XmlfTerm -> Bool
termHasLeadingTypeAbstraction term =
  case term of
    Elab.ETyAbsRef {} -> True
    Elab.ETyInst inner _ -> termHasLeadingTypeAbstraction inner
    _ -> False

elabTypesInTerm :: Elab.XmlfTerm -> [Elab.ElabType]
elabTypesInTerm term =
  case term of
    Elab.EVarNode resolved -> [Elab.resolvedVarType resolved]
    Elab.ELit _ -> []
    Elab.ELam resolved body ->
      Elab.resolvedVarType resolved : elabTypesInTerm body
    Elab.EApp fun arg ->
      elabTypesInTerm fun ++ elabTypesInTerm arg
    Elab.ELet resolved scheme rhs body ->
      Elab.resolvedVarType resolved
        : schemeTypes scheme
          ++ elabTypesInTerm rhs
          ++ elabTypesInTerm body
    Elab.ETyAbsRef _ mbBound body ->
      maybe [] (pure . Elab.tyToElab) mbBound ++ elabTypesInTerm body
    Elab.ETyInst inner inst ->
      elabTypesInTerm inner ++ instantiationTypes inst
    Elab.ERoll ty body ->
      ty : elabTypesInTerm body
    Elab.EUnroll body ->
      elabTypesInTerm body
  where
    schemeTypes scheme =
      Elab.schemeBody scheme
        : [ Elab.tyToElab bound
          | (_, Just bound) <- Elab.schemeBinderRefs scheme
          ]

    instantiationTypes inst =
      case inst of
        Elab.InstId -> []
        Elab.InstApp ty -> [ty]
        Elab.InstBot ty -> [ty]
        Elab.InstIntro -> []
        Elab.InstElim -> []
        Elab.InstInside inner -> instantiationTypes inner
        Elab.InstSeq left right ->
          instantiationTypes left ++ instantiationTypes right
        Elab.InstAbstrRef _ -> []
        Elab.InstUnderRef _ inner -> instantiationTypes inner

recursiveTypesInType :: Elab.ElabType -> [Elab.ElabType]
recursiveTypesInType ty =
  case ty of
    Elab.TVarRef _ -> []
    Elab.TArrow domain codomain ->
      recursiveTypesInType domain ++ recursiveTypesInType codomain
    Elab.TConWithIdentity _ _ args ->
      concatMap recursiveTypesInType args
    Elab.TVarAppRef _ args ->
      concatMap recursiveTypesInType args
    Elab.TBaseWithIdentity _ _ -> []
    Elab.TBottom -> []
    Elab.TForallRef _ mbBound body ->
      maybe [] (recursiveTypesInType . Elab.tyToElab) mbBound
        ++ recursiveTypesInType body
    recursive@Elab.TMuRef {} ->
      [recursive]

containsBottomType :: Elab.ElabType -> Bool
containsBottomType ty =
  case ty of
    Elab.TVarRef _ -> False
    Elab.TArrow domain codomain ->
      containsBottomType domain || containsBottomType codomain
    Elab.TConWithIdentity _ _ args ->
      any containsBottomType args
    Elab.TVarAppRef _ args ->
      any containsBottomType args
    Elab.TBaseWithIdentity _ _ -> False
    Elab.TBottom -> True
    Elab.TForallRef _ mbBound body ->
      maybe False (containsBottomType . Elab.tyToElab) mbBound
        || containsBottomType body
    Elab.TMuRef _ body ->
      containsBottomType body

hasBottomTypedConstructorRef :: Elab.XmlfTerm -> Bool
hasBottomTypedConstructorRef term =
  case term of
    Elab.EVarNode resolved ->
      Elab.resolvedVarType resolved == Elab.TBottom
        && case Elab.resolvedVarConstructorRef resolved of
          Just _ -> True
          Nothing -> False
    Elab.ELit _ -> False
    Elab.ELam _ body ->
      hasBottomTypedConstructorRef body
    Elab.EApp fun arg ->
      hasBottomTypedConstructorRef fun || hasBottomTypedConstructorRef arg
    Elab.ELet _ _ rhs body ->
      hasBottomTypedConstructorRef rhs || hasBottomTypedConstructorRef body
    Elab.ETyAbsRef _ _ body ->
      hasBottomTypedConstructorRef body
    Elab.ETyInst inner _ ->
      hasBottomTypedConstructorRef inner
    Elab.ERoll _ body ->
      hasBottomTypedConstructorRef body
    Elab.EUnroll body ->
      hasBottomTypedConstructorRef body

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
