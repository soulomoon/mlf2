module BackendConvertSpec (spec) where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import MLF.Backend.Convert
import MLF.Backend.IR
import MLF.Constraint.Types.Graph (BaseTy (..))
import qualified MLF.Elab.Types as Elab
import MLF.Frontend.Syntax (Lit (..), SrcTy (..), SrcType)
import MLF.Program
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
          backendFunction = BackendVar {backendVarName = "Main__id"},
          backendArgument = BackendLit {backendLit = LInt 1}
        } ->
          resultTy `shouldBe` intTy
      other -> expectationFailure ("expected backend application, got " ++ show other)

  it "recovers explicit backend constructors and cases from checked ADT paths" $ do
    checked <- requireChecked adtCaseProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()
    backendDataNames backend `shouldSatisfy` (not . null)
    backendConstructorNames backend `shouldSatisfy` (not . null)

    mainBinding <- requireBinding (backendProgramMain backend) backend
    backendBindingExpr mainBinding `shouldSatisfy` containsBackendCase
    collectConstructNames (backendBindingExpr mainBinding) `shouldSatisfy` (not . null)

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

  it "falls back to ordinary application for over-applied case-shaped terms" $ do
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
      BackendApp
        { backendExprType = resultTy,
          backendFunction = fun,
          backendArgument = BackendLit {backendLit = LInt 1}
        } -> do
          resultTy `shouldBe` intTy
          fun `shouldSatisfy` containsBackendCase
      other -> expectationFailure ("expected backend application of recovered case, got " ++ show other)

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

  it "renames expected forall bodies to actual type abstraction binders" $ do
    checked <- requireChecked =<< readFile "test/programs/unified/first-class-polymorphism.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "synthesizes constructor bindings for checked GADT and existential programs" $ do
    mapM_
      ( \path -> do
          checked <- requireChecked =<< readFile path
          backend <- requireRight (convertCheckedProgram checked)
          validateBackendProgram backend `shouldBe` Right ()
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

  it "preserves constructor type applications when checking constructor fields" $ do
    checked <- requireChecked constructorForallApplicationProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Pack"]

  it "preserves bounded constructor foralls in backend metadata" $ do
    checked <- requireChecked boundedConstructorForallProgram
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    constructor <- requireConstructor "Main__Pack" backend
    backendConstructorForalls constructor `shouldBe` [BackendTypeBinder "a" (Just intTy)]

    let corruptedExpr =
          BackendConstruct
            { backendExprType = backendConstructorResult constructor,
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
      `shouldBe` Left (BackendConstructorArgumentMismatch "Main__Pack" 0 intTy boolTy)

  it "converts nested constructor arguments under expected constructor field types" $ do
    checked <- requireChecked =<< readFile "test/programs/recursive-adt/typeclass-integration.mlfp"
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

  it "ignores stale constructor head type instantiations" $ do
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
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

    mainBinding <- requireBinding (backendProgramMain backend) backend
    collectConstructNames (backendBindingExpr mainBinding) `shouldContain` ["Main__Pack"]

  it "matches repeated constructor parameters modulo alpha-equivalence" $ do
    checked0 <- requireChecked repeatedPolymorphicParameterProgram
    let checked =
          mapMainBinding
            ( \binding ->
                binding
                  { checkedBindingType = boolElabTy,
                    checkedBindingTerm = repeatedPolymorphicParameterCaseTerm
                  }
            )
            checked0
    backend <- requireRight (convertCheckedProgram checked)

    validateBackendProgram backend `shouldBe` Right ()

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

  it "rejects recursive local lets before emitting invalid backend IR" $ do
    checked <- requireChecked =<< readFile "test/programs/unified/authoritative-recursive-let.mlfp"

    convertCheckedProgram checked `shouldBe` Left (BackendUnsupportedRecursiveLet "$peel#0")

  it "reports unsupported source type applications instead of weakening them" $ do
    convertSourceType unsupportedVariableHeadType
      `shouldBe` Left (BackendUnsupportedSourceType unsupportedVariableHeadType)

simpleFunctionProgram :: String
simpleFunctionProgram =
  unlines
    [ "module Main export (main) {",
      "  def id : Int -> Int = \\x x;",
      "  def main : Int = id 1;",
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

functionCaseProgram :: String
functionCaseProgram =
  unlines
    [ "module Main export (Box(..), main) {",
      "  data Box =",
      "      Box : Int -> Box;",
      "",
      "  def main : Int -> Int = case Box 0 of {",
      "    Box n -> \\x x",
      "  };",
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

constructorForallApplicationProgram :: String
constructorForallApplicationProgram =
  unlines
    [ "module Main export (Pack(..), main) {",
      "  data Pack =",
      "      Pack : forall a. a -> Pack;",
      "",
      "  def main : Pack = Pack 1;",
      "}"
    ]

boundedConstructorForallProgram :: String
boundedConstructorForallProgram =
  unlines
    [ "module Main export (Pack(..), main) {",
      "  data Pack =",
      "      Pack : forall (a >= Int). a -> Pack;",
      "",
      "  def main : Pack = Pack 1;",
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

unsupportedVariableHeadType :: SrcType
unsupportedVariableHeadType =
  STVarApp "f" (STBase "Int" :| [])

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

requireBinding :: String -> BackendProgram -> IO BackendBinding
requireBinding name backend =
  case find ((== name) . backendBindingName) (backendBindings backend) of
    Just binding -> pure binding
    Nothing -> expectationFailure ("missing backend binding " ++ show name) >> fail "missing binding"

requireConstructor :: String -> BackendProgram -> IO BackendConstructor
requireConstructor name backend =
  case find ((== name) . backendConstructorName) (backendConstructors backend) of
    Just constructor -> pure constructor
    Nothing -> expectationFailure ("missing backend constructor " ++ show name) >> fail "missing constructor"

backendBindings :: BackendProgram -> [BackendBinding]
backendBindings =
  concatMap backendModuleBindings . backendProgramModules

backendConstructors :: BackendProgram -> [BackendConstructor]
backendConstructors =
  concatMap (concatMap backendDataConstructors . backendModuleData) . backendProgramModules

backendDataNames :: BackendProgram -> [String]
backendDataNames =
  concatMap (map backendDataName . backendModuleData) . backendProgramModules

backendConstructorNames :: BackendProgram -> [String]
backendConstructorNames =
  concatMap (concatMap (map backendConstructorName . backendDataConstructors) . backendModuleData) . backendProgramModules

containsBackendCase :: BackendExpr -> Bool
containsBackendCase expr =
  case expr of
    BackendCase {} -> True
    BackendLam {backendBody = body} -> containsBackendCase body
    BackendApp {backendFunction = fun, backendArgument = arg} -> containsBackendCase fun || containsBackendCase arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} -> containsBackendCase rhs || containsBackendCase body
    BackendTyAbs {backendTyAbsBody = body} -> containsBackendCase body
    BackendTyApp {backendTyFunction = fun} -> containsBackendCase fun
    BackendConstruct {backendConstructArgs = args} -> any containsBackendCase args
    BackendRoll {backendRollPayload = body} -> containsBackendCase body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendCase body
    _ -> False

containsBackendTyApp :: BackendExpr -> Bool
containsBackendTyApp expr =
  case expr of
    BackendTyApp {} -> True
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      containsBackendTyApp scrutinee || any (containsBackendTyApp . backendAltBody) (toList alternatives)
    BackendLam {backendBody = body} -> containsBackendTyApp body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      containsBackendTyApp fun || containsBackendTyApp arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} ->
      containsBackendTyApp rhs || containsBackendTyApp body
    BackendTyAbs {backendTyAbsBody = body} -> containsBackendTyApp body
    BackendConstruct {backendConstructArgs = args} -> any containsBackendTyApp args
    BackendRoll {backendRollPayload = body} -> containsBackendTyApp body
    BackendUnroll {backendUnrollPayload = body} -> containsBackendTyApp body
    _ -> False

findBackendCase :: BackendExpr -> Maybe BackendExpr
findBackendCase expr =
  case expr of
    BackendCase {} -> Just expr
    BackendLam {backendBody = body} -> findBackendCase body
    BackendApp {backendFunction = fun, backendArgument = arg} ->
      findBackendCase fun <|> findBackendCase arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} ->
      findBackendCase rhs <|> findBackendCase body
    BackendTyAbs {backendTyAbsBody = body} -> findBackendCase body
    BackendTyApp {backendTyFunction = fun} -> findBackendCase fun
    BackendConstruct {backendConstructArgs = args} -> firstJust (map findBackendCase args)
    BackendRoll {backendRollPayload = body} -> findBackendCase body
    BackendUnroll {backendUnrollPayload = body} -> findBackendCase body
    _ -> Nothing

collectConstructNames :: BackendExpr -> [String]
collectConstructNames expr =
  case expr of
    BackendConstruct {backendConstructName = name, backendConstructArgs = args} ->
      name : concatMap collectConstructNames args
    BackendCase {backendScrutinee = scrutinee, backendAlternatives = alternatives} ->
      collectConstructNames scrutinee ++ concatMap (collectConstructNames . backendAltBody) (toList alternatives)
    BackendLam {backendBody = body} -> collectConstructNames body
    BackendApp {backendFunction = fun, backendArgument = arg} -> collectConstructNames fun ++ collectConstructNames arg
    BackendLet {backendLetRhs = rhs, backendLetBody = body} -> collectConstructNames rhs ++ collectConstructNames body
    BackendTyAbs {backendTyAbsBody = body} -> collectConstructNames body
    BackendTyApp {backendTyFunction = fun} -> collectConstructNames fun
    BackendRoll {backendRollPayload = body} -> collectConstructNames body
    BackendUnroll {backendUnrollPayload = body} -> collectConstructNames body
    _ -> []

intTy :: BackendType
intTy =
  BTBase (BaseTy "Int")

boolTy :: BackendType
boolTy =
  BTBase (BaseTy "Bool")

intElabTy :: Elab.ElabType
intElabTy =
  Elab.TBase (BaseTy "Int")

boolElabTy :: Elab.ElabType
boolElabTy =
  Elab.TBase (BaseTy "Bool")

polymorphicIdentityElabTy :: Elab.ElabType
polymorphicIdentityElabTy =
  Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))

alphaEquivalentIdentityElabTy :: Elab.ElabType
alphaEquivalentIdentityElabTy =
  Elab.TForall "b" Nothing (Elab.TArrow (Elab.TVar "b") (Elab.TVar "b"))

repeatedPolymorphicParameterCaseTerm :: Elab.ElabTerm
repeatedPolymorphicParameterCaseTerm =
  Elab.EApp
    (Elab.ETyInst (Elab.EUnroll pairScrutinee) (Elab.InstApp boolElabTy))
    ( Elab.ELam
        "$pair_f"
        polymorphicIdentityElabTy
        (Elab.ELam "$pair_g" alphaEquivalentIdentityElabTy (Elab.ELit (LBool True)))
    )
  where
    pairScrutinee =
      Elab.EApp
        (Elab.EApp (Elab.EVar "Main__Pair") polymorphicIdentityTerm)
        (Elab.ETyInst alphaEquivalentIdentityTerm Elab.InstId)

polymorphicIdentityTerm :: Elab.ElabTerm
polymorphicIdentityTerm =
  Elab.ETyAbs
    "a"
    Nothing
    (Elab.ELam "$poly_id_a" (Elab.TVar "a") (Elab.EVar "$poly_id_a"))

alphaEquivalentIdentityTerm :: Elab.ElabTerm
alphaEquivalentIdentityTerm =
  Elab.ETyAbs
    "b"
    Nothing
    (Elab.ELam "$poly_id_b" (Elab.TVar "b") (Elab.EVar "$poly_id_b"))

mapMainBinding :: (CheckedBinding -> CheckedBinding) -> CheckedProgram -> CheckedProgram
mapMainBinding f checked =
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
      | checkedBindingName binding == checkedProgramMain checked = f binding
      | otherwise = binding

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

addStaleConstructorHeadInstantiation :: String -> Elab.ElabType -> Elab.ElabTerm -> Elab.ElabTerm
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
            Elab.ELam name ty body ->
              Elab.ELam name ty (go body)
            Elab.EApp fun arg ->
              Elab.EApp (go fun) (go arg)
            Elab.ELet name scheme rhs body ->
              Elab.ELet name scheme (go rhs) (go body)
            Elab.ETyAbs name mbBound body ->
              Elab.ETyAbs name mbBound (go body)
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
        Elab.EVar name -> name == target
        _ -> False

stripElabTypeInsts :: Elab.ElabTerm -> Elab.ElabTerm
stripElabTypeInsts term =
  case term of
    Elab.ETyInst inner _ -> stripElabTypeInsts inner
    other -> other

wrapCaseHandlersWithTypeWrappers :: Elab.ElabTerm -> Elab.ElabTerm
wrapCaseHandlersWithTypeWrappers term =
  case collectAppsElab term of
    (headTerm@(Elab.ETyInst (Elab.EUnroll _) _), handlers@(_ : _)) ->
      rebuildAppsElab headTerm (map wrapHandler handlers)
    _ ->
      case term of
        Elab.ELam name ty body ->
          Elab.ELam name ty (wrapCaseHandlersWithTypeWrappers body)
        Elab.EApp fun arg ->
          Elab.EApp (wrapCaseHandlersWithTypeWrappers fun) (wrapCaseHandlersWithTypeWrappers arg)
        Elab.ELet name scheme rhs body ->
          Elab.ELet name scheme (wrapCaseHandlersWithTypeWrappers rhs) (wrapCaseHandlersWithTypeWrappers body)
        Elab.ETyAbs name mbBound body ->
          Elab.ETyAbs name mbBound (wrapCaseHandlersWithTypeWrappers body)
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
      Elab.ETyAbs "$case_handler_a" Nothing (Elab.ETyInst handler (Elab.InstApp intElabTy))

replaceCaseHandlerBodiesAfterLams :: Int -> Elab.ElabTerm -> Elab.ElabTerm -> Elab.ElabTerm
replaceCaseHandlerBodiesAfterLams lamCount replacement term =
  case collectAppsElab term of
    (headTerm@(Elab.ETyInst (Elab.EUnroll _) _), handlers@(_ : _)) ->
      rebuildAppsElab headTerm (map (replaceHandlerBody lamCount) handlers)
    _ ->
      case term of
        Elab.ELam name ty body ->
          Elab.ELam name ty (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.EApp fun arg ->
          Elab.EApp (replaceCaseHandlerBodiesAfterLams lamCount replacement fun) (replaceCaseHandlerBodiesAfterLams lamCount replacement arg)
        Elab.ELet name scheme rhs body ->
          Elab.ELet name scheme (replaceCaseHandlerBodiesAfterLams lamCount replacement rhs) (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
        Elab.ETyAbs name mbBound body ->
          Elab.ETyAbs name mbBound (replaceCaseHandlerBodiesAfterLams lamCount replacement body)
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
            Elab.ELam name ty body ->
              Elab.ELam name ty (replaceHandlerBody (remaining - 1) body)
            _ ->
              handler

instantiatedIntIdentity :: Elab.ElabTerm
instantiatedIntIdentity =
  Elab.ETyInst
    ( Elab.ETyAbs
        "$case_body_a"
        Nothing
        (Elab.ELam "$case_body_x" (Elab.TVar "$case_body_a") (Elab.EVar "$case_body_x"))
    )
    (Elab.InstApp intElabTy)

alphaEquivalentIdentityInstId :: Elab.ElabTerm
alphaEquivalentIdentityInstId =
  Elab.ETyInst
    ( Elab.ETyAbs
        "$case_body_b"
        Nothing
        (Elab.ELam "$case_body_y" (Elab.TVar "$case_body_b") (Elab.EVar "$case_body_y"))
    )
    Elab.InstId

collectAppsElab :: Elab.ElabTerm -> (Elab.ElabTerm, [Elab.ElabTerm])
collectAppsElab =
  go []
  where
    go args term =
      case term of
        Elab.EApp fun arg -> go (arg : args) fun
        other -> (other, args)

rebuildAppsElab :: Elab.ElabTerm -> [Elab.ElabTerm] -> Elab.ElabTerm
rebuildAppsElab =
  foldl Elab.EApp

firstJust :: [Maybe a] -> Maybe a
firstJust [] =
  Nothing
firstJust (value : rest) =
  case value of
    Just _ -> value
    Nothing -> firstJust rest
