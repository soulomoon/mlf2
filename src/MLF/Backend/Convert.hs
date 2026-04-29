{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : MLF.Backend.Convert
Description : Convert checked .mlfp programs to typed backend IR

This module is the backend-owned cut from checked `.mlfp` artifacts into the
typed IR from "MLF.Backend.IR". It does not infer or repair programs: inputs
must already have passed the `.mlfp` checker and xMLF typecheck guard, and the
converter reports unsupported checked shapes explicitly.
-}
module MLF.Backend.Convert
  ( BackendConversionError (..),
    convertCheckedProgram,
    convertElabType,
    convertSourceType,
  )
where

import Control.Monad (foldM, forM, unless, when, zipWithM)
import Control.Monad.State.Strict (StateT (StateT), get, modify, runStateT)
import Data.List (find, intercalate, nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Backend.IR
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.TypeCheck (Env (..), typeCheckWithEnv)
import MLF.Elab.Types
  ( ElabTerm (..),
    ElabScheme,
    ElabType,
    BoundType,
    Instantiation (..),
    Ty (..),
    TypeCheckError,
    elabToBound,
    schemeFromType,
    tyToElab,
  )
import MLF.Frontend.Program.Elaborate (ElaborateScope, lowerType, mkElaborateScope)
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ConstructorInfo (..),
    DataInfo (..),
    ResolvedModule (..),
    ResolvedProgram (..),
    ResolvedScope (..),
    ResolvedSymbol (..),
    SymbolIdentity (..),
  )
import MLF.Frontend.Syntax (SrcBound (..), SrcTy (..), SrcType)
import MLF.Util.Names (freshNameLike)

data BackendConversionError
  = BackendUnsupportedSourceType SrcType
  | BackendUnsupportedInstantiation Instantiation
  | BackendUnsupportedRecursiveLet String
  | BackendUnsupportedCaseShape String
  | BackendTypeCheckFailed ElabTerm TypeCheckError
  | BackendValidationFailed BackendValidationError
  deriving (Eq, Show)

data ConvertContext = ConvertContext
  { ccModuleScopes :: Map String ElaborateScope,
    ccConstructors :: Map String ConstructorMeta,
    ccBindingData :: Map String DataMeta,
    ccData :: [DataMeta],
    ccGlobalTerms :: Set.Set String,
    ccCurrentBindingName :: String
  }

data ConstructorMeta = ConstructorMeta
  { cmInfo :: ConstructorInfo,
    cmBackend :: BackendConstructor,
    cmData :: DataMeta
  }

data DataMeta = DataMeta
  { dmInfo :: DataInfo,
    dmBackend :: BackendData
  }

type BackendParameterBounds = Map String (Maybe BackendType)

type BackendTypeBounds = Map String (Maybe BackendType)

data BackendTypeAbsBinder = BackendTypeAbsBinder String (Maybe BackendType)

data LiftedRecursiveLet = LiftedRecursiveLet
  { lrlName :: String,
    lrlElabType :: ElabType,
    lrlBackendType :: BackendType,
    lrlTerm :: ElabTerm
  }

data LiftState = LiftState
  { lsNextHelperIndex :: Int,
    lsLiftedRecursiveLets :: [LiftedRecursiveLet],
    lsGeneratedHelperNames :: Set.Set String
  }

type LiftM = StateT LiftState (Either BackendConversionError)

convertCheckedProgram :: CheckedProgram -> Either BackendConversionError BackendProgram
convertCheckedProgram checked = do
  context <- buildConvertContext checked
  initialEnv <- buildInitialEnv context checked
  modules0 <- mapM (convertCheckedModule context initialEnv) (checkedProgramModules checked)
  let program =
        BackendProgram
          { backendProgramModules = modules0,
            backendProgramMain = checkedProgramMain checked
          }
  case validateBackendProgram program of
    Right () -> Right program
    Left err -> Left (BackendValidationFailed err)

buildInitialEnv :: ConvertContext -> CheckedProgram -> Either BackendConversionError Env
buildInitialEnv context checked = do
  terms <-
    forM
      [ (checkedModule, binding)
        | checkedModule <- checkedProgramModules checked,
          binding <- checkedModuleBindings checkedModule
      ]
      ( \(checkedModule, binding) -> do
          bindingTy <- checkedBindingCanonicalType context checkedModule binding
          Right (checkedBindingName binding, bindingTy)
      )
  Right
    Env
      { termEnv = Map.fromList terms `Map.union` backendBuiltinTermTypes,
        typeEnv = Map.empty
      }

backendBuiltinTermTypes :: Map String ElabType
backendBuiltinTermTypes =
  Map.fromList
    [ ( "__mlfp_and",
        TArrow
          (TBase (BaseTy "Bool"))
          (TArrow (TBase (BaseTy "Bool")) (TBase (BaseTy "Bool")))
      )
    ]

convertCheckedModule :: ConvertContext -> Env -> CheckedModule -> Either BackendConversionError BackendModule
convertCheckedModule context env checkedModule = do
  dataDecls <- mapM (convertDataInfo context) (Map.elems (checkedModuleData checkedModule))
  bindings <- concat <$> mapM (convertCheckedBinding context env checkedModule) (checkedModuleBindings checkedModule)
  Right
    BackendModule
      { backendModuleName = checkedModuleName checkedModule,
        backendModuleData = dataDecls,
        backendModuleBindings = bindings
      }

convertCheckedBinding :: ConvertContext -> Env -> CheckedModule -> CheckedBinding -> Either BackendConversionError [BackendBinding]
convertCheckedBinding context env checkedModule binding = do
  let bindingContext = context {ccCurrentBindingName = checkedBindingName binding}
  canonicalElabTyOpen <- checkedBindingCanonicalTypeOpen context checkedModule binding
  let freeTypeBinders = Set.toAscList (freeElabTypeVars canonicalElabTyOpen)
      canonicalElabTy = quantifyFreeElabTypeVars freeTypeBinders canonicalElabTyOpen
      checkedBindingTermClosed = wrapElabTypeAbs freeTypeBinders (checkedBindingTerm binding)
  rawBindingTy <- convertElabType canonicalElabTy
  let bindingTy =
        canonicalizeBackendType context $
          applySourceTypeIdentity context (checkedBindingSourceType binding) rawBindingTy
  (expr, liftedBindings) <-
    case Map.lookup (checkedBindingName binding) (ccConstructors context) of
      Just constructorMeta
        | constructorBindingResultMatches bindingTy constructorMeta ->
            do
              expr <- synthesizeConstructorBinding bindingTy constructorMeta
              Right (expr, [])
      _ -> do
        (liftedTerm, liftedSpecs) <- liftRecursiveLetsInBinding bindingContext checkedBindingTermClosed
        let envWithLifted =
              foldr
                (\lifted acc -> extendTermEnv (lrlName lifted) (lrlElabType lifted) acc)
                env
                liftedSpecs
        liftedBindings <- mapM (convertLiftedRecursiveLet bindingContext envWithLifted) liftedSpecs
        expr <- convertTermExpected bindingContext envWithLifted (Just bindingTy) liftedTerm
        Right (expr, liftedBindings)
  let convertedBinding =
        BackendBinding
          { backendBindingName = checkedBindingName binding,
            backendBindingType = bindingTy,
            backendBindingExpr = expr,
            backendBindingExportedAsMain = checkedBindingExportedAsMain binding
          }
  Right (convertedBinding : liftedBindings)

liftRecursiveLetsInBinding :: ConvertContext -> ElabTerm -> Either BackendConversionError (ElabTerm, [LiftedRecursiveLet])
liftRecursiveLetsInBinding context term = do
  (term', state') <-
    runStateT
      (liftRecursiveLetsInTerm context Map.empty [] term)
      LiftState
        { lsNextHelperIndex = 0,
          lsLiftedRecursiveLets = [],
          lsGeneratedHelperNames = Set.empty
        }
  Right (term', lsLiftedRecursiveLets state')

convertLiftedRecursiveLet :: ConvertContext -> Env -> LiftedRecursiveLet -> Either BackendConversionError BackendBinding
convertLiftedRecursiveLet context env lifted = do
  let bindingTy = canonicalizeBackendType context (lrlBackendType lifted)
  expr <- convertTermExpected context env (Just bindingTy) (lrlTerm lifted)
  Right
    BackendBinding
      { backendBindingName = lrlName lifted,
        backendBindingType = bindingTy,
        backendBindingExpr = expr,
        backendBindingExportedAsMain = False
      }

liftRecursiveLetsInTerm ::
  ConvertContext ->
  Map String ElabType ->
  [(String, Maybe BoundType)] ->
  ElabTerm ->
  LiftM ElabTerm
liftRecursiveLetsInTerm context lexicalTerms lexicalTypes term =
  case term of
    EVar {} ->
      pure term
    ELit {} ->
      pure term
    ELam name ty body ->
      ELam name ty <$> liftRecursiveLetsInTerm context (Map.insert name ty lexicalTerms) lexicalTypes body
    EApp fun arg ->
      EApp
        <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes fun
        <*> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes arg
    ELet name scheme rhs body -> do
      let schemeTy = schemeToType scheme
          bodyTerms = Map.insert name schemeTy lexicalTerms
          recursiveRhs = isFunctionValueTerm rhs && termMentionsFreeVariable name rhs
      if recursiveRhs
        then do
          bindingTy <- liftEitherConversion (convertElabType schemeTy)
          termCaptures <- capturedTermBindings (Map.delete name lexicalTerms) rhs
          typeCaptures <- capturedTypeBindings lexicalTypes schemeTy termCaptures rhs
          ensureLiftableRecursiveLet name bindingTy termCaptures rhs
          helperName <- freshLiftedRecursiveLetName context name
          rhs' <- liftRecursiveLetsInTerm context bodyTerms lexicalTypes rhs
          let helperRef = applyHelperCaptures helperName typeCaptures termCaptures
              helperElabType = helperType typeCaptures termCaptures schemeTy
              helperTerm =
                wrapHelperTypeCaptures typeCaptures $
                  wrapHelperTermCaptures termCaptures $
                    replaceFreeTermVariable name helperRef rhs'
          helperBackendType <- liftEitherConversion (canonicalizeBackendType context <$> convertElabType helperElabType)
          emitLiftedRecursiveLet
            LiftedRecursiveLet
              { lrlName = helperName,
                lrlElabType = helperElabType,
                lrlBackendType = helperBackendType,
                lrlTerm = helperTerm
              }
          body' <- liftRecursiveLetsInTerm context bodyTerms lexicalTypes body
          pure (ELet name scheme helperRef body')
        else
          ELet name scheme
            <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes rhs
            <*> liftRecursiveLetsInTerm context bodyTerms lexicalTypes body
    ETyAbs name mbBound body ->
      ETyAbs name mbBound <$> liftRecursiveLetsInTerm context lexicalTerms (insertLexicalTypeBinding name mbBound lexicalTypes) body
    ETyInst inner inst ->
      ETyInst <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes inner <*> pure inst
    ERoll ty body ->
      ERoll ty <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes body
    EUnroll body ->
      EUnroll <$> liftRecursiveLetsInTerm context lexicalTerms lexicalTypes body

capturedTermBindings :: Map String ElabType -> ElabTerm -> LiftM [(String, ElabType)]
capturedTermBindings lexicalTerms rhs =
  pure
    [ (name, ty)
    | (name, ty) <- Map.toAscList lexicalTerms,
      Set.member name freeVars
    ]
  where
    freeVars = freeTermVariables rhs

insertLexicalTypeBinding :: String -> Maybe BoundType -> [(String, Maybe BoundType)] -> [(String, Maybe BoundType)]
insertLexicalTypeBinding name mbBound lexicalTypes =
  filter ((/= name) . fst) lexicalTypes ++ [(name, mbBound)]

capturedTypeBindings :: [(String, Maybe BoundType)] -> ElabType -> [(String, ElabType)] -> ElabTerm -> LiftM [(String, Maybe BoundType)]
capturedTypeBindings lexicalTypes schemeTy termCaptures rhs =
  pure
    [ (name, mbBound)
    | (name, mbBound) <- lexicalTypes,
      Set.member name freeVars
    ]
  where
    freeVars =
      freeElabTypeVars schemeTy
        `Set.union` Set.unions (map (freeElabTypeVars . snd) termCaptures)
        `Set.union` freeElabTermTypeVars rhs

helperType :: [(String, Maybe BoundType)] -> [(String, ElabType)] -> ElabType -> ElabType
helperType typeCaptures termCaptures bodyTy =
  foldr wrapType (foldr (TArrow . snd) bodyTy termCaptures) typeCaptures
  where
    wrapType (name, mbBound) acc =
      TForall name mbBound acc

wrapHelperTypeCaptures :: [(String, Maybe BoundType)] -> ElabTerm -> ElabTerm
wrapHelperTypeCaptures typeCaptures body =
  foldr wrap body typeCaptures
  where
    wrap (name, mbBound) acc =
      ETyAbs name mbBound acc

wrapHelperTermCaptures :: [(String, ElabType)] -> ElabTerm -> ElabTerm
wrapHelperTermCaptures termCaptures body =
  foldr wrap body termCaptures
  where
    wrap (name, ty) acc =
      ELam name ty acc

applyHelperCaptures :: String -> [(String, Maybe BoundType)] -> [(String, ElabType)] -> ElabTerm
applyHelperCaptures helperName typeCaptures termCaptures =
  foldl EApp typedHelper [EVar name | (name, _) <- termCaptures]
  where
    typedHelper =
      foldl
        (\acc (name, _) -> ETyInst acc (InstApp (TVar name)))
        (EVar helperName)
        typeCaptures

ensureLiftableRecursiveLet :: String -> BackendType -> [(String, ElabType)] -> ElabTerm -> LiftM ()
ensureLiftableRecursiveLet name bindingTy captures rhs = do
  let unsupported reason =
        BackendUnsupportedRecursiveLet
          ( name
              ++ " ("
              ++ reason
              ++ ")"
          )
  unless (all (isEvidenceCaptureName . fst) captures) $
    throwLiftError
      ( unsupported
          ("captures lexical bindings: " ++ intercalate ", " (map fst captures))
      )
  unless (isMonomorphicFirstOrderFunctionType bindingTy) $
    throwLiftError (unsupported "expected a monomorphic first-order function type")
  unless (isFunctionValueTerm rhs) $
    throwLiftError (unsupported "expected a function-valued recursive right-hand side")

isEvidenceCaptureName :: String -> Bool
isEvidenceCaptureName name =
  "$evidence_" `prefixOf` name
  where
    prefixOf [] _ = True
    prefixOf _ [] = False
    prefixOf (p : ps) (x : xs) = p == x && prefixOf ps xs

freshLiftedRecursiveLetName :: ConvertContext -> String -> LiftM String
freshLiftedRecursiveLetName context localName = do
  state0 <- get
  let (name, nextIndex) = pickName (lsNextHelperIndex state0)
  modify
    ( \state1 ->
        state1
          { lsNextHelperIndex = nextIndex,
            lsGeneratedHelperNames = Set.insert name (lsGeneratedHelperNames state1)
          }
    )
  pure name
  where
    pickName index0 =
      let candidate =
            ccCurrentBindingName context
              ++ "$letrec$"
              ++ localName
              ++ "$"
              ++ show index0
       in if Set.member candidate (ccGlobalTerms context)
            then pickName (index0 + 1)
            else (candidate, index0 + 1)

emitLiftedRecursiveLet :: LiftedRecursiveLet -> LiftM ()
emitLiftedRecursiveLet lifted =
  modify
    ( \state0 ->
        state0
          { lsLiftedRecursiveLets = lsLiftedRecursiveLets state0 ++ [lifted]
          }
    )

liftEitherConversion :: Either BackendConversionError a -> LiftM a
liftEitherConversion result =
  StateT $ \state0 ->
    case result of
      Right value -> Right (value, state0)
      Left err -> Left err

throwLiftError :: BackendConversionError -> LiftM a
throwLiftError err =
  StateT (const (Left err))

isMonomorphicFirstOrderFunctionType :: BackendType -> Bool
isMonomorphicFirstOrderFunctionType ty =
  case ty of
    BTForall {} ->
      False
    _ ->
      let (args, resultTy) = splitBackendArrows ty
       in not (null args) && all (isFirstOrderValueType Set.empty) (resultTy : args)

isFirstOrderValueType :: Set.Set String -> BackendType -> Bool
isFirstOrderValueType bound =
  \case
    BTVar name ->
      Set.member name bound
    BTArrow {} ->
      False
    BTBase {} ->
      True
    BTCon _ args ->
      all (isFirstOrderValueType bound) args
    BTForall {} ->
      False
    BTMu {} ->
      True
    BTBottom ->
      False

isFunctionValueTerm :: ElabTerm -> Bool
isFunctionValueTerm term =
  case stripAdministrativeTermWrappers term of
    ELam {} -> True
    _ -> False

stripAdministrativeTermWrappers :: ElabTerm -> ElabTerm
stripAdministrativeTermWrappers =
  \case
    ETyAbs _ _ body -> stripAdministrativeTermWrappers body
    ETyInst inner _ -> stripAdministrativeTermWrappers inner
    ERoll _ body -> stripAdministrativeTermWrappers body
    term -> term

freeTermVariables :: ElabTerm -> Set.Set String
freeTermVariables =
  go Set.empty
  where
    go bound =
      \case
        EVar name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        ELit {} ->
          Set.empty
        ELam name _ body ->
          go (Set.insert name bound) body
        EApp fun arg ->
          go bound fun `Set.union` go bound arg
        ELet name _ rhs body ->
          go bound rhs `Set.union` go (Set.insert name bound) body
        ETyAbs _ _ body ->
          go bound body
        ETyInst inner _ ->
          go bound inner
        ERoll _ body ->
          go bound body
        EUnroll body ->
          go bound body

freeElabTypeVars :: ElabType -> Set.Set String
freeElabTypeVars =
  freeElabTypeVarsIn Set.empty

freeElabTypeVarsIn :: Set.Set String -> ElabType -> Set.Set String
freeElabTypeVarsIn initialBound =
  go Set.empty
  where
    go bound =
      \case
        TVar name
          | Set.member name (initialBound `Set.union` bound) -> Set.empty
          | otherwise -> Set.singleton name
        TArrow dom cod ->
          go bound dom `Set.union` go bound cod
        TCon _ args ->
          Set.unions (map (go bound) (NE.toList args))
        TBase {} ->
          Set.empty
        TForall name mb body ->
          maybe Set.empty (go bound . tyToElab) mb
            `Set.union` go (Set.insert name bound) body
        TMu name body ->
          go (Set.insert name bound) body
        TBottom ->
          Set.empty

freeElabTermTypeVars :: ElabTerm -> Set.Set String
freeElabTermTypeVars =
  go Set.empty
  where
    go bound =
      \case
        EVar {} ->
          Set.empty
        ELit {} ->
          Set.empty
        ELam _ ty body ->
          freeElabTypeVarsIn bound ty `Set.union` go bound body
        EApp fun arg ->
          go bound fun `Set.union` go bound arg
        ELet _ scheme rhs body ->
          Set.unions
            [ freeElabTypeVarsIn bound (schemeToType scheme),
              go bound rhs,
              go bound body
            ]
        ETyAbs name mbBound body ->
          maybe Set.empty (freeElabTypeVarsIn bound . tyToElab) mbBound
            `Set.union` go (Set.insert name bound) body
        ETyInst inner inst ->
          go bound inner `Set.union` freeInstantiationTypeVarsIn bound inst
        ERoll ty body ->
          freeElabTypeVarsIn bound ty `Set.union` go bound body
        EUnroll body ->
          go bound body

freeInstantiationTypeVarsIn :: Set.Set String -> Instantiation -> Set.Set String
freeInstantiationTypeVarsIn bound =
  \case
    InstId ->
      Set.empty
    InstApp ty ->
      freeElabTypeVarsIn bound ty
    InstBot ty ->
      freeElabTypeVarsIn bound ty
    InstIntro ->
      Set.empty
    InstElim ->
      Set.empty
    InstAbstr name
      | Set.member name bound -> Set.empty
      | otherwise -> Set.singleton name
    InstUnder name inner ->
      freeInstantiationTypeVarsIn (Set.insert name bound) inner
    InstInside inner ->
      freeInstantiationTypeVarsIn bound inner
    InstSeq left right ->
      freeInstantiationTypeVarsIn bound left `Set.union` freeInstantiationTypeVarsIn bound right

termVariableNames :: ElabTerm -> Set.Set String
termVariableNames =
  \case
    EVar name ->
      Set.singleton name
    ELit {} ->
      Set.empty
    ELam name _ body ->
      Set.insert name (termVariableNames body)
    EApp fun arg ->
      termVariableNames fun `Set.union` termVariableNames arg
    ELet name _ rhs body ->
      Set.insert name (termVariableNames rhs `Set.union` termVariableNames body)
    ETyAbs _ _ body ->
      termVariableNames body
    ETyInst inner _ ->
      termVariableNames inner
    ERoll _ body ->
      termVariableNames body
    EUnroll body ->
      termVariableNames body

elabTermTypeVariableNames :: ElabTerm -> Set.Set String
elabTermTypeVariableNames =
  \case
    EVar {} ->
      Set.empty
    ELit {} ->
      Set.empty
    ELam _ ty body ->
      elabTypeVariableNames ty `Set.union` elabTermTypeVariableNames body
    EApp fun arg ->
      elabTermTypeVariableNames fun `Set.union` elabTermTypeVariableNames arg
    ELet _ scheme rhs body ->
      Set.unions
        [ elabTypeVariableNames (schemeToType scheme),
          elabTermTypeVariableNames rhs,
          elabTermTypeVariableNames body
        ]
    ETyAbs name mbBound body ->
      Set.insert name $
        maybe Set.empty (elabTypeVariableNames . tyToElab) mbBound
          `Set.union` elabTermTypeVariableNames body
    ETyInst inner inst ->
      elabTermTypeVariableNames inner `Set.union` instantiationTypeVariableNames inst
    ERoll ty body ->
      elabTypeVariableNames ty `Set.union` elabTermTypeVariableNames body
    EUnroll body ->
      elabTermTypeVariableNames body

elabTypeVariableNames :: ElabType -> Set.Set String
elabTypeVariableNames =
  \case
    TVar name ->
      Set.singleton name
    TArrow dom cod ->
      elabTypeVariableNames dom `Set.union` elabTypeVariableNames cod
    TCon _ args ->
      Set.unions (map elabTypeVariableNames (NE.toList args))
    TBase {} ->
      Set.empty
    TForall name mbBound body ->
      Set.insert name $
        maybe Set.empty (elabTypeVariableNames . tyToElab) mbBound
          `Set.union` elabTypeVariableNames body
    TMu name body ->
      Set.insert name (elabTypeVariableNames body)
    TBottom ->
      Set.empty

instantiationTypeVariableNames :: Instantiation -> Set.Set String
instantiationTypeVariableNames =
  \case
    InstId ->
      Set.empty
    InstApp ty ->
      elabTypeVariableNames ty
    InstBot ty ->
      elabTypeVariableNames ty
    InstIntro ->
      Set.empty
    InstElim ->
      Set.empty
    InstAbstr name ->
      Set.singleton name
    InstUnder name inner ->
      Set.insert name (instantiationTypeVariableNames inner)
    InstInside inner ->
      instantiationTypeVariableNames inner
    InstSeq left right ->
      instantiationTypeVariableNames left `Set.union` instantiationTypeVariableNames right

replaceFreeTermVariable :: String -> ElabTerm -> ElabTerm -> ElabTerm
replaceFreeTermVariable needle replacement =
  go
  where
    replacementFreeTerms = freeTermVariables replacement
    replacementFreeTypes = freeElabTermTypeVars replacement

    go =
      \case
        EVar name
          | name == needle -> replacement
          | otherwise -> EVar name
        ELit lit ->
          ELit lit
        ELam name ty body
          | name == needle ->
              ELam name ty body
          | shouldRenameTermBinder name body ->
              let used = Set.unions [termVariableNames body, termVariableNames replacement, Set.singleton needle]
                  name' = freshNameLike name used
                  body' = renameBoundTermVariable name name' body
               in ELam name' ty (go body')
          | otherwise ->
              ELam name ty (go body)
        EApp fun arg ->
          EApp (go fun) (go arg)
        ELet name scheme rhs body
          | name == needle ->
              ELet name scheme (go rhs) body
          | shouldRenameTermBinder name body ->
              let used =
                    Set.unions
                      [ termVariableNames rhs,
                        termVariableNames body,
                        termVariableNames replacement,
                        Set.singleton needle
                      ]
                  name' = freshNameLike name used
                  body' = renameBoundTermVariable name name' body
               in ELet name' scheme (go rhs) (go body')
          | otherwise ->
              ELet name scheme (go rhs) (go body)
        ETyAbs name mbBound body
          | shouldRenameTypeBinder name body ->
              let used =
                    Set.unions
                      [ elabTermTypeVariableNames body,
                        maybe Set.empty (elabTypeVariableNames . tyToElab) mbBound,
                        elabTermTypeVariableNames replacement
                      ]
                  name' = freshNameLike name used
                  body' = renameTermTypeVariable name name' body
               in ETyAbs name' mbBound (go body')
          | otherwise ->
              ETyAbs name mbBound (go body)
        ETyInst inner inst ->
          ETyInst (go inner) inst
        ERoll ty body ->
          ERoll ty (go body)
        EUnroll body ->
          EUnroll (go body)

    shouldRenameTermBinder name body =
      Set.member name replacementFreeTerms && termMentionsFreeVariable needle body

    shouldRenameTypeBinder name body =
      Set.member name replacementFreeTypes && termMentionsFreeVariable needle body

renameBoundTermVariable :: String -> String -> ElabTerm -> ElabTerm
renameBoundTermVariable old new =
  go
  where
    go =
      \case
        EVar name
          | name == old -> EVar new
          | otherwise -> EVar name
        ELit lit ->
          ELit lit
        ELam name ty body
          | name == old -> ELam name ty body
          | otherwise -> ELam name ty (go body)
        EApp fun arg ->
          EApp (go fun) (go arg)
        ELet name scheme rhs body
          | name == old -> ELet name scheme (go rhs) body
          | otherwise -> ELet name scheme (go rhs) (go body)
        ETyAbs name mbBound body ->
          ETyAbs name mbBound (go body)
        ETyInst inner inst ->
          ETyInst (go inner) inst
        ERoll ty body ->
          ERoll ty (go body)
        EUnroll body ->
          EUnroll (go body)

renameTermTypeVariable :: String -> String -> ElabTerm -> ElabTerm
renameTermTypeVariable old new =
  go
  where
    go =
      \case
        EVar name ->
          EVar name
        ELit lit ->
          ELit lit
        ELam name ty body ->
          ELam name (renameElabTypeVariable old new ty) (go body)
        EApp fun arg ->
          EApp (go fun) (go arg)
        ELet name scheme rhs body ->
          ELet name (renameElabSchemeTypeVariable old new scheme) (go rhs) (go body)
        ETyAbs name mbBound body
          | name == old ->
              ETyAbs name (fmap (renameElabTypeVariable old new) mbBound) body
          | otherwise ->
              ETyAbs name (fmap (renameElabTypeVariable old new) mbBound) (go body)
        ETyInst inner inst ->
          ETyInst (go inner) (renameInstantiationTypeVariable old new inst)
        ERoll ty body ->
          ERoll (renameElabTypeVariable old new ty) (go body)
        EUnroll body ->
          EUnroll (go body)

renameElabSchemeTypeVariable :: String -> String -> ElabScheme -> ElabScheme
renameElabSchemeTypeVariable old new =
  schemeFromType . renameElabTypeVariable old new . schemeToType

renameElabTypeVariable :: String -> String -> Ty var -> Ty var
renameElabTypeVariable old new =
  \case
    TVar name
      | name == old -> TVar new
      | otherwise -> TVar name
    TArrow dom cod ->
      TArrow (renameElabTypeVariable old new dom) (renameElabTypeVariable old new cod)
    TCon con args ->
      TCon con (fmap (renameElabTypeVariable old new) args)
    TBase base ->
      TBase base
    TForall name mbBound body
      | name == old ->
          TForall name (fmap (renameElabTypeVariable old new) mbBound) body
      | otherwise ->
          TForall name (fmap (renameElabTypeVariable old new) mbBound) (renameElabTypeVariable old new body)
    TMu name body
      | name == old -> TMu name body
      | otherwise -> TMu name (renameElabTypeVariable old new body)
    TBottom ->
      TBottom

renameInstantiationTypeVariable :: String -> String -> Instantiation -> Instantiation
renameInstantiationTypeVariable old new =
  go
  where
    go =
      \case
        InstId ->
          InstId
        InstApp ty ->
          InstApp (renameElabTypeVariable old new ty)
        InstBot ty ->
          InstBot (renameElabTypeVariable old new ty)
        InstIntro ->
          InstIntro
        InstElim ->
          InstElim
        InstAbstr name
          | name == old -> InstAbstr new
          | otherwise -> InstAbstr name
        InstUnder name inner
          | name == old -> InstUnder name inner
          | otherwise -> InstUnder name (go inner)
        InstInside inner ->
          InstInside (go inner)
        InstSeq left right ->
          InstSeq (go left) (go right)

checkedBindingCanonicalType :: ConvertContext -> CheckedModule -> CheckedBinding -> Either BackendConversionError ElabType
checkedBindingCanonicalType context checkedModule binding =
  closeFreeElabType <$> checkedBindingCanonicalTypeOpen context checkedModule binding

checkedBindingCanonicalTypeOpen :: ConvertContext -> CheckedModule -> CheckedBinding -> Either BackendConversionError ElabType
checkedBindingCanonicalTypeOpen context checkedModule binding = do
  let checkedTy = checkedBindingType binding
      scope = scopeForModule context (checkedModuleName checkedModule)
  checkedBackendTy <- convertElabType checkedTy
  canonicalTy <- sourceTypeToElabType (lowerType scope (checkedBindingSourceType binding))
  canonicalBackendTy <- convertElabType canonicalTy
  let strippedCheckedBackendTy = stripVacuousBackendForalls checkedBackendTy
  if alphaEqBackendType checkedBackendTy canonicalBackendTy
    then Right canonicalTy
    else
      if alphaEqBackendType (normalizeBuiltinBackendType strippedCheckedBackendTy) (normalizeBuiltinBackendType canonicalBackendTy)
        then Right (backendTypeToElab strippedCheckedBackendTy)
        else Right checkedTy

stripVacuousBackendForalls :: BackendType -> BackendType
stripVacuousBackendForalls =
  \case
    BTArrow dom cod ->
      BTArrow (stripVacuousBackendForalls dom) (stripVacuousBackendForalls cod)
    BTCon con args ->
      BTCon con (fmap stripVacuousBackendForalls args)
    BTForall name mbBound body ->
      let body' = stripVacuousBackendForalls body
          mbBound' = fmap stripVacuousBackendForalls mbBound
       in if Set.member name (freeBackendTypeVars body')
            then BTForall name mbBound' body'
            else body'
    BTMu name body ->
      BTMu name (stripVacuousBackendForalls body)
    ty ->
      ty

normalizeBuiltinBackendType :: BackendType -> BackendType
normalizeBuiltinBackendType =
  \case
    BTArrow dom cod ->
      BTArrow (normalizeBuiltinBackendType dom) (normalizeBuiltinBackendType cod)
    BTBase base ->
      BTBase (normalizeBuiltinBase base)
    BTCon con args ->
      BTCon (normalizeBuiltinBase con) (fmap normalizeBuiltinBackendType args)
    BTForall name mbBound body ->
      BTForall name (fmap normalizeBuiltinBackendType mbBound) (normalizeBuiltinBackendType body)
    BTMu name body ->
      BTMu name (normalizeBuiltinBackendType body)
    ty ->
      ty

normalizeBuiltinBase :: BaseTy -> BaseTy
normalizeBuiltinBase (BaseTy name) =
  BaseTy $
    case name of
      "<builtin>.Int" -> "Int"
      "<builtin>.Bool" -> "Bool"
      "<builtin>.String" -> "String"
      _ -> name

closeFreeElabType :: ElabType -> ElabType
closeFreeElabType ty =
  quantifyFreeElabTypeVars (Set.toAscList (freeElabTypeVars ty)) ty

quantifyFreeElabTypeVars :: [String] -> ElabType -> ElabType
quantifyFreeElabTypeVars names ty =
  foldr (`TForall` Nothing) ty names

wrapElabTypeAbs :: [String] -> ElabTerm -> ElabTerm
wrapElabTypeAbs names term =
  foldr (\name acc -> ETyAbs name Nothing acc) term names

scopeForModule :: ConvertContext -> String -> ElaborateScope
scopeForModule context moduleName =
  Map.findWithDefault
    (fallbackElaborateScope (map dmInfo (ccData context)))
    moduleName
    (ccModuleScopes context)

fallbackElaborateScope :: [DataInfo] -> ElaborateScope
fallbackElaborateScope dataInfos =
  mkElaborateScope Map.empty (qualifiedDataInfoMap dataInfos) Map.empty []

sourceTypeToElabType :: SrcTy n v -> Either BackendConversionError ElabType
sourceTypeToElabType =
  \case
    STVar name -> Right (TVar name)
    STArrow dom cod -> TArrow <$> sourceTypeToElabType dom <*> sourceTypeToElabType cod
    STBase name -> Right (TBase (BaseTy name))
    STCon name args -> TCon (BaseTy name) <$> traverse sourceTypeToElabType args
    STVarApp name _ -> Left (BackendUnsupportedCaseShape ("unsupported variable-headed source type application `" ++ name ++ "`"))
    STForall name mb body ->
      TForall name
        <$> maybe (Right Nothing) sourceBoundToElabBound mb
        <*> sourceTypeToElabType body
    STMu name body -> TMu name <$> sourceTypeToElabType body
    STBottom -> Right TBottom

sourceBoundToElabBound :: SrcBound n -> Either BackendConversionError (Maybe BoundType)
sourceBoundToElabBound (SrcBound boundTy) =
  case sourceTypeToElabType boundTy of
    Right (TVar {}) -> Right Nothing
    Right TBottom -> Right Nothing
    Right (TArrow dom cod) -> Right (Just (TArrow dom cod))
    Right (TBase base) -> Right (Just (TBase base))
    Right (TCon con args) -> Right (Just (TCon con args))
    Right (TForall name mb body) -> Right (Just (TForall name mb body))
    Right (TMu name body) -> Right (Just (TMu name body))
    Left err -> Left err

constructorBindingResultMatches :: BackendType -> ConstructorMeta -> Bool
constructorBindingResultMatches bindingTy constructorMeta =
  case matchBackendTypeParameters Map.empty parameters Map.empty (backendConstructorResult constructor) resultTy of
    Just _ -> True
    Nothing -> False
  where
    constructor = cmBackend constructorMeta
    parameters = constructorTypeParameters constructorMeta
    (_, bodyTy) = splitBackendForalls bindingTy
    (_, resultTy) = splitBackendArrows bodyTy

synthesizeConstructorBinding :: BackendType -> ConstructorMeta -> Either BackendConversionError BackendExpr
synthesizeConstructorBinding bindingTy constructorMeta = do
  let constructor = cmBackend constructorMeta
      (typeBinders, bodyTy) = splitBackendForalls bindingTy
      (argTys, resultTy) = splitBackendArrows bodyTy
      fields = backendConstructorFields constructor
  unless (length argTys == length fields) $
    Left
      ( BackendUnsupportedCaseShape
          ("constructor binding arity does not match metadata for `" ++ backendConstructorName constructor ++ "`")
      )
  let argNames = ["$" ++ backendConstructorName constructor ++ "_arg" ++ show ix | ix <- [1 .. length argTys]]
      argExprs = zipWith BackendVar argTys argNames
      constructExpr =
        BackendConstruct
          { backendExprType = resultTy,
            backendConstructName = backendConstructorName constructor,
            backendConstructArgs = argExprs
          }
      expr =
        wrapBackendTypeAbs typeBinders $
          wrapBackendLams (zip argNames argTys) constructExpr
  unless (alphaEqBackendType (backendExprType expr) bindingTy) $
    Left
      ( BackendUnsupportedCaseShape
          ("synthesized constructor binding type does not match checked binding type for `" ++ backendConstructorName constructor ++ "`")
      )
  Right expr

splitBackendForalls :: BackendType -> ([BackendTypeAbsBinder], BackendType)
splitBackendForalls =
  go []
  where
    go binders ty =
      case ty of
        BTForall name mbBound body -> go (binders ++ [BackendTypeAbsBinder name mbBound]) body
        _ -> (binders, ty)

splitBackendArrows :: BackendType -> ([BackendType], BackendType)
splitBackendArrows =
  go []
  where
    go args ty =
      case ty of
        BTArrow arg result -> go (args ++ [arg]) result
        _ -> (args, ty)

wrapBackendTypeAbs :: [BackendTypeAbsBinder] -> BackendExpr -> BackendExpr
wrapBackendTypeAbs binders body =
  foldr wrap body binders
  where
    wrap (BackendTypeAbsBinder name mbBound) expr =
      BackendTyAbs
        { backendExprType = BTForall name mbBound (backendExprType expr),
          backendTyParamName = name,
          backendTyParamBound = mbBound,
          backendTyAbsBody = expr
        }

wrapBackendLams :: [(String, BackendType)] -> BackendExpr -> BackendExpr
wrapBackendLams params body =
  foldr wrap body params
  where
    wrap (name, paramTy) expr =
      BackendLam
        { backendExprType = BTArrow paramTy (backendExprType expr),
          backendParamName = name,
          backendParamType = paramTy,
          backendBody = expr
        }

buildConvertContext :: CheckedProgram -> Either BackendConversionError ConvertContext
buildConvertContext checked = do
  let dataInfos = allDataInfos checked
      dataByIdentity = dataInfoIdentityMap dataInfos
      moduleScopes = moduleElaborateScopes checked dataByIdentity
  dataMetas <- mapM (buildDataMetaForDataInfo moduleScopes dataInfos) dataInfos
  let constructorMetas =
        [ (backendConstructorName (cmBackend constructorMeta), constructorMeta)
          | dataMeta <- dataMetas,
            constructorMeta <- constructorMetasForData dataMeta
        ]
      bindingData = bindingDataHints dataMetas checked
  Right
    ConvertContext
      { ccModuleScopes = moduleScopes,
        ccConstructors = Map.fromList constructorMetas,
        ccBindingData = bindingData,
        ccData = dataMetas,
        ccGlobalTerms = checkedProgramGlobalTerms checked,
        ccCurrentBindingName = ""
      }

allDataInfos :: CheckedProgram -> [DataInfo]
allDataInfos checked =
  [ dataInfo
    | checkedModule <- checkedProgramModules checked,
      dataInfo <- Map.elems (checkedModuleData checkedModule)
  ]

checkedProgramGlobalTerms :: CheckedProgram -> Set.Set String
checkedProgramGlobalTerms checked =
  Set.fromList (Map.keys backendBuiltinTermTypes)
    `Set.union` Set.fromList
      [ checkedBindingName binding
        | checkedModule <- checkedProgramModules checked,
          binding <- checkedModuleBindings checkedModule
      ]

dataInfoIdentityMap :: [DataInfo] -> Map SymbolIdentity DataInfo
dataInfoIdentityMap dataInfos =
  Map.fromList [(dataInfoSymbol info, info) | info <- dataInfos]

moduleElaborateScopes :: CheckedProgram -> Map SymbolIdentity DataInfo -> Map String ElaborateScope
moduleElaborateScopes checked dataByIdentity =
  Map.fromList
    [ (resolvedModuleName resolvedModule, elaborateScopeForResolvedModule dataByIdentity resolvedModule)
      | resolvedModule <- resolvedProgramModules (checkedProgramResolved checked)
    ]

elaborateScopeForResolvedModule :: Map SymbolIdentity DataInfo -> ResolvedModule -> ElaborateScope
elaborateScopeForResolvedModule dataByIdentity resolvedModule =
  mkElaborateScope Map.empty dataTypes Map.empty []
  where
    dataTypes =
      visibleDataInfoMap dataByIdentity (resolvedScopeTypes (resolvedModuleScope resolvedModule))
        `Map.union` qualifiedDataInfoMap (Map.elems dataByIdentity)

visibleDataInfoMap :: Map SymbolIdentity DataInfo -> Map String ResolvedSymbol -> Map String DataInfo
visibleDataInfoMap dataByIdentity =
  Map.mapMaybe (\symbol -> canonicalDataInfo <$> Map.lookup (resolvedSymbolIdentity symbol) dataByIdentity)

qualifiedDataInfoMap :: [DataInfo] -> Map String DataInfo
qualifiedDataInfoMap dataInfos =
  Map.fromList [(qualifiedDataName info, canonicalDataInfo info) | info <- dataInfos]

fallbackElaborateScopeForDataInfo :: [DataInfo] -> DataInfo -> ElaborateScope
fallbackElaborateScopeForDataInfo dataInfos info =
  mkElaborateScope Map.empty dataTypes Map.empty []
  where
    dataTypes =
      localDataInfoMap dataInfos info
        `Map.union` uniqueUnqualifiedDataInfoMap dataInfos
        `Map.union` qualifiedDataInfoMap dataInfos

localDataInfoMap :: [DataInfo] -> DataInfo -> Map String DataInfo
localDataInfoMap dataInfos info =
  Map.fromList
    [ (dataName candidate, canonicalDataInfo candidate)
      | candidate <- dataInfos,
        dataModule candidate == dataModule info
    ]

uniqueUnqualifiedDataInfoMap :: [DataInfo] -> Map String DataInfo
uniqueUnqualifiedDataInfoMap dataInfos =
  Map.fromList
    [ (name, canonicalDataInfo info)
      | (name, infos) <- Map.toList grouped,
        [info] <- [infos]
    ]
  where
    grouped =
      Map.fromListWith (++)
        [ (dataName info, [info])
          | info <- dataInfos
        ]

canonicalDataInfo :: DataInfo -> DataInfo
canonicalDataInfo info =
  info {dataName = qualifiedDataName info}

bindingDataHints :: [DataMeta] -> CheckedProgram -> Map String DataMeta
bindingDataHints dataMetas checked =
  Map.fromList
    [ (checkedBindingName binding, dataMeta)
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule,
        Just dataMeta <- [bindingDataHint dataMetas binding]
    ]

bindingDataHint :: [DataMeta] -> CheckedBinding -> Maybe DataMeta
bindingDataHint dataMetas binding =
  case splitSourceArrows (dropSourceForalls (checkedBindingSourceType binding)) of
    ([], resultTy) -> sourceTypeDataMeta dataMetas resultTy
    _ -> Nothing

sourceTypeDataMeta :: [DataMeta] -> SrcType -> Maybe DataMeta
sourceTypeDataMeta dataMetas ty =
  sourceTypeDataHead ty >>= \name ->
    case filter (sourceDataNameMatches name) dataMetas of
      [dataMeta] -> Just dataMeta
      _ -> Nothing
  where
    sourceDataNameMatches name dataMeta =
      backendDataName (dmBackend dataMeta) == name
        || qualifiedDataName (dmInfo dataMeta) == name
        || dataName (dmInfo dataMeta) == name

applySourceTypeIdentity :: ConvertContext -> SrcType -> BackendType -> BackendType
applySourceTypeIdentity context sourceTy backendTy =
  case (sourceTy, backendTy) of
    (STArrow sourceDom sourceCod, BTArrow backendDom backendCod) ->
      BTArrow
        (applySourceTypeIdentity context sourceDom backendDom)
        (applySourceTypeIdentity context sourceCod backendCod)
    (STForall _sourceName sourceBound sourceBody, BTForall backendName backendBound backendForallBody) ->
      BTForall
        backendName
        (applySourceTypeIdentity context (maybe STBottom unSrcBound sourceBound) <$> backendBound)
        (applySourceTypeIdentity context sourceBody backendForallBody)
    _
      | Just dataMeta <- sourceTypeDataMeta (ccData context) sourceTy,
        Just dataTy <- canonicalDataTypeForSource dataMeta backendTy ->
          dataTy
    _ ->
      backendTy

canonicalDataTypeForSource :: DataMeta -> BackendType -> Maybe BackendType
canonicalDataTypeForSource dataMeta backendTy =
  case candidates of
    candidate : _ -> Just candidate
    [] -> Nothing
  where
    candidates =
      nub
        [ candidate
        | constructor <- backendDataConstructors (dmBackend dataMeta),
          candidate <- candidateConstructorResultTypes (dmBackend dataMeta) constructor backendTy
        ]

sourceTypeDataHead :: SrcType -> Maybe String
sourceTypeDataHead =
  \case
    STBase name -> Just name
    STCon name _ -> Just name
    _ -> Nothing

dropSourceForalls :: SrcType -> SrcType
dropSourceForalls =
  \case
    STForall _ _ body -> dropSourceForalls body
    ty -> ty

splitSourceArrows :: SrcType -> ([SrcType], SrcType)
splitSourceArrows =
  go []
  where
    go args ty =
      case ty of
        STArrow arg result -> go (args ++ [arg]) result
        _ -> (args, ty)

buildDataMetaForDataInfo :: Map String ElaborateScope -> [DataInfo] -> DataInfo -> Either BackendConversionError DataMeta
buildDataMetaForDataInfo moduleScopes dataInfos info =
  buildDataMeta (elaborateScopeForDataInfo moduleScopes dataInfos info) info

elaborateScopeForDataInfo :: Map String ElaborateScope -> [DataInfo] -> DataInfo -> ElaborateScope
elaborateScopeForDataInfo moduleScopes dataInfos info =
  Map.findWithDefault
    (fallbackElaborateScopeForDataInfo dataInfos info)
    (dataModule info)
    moduleScopes

qualifiedDataName :: DataInfo -> String
qualifiedDataName info =
  symbolDefiningModule (dataInfoSymbol info) ++ "." ++ symbolDefiningName (dataInfoSymbol info)

buildDataMeta :: ElaborateScope -> DataInfo -> Either BackendConversionError DataMeta
buildDataMeta scope info = do
  constructors <- mapM (convertConstructorInfo scope) (dataConstructors info)
  Right
    DataMeta
      { dmInfo = info,
        dmBackend =
          BackendData
            { backendDataName = qualifiedDataName info,
              backendDataParameters = dataParams info,
              backendDataConstructors = constructors
            }
      }

constructorMetasForData :: DataMeta -> [ConstructorMeta]
constructorMetasForData dataMeta =
  [ ConstructorMeta
      { cmInfo = ctorInfo,
        cmBackend = backendCtor,
        cmData = dataMeta
      }
    | (ctorInfo, backendCtor) <- zip (dataConstructors (dmInfo dataMeta)) (backendDataConstructors (dmBackend dataMeta))
  ]

convertDataInfo :: ConvertContext -> DataInfo -> Either BackendConversionError BackendData
convertDataInfo context info =
  case find ((== dataInfoSymbol info) . dataInfoSymbol . dmInfo) (ccData context) of
    Just dataMeta -> Right (dmBackend dataMeta)
    Nothing ->
      buildDataMeta
        (elaborateScopeForDataInfo (ccModuleScopes context) (map dmInfo (ccData context)) info)
        info
        >>= Right . dmBackend

convertConstructorInfo :: ElaborateScope -> ConstructorInfo -> Either BackendConversionError BackendConstructor
convertConstructorInfo scope info = do
  foralls <- mapM (convertConstructorForall scope) (ctorForalls info)
  fields <- mapM (convertLoweredSourceType scope) (ctorArgs info)
  resultTy <- convertLoweredSourceType scope (ctorResult info)
  Right
    BackendConstructor
      { backendConstructorName = ctorRuntimeName info,
        backendConstructorForalls = foralls,
        backendConstructorFields = fields,
        backendConstructorResult = resultTy
      }

convertConstructorForall :: ElaborateScope -> (String, Maybe SrcType) -> Either BackendConversionError BackendTypeBinder
convertConstructorForall scope (name, mbBound) =
  BackendTypeBinder name <$> traverse (convertLoweredSourceType scope) mbBound

convertLoweredSourceType :: ElaborateScope -> SrcType -> Either BackendConversionError BackendType
convertLoweredSourceType scope =
  convertSourceType . lowerType scope

convertSourceType :: SrcType -> Either BackendConversionError BackendType
convertSourceType =
  \case
    STVar name -> Right (BTVar name)
    STArrow dom cod -> BTArrow <$> convertSourceType dom <*> convertSourceType cod
    STBase name -> Right (BTBase (BaseTy name))
    STCon name args -> BTCon (BaseTy name) <$> traverse convertSourceType args
    ty@STVarApp {} -> Left (BackendUnsupportedSourceType ty)
    STForall name mb body ->
      BTForall name
        <$> traverse (convertSourceType . unSrcBound) mb
        <*> convertSourceType body
    STMu name body -> BTMu name <$> convertSourceType body
    STBottom -> Right BTBottom

convertElabType :: ElabType -> Either BackendConversionError BackendType
convertElabType =
  \case
    TVar name -> Right (BTVar name)
    TArrow dom cod -> BTArrow <$> convertElabType dom <*> convertElabType cod
    TCon name args -> BTCon name <$> traverse convertElabType args
    TBase name -> Right (BTBase name)
    TForall name mb body ->
      BTForall name
        <$> traverse (convertElabType . tyToElab) mb
        <*> convertElabType body
    TMu name body -> BTMu name <$> convertElabType body
    TBottom -> Right BTBottom

backendTypeToElab :: BackendType -> ElabType
backendTypeToElab =
  \case
    BTVar name -> TVar name
    BTArrow dom cod -> TArrow (backendTypeToElab dom) (backendTypeToElab cod)
    BTBase base -> TBase base
    BTCon con args -> TCon con (fmap backendTypeToElab args)
    BTForall name mb body ->
      TForall name (mb >>= backendTypeToBound) (backendTypeToElab body)
    BTMu name body -> TMu name (backendTypeToElab body)
    BTBottom -> TBottom

backendTypeToBound :: BackendType -> Maybe BoundType
backendTypeToBound ty =
  case elabToBound (backendTypeToElab ty) of
    Right boundTy -> Just boundTy
    Left _ -> Nothing

canonicalizeBackendType :: ConvertContext -> BackendType -> BackendType
canonicalizeBackendType context =
  canonicalizeDataResult . go
  where
    go =
      \case
        BTArrow dom cod ->
          BTArrow (go dom) (go cod)
        BTCon con args ->
          BTCon con (fmap go args)
        BTForall name mb body ->
          BTForall name (fmap go mb) (go body)
        BTMu name body ->
          BTMu name (go body)
        ty ->
          ty

    canonicalizeDataResult ty =
      case exactMatches of
        [candidate] -> candidate
        _ ->
          case structuralMatches of
            [candidate] -> candidate
            _ -> ty
      where
        candidates = candidateDataResultTypes context ty
        exactMatches = [candidate | candidate <- candidates, candidate == ty]
        structuralMatches = candidates

candidateDataResultTypes :: ConvertContext -> BackendType -> [BackendType]
candidateDataResultTypes context ty =
  nub
    [ substituteBackendTypes completed (backendConstructorResult constructor)
    | dataMeta <- ccData context,
      constructor <- backendDataConstructors (dmBackend dataMeta),
      let parameters = constructorTypeParameterBoundsFor (dmBackend dataMeta) constructor,
      Just substitution <- [matchBackendTypeParameters Map.empty parameters Map.empty (backendConstructorResult constructor) ty],
      let completed = completeBackendParameterSubstitution parameters substitution
    ]

convertTerm :: ConvertContext -> Env -> ElabTerm -> Either BackendConversionError BackendExpr
convertTerm context env =
  convertTermExpected context env Nothing

convertTermExpected :: ConvertContext -> Env -> Maybe BackendType -> ElabTerm -> Either BackendConversionError BackendExpr
convertTermExpected context env mbExpectedTy term =
  case mbExpectedTy of
    Just resultTy0 ->
      let resultTy = canonicalizeBackendType context resultTy0
       in convertSpecialTerm context env term resultTy
            >>= \case
              Just expr -> Right expr
              Nothing -> convertOrdinaryTerm context env term resultTy
    Nothing -> do
      resultTy <- canonicalizeBackendType context <$> inferBackendType env term
      convertSpecialTerm context env term resultTy
        >>= \case
          Just expr -> Right expr
          Nothing -> convertOrdinaryTerm context env term resultTy

convertSpecialTerm ::
  ConvertContext ->
  Env ->
  ElabTerm ->
  BackendType ->
  Either BackendConversionError (Maybe BackendExpr)
convertSpecialTerm context env term resultTy =
  convertCaseApplication context env term resultTy
    >>= \case
      Just expr -> Right (Just expr)
      Nothing ->
        convertConstructorApplication context env term resultTy

convertOrdinaryTerm :: ConvertContext -> Env -> ElabTerm -> BackendType -> Either BackendConversionError BackendExpr
convertOrdinaryTerm context env term resultTy =
  case term of
    EVar name ->
      Right
        BackendVar
          { backendExprType = resultTy,
            backendVarName = name
          }
    ELit lit ->
      Right
        BackendLit
          { backendExprType = resultTy,
            backendLit = lit
          }
    ELam name paramTy body -> do
      rawParamBackendTy <- convertElabType paramTy
      let (paramBackendTy, bodyExpected) =
            case resultTy of
              BTArrow expectedParam cod -> (expectedParam, Just cod)
              _ -> (canonicalizeBackendType context rawParamBackendTy, Nothing)
      bodyExpr <- convertTermExpected context (extendTermEnv name (backendTypeToElab paramBackendTy) env) bodyExpected body
      Right
        BackendLam
          { backendExprType = resultTy,
            backendParamName = name,
            backendParamType = paramBackendTy,
            backendBody = bodyExpr
          }
    EApp fun arg ->
      convertApplication context env resultTy fun arg
    ELet name scheme rhs body -> do
      let schemeTy = schemeToType scheme
      when (termMentionsFreeVariable name rhs) $
        Left (BackendUnsupportedRecursiveLet name)
      bindingTy <- canonicalizeBackendType context <$> convertElabType schemeTy
      rhsExpr <- convertTermExpected context env (Just bindingTy) rhs
      bodyExpr <- convertTermExpected context (extendTermEnv name (backendTypeToElab bindingTy) env) (Just resultTy) body
      Right
        BackendLet
          { backendExprType = resultTy,
            backendLetName = name,
            backendLetType = bindingTy,
            backendLetRhs = rhsExpr,
            backendLetBody = bodyExpr
          }
    ETyAbs name mbBound body ->
      case resultTy of
        BTForall expectedName _ bodyTy -> do
          mbBackendBound <- traverse (fmap (canonicalizeBackendType context) . convertElabType . tyToElab) mbBound
          let boundTy = maybe TBottom tyToElab mbBound
              bodyExpected = Just (substituteBackendType expectedName (BTVar name) bodyTy)
          bodyExpr <- convertTermExpected context (extendTypeEnv name boundTy env) bodyExpected body
          Right
            BackendTyAbs
              { backendExprType = resultTy,
                backendTyParamName = name,
                backendTyParamBound = mbBackendBound,
                backendTyAbsBody = bodyExpr
              }
        _ ->
          convertTermExpected context env (Just resultTy) body
    ETyInst inner inst ->
      convertTypeInstantiation context env resultTy inner inst
    ERoll _ body -> do
      let bodyExpected = unfoldBackendRecursiveType resultTy
      bodyExpr <- convertTermExpected context env bodyExpected body
      Right
        BackendRoll
          { backendExprType = resultTy,
            backendRollPayload = bodyExpr
          }
    EUnroll body -> do
      bodyExpr <- convertTerm context env body
      Right
        BackendUnroll
          { backendExprType = resultTy,
            backendUnrollPayload = bodyExpr
          }

convertApplication ::
  ConvertContext ->
  Env ->
  BackendType ->
  ElabTerm ->
  ElabTerm ->
  Either BackendConversionError BackendExpr
convertApplication context env resultTy fun arg =
  convertApplicationFromFunction context env resultTy fun arg
    `orElseConversion` convertApplicationFromExpectedResult context env resultTy fun arg

convertApplicationFromFunction ::
  ConvertContext ->
  Env ->
  BackendType ->
  ElabTerm ->
  ElabTerm ->
  Either BackendConversionError BackendExpr
convertApplicationFromFunction context env resultTy fun arg = do
  funExpr <- convertTerm context env fun
  argExpr <-
    case backendExprType funExpr of
      BTArrow expectedArg _ -> convertTermExpected context env (Just expectedArg) arg
      _ -> convertTerm context env arg
  Right (backendApplication (applicationResultType resultTy funExpr) funExpr argExpr)

convertApplicationFromExpectedResult ::
  ConvertContext ->
  Env ->
  BackendType ->
  ElabTerm ->
  ElabTerm ->
  Either BackendConversionError BackendExpr
convertApplicationFromExpectedResult context env resultTy fun arg =
  case inferBackendType env arg of
    Right rawArgTy -> do
      let argTy = canonicalizeBackendType context rawArgTy
      funExpr <- convertTermExpected context env (Just (BTArrow argTy resultTy)) fun
      argExpr <- convertTermExpected context env (Just argTy) arg
      Right (backendApplication resultTy funExpr argExpr)
    Left err -> Left err

backendApplication :: BackendType -> BackendExpr -> BackendExpr -> BackendExpr
backendApplication resultTy funExpr argExpr =
  BackendApp
    { backendExprType = resultTy,
      backendFunction = funExpr,
      backendArgument = argExpr
    }

applicationResultType :: BackendType -> BackendExpr -> BackendType
applicationResultType resultTy funExpr =
  case backendExprType funExpr of
    BTArrow _ actualResultTy
      | not (alphaEqBackendType actualResultTy resultTy) -> actualResultTy
    _ -> resultTy

orElseConversion :: Either BackendConversionError a -> Either BackendConversionError a -> Either BackendConversionError a
orElseConversion primary fallback =
  case primary of
    Right value -> Right value
    Left _ -> fallback

termMentionsFreeVariable :: String -> ElabTerm -> Bool
termMentionsFreeVariable needle =
  go
  where
    go term =
      case term of
        EVar name ->
          name == needle
        ELit {} ->
          False
        ELam name _ body
          | name == needle -> False
          | otherwise -> go body
        EApp fun arg ->
          go fun || go arg
        ELet name _ rhs body
          | name == needle -> go rhs
          | otherwise -> go rhs || go body
        ETyAbs _ _ body ->
          go body
        ETyInst inner _ ->
          go inner
        ERoll _ body ->
          go body
        EUnroll body ->
          go body

convertTypeInstantiation ::
  ConvertContext ->
  Env ->
  BackendType ->
  ElabTerm ->
  Instantiation ->
  Either BackendConversionError BackendExpr
convertTypeInstantiation context env resultTy inner inst =
  case inst of
    InstId -> do
      innerExpr <- convertTerm context env inner
      if alphaEqBackendType (backendExprType innerExpr) resultTy
        then Right innerExpr
        else Left (BackendUnsupportedInstantiation inst)
    _ ->
      case appLikeInstantiationType inst of
        Just tyArg -> do
          innerExpr <- convertAppLikeInstantiationFunction context env inner
          case backendExprType innerExpr of
            BTForall name _ bodyTy -> do
              backendTyArg <- canonicalizeBackendType context <$> convertElabType tyArg
              let appliedTy = canonicalizeBackendType context (substituteBackendType name backendTyArg bodyTy)
                  resultTy' =
                    if alphaEqBackendType appliedTy resultTy
                      then resultTy
                      else appliedTy
              Right
                BackendTyApp
                  { backendExprType = resultTy',
                    backendTyFunction = innerExpr,
                    backendTyArgument = backendTyArg
                  }
            _
              | alphaEqBackendType (backendExprType innerExpr) resultTy -> Right innerExpr
              | otherwise -> Left (BackendUnsupportedInstantiation inst)
        Nothing -> Left (BackendUnsupportedInstantiation inst)

convertAppLikeInstantiationFunction ::
  ConvertContext ->
  Env ->
  ElabTerm ->
  Either BackendConversionError BackendExpr
convertAppLikeInstantiationFunction context env inner =
  case convertTerm context env inner of
    Right expr
      | hasForallResult expr -> Right expr
    Right expr ->
      case convertStrippedElim of
        Right strippedExpr
          | hasForallResult strippedExpr -> Right strippedExpr
        _ -> Right expr
    Left err ->
      case convertStrippedElim of
        Right strippedExpr -> Right strippedExpr
        Left _ -> Left err
  where
    hasForallResult expr =
      case backendExprType expr of
        BTForall {} -> True
        _ -> False

    convertStrippedElim =
      let stripped = dropLeadingElimInstantiations inner
       in if stripped == inner
            then Left (BackendUnsupportedInstantiation InstElim)
            else convertTerm context env stripped

dropLeadingElimInstantiations :: ElabTerm -> ElabTerm
dropLeadingElimInstantiations term =
  case term of
    ETyInst inner InstElim -> dropLeadingElimInstantiations inner
    _ -> term

appLikeInstantiationType :: Instantiation -> Maybe ElabType
appLikeInstantiationType inst =
  case inst of
    InstApp ty -> Just ty
    InstSeq (InstInside (InstBot ty)) InstElim -> Just ty
    InstSeq (InstInside (InstApp ty)) InstElim -> Just ty
    _ -> Nothing

convertConstructorApplication ::
  ConvertContext ->
  Env ->
  ElabTerm ->
  BackendType ->
  Either BackendConversionError (Maybe BackendExpr)
convertConstructorApplication context env term resultTy =
  case collectApps term of
    (headTerm, args) ->
      case constructorHead headTerm of
        Just (constructorName, headTypeArgs) ->
          case Map.lookup constructorName (ccConstructors context) of
            Just constructorMeta -> do
              let constructor = cmBackend constructorMeta
                  parameters = constructorTypeParameters constructorMeta
                  rawFields = backendConstructorFields constructor
              if length args == length rawFields
                then do
                  typeBounds <- backendTypeBoundsFromEnv env
                  initialSubstitution <- constructorTypeApplicationSubstitution constructorMeta headTypeArgs
                  resultSubstitution <-
                    case matchBackendTypeParameters typeBounds parameters initialSubstitution (backendConstructorResult constructor) resultTy of
                      Just substitution -> Right substitution
                      Nothing ->
                        case looseConstructorResultSubstitution initialSubstitution (backendConstructorResult constructor) resultTy of
                          Just substitution -> Right substitution
                          Nothing ->
                            Left
                              ( BackendUnsupportedCaseShape
                                  ( "constructor result type does not match expected result for `"
                                      ++ backendConstructorName constructor
                                      ++ "`: expected "
                                      ++ show resultTy
                                      ++ ", constructor result "
                                      ++ show (backendConstructorResult constructor)
                                  )
                              )
                  substitution <-
                    foldM
                      (matchConstructorApplicationArgument env typeBounds parameters)
                      resultSubstitution
                      (zip rawFields args)
                  let completedSubstitution = completeBackendParameterSubstitution parameters substitution
                      fields = map (substituteBackendTypes completedSubstitution) rawFields
                  argExprs <- zipWithM (convertTermExpected context env . Just) fields args
                  Right
                    ( Just
                        BackendConstruct
                          { backendExprType = resultTy,
                            backendConstructName = backendConstructorName constructor,
                            backendConstructArgs = argExprs
                          }
                    )
                else Right Nothing
            Nothing -> Right Nothing
        Nothing -> Right Nothing

constructorTypeParameters :: ConstructorMeta -> BackendParameterBounds
constructorTypeParameters constructorMeta =
  constructorTypeParameterBoundsFor (dmBackend (cmData constructorMeta)) (cmBackend constructorMeta)

looseConstructorResultSubstitution :: Map String BackendType -> BackendType -> BackendType -> Maybe (Map String BackendType)
looseConstructorResultSubstitution substitution constructorResult expectedResult =
  case (constructorResult, expectedResult) of
    (BTMu constructorName constructorBody, BTMu expectedName expectedBody)
      | Set.notMember constructorName (freeBackendTypeVars constructorBody),
        Set.notMember expectedName (freeBackendTypeVars expectedBody),
        alphaEqBackendType (substituteBackendTypes substitution constructorBody) expectedBody ->
          Just substitution
    _ -> Nothing

freeBackendTypeVars :: BackendType -> Set.Set String
freeBackendTypeVars =
  go Set.empty
  where
    go bound =
      \case
        BTVar name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        BTArrow dom cod ->
          go bound dom `Set.union` go bound cod
        BTBase {} ->
          Set.empty
        BTCon _ args ->
          Set.unions (map (go bound) (NE.toList args))
        BTForall name mb body ->
          maybe Set.empty (go bound) mb `Set.union` go (Set.insert name bound) body
        BTMu name body ->
          go (Set.insert name bound) body
        BTBottom ->
          Set.empty

constructorTypeParameterBoundsFor :: BackendData -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsFor dataDecl constructor =
  Map.fromList $
    [(name, Nothing) | name <- backendDataParameters dataDecl]
      ++ [ (backendTypeBinderName binder, backendTypeBinderBound binder)
           | binder <- backendConstructorForalls constructor
         ]

constructorTypeApplicationSubstitution ::
  ConstructorMeta ->
  [BackendType] ->
  Either BackendConversionError (Map String BackendType)
constructorTypeApplicationSubstitution constructorMeta typeArgs =
  Right (Map.fromList (zip typeApplicationNames typeArgs))
  where
    typeApplicationNames = constructorTypeApplicationParameterNames constructorMeta

constructorTypeApplicationParameterNames :: ConstructorMeta -> [String]
constructorTypeApplicationParameterNames constructorMeta =
  sort (Set.toList (freeSourceTypeVars (ctorType (cmInfo constructorMeta))))
    ++ map backendTypeBinderName (backendConstructorForalls (cmBackend constructorMeta))

freeSourceTypeVars :: SrcType -> Set.Set String
freeSourceTypeVars =
  go Set.empty
  where
    go bound =
      \case
        STVar name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        STArrow dom cod ->
          go bound dom `Set.union` go bound cod
        STBase {} ->
          Set.empty
        STCon _ args ->
          foldMap (go bound) args
        STVarApp name args ->
          let headVars =
                if Set.member name bound
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` foldMap (go bound) args
        STForall name mb body ->
          maybe Set.empty (go bound . unSrcBound) mb
            `Set.union` go (Set.insert name bound) body
        STMu name body ->
          go (Set.insert name bound) body
        STBottom ->
          Set.empty

constructorHead :: ElabTerm -> Maybe (String, [BackendType])
constructorHead term =
  case collectConstructorHeadTypes [] term of
    Just (name, typeArgs) ->
      case traverse convertElabType typeArgs of
        Right backendTypeArgs -> Just (name, backendTypeArgs)
        Left _ -> Nothing
    Nothing -> Nothing
  where
    collectConstructorHeadTypes typeArgs =
      \case
        ETyInst inner inst
          | Just ty <- appLikeInstantiationType inst ->
              collectConstructorHeadTypes (ty : typeArgs) inner
        EVar name -> Just (name, typeArgs)
        _ -> Nothing

stripTypeInsts :: ElabTerm -> ElabTerm
stripTypeInsts =
  \case
    ETyInst inner _ -> stripTypeInsts inner
    other -> other

convertCaseApplication ::
  ConvertContext ->
  Env ->
  ElabTerm ->
  BackendType ->
  Either BackendConversionError (Maybe BackendExpr)
convertCaseApplication context env term resultTy =
  case collectApps term of
    (headTerm, args) ->
      case caseScrutinee headTerm of
        Nothing -> Right Nothing
        Just scrutineeTerm -> do
          (backendScrutineeTy, mbScrutineeData) <- caseScrutineeInfo context env scrutineeTerm
          scrutineeExpr <- convertTermExpected context env (Just backendScrutineeTy) scrutineeTerm
          dataMeta <-
            case mbScrutineeData of
              Just scrutineeData -> Right scrutineeData
              Nothing -> do
                typeBounds <- backendTypeBoundsFromEnv env
                requireCaseData context typeBounds (backendExprType scrutineeExpr)
          let constructors = backendDataConstructors (dmBackend dataMeta)
          case compare (length args) (length constructors) of
            EQ -> Just <$> convertCaseWithHandlers context env resultTy scrutineeExpr dataMeta constructors args
            GT -> do
              let (handlers, extraArgs) = splitAt (length constructors) args
              extraArgTys <- mapM (inferBackendType env) extraArgs
              case scanr BTArrow resultTy extraArgTys of
                caseResultTy : appliedResultTys -> do
                  caseExpr <- convertCaseWithHandlers context env caseResultTy scrutineeExpr dataMeta constructors handlers
                  Just
                    <$> foldM
                      (applyCaseExtraArgument context env)
                      caseExpr
                      (zip3 extraArgs extraArgTys appliedResultTys)
                [] -> Right Nothing
            LT -> Right Nothing

convertCaseWithHandlers ::
  ConvertContext ->
  Env ->
  BackendType ->
  BackendExpr ->
  DataMeta ->
  [BackendConstructor] ->
  [ElabTerm] ->
  Either BackendConversionError BackendExpr
convertCaseWithHandlers context env resultTy scrutineeExpr dataMeta constructors handlers = do
  alternatives <- zipWithMCase (convertCaseAlternative context env resultTy dataMeta (backendExprType scrutineeExpr)) constructors handlers
  Right
    BackendCase
      { backendExprType = resultTy,
        backendScrutinee = scrutineeExpr,
        backendAlternatives = alternatives
      }

applyCaseExtraArgument ::
  ConvertContext ->
  Env ->
  BackendExpr ->
  (ElabTerm, BackendType, BackendType) ->
  Either BackendConversionError BackendExpr
applyCaseExtraArgument context env funExpr (arg, argTy, resultTy) = do
  argExpr <- convertTermExpected context env (Just argTy) arg
  Right
    BackendApp
      { backendExprType = resultTy,
        backendFunction = funExpr,
        backendArgument = argExpr
      }

caseScrutinee :: ElabTerm -> Maybe ElabTerm
caseScrutinee term =
  case term of
    ETyInst (EUnroll scrutinee) inst
      | Just _ <- appLikeInstantiationType inst -> Just scrutinee
    _ -> Nothing

caseScrutineeInfo :: ConvertContext -> Env -> ElabTerm -> Either BackendConversionError (BackendType, Maybe DataMeta)
caseScrutineeInfo context env scrutineeTerm =
  constructorApplicationResultType context env scrutineeTerm
    >>= \case
      Just info -> Right info
      Nothing -> do
        scrutineeTy <- inferBackendType env scrutineeTerm
        Right (scrutineeTy, scrutineeDataHint context scrutineeTerm)

scrutineeDataHint :: ConvertContext -> ElabTerm -> Maybe DataMeta
scrutineeDataHint context term =
  case stripTypeInsts term of
    EVar name -> Map.lookup name (ccBindingData context)
    _ -> Nothing

constructorApplicationResultType :: ConvertContext -> Env -> ElabTerm -> Either BackendConversionError (Maybe (BackendType, Maybe DataMeta))
constructorApplicationResultType context env term =
  case collectApps term of
    (headTerm, args) ->
      case constructorHead headTerm of
        Just (constructorName, headTypeArgs) ->
          case Map.lookup constructorName (ccConstructors context) of
            Just constructorMeta
              | length args == length fields -> do
                  typeBounds <- backendTypeBoundsFromEnv env
                  initialSubstitution <- constructorTypeApplicationSubstitution constructorMeta headTypeArgs
                  substitution <-
                    foldM
                      (matchConstructorApplicationArgument env typeBounds parameters)
                      initialSubstitution
                      (zip fields args)
                  let resultTy =
                        substituteBackendTypes
                          (completeBackendParameterSubstitution parameters substitution)
                          (backendConstructorResult constructor)
                  Right (Just (resultTy, Just (cmData constructorMeta)))
              | otherwise -> Right Nothing
              where
                constructor = cmBackend constructorMeta
                fields = backendConstructorFields constructor
                parameters = constructorTypeParameters constructorMeta
            Nothing -> Right Nothing
        Nothing -> Right Nothing

matchConstructorApplicationArgument ::
  Env ->
  BackendTypeBounds ->
  BackendParameterBounds ->
  Map String BackendType ->
  (BackendType, ElabTerm) ->
  Either BackendConversionError (Map String BackendType)
matchConstructorApplicationArgument env typeBounds parameters substitution (expectedTy, arg) =
  -- This is only a best-effort way to recover constructor type parameters.
  -- Expected-type conversion of the argument remains authoritative because it
  -- can canonicalize nested constructor applications before validation.
  case inferBackendType env arg of
    Right actualTy ->
      case matchBackendTypeParameters typeBounds parameters substitution expectedTy actualTy of
        Just substitution' -> Right substitution'
        Nothing -> Right substitution
    Left _ -> Right substitution

requireCaseData :: ConvertContext -> BackendTypeBounds -> BackendType -> Either BackendConversionError DataMeta
requireCaseData context typeBounds scrutineeTy =
  case filter (dataMatchesScrutineeExactly scrutineeTy) (ccData context) of
    [dataMeta] -> Right dataMeta
    [] ->
      case filter (dataMatchesRecursiveBinderHint scrutineeTy) (ccData context) of
        [dataMeta] -> Right dataMeta
        _ ->
          case filter (dataMatchesScrutinee typeBounds scrutineeTy) (ccData context) of
            [dataMeta] -> Right dataMeta
            [] -> Left (BackendUnsupportedCaseShape ("no backend data matches scrutinee type " ++ show scrutineeTy))
            matches ->
              Left
                ( BackendUnsupportedCaseShape
                    ("ambiguous backend data matches scrutinee type " ++ show scrutineeTy ++ ": " ++ show (map (backendDataName . dmBackend) matches))
                )
    matches ->
      Left
        ( BackendUnsupportedCaseShape
            ("ambiguous exact backend data matches scrutinee type " ++ show scrutineeTy ++ ": " ++ show (map (backendDataName . dmBackend) matches))
        )

dataMatchesScrutineeExactly :: BackendType -> DataMeta -> Bool
dataMatchesScrutineeExactly scrutineeTy dataMeta =
  any
    ( \constructor ->
        any
          (== scrutineeTy)
          (candidateConstructorResultTypes (dmBackend dataMeta) constructor scrutineeTy)
    )
    (backendDataConstructors (dmBackend dataMeta))

dataMatchesRecursiveBinderHint :: BackendType -> DataMeta -> Bool
dataMatchesRecursiveBinderHint scrutineeTy dataMeta =
  case scrutineeTy of
    BTMu binderName _ ->
      dataName (dmInfo dataMeta) `elem` recursiveBinderNameHints binderName
        || backendDataName (dmBackend dataMeta) `elem` recursiveBinderNameHints binderName
    _ -> False

recursiveBinderNameHints :: String -> [String]
recursiveBinderNameHints binderName =
  nub [raw, withoutDollar, beforeSelf withoutDollar, suffixAfterDot (beforeSelf withoutDollar)]
  where
    raw = binderName
    withoutDollar =
      case raw of
        '$' : rest -> rest
        _ -> raw

    beforeSelf value =
      case reverse value of
        'f' : 'l' : 'e' : 's' : '_' : rest -> reverse rest
        _ -> value

    suffixAfterDot value =
      case break (== '.') (reverse value) of
        (suffix, _ : _) -> reverse suffix
        _ -> value

candidateConstructorResultTypes :: BackendData -> BackendConstructor -> BackendType -> [BackendType]
candidateConstructorResultTypes dataDecl constructor scrutineeTy =
  case matchBackendTypeParameters Map.empty parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
    Just substitution ->
      let completed = completeBackendParameterSubstitution parameters substitution
       in [substituteBackendTypes completed (backendConstructorResult constructor)]
    Nothing ->
      []
  where
    parameters = constructorTypeParameterBoundsFor dataDecl constructor

dataMatchesScrutinee :: BackendTypeBounds -> BackendType -> DataMeta -> Bool
dataMatchesScrutinee typeBounds scrutineeTy dataMeta =
  any
    ( \constructor ->
        case matchBackendTypeParameters typeBounds (constructorTypeParameterBoundsFor (dmBackend dataMeta) constructor) Map.empty (backendConstructorResult constructor) scrutineeTy of
          Just _ -> True
          Nothing -> False
    )
    (backendDataConstructors (dmBackend dataMeta))

convertCaseAlternative ::
  ConvertContext ->
  Env ->
  BackendType ->
  DataMeta ->
  BackendType ->
  BackendConstructor ->
  ElabTerm ->
  Either BackendConversionError BackendAlternative
convertCaseAlternative context env resultTy dataMeta scrutineeTy constructor handler = do
  fields <- caseAlternativeFieldTypes env dataMeta scrutineeTy constructor
  let (params, body) = collectLeadingLams (length fields) handler
  when (length params /= length fields) $
    Left
      ( BackendUnsupportedCaseShape
          ("handler arity does not match constructor `" ++ backendConstructorName constructor ++ "`")
      )
  let env' =
        foldr
          (\(name, ty) acc -> extendTermEnv name (backendTypeToElab ty) acc)
          env
          (zip (map fst params) fields)
  bodyExpr <- convertTermExpected context env' (Just resultTy) body
  unless (alphaEqBackendType (backendExprType bodyExpr) resultTy) $
    Left
      ( BackendUnsupportedCaseShape
          ("handler result type does not match case result for `" ++ backendConstructorName constructor ++ "`")
      )
  Right
    BackendAlternative
      { backendAltPattern = BackendConstructorPattern (backendConstructorName constructor) (map fst params),
        backendAltBody = bodyExpr
      }

caseAlternativeFieldTypes :: Env -> DataMeta -> BackendType -> BackendConstructor -> Either BackendConversionError [BackendType]
caseAlternativeFieldTypes env dataMeta scrutineeTy constructor = do
  typeBounds <- backendTypeBoundsFromEnv env
  let parameters = constructorTypeParameterBoundsFor (dmBackend dataMeta) constructor
  case matchBackendTypeParameters typeBounds parameters Map.empty (backendConstructorResult constructor) scrutineeTy of
    Just substitution ->
      let completed = completeBackendParameterSubstitution parameters substitution
       in Right (map (substituteBackendTypes completed) (backendConstructorFields constructor))
    Nothing ->
      Left
        ( BackendUnsupportedCaseShape
            ("constructor result type does not match case scrutinee for `" ++ backendConstructorName constructor ++ "`")
        )

collectLeadingLams :: Int -> ElabTerm -> ([(String, ElabType)], ElabTerm)
collectLeadingLams arity =
  go [] arity . stripLeadingTypeWrappers
  where
    go params remaining term
      | remaining <= 0 = (params, term)
      | otherwise =
          case term of
            ETyAbs _ _ body -> go params remaining body
            ETyInst inner _ -> go params remaining inner
            ELam name ty body -> go (params ++ [(name, ty)]) (remaining - 1) body
            other -> (params, other)

    stripLeadingTypeWrappers term =
      case term of
        ETyAbs _ _ body -> stripLeadingTypeWrappers body
        ETyInst inner _ -> stripLeadingTypeWrappers inner
        other -> other

collectApps :: ElabTerm -> (ElabTerm, [ElabTerm])
collectApps =
  go []
  where
    go args term =
      case term of
        EApp fun arg -> go (arg : args) fun
        other -> (other, args)

inferBackendType :: Env -> ElabTerm -> Either BackendConversionError BackendType
inferBackendType env term =
  case typeCheckWithEnv env term of
    Right ty -> convertElabType ty
    Left err -> Left (BackendTypeCheckFailed term err)

extendTermEnv :: String -> ElabType -> Env -> Env
extendTermEnv name ty env =
  env {termEnv = Map.insert name ty (termEnv env)}

extendTypeEnv :: String -> ElabType -> Env -> Env
extendTypeEnv name ty env =
  env {typeEnv = Map.insert name ty (typeEnv env)}

backendTypeBoundsFromEnv :: Env -> Either BackendConversionError BackendTypeBounds
backendTypeBoundsFromEnv env =
  traverse convertTypeBound (typeEnv env)
  where
    convertTypeBound TBottom = Right Nothing
    convertTypeBound boundTy = Just <$> convertElabType boundTy

zipWithMCase ::
  (BackendConstructor -> ElabTerm -> Either BackendConversionError BackendAlternative) ->
  [BackendConstructor] ->
  [ElabTerm] ->
  Either BackendConversionError (NonEmpty BackendAlternative)
zipWithMCase f constructors handlers =
  case zipWith f constructors handlers of
    firstAlt : restAlts ->
      (:|) <$> firstAlt <*> sequence restAlts
    [] ->
      Left (BackendUnsupportedCaseShape "case expression has no alternatives")

matchBackendTypeParameters ::
  BackendTypeBounds ->
  BackendParameterBounds ->
  Map String BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map String BackendType)
matchBackendTypeParameters typeBounds parameterBounds =
  go Map.empty Map.empty
  where
    go leftEnv rightEnv substitution expected actual =
      case expected of
        BTVar name
          | Map.member name parameterBounds,
            Map.notMember name leftEnv ->
              insertParameterSubstitution name actual substitution
        _ ->
          case (expected, actual) of
            (BTVar expectedName, BTVar actualName)
              | sameTypeVar leftEnv rightEnv expectedName actualName ->
                  Just substitution
            (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
              go leftEnv rightEnv substitution expectedDom actualDom
                >>= \substitution' -> go leftEnv rightEnv substitution' expectedCod actualCod
            (BTBase expectedBase, BTBase actualBase)
              | expectedBase == actualBase ->
                  Just substitution
            (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs)
              | expectedCon == actualCon,
                length expectedArgs == length actualArgs ->
                  foldM
                    ( \substitutionAcc (expectedArg, actualArg) ->
                        go leftEnv rightEnv substitutionAcc expectedArg actualArg
                    )
                    substitution
                    (zip (NE.toList expectedArgs) (NE.toList actualArgs))
            (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) -> do
              substitution' <-
                case (expectedBound, actualBound) of
                  (Nothing, Nothing) -> Just substitution
                  (Just expectedBoundTy, Just actualBoundTy) -> go leftEnv rightEnv substitution expectedBoundTy actualBoundTy
                  _ -> Nothing
              go
                (Map.insert expectedName actualName leftEnv)
                (Map.insert actualName expectedName rightEnv)
                substitution'
                expectedBody
                actualBody
            (BTMu expectedName expectedBody, BTMu actualName actualBody) ->
              go
                (Map.insert expectedName actualName leftEnv)
                (Map.insert actualName expectedName rightEnv)
                substitution
                expectedBody
                actualBody
            (BTBottom, BTBottom) ->
              Just substitution
            _ ->
              Nothing

    sameTypeVar leftEnv rightEnv expectedName actualName =
      case (Map.lookup expectedName leftEnv, Map.lookup actualName rightEnv) of
        (Just expectedActual, Just actualExpected) -> expectedActual == actualName && actualExpected == expectedName
        (Nothing, Nothing) -> expectedName == actualName
        _ -> False

    insertParameterSubstitution name actual substitution =
      case Map.lookup name substitution of
        Nothing ->
          if backendParameterBoundMatches name actual substitution
            then Just (Map.insert name actual substitution)
            else Nothing
        Just previous
          | alphaEqBackendType previous actual && backendParameterBoundMatches name previous substitution -> Just substitution
        _ -> Nothing

    backendParameterBoundMatches name actual substitution =
      case Map.lookup name parameterBounds of
        Just (Just _)
          | BTVar actualName <- actual,
            actualName == name ->
              True
        Just (Just boundTy)
          | not (alphaEqBackendType boundTy BTBottom) ->
              let dependencySubstitution =
                    completeBackendParameterSubstitution
                      (Map.delete name parameterBounds)
                      (Map.delete name substitution)
                  expectedBound = substituteBackendTypes dependencySubstitution boundTy
               in typeBoundDependenciesMatch actual expectedBound || actualTypeVariableBoundMatches actual expectedBound
        _ ->
          True

    typeBoundDependenciesMatch actual expectedBound =
      alphaEqBackendType
        (resolveTypeBoundDependencies actual)
        (resolveTypeBoundDependencies expectedBound)

    actualTypeVariableBoundMatches actual expectedBound =
      case actual of
        BTVar actualName ->
          case Map.lookup actualName typeBounds of
            Just (Just actualBound) ->
              typeBoundDependenciesMatch actualBound expectedBound
            _ ->
              False
        _ ->
          False

    resolveTypeBoundDependencies =
      substituteBackendTypes resolvedTypeBounds

    resolvedTypeBounds =
      completeBackendParameterSubstitution typeBounds Map.empty

completeBackendParameterSubstitution :: BackendParameterBounds -> Map String BackendType -> Map String BackendType
completeBackendParameterSubstitution parameterBounds substitution0 =
  resolveDefaultedBounds defaultedNames substitution1
  where
    substitution1 =
      foldl insertBoundDefault substitution0 (Map.toList parameterBounds)

    defaultedNames =
      Set.fromList
        [ name
          | (name, Just boundTy) <- Map.toList parameterBounds,
            Map.notMember name substitution0,
            not (alphaEqBackendType boundTy BTBottom)
        ]

    insertBoundDefault substitution (name, Just boundTy)
      | Map.member name substitution = substitution
      | alphaEqBackendType boundTy BTBottom = substitution
      | otherwise = Map.insert name (substituteBackendTypes substitution boundTy) substitution
    insertBoundDefault substitution _ =
      substitution

    resolveDefaultedBounds names =
      go (Set.size names + Map.size parameterBounds + 1)
      where
        go remaining substitution
          | remaining <= 0 = substitution
          | substitution' == substitution = substitution
          | otherwise = go (remaining - 1) substitution'
          where
            substitution' =
              foldl resolveDefaultedBound substitution (Set.toList names)

    resolveDefaultedBound substitution name =
      case Map.lookup name substitution of
        Just ty ->
          Map.insert name (substituteBackendTypes (Map.delete name substitution) ty) substitution
        Nothing ->
          substitution
