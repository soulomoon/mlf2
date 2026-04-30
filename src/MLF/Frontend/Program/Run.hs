{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Run
  ( Value (..),
    runProgram,
    runLocatedProgram,
    prettyValue,
  )
where

import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Elab.Pipeline (ElabTerm (..), Pretty (..), Ty (..), freeTypeVarsType, normalize, schemeFromType, typeCheck)
import MLF.Frontend.Program.Check (checkLocatedProgram, checkProgram)
import MLF.Frontend.Program.Elaborate (ElaborateScope, mkElaborateScope)
import MLF.Frontend.Program.Finalize (recoverSourceType)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    CheckedModule (..),
    CheckedProgram (..),
    ConstructorInfo (..),
    DataInfo (..),
    ProgramDiagnostic,
    ProgramError (..),
    SymbolIdentity (..),
    diagnosticForProgramError,
  )
import MLF.Frontend.Syntax (Lit (..), SrcBound (..), SrcTy (..), SrcType)
import qualified MLF.Frontend.Syntax.Program as ProgramSyntax

data Value
  = VLit Lit
  | VData String [Value]
  | VTerm ElabTerm
  deriving (Eq, Show)

runProgram :: ProgramSyntax.Program -> Either ProgramError Value
runProgram program = do
  checked <- checkProgram program
  rejectOpaqueMain checked
  pure (toValueWithProgram checked (normalizeProgramTerm (programMainTerm checked)))

runLocatedProgram :: ProgramSyntax.LocatedProgram -> Either ProgramDiagnostic Value
runLocatedProgram located = do
  checked <- checkLocatedProgram located
  case rejectOpaqueMain checked of
    Left err -> Left (diagnosticForProgramError (Just located) err)
    Right () -> pure (toValueWithProgram checked (normalizeProgramTerm (programMainTerm checked)))

programMainTerm :: CheckedProgram -> ElabTerm
programMainTerm checked =
  foldr bindAll (EVar (checkedProgramMain checked)) (reachableRuntimeBindings checked)
  where
    bindAll binding body =
      ELet
        (checkedBindingName binding)
        (schemeFromType (checkedBindingType binding))
        (checkedBindingTerm binding)
        body

rejectOpaqueMain :: CheckedProgram -> Either ProgramError ()
rejectOpaqueMain checked =
  case mainSourceType checked of
    Just ty
      | Builtins.srcTypeMentionsOpaqueBuiltin ty ->
          Left (ProgramPipelineError "run-program does not support IO main values yet")
    _ ->
      case reachableOpaqueRuntimeBindings checked of
        [] -> Right ()
        bindings ->
          Left
            ( ProgramPipelineError
                ( "run-program does not support IO dependencies yet: "
                    ++ intercalate ", " (map checkedBindingName bindings)
                )
            )

reachableRuntimeBindings :: CheckedProgram -> [CheckedBinding]
reachableRuntimeBindings checked =
  [ binding
    | binding <- allCheckedBindings checked,
      checkedBindingName binding `Set.member` reachableNames,
      not (checkedBindingMentionsOpaqueBuiltin binding)
  ]
  where
    reachableNames = reachableBindingNames checked (Set.singleton (checkedProgramMain checked))

reachableOpaqueRuntimeBindings :: CheckedProgram -> [CheckedBinding]
reachableOpaqueRuntimeBindings checked =
  [ binding
    | binding <- allCheckedBindings checked,
      checkedBindingName binding `Set.member` reachableNames,
      checkedBindingMentionsOpaqueBuiltin binding
  ]
  where
    reachableNames = reachableBindingNames checked (Set.singleton (checkedProgramMain checked))

reachableBindingNames :: CheckedProgram -> Set.Set String -> Set.Set String
reachableBindingNames checked roots =
  go Set.empty roots
  where
    bindingMap = Map.fromList [(checkedBindingName binding, binding) | binding <- allCheckedBindings checked]
    topLevelNames = Map.keysSet bindingMap

    go visited pending =
      case Set.minView pending of
        Nothing -> visited
        Just (name, rest)
          | name `Set.member` visited -> go visited rest
          | Just binding <- Map.lookup name bindingMap ->
              let deps = freeTermVariables (checkedBindingTerm binding) `Set.intersection` topLevelNames
               in go (Set.insert name visited) (rest `Set.union` deps)
          | otherwise -> go visited rest

allCheckedBindings :: CheckedProgram -> [CheckedBinding]
allCheckedBindings checked =
  [ binding
    | checkedModule <- checkedProgramModules checked,
      binding <- checkedModuleBindings checkedModule
  ]

checkedBindingMentionsOpaqueBuiltin :: CheckedBinding -> Bool
checkedBindingMentionsOpaqueBuiltin =
  Builtins.srcTypeMentionsOpaqueBuiltin . checkedBindingSourceType

freeTermVariables :: ElabTerm -> Set.Set String
freeTermVariables =
  go Set.empty
  where
    go bound term =
      case term of
        EVar name
          | name `Set.member` bound -> Set.empty
          | otherwise -> Set.singleton name
        ELit {} -> Set.empty
        ELam name _ body -> go (Set.insert name bound) body
        EApp fun arg -> go bound fun `Set.union` go bound arg
        ELet name _ rhs body -> go bound rhs `Set.union` go (Set.insert name bound) body
        ETyAbs _ _ body -> go bound body
        ETyInst inner _ -> go bound inner
        ERoll _ body -> go bound body
        EUnroll body -> go bound body

normalizeProgramTerm :: ElabTerm -> ElabTerm
normalizeProgramTerm term =
  let termNorm = normalize term
      termSimplified = case termNorm of
        ELet v _ rhs (EVar bodyV)
          | v == bodyV -> rhs
        _ -> termNorm
      termUnderTyAbs =
        case termSimplified of
          ETyAbs v mbBound body ->
            let body' = normalizeProgramTerm body
                rebuilt = ETyAbs v mbBound body'
             in case typeCheck rebuilt of
                  Right (TForall _ _ bodyTy)
                    | v `notElem` freeTypeVarsType bodyTy -> body'
                  _ -> rebuilt
          _ -> termSimplified
      termStripped = stripUnusedTopTyAbs termUnderTyAbs
   in if termStripped == term
        then termStripped
        else normalizeProgramTerm termStripped

stripUnusedTopTyAbs :: ElabTerm -> ElabTerm
stripUnusedTopTyAbs term = case term of
  ETyAbs v mbBound body ->
    let body' = stripUnusedTopTyAbs body
        term' = ETyAbs v mbBound body'
     in case typeCheck term' of
          Right (TForall _ _ bodyTy)
            | v `notElem` freeTypeVarsType bodyTy -> body'
          _ -> term'
  ELam v ty body -> ELam v ty (stripUnusedTopTyAbs body)
  EApp f a -> EApp (stripUnusedTopTyAbs f) (stripUnusedTopTyAbs a)
  ELet v sch rhs body -> ELet v sch (stripUnusedTopTyAbs rhs) (stripUnusedTopTyAbs body)
  ETyInst e inst -> ETyInst (stripUnusedTopTyAbs e) inst
  ERoll ty body -> ERoll ty (stripUnusedTopTyAbs body)
  EUnroll body -> EUnroll (stripUnusedTopTyAbs body)
  _ -> term

toValueWithProgram :: CheckedProgram -> ElabTerm -> Value
toValueWithProgram checked term =
  case mainSourceType checked of
    Just srcTy ->
      case decodeSourceValue checked srcTy term of
        Just value -> value
        Nothing
          | sourceTypeIsData checked srcTy ->
              case decodeAnyData checked term of
                Just value -> value
                Nothing -> toValue term
          | otherwise -> toValue term
    Nothing ->
      case decodeAnyData checked term of
        Just value -> value
        Nothing -> toValue term

toValue :: ElabTerm -> Value
toValue term = case stripRuntimeWrappers term of
  ELit lit -> VLit lit
  other -> VTerm other

prettyValue :: Value -> String
prettyValue value = case value of
  VLit (LInt i) -> show i
  VLit (LBool b) -> if b then "true" else "false"
  VLit (LString s) -> show s
  VData ctor [] -> ctor
  VData ctor args -> unwords (ctor : map prettyValueArg args)
  VTerm term -> pretty term

prettyValueArg :: Value -> String
prettyValueArg value = case value of
  VData _ (_ : _) -> "(" ++ prettyValue value ++ ")"
  _ -> prettyValue value

mainSourceType :: CheckedProgram -> Maybe SrcType
mainSourceType checked =
  case
    [ checkedBindingSourceType binding
      | checkedModule <- checkedProgramModules checked,
        binding <- checkedModuleBindings checkedModule,
        checkedBindingName binding == checkedProgramMain checked
    ]
  of
    ty : _ -> Just (recoverMainSourceType checked ty)
    [] -> Nothing

recoverMainSourceType :: CheckedProgram -> SrcType -> SrcType
recoverMainSourceType checked ty =
  case ty of
    STArrow {} -> ty
    _ -> recoverSourceType (programElaborateScope checked) ty

programElaborateScope :: CheckedProgram -> ElaborateScope
programElaborateScope checked =
  mkElaborateScope Map.empty (Map.fromList [(qualifiedDataName info, info) | info <- allDataInfos checked]) Map.empty []

decodeSourceValue :: CheckedProgram -> SrcType -> ElabTerm -> Maybe Value
decodeSourceValue checked srcTy term =
  case lookupDataInfosForType checked srcTy of
    [] ->
      case stripRuntimeWrappers term of
        ELit lit -> Just (VLit lit)
        _ -> Nothing
    dataInfos ->
      firstJust
        [ decodeChurchData checked dataInfo (dataTypeSubst dataInfo srcTy) term
          | dataInfo <- dataInfos
        ]

firstJust :: [Maybe a] -> Maybe a
firstJust values =
  case [value | Just value <- values] of
    value : _ -> Just value
    [] -> Nothing

lookupDataInfosForType :: CheckedProgram -> SrcType -> [DataInfo]
lookupDataInfosForType checked srcTy =
  case srcTy of
    STBase name -> lookupDataInfosByName checked name
    STCon name _ -> lookupDataInfosByName checked name
    _ -> []

sourceTypeIsData :: CheckedProgram -> SrcType -> Bool
sourceTypeIsData checked srcTy =
  not (null (lookupDataInfosForType checked srcTy))

lookupDataInfosByName :: CheckedProgram -> String -> [DataInfo]
lookupDataInfosByName checked name =
  [ dataInfo
    | dataInfo <- allDataInfos checked,
      dataTypeHeadMatches dataInfo name
  ]

dataTypeHeadMatches :: DataInfo -> String -> Bool
dataTypeHeadMatches dataInfo name =
  if isQualifiedSourceHead name
    then qualifiedDataName dataInfo == name
    else symbolDefiningName (dataInfoSymbol dataInfo) == name

isQualifiedSourceHead :: String -> Bool
isQualifiedSourceHead = elem '.'

qualifiedDataName :: DataInfo -> String
qualifiedDataName dataInfo =
  symbolDefiningModule (dataInfoSymbol dataInfo) ++ "." ++ symbolDefiningName (dataInfoSymbol dataInfo)

allDataInfos :: CheckedProgram -> [DataInfo]
allDataInfos checked =
  [ dataInfo
    | checkedModule <- checkedProgramModules checked,
      dataInfo <- map snd (Map.toList (checkedModuleData checkedModule))
  ]

decodeAnyData :: CheckedProgram -> ElabTerm -> Maybe Value
decodeAnyData checked term =
  case [value | dataInfo <- allDataInfos checked, Just value <- [decodeChurchData checked dataInfo Map.empty term]] of
    value : _ -> Just value
    [] -> Nothing

decodeChurchData :: CheckedProgram -> DataInfo -> Map.Map String SrcType -> ElabTerm -> Maybe Value
decodeChurchData checked dataInfo subst term = do
  let stripped = stripRuntimeWrappers term
      (handlerNames, body) = collectLeadingLams stripped
      constructors = dataConstructors dataInfo
  if length handlerNames < length constructors
    then Nothing
    else do
      let activeHandlers = take (length constructors) handlerNames
          (headTerm, args) = collectElabApps (stripRuntimeWrappers body)
      selectedHandler <- case headTerm of
        EVar name -> Just name
        _ -> Nothing
      ctorInfo <- lookupByHandler activeHandlers constructors selectedHandler
      if length args /= length (ctorArgs ctorInfo)
        then Nothing
        else
          let argTypes = map (canonicalFieldType checked dataInfo . substDataParams subst) (ctorArgs ctorInfo)
           in Just (VData (ctorName ctorInfo) (zipWith (decodeArg checked) argTypes args))

dataTypeSubst :: DataInfo -> SrcType -> Map.Map String SrcType
dataTypeSubst dataInfo srcTy =
  case (dataParams dataInfo, srcTy) of
    ([], STBase name)
      | dataTypeHeadMatches dataInfo name -> Map.empty
    (params, STCon name args)
      | dataTypeHeadMatches dataInfo name && length params == length args ->
          Map.fromList (zip params (toList args))
    _ -> Map.empty

substDataParams :: Map.Map String SrcType -> SrcType -> SrcType
substDataParams subst ty =
  case ty of
    STVar name -> Map.findWithDefault ty name subst
    STArrow dom cod -> STArrow (substDataParams subst dom) (substDataParams subst cod)
    STBase {} -> ty
    STCon name args -> STCon name (fmap (substDataParams subst) args)
    STVarApp name args ->
      let args' = fmap (substDataParams subst) args
       in case Map.lookup name subst of
            Just (STVar replacementName) -> STVarApp replacementName args'
            Just (STBase replacementName) -> STCon replacementName args'
            Just (STCon replacementName replacementArgs) -> STCon replacementName (replacementArgs <> args')
            Just (STVarApp replacementName replacementArgs) -> STVarApp replacementName (replacementArgs <> args')
            _ -> STVarApp name args'
    STForall name mb body ->
      let subst' = Map.delete name subst
       in STForall name (fmap (SrcBound . substDataParams subst' . unSrcBound) mb) (substDataParams subst' body)
    STMu name body ->
      STMu name (substDataParams (Map.delete name subst) body)
    STBottom -> STBottom

canonicalFieldType :: CheckedProgram -> DataInfo -> SrcType -> SrcType
canonicalFieldType checked ownerInfo = canonical
  where
    canonical ty =
      case ty of
        STVar {} -> ty
        STBase name ->
          case lookupDataInfoInModule checked (dataModule ownerInfo) name of
            Just info -> STBase (qualifiedDataName info)
            Nothing -> ty
        STCon name args ->
          let args' = fmap canonical args
           in case lookupDataInfoInModule checked (dataModule ownerInfo) name of
                Just info -> STCon (qualifiedDataName info) args'
                Nothing -> STCon name args'
        STVarApp name args -> STVarApp name (fmap canonical args)
        STArrow dom cod -> STArrow (canonical dom) (canonical cod)
        STForall name mb body ->
          STForall name (fmap (SrcBound . canonical . unSrcBound) mb) (canonical body)
        STMu name body -> STMu name (canonical body)
        STBottom -> STBottom

lookupDataInfoInModule :: CheckedProgram -> ProgramSyntax.ModuleName -> String -> Maybe DataInfo
lookupDataInfoInModule checked moduleName0 name =
  case
    [ dataInfo
      | dataInfo <- allDataInfos checked,
        symbolDefiningModule (dataInfoSymbol dataInfo) == moduleName0,
        symbolDefiningName (dataInfoSymbol dataInfo) == name
    ]
  of
    dataInfo : _ -> Just dataInfo
    [] -> Nothing

decodeArg :: CheckedProgram -> SrcType -> ElabTerm -> Value
decodeArg checked srcTy term =
  case decodeSourceValue checked srcTy term of
    Just value -> value
    Nothing
      | sourceTypeIsData checked srcTy ->
          case decodeAnyData checked term of
            Just value -> value
            Nothing -> toValue term
      | otherwise -> toValue term

lookupByHandler :: [String] -> [ConstructorInfo] -> String -> Maybe ConstructorInfo
lookupByHandler handlerNames constructors selected =
  case [ctor | (handlerName, ctor) <- zip handlerNames constructors, handlerName == selected] of
    ctor : _ -> Just ctor
    [] -> Nothing

collectLeadingLams :: ElabTerm -> ([String], ElabTerm)
collectLeadingLams = go []
  where
    go acc term =
      case stripRuntimeWrappers term of
        ELam name _ body -> go (acc ++ [name]) body
        other -> (acc, other)

collectElabApps :: ElabTerm -> (ElabTerm, [ElabTerm])
collectElabApps = go []
  where
    go acc term =
      case stripRuntimeWrappers term of
        EApp fun arg -> go (stripRuntimeWrappers arg : acc) fun
        other -> (other, acc)

stripRuntimeWrappers :: ElabTerm -> ElabTerm
stripRuntimeWrappers term =
  case term of
    ETyAbs _ _ body -> stripRuntimeWrappers body
    ETyInst inner _ -> stripRuntimeWrappers inner
    ERoll _ body -> stripRuntimeWrappers body
    EUnroll body -> stripRuntimeWrappers body
    _ -> term
