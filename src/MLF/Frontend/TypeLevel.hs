module MLF.Frontend.TypeLevel
  ( TypeLevelKind (..),
    TypeLevelTy (..),
    TypeLevelPattern (..),
    TypeFamilyEquation (..),
    TypeFamilyDecl (..),
    TypeFamilyEnv,
    TypeLevelNormalizeError (..),
    familyEnvFromDecls,
    normalizeTypeLevel,
    normalizeTypeLevelWith,
    isFamilyFree,
    freeTypeLevelVars,
    substTypeLevel,
  )
where

import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Util.Names (freshNameLike)

data TypeLevelKind
  = TLKType
  | TLKVar String
  | TLKArrow TypeLevelKind TypeLevelKind
  deriving (Eq, Ord, Show)

data TypeLevelTy
  = TLTVar String
  | TLTCon String
  | TLTArrow TypeLevelTy TypeLevelTy
  | TLTLam String TypeLevelKind TypeLevelTy
  | TLTApp TypeLevelTy TypeLevelTy
  | TLTFamilyApp String [TypeLevelTy]
  deriving (Eq, Ord, Show)

data TypeLevelPattern
  = TLPVar String
  | TLPCon String [TypeLevelPattern]
  deriving (Eq, Ord, Show)

data TypeFamilyEquation = TypeFamilyEquation
  { familyEquationPatterns :: [TypeLevelPattern],
    familyEquationRhs :: TypeLevelTy
  }
  deriving (Eq, Ord, Show)

data TypeFamilyDecl = TypeFamilyDecl
  { familyDeclName :: String,
    familyDeclParams :: [(String, TypeLevelKind)],
    familyDeclResultKind :: TypeLevelKind,
    familyDeclEquations :: [TypeFamilyEquation]
  }
  deriving (Eq, Ord, Show)

type TypeFamilyEnv = Map.Map String TypeFamilyDecl

data TypeLevelNormalizeError
  = UnknownTypeFamily String
  | TypeFamilyArityMismatch String Int Int
  | TypeFamilyStuck String [TypeLevelTy]
  | TypeFamilyCycle [String]
  | TypeLevelFuelExhausted TypeLevelTy
  deriving (Eq, Show)

familyEnvFromDecls :: [TypeFamilyDecl] -> Either String TypeFamilyEnv
familyEnvFromDecls decls =
  go Map.empty decls
  where
    go env [] = Right env
    go env (decl : rest)
      | Map.member (familyDeclName decl) env =
          Left ("duplicate type family `" ++ familyDeclName decl ++ "`")
      | otherwise =
          go (Map.insert (familyDeclName decl) decl env) rest

normalizeTypeLevel :: TypeFamilyEnv -> TypeLevelTy -> Either TypeLevelNormalizeError TypeLevelTy
normalizeTypeLevel = normalizeTypeLevelWith 128

normalizeTypeLevelWith :: Int -> TypeFamilyEnv -> TypeLevelTy -> Either TypeLevelNormalizeError TypeLevelTy
normalizeTypeLevelWith initialFuel env ty =
  go initialFuel [] ty
  where
    go fuel stack current =
      case current of
        TLTVar {} -> Right current
        TLTCon {} -> Right current
        TLTArrow left right ->
          TLTArrow <$> go fuel stack left <*> go fuel stack right
        TLTLam {} ->
          Right current
        TLTApp fun arg -> do
          funNorm <- go fuel stack fun
          argNorm <- go fuel stack arg
          case funNorm of
            TLTLam name _ body -> do
              fuel' <- spendFuel fuel current
              go fuel' stack (substTypeLevel name argNorm body)
            _ -> Right (TLTApp funNorm argNorm)
        TLTFamilyApp name args -> do
          argsNorm <- mapM (go fuel stack) args
          reduceFamily fuel stack name argsNorm current

    reduceFamily fuel stack name args original = do
      decl <-
        case Map.lookup name env of
          Just decl -> Right decl
          Nothing -> Left (UnknownTypeFamily name)
      let expected = length (familyDeclParams decl)
          actual = length args
      if expected /= actual
        then Left (TypeFamilyArityMismatch name expected actual)
        else do
          let key = (name, args)
          case findIndex ((== key) . fst) stack of
            Just ix ->
              Left (TypeFamilyCycle (map snd (reverse (take (ix + 1) stack)) ++ [name]))
            Nothing ->
              case firstMatchingEquation args (familyDeclEquations decl) of
                Nothing -> Left (TypeFamilyStuck name args)
                Just (subst, equation) -> do
                  fuel' <- spendFuel fuel original
                  go fuel' ((key, name) : stack) (substMany subst (familyEquationRhs equation))

    firstMatchingEquation args equations =
      firstJust (map matchEquation equations)
      where
        matchEquation equation
          | length (familyEquationPatterns equation) /= length args = Nothing
          | otherwise =
              case
                foldl
                  (\acc (pat, arg) -> acc >>= \subst0 -> matchPattern subst0 pat arg)
                  (Just Map.empty)
                  (zip (familyEquationPatterns equation) args)
              of
                Nothing -> Nothing
                Just subst -> Just (subst, equation)

    spendFuel fuel original
      | fuel <= 0 = Left (TypeLevelFuelExhausted original)
      | otherwise = Right (fuel - 1)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : rest) = firstJust rest
firstJust (Just x : _) = Just x

matchPattern :: Map.Map String TypeLevelTy -> TypeLevelPattern -> TypeLevelTy -> Maybe (Map.Map String TypeLevelTy)
matchPattern subst pattern0 ty =
  case pattern0 of
    TLPVar name ->
      case Map.lookup name subst of
        Nothing -> Just (Map.insert name ty subst)
        Just existing
          | existing == ty -> Just subst
          | otherwise -> Nothing
    TLPCon name pats ->
      case collectConApp ty of
        Just (actualName, args)
          | name == actualName && length pats == length args ->
              foldl
                (\acc (pat, arg) -> acc >>= \subst0 -> matchPattern subst0 pat arg)
                (Just subst)
                (zip pats args)
        _ -> Nothing

collectConApp :: TypeLevelTy -> Maybe (String, [TypeLevelTy])
collectConApp ty =
  case go [] ty of
    (TLTCon name, args) -> Just (name, args)
    _ -> Nothing
  where
    go args (TLTApp fun arg) = go (arg : args) fun
    go args headTy = (headTy, args)

substMany :: Map.Map String TypeLevelTy -> TypeLevelTy -> TypeLevelTy
substMany subst ty =
  go subst ty
  where
    go subst0 current =
      case current of
        TLTVar name ->
          Map.findWithDefault current name subst0
        TLTCon {} -> current
        TLTArrow left right -> TLTArrow (go subst0 left) (go subst0 right)
        TLTLam name kind body ->
          let subst1 = Map.delete name subst0
           in if name `Set.member` freeSubstVars subst1
                then
                  let used =
                        Set.unions
                          [ freeSubstVars subst1,
                            freeTypeLevelVars body,
                            Set.singleton name
                          ]
                      name' = freshNameLike name used
                      body' = substTypeLevel name (TLTVar name') body
                   in TLTLam name' kind (go subst1 body')
                else TLTLam name kind (go subst1 body)
        TLTApp fun arg -> TLTApp (go subst0 fun) (go subst0 arg)
        TLTFamilyApp name args -> TLTFamilyApp name (map (go subst0) args)

    freeSubstVars subst0 =
      Set.unions (map freeTypeLevelVars (Map.elems subst0))

isFamilyFree :: TypeLevelTy -> Bool
isFamilyFree = \case
  TLTVar {} -> True
  TLTCon {} -> True
  TLTArrow left right -> isFamilyFree left && isFamilyFree right
  TLTLam _ _ body -> isFamilyFree body
  TLTApp fun arg -> isFamilyFree fun && isFamilyFree arg
  TLTFamilyApp {} -> False

freeTypeLevelVars :: TypeLevelTy -> Set.Set String
freeTypeLevelVars =
  go Set.empty
  where
    go bound ty =
      case ty of
        TLTVar name
          | Set.member name bound -> Set.empty
          | otherwise -> Set.singleton name
        TLTCon {} -> Set.empty
        TLTArrow left right -> go bound left `Set.union` go bound right
        TLTLam name _ body -> go (Set.insert name bound) body
        TLTApp fun arg -> go bound fun `Set.union` go bound arg
        TLTFamilyApp _ args -> foldMap (go bound) args

substTypeLevel :: String -> TypeLevelTy -> TypeLevelTy -> TypeLevelTy
substTypeLevel needle replacement =
  go
  where
    freeReplacement = freeTypeLevelVars replacement

    go ty =
      case ty of
        TLTVar name
          | name == needle -> replacement
          | otherwise -> ty
        TLTCon {} -> ty
        TLTArrow left right -> TLTArrow (go left) (go right)
        TLTLam name kind body
          | name == needle -> ty
          | Set.member name freeReplacement ->
              let used =
                    Set.unions
                      [ freeReplacement,
                        freeTypeLevelVars body,
                        Set.singleton name
                      ]
                  name' = freshNameLike name used
                  body' = substTypeLevel name (TLTVar name') body
               in TLTLam name' kind (go body')
          | otherwise -> TLTLam name kind (go body)
        TLTApp fun arg -> TLTApp (go fun) (go arg)
        TLTFamilyApp name args -> TLTFamilyApp name (map go args)
