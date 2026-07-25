{-# LANGUAGE GADTs #-}
module MLF.XMLF.Pretty (
    prettyXmlfType,
    prettyXmlfComp,
    prettyXmlfTerm,
    prettyCheckedType,
    prettyCheckedComp
) where

import Control.Monad.State.Strict (State, evalState, gets, modify')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Syntax (Lit (..))
import qualified MLF.Types.Elab as Checked
import MLF.Types.Identity
    ( TypeBinderIdentity
    , typeBinderIdentityStableName
    )
import MLF.Util.Names (freshNameLike)
import MLF.XMLF.Syntax (XmlfComp (..), XmlfType (..))

prettyXmlfType :: XmlfType -> String
prettyXmlfType = goType 0
  where
    goType :: Int -> XmlfType -> String
    goType p ty = case ty of
        XTVar v -> v
        XTBottom -> "⊥"
        XTBase b -> b
        XTCon c args -> c ++ " " ++ unwords (map (goArg 2) (toListNE args))
        XTVarApp v args -> v ++ " " ++ unwords (map (goArg 2) (toListNE args))
        XTArrow a b ->
            paren (p > 1) (goType 2 a ++ " -> " ++ goType 1 b)
        XTForall v bound body ->
            paren (p > 0) ("∀(" ++ v ++ " ⩾ " ++ goType 0 bound ++ ") " ++ goType 0 body)
        XTMu v body ->
            paren (p > 0) ("μ" ++ v ++ ". " ++ goType 0 body)

    goArg :: Int -> XmlfType -> String
    goArg prec ty = case ty of
        XTVar{} -> goType prec ty
        XTBottom{} -> goType prec ty
        XTBase{} -> goType prec ty
        _ -> "(" ++ goType 0 ty ++ ")"

prettyXmlfComp :: XmlfComp -> String
prettyXmlfComp = goComp 0
  where
    goComp :: Int -> XmlfComp -> String
    goComp p comp = case comp of
        XCId -> "ε"
        XCBot ty -> "⊲" ++ compType ty
        XCHyp v -> v ++ "⊳"
        XCInner c ->
            paren (p > 1) ("∀(⩾ " ++ goComp 0 c ++ ")")
        XCOuter v c ->
            paren (p > 1) ("∀(" ++ v ++ " ⩾) " ++ goComp 1 c)
        XCElim -> "N"
        XCIntro -> "O"
        XCSeq c1 c2 ->
            paren (p > 0) (goComp 0 c1 ++ "; " ++ goComp 1 c2)

    compType :: XmlfType -> String
    compType ty = case ty of
        XTVar{} -> prettyXmlfType ty
        XTBottom{} -> prettyXmlfType ty
        XTBase{} -> prettyXmlfType ty
        XTCon{} -> "(" ++ prettyXmlfType ty ++ ")"
        XTVarApp{} -> "(" ++ prettyXmlfType ty ++ ")"
        XTArrow{} -> "(" ++ prettyXmlfType ty ++ ")"
        XTForall{} -> "(" ++ prettyXmlfType ty ++ ")"
        XTMu{} -> "(" ++ prettyXmlfType ty ++ ")"

prettyXmlfTerm :: Checked.XmlfTerm -> String
prettyXmlfTerm term =
    evalState (goTerm 0 term) emptyCheckedNameState
  where
    goTerm :: Int -> Checked.XmlfTerm -> CheckedPretty String
    goTerm p tm = case tm of
        Checked.EVarNode resolved ->
            pure (Checked.resolvedVarReferenceName resolved)
        Checked.ELit l ->
            pure (prettyLit l)
        Checked.ELam resolved body -> do
            binderType <- renderCheckedType (Checked.resolvedVarType resolved)
            bodyText <- goTerm 0 body
            pure $
                paren
                    (p > 0)
                    ( "λ("
                        ++ Checked.resolvedVarReferenceName resolved
                        ++ " : "
                        ++ binderType
                        ++ ") "
                        ++ bodyText
                    )
        Checked.EApp f a -> do
            functionText <- goTerm 1 f
            argumentText <- goAppArg a
            pure (paren (p > 1) (functionText ++ " " ++ argumentText))
        Checked.ELet resolved _ rhs body -> do
            rhsText <- goTerm 0 rhs
            bodyText <- goTerm 0 body
            pure $
                paren
                    (p > 0)
                    ( "let "
                        ++ Checked.resolvedVarReferenceName resolved
                        ++ " = "
                        ++ rhsText
                        ++ " in "
                        ++ bodyText
                    )
        Checked.ETyAbsRef ref mbBound body -> do
            binderName <- checkedRefName ref
            boundText <-
                case mbBound of
                    Nothing -> pure "⊥"
                    Just bound -> renderCheckedBound bound
            bodyText <- goTerm 0 body
            pure $
                paren
                    (p > 0)
                    ( "Λ("
                        ++ binderName
                        ++ " ⩾ "
                        ++ boundText
                        ++ ") "
                        ++ bodyText
                    )
        Checked.ETyInst e inst -> do
            expressionText <- goTerm 1 e
            instantiationText <- renderCheckedComp inst
            pure $
                paren
                    (p > 1)
                    (expressionText ++ "[" ++ instantiationText ++ "]")
        Checked.ERoll ty body -> do
            typeText <- renderCheckedType ty
            bodyText <- goPrefixArg body
            pure $
                paren
                    (p > 0)
                    ("roll[" ++ typeText ++ "] " ++ bodyText)
        Checked.EUnroll body -> do
            bodyText <- goPrefixArg body
            pure (paren (p > 0) ("unroll " ++ bodyText))

    goAppArg :: Checked.XmlfTerm -> CheckedPretty String
    goAppArg tm = case tm of
        Checked.EVarNode{} -> goTerm 2 tm
        Checked.ELit{} -> goTerm 2 tm
        _ -> do
            text <- goTerm 0 tm
            pure ("(" ++ text ++ ")")

    goPrefixArg :: Checked.XmlfTerm -> CheckedPretty String
    goPrefixArg tm = case tm of
        Checked.EVarNode{} -> goTerm 1 tm
        Checked.ELit{} -> goTerm 1 tm
        Checked.ETyInst{} -> goTerm 1 tm
        _ -> do
            text <- goTerm 0 tm
            pure ("(" ++ text ++ ")")

    prettyLit :: Lit -> String
    prettyLit lit = case lit of
        LInt i -> show i
        LBool b -> if b then "true" else "false"
        LChar c -> show c
        LString s -> show s

prettyCheckedType :: Checked.ElabType -> String
prettyCheckedType ty =
    evalState (renderCheckedType ty) emptyCheckedNameState

prettyCheckedComp :: Checked.Instantiation -> String
prettyCheckedComp inst =
    evalState (renderCheckedComp inst) emptyCheckedNameState

data CheckedNameState = CheckedNameState
    { checkedIdentityNames :: Map.Map TypeBinderIdentity String
    , checkedUsedNames :: Set.Set String
    }

type CheckedPretty = State CheckedNameState

emptyCheckedNameState :: CheckedNameState
emptyCheckedNameState =
    CheckedNameState
        { checkedIdentityNames = Map.empty
        , checkedUsedNames = Set.empty
        }

checkedRefName :: Checked.TypeBinderRef -> CheckedPretty String
checkedRefName ref = do
    let identity = Checked.typeBinderRefIdentity ref
    knownName <- gets (Map.lookup identity . checkedIdentityNames)
    case knownName of
        Just name ->
            pure name
        Nothing -> do
            usedNames <- gets checkedUsedNames
            let rawName = Checked.typeBinderRefName ref
                preferredName
                    | rawName == typeBinderIdentityStableName identity = "a"
                    | otherwise = rawName
                name = freshNameLike preferredName usedNames
            modify' $ \state ->
                state
                    { checkedIdentityNames =
                        Map.insert identity name (checkedIdentityNames state)
                    , checkedUsedNames =
                        Set.insert name (checkedUsedNames state)
                    }
            pure name

renderCheckedType :: Checked.ElabType -> CheckedPretty String
renderCheckedType ty =
    prettyXmlfType <$> checkedType ty

renderCheckedBound :: Checked.BoundType -> CheckedPretty String
renderCheckedBound bound =
    prettyXmlfType <$> checkedBound bound

renderCheckedComp :: Checked.Instantiation -> CheckedPretty String
renderCheckedComp inst =
    prettyXmlfComp <$> checkedComp inst

checkedType :: Checked.ElabType -> CheckedPretty XmlfType
checkedType ty = case ty of
    Checked.TVarRef ref ->
        XTVar <$> checkedRefName ref
    Checked.TArrow a b ->
        XTArrow <$> checkedType a <*> checkedType b
    Checked.TConWithIdentity _ (BaseTy c) args ->
        XTCon c <$> traverse checkedType args
    Checked.TVarAppRef ref args ->
        XTVarApp <$> checkedRefName ref <*> traverse checkedType args
    Checked.TBaseWithIdentity _ (BaseTy b) ->
        pure (XTBase b)
    Checked.TForallRef ref mb body ->
        XTForall
            <$> checkedRefName ref
            <*> maybe (pure XTBottom) checkedBound mb
            <*> checkedType body
    Checked.TMuRef ref body ->
        XTMu <$> checkedRefName ref <*> checkedType body
    Checked.TBottom ->
        pure XTBottom

checkedBound :: Checked.BoundType -> CheckedPretty XmlfType
checkedBound bound = case bound of
    Checked.TArrow a b ->
        XTArrow <$> checkedType a <*> checkedType b
    Checked.TConWithIdentity _ (BaseTy c) args ->
        XTCon c <$> traverse checkedType args
    Checked.TVarAppRef ref args ->
        XTVarApp <$> checkedRefName ref <*> traverse checkedType args
    Checked.TBaseWithIdentity _ (BaseTy b) ->
        pure (XTBase b)
    Checked.TForallRef ref mb body ->
        XTForall
            <$> checkedRefName ref
            <*> maybe (pure XTBottom) checkedBound mb
            <*> checkedType body
    Checked.TMuRef ref body ->
        XTMu <$> checkedRefName ref <*> checkedType body
    Checked.TBottom ->
        pure XTBottom

checkedComp :: Checked.Instantiation -> CheckedPretty XmlfComp
checkedComp inst = case inst of
    Checked.InstId ->
        pure XCId
    Checked.InstApp ty -> do
        typeValue <- checkedType ty
        pure (XCSeq (XCInner (XCBot typeValue)) XCElim)
    Checked.InstBot ty ->
        XCBot <$> checkedType ty
    Checked.InstIntro ->
        pure XCIntro
    Checked.InstElim ->
        pure XCElim
    Checked.InstAbstrRef ref ->
        XCHyp <$> checkedRefName ref
    Checked.InstUnderRef ref inner ->
        XCOuter <$> checkedRefName ref <*> checkedComp inner
    Checked.InstInside inner ->
        XCInner <$> checkedComp inner
    Checked.InstSeq left right ->
        XCSeq <$> checkedComp left <*> checkedComp right

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

paren :: Bool -> String -> String
paren True s = "(" ++ s ++ ")"
paren False s = s
