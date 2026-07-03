{-# LANGUAGE GADTs #-}
module MLF.XMLF.Pretty (
    prettyXmlfType,
    prettyXmlfComp,
    prettyXmlfTerm
) where

import Data.List.NonEmpty (NonEmpty (..))
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Syntax (Lit (..))
import qualified MLF.Types.Elab as Checked
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
prettyXmlfTerm = goTerm 0
  where
    goTerm :: Int -> Checked.XmlfTerm -> String
    goTerm p tm = case tm of
        Checked.EVarNode resolved -> Checked.resolvedVarReferenceName resolved
        Checked.ELit l -> prettyLit l
        Checked.ELam resolved body ->
            paren (p > 0) ("λ(" ++ Checked.resolvedVarReferenceName resolved ++ " : " ++ prettyCheckedType (Checked.resolvedVarType resolved) ++ ") " ++ goTerm 0 body)
        Checked.EApp f a ->
            paren (p > 1) (goTerm 1 f ++ " " ++ goAppArg a)
        Checked.ELet resolved _ rhs body ->
            paren (p > 0) ("let " ++ Checked.resolvedVarReferenceName resolved ++ " = " ++ goTerm 0 rhs ++ " in " ++ goTerm 0 body)
        Checked.ETyAbsRef ref mbBound body ->
            paren (p > 0) ("Λ(" ++ Checked.typeBinderRefName ref ++ " ⩾ " ++ maybe "⊥" prettyCheckedBound mbBound ++ ") " ++ goTerm 0 body)
        Checked.ETyInst e inst ->
            paren (p > 1) (goTerm 1 e ++ "[" ++ prettyCheckedComp inst ++ "]")
        Checked.ERoll ty body ->
            paren (p > 0) ("roll[" ++ prettyCheckedType ty ++ "] " ++ goPrefixArg body)
        Checked.EUnroll body ->
            paren (p > 0) ("unroll " ++ goPrefixArg body)

    goAppArg :: Checked.XmlfTerm -> String
    goAppArg tm = case tm of
        Checked.EVarNode{} -> goTerm 2 tm
        Checked.ELit{} -> goTerm 2 tm
        _ -> "(" ++ goTerm 0 tm ++ ")"

    goPrefixArg :: Checked.XmlfTerm -> String
    goPrefixArg tm = case tm of
        Checked.EVarNode{} -> goTerm 1 tm
        Checked.ELit{} -> goTerm 1 tm
        Checked.ETyInst{} -> goTerm 1 tm
        _ -> "(" ++ goTerm 0 tm ++ ")"

    prettyLit :: Lit -> String
    prettyLit lit = case lit of
        LInt i -> show i
        LBool b -> if b then "true" else "false"
        LChar c -> show c
        LString s -> show s

prettyCheckedType :: Checked.ElabType -> String
prettyCheckedType = prettyXmlfType . checkedType

prettyCheckedBound :: Checked.BoundType -> String
prettyCheckedBound = prettyXmlfType . checkedBound

prettyCheckedComp :: Checked.Instantiation -> String
prettyCheckedComp = prettyXmlfComp . checkedComp

checkedType :: Checked.ElabType -> XmlfType
checkedType ty = case ty of
    Checked.TVarRef ref -> XTVar (Checked.typeBinderRefName ref)
    Checked.TArrow a b -> XTArrow (checkedType a) (checkedType b)
    Checked.TConWithIdentity _ (BaseTy c) args -> XTCon c (fmap checkedType args)
    Checked.TVarAppRef ref args -> XTVarApp (Checked.typeBinderRefName ref) (fmap checkedType args)
    Checked.TBaseWithIdentity _ (BaseTy b) -> XTBase b
    Checked.TForallRef ref mb body ->
        XTForall (Checked.typeBinderRefName ref) (maybe XTBottom checkedBound mb) (checkedType body)
    Checked.TMuRef ref body -> XTMu (Checked.typeBinderRefName ref) (checkedType body)
    Checked.TBottom -> XTBottom

checkedBound :: Checked.BoundType -> XmlfType
checkedBound bound = case bound of
    Checked.TArrow a b -> XTArrow (checkedType a) (checkedType b)
    Checked.TConWithIdentity _ (BaseTy c) args -> XTCon c (fmap checkedType args)
    Checked.TVarAppRef ref args -> XTVarApp (Checked.typeBinderRefName ref) (fmap checkedType args)
    Checked.TBaseWithIdentity _ (BaseTy b) -> XTBase b
    Checked.TForallRef ref mb body ->
        XTForall (Checked.typeBinderRefName ref) (maybe XTBottom checkedBound mb) (checkedType body)
    Checked.TMuRef ref body -> XTMu (Checked.typeBinderRefName ref) (checkedType body)
    Checked.TBottom -> XTBottom

checkedComp :: Checked.Instantiation -> XmlfComp
checkedComp inst = case inst of
    Checked.InstId -> XCId
    Checked.InstApp ty -> XCSeq (XCInner (XCBot (checkedType ty))) XCElim
    Checked.InstBot ty -> XCBot (checkedType ty)
    Checked.InstIntro -> XCIntro
    Checked.InstElim -> XCElim
    Checked.InstAbstrRef ref -> XCHyp (Checked.typeBinderRefName ref)
    Checked.InstUnderRef ref inner -> XCOuter (Checked.typeBinderRefName ref) (checkedComp inner)
    Checked.InstInside inner -> XCInner (checkedComp inner)
    Checked.InstSeq left right -> XCSeq (checkedComp left) (checkedComp right)

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

paren :: Bool -> String -> String
paren True s = "(" ++ s ++ ")"
paren False s = s
