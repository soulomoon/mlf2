module MLF.XMLF.Pretty (
    prettyXmlfType,
    prettyXmlfComp,
    prettyXmlfTerm
) where

import Data.List.NonEmpty (NonEmpty (..))
import MLF.Frontend.Syntax (Lit (..))
import MLF.XMLF.Syntax (XmlfComp (..), XmlfTerm (..), XmlfType (..))

prettyXmlfType :: XmlfType -> String
prettyXmlfType = goType 0
  where
    goType :: Int -> XmlfType -> String
    goType p ty = case ty of
        XTVar v -> v
        XTBottom -> "⊥"
        XTBase b -> b
        XTCon c args -> c ++ " " ++ unwords (map (goArg 2) (toListNE args))
        XTArrow a b ->
            paren (p > 1) (goType 2 a ++ " -> " ++ goType 1 b)
        XTForall v bound body ->
            paren (p > 0) ("∀(" ++ v ++ " ⩾ " ++ goType 0 bound ++ ") " ++ goType 0 body)

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
        XTArrow{} -> "(" ++ prettyXmlfType ty ++ ")"
        XTForall{} -> "(" ++ prettyXmlfType ty ++ ")"

prettyXmlfTerm :: XmlfTerm -> String
prettyXmlfTerm = goTerm 0
  where
    goTerm :: Int -> XmlfTerm -> String
    goTerm p tm = case tm of
        XVar v -> v
        XLit l -> prettyLit l
        XLam v ty body ->
            paren (p > 0) ("λ(" ++ v ++ " : " ++ prettyXmlfType ty ++ ") " ++ goTerm 0 body)
        XApp f a ->
            paren (p > 1) (goTerm 1 f ++ " " ++ goArg a)
        XTyAbs v bound body ->
            paren (p > 0) ("Λ(" ++ v ++ " ⩾ " ++ prettyXmlfType bound ++ ") " ++ goTerm 0 body)
        XTyInst e comp ->
            paren (p > 1) (goTerm 1 e ++ "[" ++ prettyXmlfComp comp ++ "]")
        XLet v rhs body ->
            paren (p > 0) ("let " ++ v ++ " = " ++ goTerm 0 rhs ++ " in " ++ goTerm 0 body)

    goArg :: XmlfTerm -> String
    goArg tm = case tm of
        XVar{} -> goTerm 2 tm
        XLit{} -> goTerm 2 tm
        _ -> "(" ++ goTerm 0 tm ++ ")"

    prettyLit :: Lit -> String
    prettyLit lit = case lit of
        LInt i -> show i
        LBool b -> if b then "true" else "false"
        LString s -> show s

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

paren :: Bool -> String -> String
paren True s = "(" ++ s ++ ")"
paren False s = s
