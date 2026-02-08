{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module MLF.Frontend.Pretty (
    prettyEmlfType,
    prettyEmlfExpr
) where

import Data.List.NonEmpty (NonEmpty (..))
import MLF.Frontend.Syntax
    ( BoundTopVar
    , ExprStage (..)
    , Expr (..)
    , Lit (..)
    , SrcBound (..)
    , SrcTopVar (..)
    , SrcTy (..)
    )

prettyEmlfType :: SrcTy n v -> String
prettyEmlfType = goType 0
  where
    goType :: Int -> SrcTy n v -> String
    goType p ty = case ty of
        STVar v -> v
        STBase b -> b
        STBottom -> "⊥"
        STCon c args -> c ++ " " ++ unwords (map (goArg 2) (toListNE args))
        STArrow a b ->
            paren (p > 1) (goType 2 a ++ " -> " ++ goType 1 b)
        STForall v mb body ->
            let (tailBinds, tailBody) = collectForalls body
                binds = (v, fmap unSrcBound mb) : tailBinds
                bindStr = unwords (map prettyBind binds)
            in paren (p > 0) ("∀" ++ bindStr ++ ". " ++ goType 0 tailBody)

    goArg :: Int -> SrcTy n v -> String
    goArg p ty = case ty of
        STVar{} -> goType p ty
        STBase{} -> goType p ty
        STBottom{} -> goType p ty
        _ -> "(" ++ goType 0 ty ++ ")"

    prettyBind :: (String, Maybe (SrcTy n (BoundTopVar n))) -> String
    prettyBind (v, Nothing) = v
    prettyBind (v, Just bound) = "(" ++ v ++ " ⩾ " ++ goType 0 bound ++ ")"

    collectForalls
        :: SrcTy n 'TopVarAllowed
        -> ([(String, Maybe (SrcTy n (BoundTopVar n)))], SrcTy n 'TopVarAllowed)
    collectForalls ty = case ty of
        STForall v mb body ->
            let (rest, body') = collectForalls body
            in ((v, fmap unSrcBound mb) : rest, body')
        _ -> ([], ty)

prettyEmlfExpr :: Expr 'Surface (SrcTy n v) -> String
prettyEmlfExpr = goExpr 0
  where
    goExpr :: Int -> Expr 'Surface (SrcTy n v) -> String
    goExpr p expr = case expr of
        EVar v -> v
        ELit l -> prettyLit l
        ELam v body ->
            paren (p > 0) ("λ(" ++ v ++ ") " ++ goExpr 0 body)
        ELamAnn v ty body ->
            paren (p > 0) ("λ(" ++ v ++ " : " ++ prettyEmlfType ty ++ ") " ++ goExpr 0 body)
        EApp f a ->
            paren (p > 1) (goExpr 1 f ++ " " ++ goArg a)
        ELet v rhs body ->
            paren (p > 0) ("let " ++ v ++ " = " ++ goExpr 0 rhs ++ " in " ++ goExpr 0 body)
        EAnn e ty ->
            "(" ++ goExpr 0 e ++ " : " ++ prettyEmlfType ty ++ ")"

    goArg :: Expr 'Surface (SrcTy n v) -> String
    goArg expr = case expr of
        EVar{} -> goExpr 2 expr
        ELit{} -> goExpr 2 expr
        EAnn{} -> goExpr 2 expr
        _ -> "(" ++ goExpr 0 expr ++ ")"

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
