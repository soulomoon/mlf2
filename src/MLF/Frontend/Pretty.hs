{-# LANGUAGE GADTs #-}
module MLF.Frontend.Pretty (
    prettyEmlfType,
    prettyEmlfExpr
) where

import Data.List.NonEmpty (NonEmpty (..))
import MLF.Frontend.Syntax
    ( Expr (..)
    , Lit (..)
    , SrcType (..)
    , SurfaceExpr
    )

prettyEmlfType :: SrcType -> String
prettyEmlfType = goType 0
  where
    goType :: Int -> SrcType -> String
    goType p ty = case ty of
        STVar v -> v
        STBase b -> b
        STBottom -> "⊥"
        STCon c args -> c ++ " " ++ unwords (map (goArg 2) (toListNE args))
        STArrow a b ->
            paren (p > 1) (goType 2 a ++ " -> " ++ goType 1 b)
        STForall{} ->
            let (binds, body) = collectForalls ty
                bindStr = unwords (map prettyBind binds)
            in paren (p > 0) ("∀" ++ bindStr ++ ". " ++ goType 0 body)

    goArg :: Int -> SrcType -> String
    goArg p ty = case ty of
        STVar{} -> goType p ty
        STBase{} -> goType p ty
        STBottom{} -> goType p ty
        _ -> "(" ++ goType 0 ty ++ ")"

    prettyBind :: (String, Maybe SrcType) -> String
    prettyBind (v, Nothing) = v
    prettyBind (v, Just bound) = "(" ++ v ++ " ⩾ " ++ goType 0 bound ++ ")"

prettyEmlfExpr :: SurfaceExpr -> String
prettyEmlfExpr = goExpr 0
  where
    goExpr :: Int -> SurfaceExpr -> String
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

    goArg :: SurfaceExpr -> String
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

collectForalls :: SrcType -> ([(String, Maybe SrcType)], SrcType)
collectForalls = go []
  where
    go acc ty = case ty of
        STForall v mb body -> go (acc ++ [(v, mb)]) body
        _ -> (acc, ty)

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

paren :: Bool -> String -> String
paren True s = "(" ++ s ++ ")"
paren False s = s
