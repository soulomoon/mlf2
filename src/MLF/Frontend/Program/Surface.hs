module MLF.Frontend.Program.Surface
  ( surfaceVar,
    surfaceLit,
    surfaceLam,
    surfaceApp,
    surfaceLet,
    surfaceLamAnn,
    surfaceAnn,
  )
where

import MLF.Frontend.Syntax (Expr (..), Lit, SrcType, SurfaceExpr)

surfaceVar :: String -> SurfaceExpr
surfaceVar = EVar

surfaceLit :: Lit -> SurfaceExpr
surfaceLit = ELit

surfaceLam :: String -> SurfaceExpr -> SurfaceExpr
surfaceLam = ELam

surfaceApp :: SurfaceExpr -> SurfaceExpr -> SurfaceExpr
surfaceApp = EApp

surfaceLet :: String -> SurfaceExpr -> SurfaceExpr -> SurfaceExpr
surfaceLet = ELet

surfaceLamAnn :: String -> SrcType -> SurfaceExpr -> SurfaceExpr
surfaceLamAnn = ELamAnn

surfaceAnn :: SurfaceExpr -> SrcType -> SurfaceExpr
surfaceAnn = EAnn
