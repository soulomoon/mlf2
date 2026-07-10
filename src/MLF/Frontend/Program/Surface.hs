module MLF.Frontend.Program.Surface
  ( surfaceVar,
    surfaceResolvedVar,
    surfaceLit,
    surfaceLam,
    surfaceApp,
    surfaceLet,
    surfaceLamAnn,
    surfaceAnn,
    surfaceBinderIdentity,
  )
where

import MLF.Frontend.Syntax (Expr (..), Lit, SrcType, SurfaceExpr)
import MLF.Types.Identity (IdDetails)

surfaceVar :: String -> SurfaceExpr
surfaceVar = EVar

-- | Internal post-resolution occurrence. The string remains only the
-- display/runtime projection; semantic lookup uses the carried identity.
surfaceResolvedVar :: IdDetails -> String -> SurfaceExpr
surfaceResolvedVar details = EBinderIdentity details . EVar

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

surfaceBinderIdentity :: IdDetails -> SurfaceExpr -> SurfaceExpr
surfaceBinderIdentity = EBinderIdentity
