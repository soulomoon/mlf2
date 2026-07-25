{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Desugar
  ( desugarSurface,
    desugarResolvedSurface,
  )
where

import Data.Functor.Foldable (cata)
import MLF.Frontend.Syntax
  ( Expr (..),
    NormCoreExpr,
    NormSurfaceExpr,
    ResolvedNormCoreExpr,
    ResolvedNormSurfaceExpr,
    SurfaceExprF (..),
    TermReference (..),
    resolvedTermReferenceDetails,
    termReferenceName,
  )
import MLF.Types.Identity
  ( IdDetails (LocalId),
    IdentityGenerator,
    freshLocalRef,
    idDetailsIsEvidence,
    idDetailsSameIdentity,
  )

{- Note [κσ coercions and desugaring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.1, "From λ-terms to typing constraints") treats:

  - (b : σ)     as syntactic sugar for  κσ b
  - λ(x : σ) b  as syntactic sugar for  λ(x) let x = κσ x in b

The thesis also phrases annotated lambdas via surface `(:)` (Chapter 12.3.2):

  λ(x : τ) a  ≜  λ(x) let x = (x : τ) in a

Following the thesis (and `emlf_typeing_rules.md`), we eliminate *all* surface
annotations before constraint generation by introducing an explicit coercion
constant `cτ` (represented as `ECoerceConst τ`) and treating annotations as
ordinary application/let:

  (a : τ)        ≜  cτ a
  λ(x : τ) a     ≜  λ(x) let x = cτ x in a

Surface annotations on let-bindings are also desugared to coercion terms:

  let x : σ = e in b   ≜   let x = (e : σ) in b   ≜   let x = cσ e in b

This is a normal let-binding where the RHS is a coercion application, not a
special "declared scheme" form. The coercion constant cσ is a term-level
operator that constrains its argument to have type σ.

Note: Source annotated lambdas desugar to a plain lambda with a let-binding
containing a coercion. Compiler-generated evidence lambdas are not source
annotations: their class-metadata type is authoritative, so the resolved-only
path emits `EExactLamNode` and deliberately does not construct κσ.
-}

desugarSurface :: NormSurfaceExpr -> NormCoreExpr
desugarSurface = cata alg
  where
    alg = \case
      EVarSurfaceF ref -> EVarNode ref
      ELitSurfaceF l -> ELit l
      ELamSurfaceF ref body -> ELamNode ref body
      EAppSurfaceF fun arg -> EApp fun arg
      ELetSurfaceF ref rhs body -> ELetNode ref rhs body
      ELamAnnSurfaceF ref ty body ->
        let name = termReferenceName ref
         in ELamNode ref (ELet name (EApp (ECoerceConst ty) (EVarNode ref)) body)
      EExactLamSurfaceF ref ty body -> EExactLamNode ref ty body
      EAnnSurfaceF expr0 ty -> EApp (ECoerceConst ty) expr0
      EExactAnnSurfaceF expr0 ty exactTy -> EApp (EExactCoerceConst ty exactTy) expr0

-- | Desugar an identity-bearing surface term without reopening a name lookup
-- seam. For source annotations, the thesis expansion introduces a shadowing
-- let binder, so allocate that binder here and redirect the already-resolved
-- body occurrences by identity. The lambda source and coercion mediator are
-- therefore distinct by construction before constraint generation sees the
-- core term. An 'EvidenceId' instead denotes a compiler-owned exact parameter,
-- not surface κσ syntax.
desugarResolvedSurface ::
  IdentityGenerator ->
  ResolvedNormSurfaceExpr ->
  (ResolvedNormCoreExpr, IdentityGenerator)
desugarResolvedSurface = go
  where
    go :: IdentityGenerator -> ResolvedNormSurfaceExpr -> (ResolvedNormCoreExpr, IdentityGenerator)
    go currentGenerator currentExpr =
      case currentExpr of
        EVarNode ref -> (EVarNode ref, currentGenerator)
        ELit literal -> (ELit literal, currentGenerator)
        ELamNode ref body ->
          let (body', generator') = go currentGenerator body
           in (ELamNode ref body', generator')
        EApp fun arg ->
          let (fun', generator1) = go currentGenerator fun
              (arg', generator2) = go generator1 arg
           in (EApp fun' arg', generator2)
        ELetNode ref rhs body ->
          let (rhs', generator1) = go currentGenerator rhs
              (body', generator2) = go generator1 body
           in (ELetNode ref rhs' body', generator2)
        ELamAnnNode sourceRef ty body ->
          if idDetailsIsEvidence (resolvedTermReferenceDetails sourceRef)
            then
              let (body', generator1) = go currentGenerator body
               in (EExactLamNode sourceRef ty body', generator1)
            else
              let name = termReferenceName sourceRef
                  (mediatorLocalRef, generator1) = freshLocalRef name currentGenerator
                  mediatorRef = ResolvedTermReference (LocalId mediatorLocalRef) name
                  (body', generator2) = go generator1 body
                  coercedSource = EApp (ECoerceConst ty) (EVarNode sourceRef)
               in ( ELamNode
                      sourceRef
                      ( ELetNode
                          mediatorRef
                          coercedSource
                          (replaceResolvedOccurrences sourceRef mediatorRef body')
                      ),
                    generator2
                  )
        EExactLamNode sourceRef ty body ->
          let (body', generator') = go currentGenerator body
           in (EExactLamNode sourceRef ty body', generator')
        EAnn inner ty ->
          let (inner', generator') = go currentGenerator inner
           in (EApp (ECoerceConst ty) inner', generator')
        EExactAnn inner ty exactTy ->
          let (inner', generator') = go currentGenerator inner
           in (EApp (EExactCoerceConst ty exactTy) inner', generator')

    replaceResolvedOccurrences sourceRef replacementRef =
      goReplace
      where
        goReplace :: ResolvedNormCoreExpr -> ResolvedNormCoreExpr
        goReplace currentExpr =
          case currentExpr of
            EVarNode ref
              | sameReferenceIdentity ref sourceRef -> EVarNode replacementRef
              | otherwise -> EVarNode ref
            ELit literal -> ELit literal
            ELamNode ref body -> ELamNode ref (goReplace body)
            EApp fun arg -> EApp (goReplace fun) (goReplace arg)
            ELetNode ref rhs body -> ELetNode ref (goReplace rhs) (goReplace body)
            EExactLamNode ref ty body -> EExactLamNode ref ty (goReplace body)
            ECoerceConst ty -> ECoerceConst ty
            EExactCoerceConst ty exactTy -> EExactCoerceConst ty exactTy

    sameReferenceIdentity left right =
      idDetailsSameIdentity
        (resolvedTermReferenceDetails left)
        (resolvedTermReferenceDetails right)
