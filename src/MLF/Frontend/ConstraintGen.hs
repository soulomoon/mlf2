{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Frontend.ConstraintGen
  ( ConstraintError (..),
    ConstraintResult (..),
    ModuleRootId (..),
    RootOwnershipIndex (..),
    ModuleConstraintRoot (..),
    ModuleConstraintResult (..),
    InstantiationTargetTopology (..),
    InstantiationSite (..),
    mkInstantiationSite,
    mkArrowInstantiationSite,
    AnnExpr (..),
    BindingKey (..),
    bindingKeyForTermReference,
    ExternalEnv,
    ExternalBindingMode (..),
    ExternalBinding (..),
    ExternalBindingIdentity,
    externalBindingIdentityFromDetails,
    externalBindingIdentityFromResolvedVar,
    externalBindingIdentityFromDeferredRef,
    externalBindingRuntimeName,
    externalBindingDetails,
    ExternalBindings,
    generateConstraints,
    generateConstraintsCore,
    generateResolvedConstraintsCore,
    generateConstraintsWithEnv,
    generateConstraintsWithExternalBindings,
    generateConstraintsWithExternalBindingsFromSupply,
    generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply,
    generateConstraintsWithResolvedExternalBindingsFromSupply,
    generateConstraintsWithResolvedExternalBindingsAndTypeIdentitiesFromSupply,
    generateModuleConstraintsKeyedWithExternalBindings,
    generateModuleConstraintsKeyedWithExternalBindingsFromSupply,
    generateModuleConstraintsKeyedWithExternalBindingsAndTypeIdentitiesFromSupply,
    generateModuleConstraintsKeyedWithResolvedExternalBindingsFromSupply,
    generateModuleConstraintsKeyedWithResolvedExternalBindingsAndTypeIdentitiesFromSupply,
    generateModuleConstraintsWithExternalBindings,
    generateConstraintsCoreWithEnv,
    generateConstraintsCoreWithExternalBindings,
    generateResolvedConstraintsCoreWithExternalBindings,
  )
where

import Data.Functor.Foldable (cata)
import Data.Bifunctor (first)
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (NodeId, PolySyms, cAnnEdges, getEdgeId)
import MLF.Frontend.Symbol (SymbolIdentity, symbolIdentityAliasMapWith)
import MLF.Frontend.ConstraintGen.State
import MLF.Frontend.ConstraintGen.Translate (buildModuleRootExprsKeyedWithExternalBindings, buildRootExprWithExternalBindings)
import MLF.Frontend.ConstraintGen.Types
import MLF.Frontend.Desugar (desugarResolvedSurface)
import MLF.Frontend.Syntax
  ( NormCoreExpr,
    NormSurfaceExpr,
    ResolvedNormCoreExpr,
    ResolvedNormSurfaceExpr,
    VarName,
  )
import MLF.Frontend.TermResolve (resolveTermReferences)
import qualified MLF.Primitive.Identity as PrimitiveIdentity
import MLF.Types.Identity
  ( IdDetails (EnvId),
    IdentityGenerator,
    TypeBinderIdentity,
    freshEnvRef,
    initialIdentityGenerator,
  )

{- Note [Phase 1: Constraint Generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module implements Phase 1 of the MLF type inference algorithm: translating
a source expression into a graphic constraint. This is the "compositional
translation" described in Rémy & Yakobowski (ICFP 2008) §1.

The translation is syntax-directed and produces:
  1. A DAG of type nodes (TyVar, TyArrow, TyBase, TyExp)
  2. A binding tree via binding edges (scope)
  3. Instantiation edges (≤) at application sites
  4. The root NodeId representing the expression's type

Key invariants maintained:
  - Binding edges encode scope (paper-style binding tree)
  - Lambda parameters are bound at the CURRENT level (monomorphic)
  - Let bindings create a CHILD level and wrap RHS in TyExp

The constraint graph is the input to subsequent phases:
  - Phase 2 normalizes via grafting/merging
  - Phase 3 checks acyclicity of instantiation dependencies
  - Phase 4 computes the principal presolution
  - Phase 5 solves remaining unification
  - Phase 6 elaborates to xMLF

Paper reference: ICFP 2008, §1 "From ML to constraints"
-}

{- Note [Lambda vs Let Polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MLF distinguishes between lambda-bound and let-bound variables in how they
handle polymorphism. This follows standard ML-style let-polymorphism.

Lambda-bound variables (monomorphic by default):
  In `λf. (f 1, f True)`, the parameter `f` gets a plain type variable `α`.
  Each use of `f` must have the SAME type, so this fails: we can't unify
  `α → Int` with `α → Bool`.

Let-bound variables (polymorphic via expansion nodes):
  In `let f = λx. x in (f 1, f True)`, the binding `f` is wrapped in an
  expansion node `s · (α → α)`. Each USE of `f` can instantiate differently,
  so `f 1 : Int` and `f True : Bool` both work.

The classic example that illustrates this difference:

    (λf. (f 1, f True)) (λx. x)     -- FAILS in ML and MLF (without annotation)
    let f = λx. x in (f 1, f True)  -- WORKS in ML and MLF

Why can't MLF infer polymorphism for lambda parameters?

To type the lambda version, you need HIGHER-RANK polymorphism:
  (λf. ...) : (∀α. α → α) → (Int, Bool)

MLF CAN express this type, but cannot INFER it without help because:
  1. The argument type (∀α. α → α) is not determined by the lambda body alone
  2. Multiple valid types exist (the type is not principal without annotation)
  3. Inference would require "guessing" where to place ∀ quantifiers

This is why our implementation:
  - ELam: allocates a plain TyVar { tnId = for, tnBound = Nothing } the parameter (monomorphic)
  - ELet: wraps the RHS in a TyExp expansion node (polymorphic)

Explicit lambda annotations (`ELamAnn`) allow the user to request higher-rank
types where needed, e.g.:

  λ(f : ∀α. α → α). (f 1, f True)

These are surface sugar and are desugared before Phase 1 (thesis §12.3.2):

  λ(x : τ) a  ≜  λ(x) let x = (x : τ) in a

Paper references:
  - ICFP 2008, §1 describes the constraint language and type syntax
  - ICFP 2008, §3 defines solved forms and expansion variables (s · τ)
  - Le Botlan & Rémy (2003) "MLF: Raising ML to the Power of System F"
    discusses the design choice of annotation-free let-polymorphism
-}

generateConstraints :: PolySyms -> NormSurfaceExpr -> Either ConstraintError (ConstraintResult p)
generateConstraints polySyms expr =
  generateConstraintsWithExternalBindings polySyms Map.empty expr

-- | Like 'generateConstraints' but with an external environment of
-- pre-existing type assumptions for free variables.
generateConstraintsWithEnv :: PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsWithEnv polySyms extEnv expr =
  let (extBindings, generator) = externalBindingsFromEnv initialIdentityGenerator extEnv
   in generateConstraintsWithExternalBindingsFromSupply generator polySyms extBindings expr

generateConstraintsWithExternalBindings :: PolySyms -> ExternalBindings -> NormSurfaceExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsWithExternalBindings polySyms extBindings expr =
  generateConstraintsWithExternalBindingsFromSupply initialIdentityGenerator polySyms extBindings expr

generateConstraintsWithExternalBindingsFromSupply :: IdentityGenerator -> PolySyms -> ExternalBindings -> NormSurfaceExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsWithExternalBindingsFromSupply generator polySyms =
  generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply generator polySyms Map.empty Map.empty

generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply :: IdentityGenerator -> PolySyms -> Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> ExternalBindings -> NormSurfaceExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply generator polySyms typeHeadIdentities typeBinderIdentities extBindings expr = do
  let identities = externalTermIdentities extBindings
  (resolvedExpr, generator2) <-
    first UnknownVariable (resolveTermReferences generator identities expr)
  generateConstraintsWithResolvedExternalBindingsAndTypeIdentitiesFromSupply
    generator2
    polySyms
    typeHeadIdentities
    typeBinderIdentities
    extBindings
    resolvedExpr

generateConstraintsWithResolvedExternalBindingsFromSupply :: IdentityGenerator -> PolySyms -> ExternalBindings -> ResolvedNormSurfaceExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsWithResolvedExternalBindingsFromSupply generator polySyms =
  generateConstraintsWithResolvedExternalBindingsAndTypeIdentitiesFromSupply generator polySyms Map.empty Map.empty

generateConstraintsWithResolvedExternalBindingsAndTypeIdentitiesFromSupply :: IdentityGenerator -> PolySyms -> Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> ExternalBindings -> ResolvedNormSurfaceExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsWithResolvedExternalBindingsAndTypeIdentitiesFromSupply generator polySyms typeHeadIdentities typeBinderIdentities extBindings expr =
  let (coreExpr, generator') = desugarResolvedSurface generator expr
   in generateResolvedConstraintsCoreWithExternalBindingsFromSupply generator' polySyms typeHeadIdentities typeBinderIdentities extBindings coreExpr

-- | Generate constraints from a normalized core expression.
--
-- This is primarily useful for regression tests that need to exercise
-- core-only forms (for example bare coercion constants) that are not
-- constructible through the surface parser/normalizer pipeline.
generateConstraintsCore :: PolySyms -> NormCoreExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsCore polySyms =
  generateConstraintsCoreWithEnv polySyms Map.empty

-- | Like 'generateConstraintsCore' but with an external environment.
generateConstraintsCoreWithEnv :: PolySyms -> ExternalEnv -> NormCoreExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsCoreWithEnv polySyms extEnv expr =
  let (extBindings, generator) = externalBindingsFromEnv initialIdentityGenerator extEnv
   in generateConstraintsCoreWithExternalBindingsFromSupply generator polySyms extBindings expr

generateConstraintsCoreWithExternalBindings :: PolySyms -> ExternalBindings -> NormCoreExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsCoreWithExternalBindings =
  generateConstraintsCoreWithExternalBindingsFromSupply initialIdentityGenerator

generateResolvedConstraintsCore :: PolySyms -> ResolvedNormCoreExpr -> Either ConstraintError (ConstraintResult p)
generateResolvedConstraintsCore polySyms =
  generateResolvedConstraintsCoreWithExternalBindings polySyms Map.empty

generateResolvedConstraintsCoreWithExternalBindings :: PolySyms -> ExternalBindings -> ResolvedNormCoreExpr -> Either ConstraintError (ConstraintResult p)
generateResolvedConstraintsCoreWithExternalBindings polySyms =
  generateResolvedConstraintsCoreWithExternalBindingsFromSupply initialIdentityGenerator polySyms Map.empty Map.empty

generateConstraintsCoreWithExternalBindingsFromSupply :: IdentityGenerator -> PolySyms -> ExternalBindings -> NormCoreExpr -> Either ConstraintError (ConstraintResult p)
generateConstraintsCoreWithExternalBindingsFromSupply generator polySyms extBindings expr = do
  (resolvedExpr, generator2) <-
    first UnknownVariable
      (resolveTermReferences generator (externalTermIdentities extBindings) expr)
  generateResolvedConstraintsCoreWithExternalBindingsFromSupply
    generator2
    polySyms
    Map.empty
    Map.empty
    extBindings
    resolvedExpr

generateResolvedConstraintsCoreWithExternalBindingsFromSupply :: IdentityGenerator -> PolySyms -> Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> ExternalBindings -> ResolvedNormCoreExpr -> Either ConstraintError (ConstraintResult p)
generateResolvedConstraintsCoreWithExternalBindingsFromSupply generator polySyms typeHeadIdentities typeBinderIdentities extBindings expr = do
  let initialState =
        (mkInitialStateWithPolySyms polySyms)
          { bsTypeHeadIdentities = constraintTypeHeadIdentities typeHeadIdentities polySyms extBindings
          , bsTypeBinderIdentities = typeBinderIdentities
          }
  ((_rootGen, initialEnv, rootNode, annRoot), finalState) <-
    runConstraintM (buildRootExprWithExternalBindings extBindings expr) initialState
  constraintResultFromState generator initialEnv rootNode annRoot finalState

generateModuleConstraintsWithExternalBindings :: PolySyms -> ExternalBindings -> [(VarName, NormSurfaceExpr)] -> Either ConstraintError (ModuleConstraintResult VarName p)
generateModuleConstraintsWithExternalBindings polySyms extBindings namedExprs =
  generateModuleConstraintsKeyedWithExternalBindings
    polySyms
    extBindings
    [(name, name, expr) | (name, expr) <- namedExprs]

generateModuleConstraintsKeyedWithExternalBindings :: (Ord key) => PolySyms -> ExternalBindings -> [(key, VarName, NormSurfaceExpr)] -> Either ConstraintError (ModuleConstraintResult key p)
generateModuleConstraintsKeyedWithExternalBindings =
  generateModuleConstraintsKeyedWithExternalBindingsFromSupply initialIdentityGenerator

generateModuleConstraintsKeyedWithExternalBindingsFromSupply :: (Ord key) => IdentityGenerator -> PolySyms -> ExternalBindings -> [(key, VarName, NormSurfaceExpr)] -> Either ConstraintError (ModuleConstraintResult key p)
generateModuleConstraintsKeyedWithExternalBindingsFromSupply generator polySyms =
  generateModuleConstraintsKeyedWithExternalBindingsAndTypeIdentitiesFromSupply generator polySyms Map.empty Map.empty Map.empty

generateModuleConstraintsKeyedWithExternalBindingsAndTypeIdentitiesFromSupply :: (Ord key) => IdentityGenerator -> PolySyms -> Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> Map.Map key (Map.Map String TypeBinderIdentity) -> ExternalBindings -> [(key, VarName, NormSurfaceExpr)] -> Either ConstraintError (ModuleConstraintResult key p)
generateModuleConstraintsKeyedWithExternalBindingsAndTypeIdentitiesFromSupply generator polySyms typeHeadIdentities typeBinderIdentities rootTypeBinderIdentities extBindings keyedExprs = do
  (resolvedExprs, generator2) <-
    resolveKeyedSurfaceExprs
      generator
      (externalTermIdentities extBindings)
      (orderKeyedExprs keyedExprs)
  generateModuleConstraintsKeyedWithResolvedExternalBindingsAndTypeIdentitiesFromSupply
    generator2
    polySyms
    typeHeadIdentities
    typeBinderIdentities
    rootTypeBinderIdentities
    extBindings
    resolvedExprs

generateModuleConstraintsKeyedWithResolvedExternalBindingsFromSupply :: (Ord key) => IdentityGenerator -> PolySyms -> ExternalBindings -> [(key, VarName, ResolvedNormSurfaceExpr)] -> Either ConstraintError (ModuleConstraintResult key p)
generateModuleConstraintsKeyedWithResolvedExternalBindingsFromSupply generator polySyms =
  generateModuleConstraintsKeyedWithResolvedExternalBindingsAndTypeIdentitiesFromSupply generator polySyms Map.empty Map.empty Map.empty

generateModuleConstraintsKeyedWithResolvedExternalBindingsAndTypeIdentitiesFromSupply :: (Ord key) => IdentityGenerator -> PolySyms -> Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> Map.Map key (Map.Map String TypeBinderIdentity) -> ExternalBindings -> [(key, VarName, ResolvedNormSurfaceExpr)] -> Either ConstraintError (ModuleConstraintResult key p)
generateModuleConstraintsKeyedWithResolvedExternalBindingsAndTypeIdentitiesFromSupply generator polySyms typeHeadIdentities typeBinderIdentities rootTypeBinderIdentities extBindings keyedExprs = do
  let (namedCoreExprs, generator') =
        desugarKeyedSurfaceExprs generator (orderKeyedExprs keyedExprs)
      initialState =
        (mkInitialStateWithPolySyms polySyms)
          { bsTypeHeadIdentities = constraintTypeHeadIdentities typeHeadIdentities polySyms extBindings
          , bsTypeBinderIdentities = typeBinderIdentities
          }
  ((_rootGen, initialEnv, roots), finalState) <-
    runConstraintM (buildModuleRootExprsKeyedWithExternalBindings rootTypeBinderIdentities extBindings namedCoreExprs) initialState
  constraintModuleResultFromState generator' initialEnv roots finalState

-- | A keyed module is a finite map of independent roots, not a sequence whose
-- input order may assign semantic identities.  Allocate syntax and graph
-- identities in key order so the same root keeps the same construction packet
-- when callers enumerate that map differently.
orderKeyedExprs :: (Ord key) => [(key, name, expr)] -> [(key, name, expr)]
orderKeyedExprs = sortOn (\(key, _, _) -> key)

externalBindingsFromEnv :: IdentityGenerator -> ExternalEnv -> (ExternalBindings, IdentityGenerator)
externalBindingsFromEnv generator bindings =
  let (generator', bindings') = Map.mapAccumWithKey resolveOne generator bindings
   in (bindings', generator')
  where
    resolveOne generator0 name srcTy =
      let (ref, generator1) = freshEnvRef name generator0
       in ( generator1,
            ExternalBinding
              { externalBindingType = srcTy,
                externalBindingMode = ExternalBindingScheme,
                externalBindingIdentity =
                  externalBindingIdentityFromDetails (EnvId ref),
                externalBindingTypeHeadIdentities = Map.empty,
                externalBindingTypeBinderIdentities = Map.empty
              }
          )

externalTermIdentities :: ExternalBindings -> Map.Map VarName IdDetails
externalTermIdentities =
  Map.map (externalBindingDetails . externalBindingIdentity)

resolveKeyedSurfaceExprs :: IdentityGenerator -> Map.Map VarName IdDetails -> [(key, VarName, NormSurfaceExpr)] -> Either ConstraintError ([(key, VarName, ResolvedNormSurfaceExpr)], IdentityGenerator)
resolveKeyedSurfaceExprs generator _ [] = Right ([], generator)
resolveKeyedSurfaceExprs generator identities ((key, name, expr) : rest) = do
  (resolvedExpr, generator1) <-
    first UnknownVariable (resolveTermReferences generator identities expr)
  (resolvedRest, generator2) <-
    resolveKeyedSurfaceExprs generator1 identities rest
  Right ((key, name, resolvedExpr) : resolvedRest, generator2)

desugarKeyedSurfaceExprs :: IdentityGenerator -> [(key, VarName, ResolvedNormSurfaceExpr)] -> ([(key, VarName, ResolvedNormCoreExpr)], IdentityGenerator)
desugarKeyedSurfaceExprs generator [] = ([], generator)
desugarKeyedSurfaceExprs generator ((key, name, expr) : rest) =
  let (coreExpr, generator1) = desugarResolvedSurface generator expr
      (coreRest, generator2) = desugarKeyedSurfaceExprs generator1 rest
   in ((key, name, coreExpr) : coreRest, generator2)

constraintTypeHeadIdentities :: Map.Map String SymbolIdentity -> PolySyms -> ExternalBindings -> Map.Map String SymbolIdentity
constraintTypeHeadIdentities supplied polySyms extBindings =
  supplied
    `Map.union` symbolIdentityAliasMapWith
      (suppliedEntries ++ builtinEntries ++ polymorphicEntries ++ externalEntries)
  where
    suppliedEntries =
      [(identity, [name]) | (name, identity) <- Map.toList supplied]

    builtinEntries =
      [ (PrimitiveIdentity.builtinTypeIdentity name, [name])
      | name <- Set.toList PrimitiveIdentity.builtinTypeNames
      ]

    polymorphicEntries =
      [(identity, []) | identity <- Set.toList polySyms]

    externalEntries =
      [ (identity, [name])
      | binding <- Map.elems extBindings,
        (name, identity) <- Map.toList (externalBindingTypeHeadIdentities binding)
      ]

constraintResultFromState :: IdentityGenerator -> Env -> NodeId -> AnnExpr -> BuildState -> Either ConstraintError (ConstraintResult p)
constraintResultFromState identityGenerator initialEnv rootNode annRoot finalState = do
  let annEdges = collectAnnEdges annRoot
      constraint = (buildConstraint finalState) {cAnnEdges = annEdges}
  pure
    ConstraintResult { crConstraint = constraint,
        crRoot = rootNode,
        crAnnotated = annRoot,
        crIdentityGenerator = identityGenerator,
        crAnnSourceTypes = bsAnnSourceTypes finalState,
        crExactProducerTypes = bsExactProducerTypes finalState,
        crSourceTypeBinderIdentities = bsTypeBinderNodeIdentities finalState,
        crSourceTypeBinderAliases = bsTypeBinderIdentities finalState,
        crInitialEnv = initialEnv
      }

constraintModuleResultFromState :: IdentityGenerator -> Env -> Map.Map key (ModuleRootId, NodeId, AnnExpr, Map.Map String TypeBinderIdentity) -> BuildState -> Either ConstraintError (ModuleConstraintResult key p)
constraintModuleResultFromState identityGenerator initialEnv roots finalState = do
  let annEdges = IntSet.unions [collectAnnEdges annRoot | (_, _rootNode, annRoot, _) <- Map.elems roots]
      constraint = (buildConstraint finalState) {cAnnEdges = annEdges}
      rootMap =
        Map.map
          ( \(rootId, rootNode, annRoot, sourceTypeBinderAliases) ->
              ModuleConstraintRoot
                { mcrRootId = rootId,
                  mcrRoot = rootNode,
                  mcrAnnotated = annRoot,
                  mcrSourceTypeBinderAliases = sourceTypeBinderAliases
                }
          )
          roots
  pure
    ModuleConstraintResult
      { mcrConstraint = constraint,
        mcrRoots = rootMap,
        mcrIdentityGenerator = identityGenerator,
        mcrAnnSourceTypes = bsAnnSourceTypes finalState,
        mcrExactProducerTypes = bsExactProducerTypes finalState,
        mcrSourceTypeBinderIdentities = bsTypeBinderNodeIdentities finalState,
        mcrInitialEnv = initialEnv,
        mcrRootOwnership = bsRootOwnership finalState
      }

collectAnnEdges :: AnnExpr -> IntSet.IntSet
collectAnnEdges = cata alg
  where
    alg expr = case expr of
      AResolvedVarF _ _ _ -> IntSet.empty
      ALitF _ _ -> IntSet.empty
      -- Lambda-body instantiation is an ordinary Figure 15.3.5 edge, not a
      -- source-annotation kappa-sigma edge.
      ALamF _ _ _ _ body _bodyEid _ -> body
      AAppF fun arg _ _ _ -> IntSet.union fun arg
      ALetF _ _ _ _ _ _ rhs body _ -> IntSet.union rhs body
      -- Compiler-owned exact checks are ordinary witnessed instantiation
      -- edges over one authoritative target.  Only source kappa-sigma edges
      -- use the annotation-wrapper ownership rules carried by 'cAnnEdges'.
      AExactAnnF inner _ _ _ -> inner
      AAnnF inner _ eid -> IntSet.insert (getEdgeId eid) inner
      ALetScopeF inner _ _ -> inner
      AUnfoldF inner _ eid -> IntSet.insert (getEdgeId eid) inner
