{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Finalize
  ( finalizeBinding,
  )
where

import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified MLF.Constraint.Types.Graph as Graph
import MLF.Elab.Pipeline (ExternalEnv, renderPipelineError, runPipelineElabWithEnv)
import MLF.Elab.Types (ElabTerm, ElabType)
import qualified MLF.Elab.Types as X
import MLF.Frontend.Normalize (normalizeExpr, normalizeType)
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeRuntimeTypes,
    lowerType,
  )
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    DataInfo (..),
    LoweredBinding (..),
    ProgramError (..),
  )
import MLF.Frontend.Syntax (Expr (..), SrcBound (..), SrcTy (..), SrcType, SurfaceExpr)
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarsType)

finalizeBinding :: ElaborateScope -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBinding scope lowered = do
  (term, actualTy) <- runSurfacePipeline scope (loweredBindingSurfaceExpr lowered)
  let actualTyForCompare = stripVacuousForalls actualTy
      expectedTyForCompare = stripVacuousForalls (srcTypeToElabType (loweredBindingExpectedType lowered))
      recoveredActualSrcTy = recoverSourceType scope (elabTypeToSrcType actualTyForCompare)
      recoveredActualTy = srcTypeToElabType (lowerType scope recoveredActualSrcTy)
  if alphaEqType actualTyForCompare expectedTyForCompare
    || churchAwareEqType actualTyForCompare expectedTyForCompare
    || alphaEqType recoveredActualTy expectedTyForCompare
    || churchAwareEqType recoveredActualTy expectedTyForCompare
    then
      Right
        CheckedBinding
          { checkedBindingName = loweredBindingName lowered,
            checkedBindingSourceType = loweredBindingExpectedType lowered,
            checkedBindingSurfaceExpr = loweredBindingSurfaceExpr lowered,
            checkedBindingTerm = term,
            checkedBindingType = actualTy,
            checkedBindingExportedAsMain = loweredBindingExportedAsMain lowered
          }
    else Left (ProgramTypeMismatch recoveredActualSrcTy (loweredBindingExpectedType lowered))

runSurfacePipeline :: ElaborateScope -> SurfaceExpr -> Either ProgramError (ElabTerm, ElabType)
runSurfacePipeline scope surfaceExpr = do
  let freeVars = sort (Set.toList (surfaceFreeVars surfaceExpr))
  envBindings <- traverse resolveRuntimeType freeVars
  let extEnv :: ExternalEnv
      extEnv =
        Map.fromList
          [ (name, normTy)
            | (name, ty) <- envBindings,
              let normTy = either (error . show) id (normalizeType ty)
          ]
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  either (Left . ProgramPipelineError . renderPipelineError) Right (runPipelineElabWithEnv Set.empty extEnv normExpr)
  where
    resolveRuntimeType name =
      case Map.lookup name (elaborateScopeRuntimeTypes scope) of
        Just ty -> Right (name, ty)
        Nothing -> Left (ProgramUnknownValue name)

{- Note [recoverSourceType]

When the eMLF pipeline infers a type, it returns raw Church-encoded μ forms
with fresh binder names.  The .mlfp layer still needs named source ADT heads
for diagnostics and instance-head comparisons.  This recovery is deliberately
downstream of lowering: `Program.Elaborate` never invokes the pipeline.
-}
recoverSourceType :: ElaborateScope -> SrcType -> SrcType
recoverSourceType scope = recover
  where
    nullaryHeads :: [(ElabType, SrcType)]
    nullaryHeads =
      [ (srcTypeToElabType (lowerType scope (STBase name)), STBase name)
        | (name, info) <- Map.toList (elaborateScopeDataTypes scope),
          null (dataParams info)
      ]

    recover ty =
      case lookupHead ty of
        Just headTy -> headTy
        Nothing -> recoverChildren ty

    lookupHead ty =
      let elabTy = srcTypeToElabType ty
       in case find (\(lowered, _) -> alphaEqType elabTy lowered || churchAwareEqType elabTy lowered) nullaryHeads of
            Just (_, headTy) -> Just headTy
            Nothing -> Nothing

    recoverChildren ty = case ty of
      STVar {} -> ty
      STBase {} -> ty
      STBottom -> ty
      STArrow dom cod -> STArrow (recover dom) (recover cod)
      STForall name mb body ->
        STForall name (fmap (SrcBound . recover . unSrcBound) mb) (recover body)
      STMu name body -> STMu name (recover body)
      STCon name args -> STCon name (fmap recover args)

stripVacuousForalls :: ElabType -> ElabType
stripVacuousForalls (X.TForall v _ body)
  | v `Set.notMember` freeTypeVarsType body = stripVacuousForalls body
stripVacuousForalls (X.TForall v mb body) =
  X.TForall v mb (stripVacuousForalls body)
stripVacuousForalls (X.TArrow dom cod) =
  X.TArrow (stripVacuousForalls dom) (stripVacuousForalls cod)
stripVacuousForalls (X.TMu name body) =
  X.TMu name (stripVacuousForalls body)
stripVacuousForalls ty = ty

surfaceFreeVars :: SurfaceExpr -> Set String
surfaceFreeVars = go Set.empty
  where
    go bound expr = case expr of
      EVar name
        | name `Set.member` bound -> Set.empty
        | otherwise -> Set.singleton name
      ELit _ -> Set.empty
      ELam name body -> go (Set.insert name bound) body
      ELamAnn name _ body -> go (Set.insert name bound) body
      EApp fun arg -> go bound fun `Set.union` go bound arg
      ELet name rhs body -> go (Set.insert name bound) rhs `Set.union` go (Set.insert name bound) body
      EAnn inner _ -> go bound inner
      ECoerceConst _ -> Set.empty

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

elabTypeToSrcType :: X.Ty v -> SrcType
elabTypeToSrcType ty = case ty of
  X.TVar name -> STVar name
  X.TArrow dom cod -> STArrow (elabTypeToSrcType dom) (elabTypeToSrcType cod)
  X.TBase (Graph.BaseTy name) -> STBase name
  X.TCon (Graph.BaseTy name) args ->
    case toListNE (fmap elabTypeToSrcType args) of
      x : xs -> STCon name (x :| xs)
      [] -> STBase name
  X.TForall name mb body ->
    STForall name (fmap (SrcBound . elabTypeToSrcType) mb) (elabTypeToSrcType body)
  X.TMu name body -> STMu name (elabTypeToSrcType body)
  X.TBottom -> STBottom

srcTypeToElabType :: SrcTy n v -> ElabType
srcTypeToElabType ty = case ty of
  STVar name -> X.TVar name
  STArrow dom cod -> X.TArrow (srcTypeToElabType dom) (srcTypeToElabType cod)
  STBase name -> X.TBase (Graph.BaseTy name)
  STCon name args -> X.TCon (Graph.BaseTy name) (fmap srcTypeToElabType args)
  STForall name mb body -> X.TForall name (mb >>= srcBoundToElabBound) (srcTypeToElabType body)
  STMu name body -> X.TMu name (srcTypeToElabType body)
  STBottom -> X.TBottom

srcBoundToElabBound :: SrcBound n -> Maybe X.BoundType
srcBoundToElabBound (SrcBound boundTy) =
  case srcTypeToElabType boundTy of
    X.TVar {} -> Nothing
    X.TBottom -> Nothing
    X.TArrow dom cod -> Just (X.TArrow dom cod)
    X.TBase base -> Just (X.TBase base)
    X.TCon con args -> Just (X.TCon con args)
    X.TForall name mb body -> Just (X.TForall name mb body)
    X.TMu name body -> Just (X.TMu name body)
