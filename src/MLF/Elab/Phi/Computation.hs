{- |
Module      : MLF.Elab.Phi.Computation
Description : Paper-shaped, validated Phi computation components
Copyright   : (c) 2024
License     : BSD-3-Clause

The thesis separates the quantifier reordering computation @phi_R@ from the
edge-local computation @T(e)@.  An occurrence consumes their composition
@phi_R;T(e)@.  These opaque types make that distinction representable before
the existing Phi producers and consumers are migrated to it.

Each component is checked against its declared endpoints.  Composition also
requires the declared intermediate endpoints to be equal by 'ElabType's
identity-bearing equality.  In particular, alpha-equivalent types whose
binder identities differ cannot be silently joined at this seam; binder
display names remain irrelevant because 'TypeBinderRef' equality is based on
identity.
-}
module MLF.Elab.Phi.Computation
  ( QuantifierReordering,
    EdgeTranslation,
    OccurrenceComputation,
    ComputationRole (..),
    PhiComputationError (..),
    mkQuantifierReordering,
    mkEdgeTranslation,
    composeOccurrenceComputation,
    quantifierReorderingSource,
    quantifierReorderingInstantiation,
    quantifierReorderingTarget,
    edgeTranslationSource,
    edgeTranslationInstantiation,
    edgeTranslationTarget,
    occurrenceComputationReordering,
    occurrenceComputationEdgeTranslation,
    occurrenceComputationSource,
    occurrenceComputationInstantiation,
    occurrenceComputationTarget,
  )
where

import Data.Bifunctor (first)

import MLF.Elab.Inst (applyInstantiation, composeInst)
import MLF.Elab.Types (ElabError, ElabType, Instantiation)
import MLF.Reify.TypeOps (alphaEqType)

-- | The part of the paper-shaped occurrence computation being validated.
data ComputationRole
  = QuantifierReorderingRole
  | EdgeTranslationRole
  | OccurrenceComputationRole
  deriving (Eq, Show)

-- | Construction failures for validated Phi computations.
data PhiComputationError
  = PhiComputationApplicationFailed
      ComputationRole
      ElabType
      Instantiation
      ElabError
  | PhiComputationEndpointMismatch
      ComputationRole
      ElabType
      ElabType
  | PhiComputationSeamMismatch
      ElabType
      ElabType
  deriving (Eq, Show)

-- | A validated source, computation, and destination triple.
data ValidatedComputation = ValidatedComputation
  { validatedSource :: !ElabType,
    validatedInstantiation :: !Instantiation,
    validatedTarget :: !ElabType
  }

-- | The thesis's quantifier reordering @phi_R : Typ(a') -> Typexp(a')@.
newtype QuantifierReordering =
  QuantifierReordering ValidatedComputation

-- | The thesis's edge-local translation @T(e) : Typexp(a') -> Type(a)@.
newtype EdgeTranslation =
  EdgeTranslation ValidatedComputation

-- | The complete occurrence computation @phi_R;T(e)@.
data OccurrenceComputation = OccurrenceComputation
  { occurrenceReordering :: !QuantifierReordering,
    occurrenceEdgeTranslation :: !EdgeTranslation,
    occurrenceValidated :: !ValidatedComputation
  }

-- | Validate and construct a quantifier-reordering component.
mkQuantifierReordering
  :: ElabType
  -> Instantiation
  -> ElabType
  -> Either PhiComputationError QuantifierReordering
mkQuantifierReordering source inst target =
  QuantifierReordering
    <$> validateComputation QuantifierReorderingRole source inst target

-- | Validate and construct an edge-local translation component.
mkEdgeTranslation
  :: ElabType
  -> Instantiation
  -> ElabType
  -> Either PhiComputationError EdgeTranslation
mkEdgeTranslation source inst target =
  EdgeTranslation
    <$> validateComputation EdgeTranslationRole source inst target

-- | Compose @phi_R@ and @T(e)@ at one strict identity-bearing seam.
composeOccurrenceComputation
  :: QuantifierReordering
  -> EdgeTranslation
  -> Either PhiComputationError OccurrenceComputation
composeOccurrenceComputation reordering edgeTranslation
  | quantifierReorderingTarget reordering
      /= edgeTranslationSource edgeTranslation =
      Left
        ( PhiComputationSeamMismatch
            (quantifierReorderingTarget reordering)
            (edgeTranslationSource edgeTranslation)
        )
  | otherwise = do
      let source = quantifierReorderingSource reordering
          inst =
            composeInst
              (quantifierReorderingInstantiation reordering)
              (edgeTranslationInstantiation edgeTranslation)
          target = edgeTranslationTarget edgeTranslation
      validated <-
        validateComputation OccurrenceComputationRole source inst target
      pure
        OccurrenceComputation
          { occurrenceReordering = reordering,
            occurrenceEdgeTranslation = edgeTranslation,
            occurrenceValidated = validated
          }

quantifierReorderingSource :: QuantifierReordering -> ElabType
quantifierReorderingSource (QuantifierReordering computation) =
  validatedSource computation

quantifierReorderingInstantiation :: QuantifierReordering -> Instantiation
quantifierReorderingInstantiation (QuantifierReordering computation) =
  validatedInstantiation computation

quantifierReorderingTarget :: QuantifierReordering -> ElabType
quantifierReorderingTarget (QuantifierReordering computation) =
  validatedTarget computation

edgeTranslationSource :: EdgeTranslation -> ElabType
edgeTranslationSource (EdgeTranslation computation) =
  validatedSource computation

edgeTranslationInstantiation :: EdgeTranslation -> Instantiation
edgeTranslationInstantiation (EdgeTranslation computation) =
  validatedInstantiation computation

edgeTranslationTarget :: EdgeTranslation -> ElabType
edgeTranslationTarget (EdgeTranslation computation) =
  validatedTarget computation

occurrenceComputationReordering
  :: OccurrenceComputation
  -> QuantifierReordering
occurrenceComputationReordering = occurrenceReordering

occurrenceComputationEdgeTranslation
  :: OccurrenceComputation
  -> EdgeTranslation
occurrenceComputationEdgeTranslation = occurrenceEdgeTranslation

occurrenceComputationSource :: OccurrenceComputation -> ElabType
occurrenceComputationSource = validatedSource . occurrenceValidated

occurrenceComputationInstantiation
  :: OccurrenceComputation
  -> Instantiation
occurrenceComputationInstantiation =
  validatedInstantiation . occurrenceValidated

occurrenceComputationTarget :: OccurrenceComputation -> ElabType
occurrenceComputationTarget = validatedTarget . occurrenceValidated

validateComputation
  :: ComputationRole
  -> ElabType
  -> Instantiation
  -> ElabType
  -> Either PhiComputationError ValidatedComputation
validateComputation role source inst target = do
  applied <-
    first
      (PhiComputationApplicationFailed role source inst)
      (applyInstantiation source inst)
  if alphaEqType applied target
    then
      Right
        ValidatedComputation
          { validatedSource = source,
            validatedInstantiation = inst,
            validatedTarget = target
          }
    else
      Left (PhiComputationEndpointMismatch role applied target)
