{-# LANGUAGE DataKinds #-}
{- |
Module      : MLF.Pipeline
Description : Focused normalized-pipeline API for downstream callers

`MLF.Pipeline` exposes the normalized eMLF pipeline surface plus `.mlfp`
checking/runtime on the shared eMLF/xMLF path, without the wider raw-syntax,
parser, or pretty-printing conveniences re-exported by `MLF.API`.

Prefer this module for consumers that already operate on normalized surface
terms and want the smallest supported entrypoint for inference/elaboration.
-}
module MLF.Pipeline
    ( -- * Staged frontend types
      NormSurfaceExpr
    , NormSrcType
    , StructBound
    , NormalizationError (..)
    , normalizeExpr
    , normalizeType
    -- * Constraint generation
    , ConstraintResult (..)
    , ConstraintError (..)
    , BaseTy (..)
    , PolySyms
    , inferConstraintGraph
    -- * Elaboration/runtime types
    , ElabType
    , Ty
        ( TVarRef
        , TArrow
        , TConWithIdentity
        , TVarAppRef
        , TBaseWithIdentity
        , TForallRef
        , TMuRef
        , TBottom
        )
    , TypeBinderRef
    , UniqueIdentity (..)
    , TypeBinderIdentity
    , typeBinderIdentityFromUnique
    , typeBinderRefFromIdentity
    , typeBinderRefIdentity
    , typeBinderRefName
    , typeBinderRefsSameIdentity
    , tCon
    , tBase
    , ElabScheme
    , mkElabSchemeWithRefs
    , schemeBinderRefs
    , schemeBody
    , XmlfTerm
    , Instantiation
        ( InstId
        , InstApp
        , InstBot
        , InstIntro
        , InstElim
        , InstInside
        , InstSeq
        , InstAbstrRef
        , InstUnderRef
        )
    , ElabError (..)
    , TypeCheckError
    , Pretty (..)
    , schemeFromType
    -- * Pipeline configuration
    , PipelineConfig(..)
    , defaultPipelineConfig
    , TraceConfig(..)
    , defaultTraceConfig
    -- * Pipeline entrypoints (normalized-only)
    , PipelineError(..)
    , CycleError(..)
    , renderPipelineError
    , formatPipelineError
    , pipelineErrorPhase
    , pipelineErrorPhaseName
    , runPipelineElab
    , runPipelineElabWithConfig
    -- * Phase 7 helpers
    , typeCheck
    , step
    , normalize
    , isValue
    -- * Unified `.mlfp` program checking/runtime
    , ProgramError(..)
    , ProgramDiagnostic(..)
    , CheckedProgram(..)
    , checkedProgramMain
    , CheckedModule(..)
    , CheckedBinding(..)
    , SymbolNamespace(..)
    , SymbolOwnerIdentity(..)
    , SymbolIdentity
    , symbolIdentityFromParts
    , symbolIdentityWithUnique
    , symbolUniqueIdentity
    , symbolNamespace
    , symbolDefiningModule
    , symbolDefiningName
    , symbolOwnerIdentity
    , SymbolOrigin(..)
    , SymbolSpelling(..)
    , ResolvedSymbol
    , resolvedSymbolIdentity
    , resolvedSymbolSpelling
    , mapResolvedSymbolIdentity
    , mkResolvedSymbol
    , ResolvedReferenceKind(..)
    , ResolvedReference
    , resolvedReferenceKind
    , resolvedReferenceName
    , resolvedReferenceSymbol
    , mkResolvedReference
    , ResolvedScope(..)
    , ResolvedModule(..)
    , ResolvedProgram(..)
    , sameResolvedSymbol
    , Value(..)
    , ProgramRunResult(..)
    , PackageId(..)
    , PackageRoot(..)
    , PackageSearchPath(..)
    , ProgramPackageDiscoveryError(..)
    , ProgramSourceUnit(..)
    , LocatedProgramSourceUnit(..)
    , ProgramPackage(..)
    , LocatedProgramPackage(..)
    , discoverLocatedProgramPackage
    , discoverLocatedProgramPackageFromSearchPath
    , trivialProgramPackage
    , trivialLocatedProgramPackage
    , checkProgram
    , checkProgramPackage
    , checkLocatedProgram
    , checkLocatedProgramPackage
    , runProgram
    , runProgramPackage
    , runLocatedProgram
    , runLocatedProgramPackage
    , runProgramOutput
    , runProgramPackageOutput
    , runLocatedProgramOutput
    , runLocatedProgramPackageOutput
    , programRunOutput
    , renderProgramDiagnostic
    , prettyValue
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import MLF.Constraint.Acyclicity (CycleError(..))
import MLF.Frontend.Syntax (NormSurfaceExpr, NormSrcType, StructBound)
import MLF.Frontend.Normalize (NormalizationError(..), normalizeExpr, normalizeType)
import MLF.Frontend.ConstraintGen (ConstraintError(..), ConstraintResult(..), generateConstraints)
import MLF.Frontend.Program.Types
    ( CheckedBinding(..)
    , CheckedModule(..)
    , CheckedProgram(..)
    , checkedProgramMain
    , ResolvedModule(..)
    , ResolvedProgram(..)
    , ResolvedReference
    , ResolvedReferenceKind(..)
    , ResolvedScope(..)
    , ResolvedSymbol
    , mapResolvedSymbolIdentity
    , mkResolvedReference
    , mkResolvedSymbol
    , resolvedReferenceKind
    , resolvedReferenceName
    , resolvedReferenceSymbol
    , resolvedSymbolIdentity
    , resolvedSymbolSpelling
    , sameResolvedSymbol
    , SymbolIdentity
    , symbolIdentityFromParts
    , symbolIdentityWithUnique
    , symbolUniqueIdentity
    , symbolNamespace
    , symbolDefiningModule
    , symbolDefiningName
    , symbolOwnerIdentity
    , SymbolNamespace(..)
    , SymbolOrigin(..)
    , SymbolOwnerIdentity(..)
    , SymbolSpelling(..)
    , ProgramDiagnostic(..)
    , ProgramError(..)
    , renderProgramDiagnostic
    )
import MLF.Frontend.Program.Check
    ( checkLocatedProgram
    , checkLocatedProgramPackage
    , checkProgram
    , checkProgramPackage
    )
import MLF.Frontend.Program.Package
    ( LocatedProgramPackage(..)
    , LocatedProgramSourceUnit(..)
    , PackageId(..)
    , PackageRoot(..)
    , PackageSearchPath(..)
    , ProgramPackage(..)
    , ProgramPackageDiscoveryError(..)
    , ProgramSourceUnit(..)
    , discoverLocatedProgramPackage
    , discoverLocatedProgramPackageFromSearchPath
    , trivialLocatedProgramPackage
    , trivialProgramPackage
    )
import MLF.Frontend.Program.Run
    ( ProgramRunResult(..)
    , Value(..)
    , prettyValue
    , programRunOutput
    , runLocatedProgram
    , runLocatedProgramPackage
    , runLocatedProgramOutput
    , runLocatedProgramPackageOutput
    , runProgram
    , runProgramPackage
    , runProgramOutput
    , runProgramPackageOutput
    )
import MLF.Constraint.Types.Graph (BaseTy(..), PolySyms)
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Elab.Pipeline
    ( ElabError (..)
    , ElabScheme
    , XmlfTerm
    , ElabType
    , Ty
        ( TVarRef
        , TArrow
        , TConWithIdentity
        , TVarAppRef
        , TBaseWithIdentity
        , TForallRef
        , TMuRef
        , TBottom
        )
    , TypeBinderRef
    , UniqueIdentity (..)
    , TypeBinderIdentity
    , typeBinderIdentityFromUnique
    , typeBinderRefFromIdentity
    , typeBinderRefIdentity
    , typeBinderRefName
    , typeBinderRefsSameIdentity
    , tCon
    , tBase
    , mkElabSchemeWithRefs
    , schemeBinderRefs
    , schemeBody
    , Instantiation
        ( InstId
        , InstApp
        , InstBot
        , InstIntro
        , InstElim
        , InstInside
        , InstSeq
        , InstAbstrRef
        , InstUnderRef
        )
    , TypeCheckError
    , PipelineConfig(..)
    , PipelineError(..)
    , Pretty (..)
    , defaultPipelineConfig
    , TraceConfig(..)
    , defaultTraceConfig
    , isValue
    , normalize
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabWithConfig
    , schemeFromType
    , step
    , typeCheck
    )

inferConstraintGraph :: PolySyms -> NormSurfaceExpr -> Either ConstraintError (ConstraintResult 'Raw)
inferConstraintGraph = generateConstraints

-- | Extract the numeric pipeline phase where the error occurred.
--
-- Phase mapping:
--
--   * 1 \u2014 Constraint generation
--   * 3 \u2014 Acyclicity check
--   * 4 \u2014 Presolution
--   * 5 \u2014 Solve (unification)
--   * 6 \u2014 Elaboration
--   * 7 \u2014 Type checking
pipelineErrorPhase :: PipelineError -> Int
pipelineErrorPhase err = case err of
    PipelineConstraintError {}  -> 1
    PipelineCycleError {}       -> 3
    PipelinePresolutionError {} -> 4
    PipelineSolveError {}       -> 5
    PipelineElabError {}        -> 6
    PipelineTypeCheckError {}   -> 7

-- | Human-readable name of the pipeline phase where the error occurred.
pipelineErrorPhaseName :: PipelineError -> String
pipelineErrorPhaseName err = case err of
    PipelineConstraintError {}  -> "constraint generation"
    PipelineCycleError {}       -> "acyclicity check"
    PipelinePresolutionError {} -> "presolution"
    PipelineSolveError {}       -> "solve"
    PipelineElabError {}        -> "elaboration"
    PipelineTypeCheckError {}   -> "type checking"

-- | Structured, multi-line 'Text' rendering of a 'PipelineError'.
--
-- Returns output in the format:
--
-- @
-- [Phase N] phase-name error:
--   \<detail from Show instance\>
-- @
--
-- Use 'renderPipelineError' for a single-line 'String' alternative.
formatPipelineError :: PipelineError -> Text
formatPipelineError err =
    T.pack $
        "[Phase "
            ++ show (pipelineErrorPhase err)
            ++ "] "
            ++ pipelineErrorPhaseName err
            ++ " error:\n  "
            ++ detail err
  where
    detail e = case e of
        PipelineConstraintError ce  -> show ce
        PipelineCycleError ce       -> show ce
        PipelinePresolutionError pe -> show pe
        PipelineSolveError se       -> show se
        PipelineElabError ee        -> show ee
        PipelineTypeCheckError te   -> show te
