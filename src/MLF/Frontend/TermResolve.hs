{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Frontend.TermResolve
  ( resolveTermReferences,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Frontend.Syntax
  ( Expr (..),
    TermReference (..),
    TermReferencePhase (..),
    VarName,
  )
import MLF.Types.Identity
  ( IdDetails (LocalId),
    IdentityGenerator,
    freshLocalRef,
  )

-- | Resolve a raw parser expression at the parser boundary. Every
-- lexical binder receives its identity before any semantic IR is built, and
-- every occurrence is rewritten to the identity selected by lexical scope.
resolveTermReferences ::
  IdentityGenerator ->
  Map VarName IdDetails ->
  Expr 'RawTermReferences stage ty ->
  Either VarName (Expr 'ResolvedTermReferences stage ty, IdentityGenerator)
resolveTermReferences generator externalBindings =
  go generator externalBindings
  where
    go :: IdentityGenerator -> Map VarName IdDetails -> Expr 'RawTermReferences s ty0 -> Either VarName (Expr 'ResolvedTermReferences s ty0, IdentityGenerator)
    go generator0 env expr =
      case expr of
        EVarNode (RawTermReference name) ->
          case Map.lookup name env of
            Just details ->
              Right
                ( EVarNode (ResolvedTermReference details name),
                  generator0
                )
            Nothing -> Left name
        ELit literal -> Right (ELit literal, generator0)
        ELamNode (RawTermReference name) body -> do
          let (ref, generator1) = freshLocalRef name generator0
              details = LocalId ref
          (body', generator2) <- go generator1 (Map.insert name details env) body
          Right
            ( ELamNode (ResolvedTermReference details name) body',
              generator2
            )
        EApp fun arg -> do
          (fun', generator1) <- go generator0 env fun
          (arg', generator2) <- go generator1 env arg
          Right (EApp fun' arg', generator2)
        ELetNode (RawTermReference name) rhs body -> do
          let (ref, generator1) = freshLocalRef name generator0
              details = LocalId ref
              bodyEnv = Map.insert name details env
              rhsEnv
                | Map.notMember name env && mentionsFree name rhs = bodyEnv
                | otherwise = env
          (rhs', generator2) <- go generator1 rhsEnv rhs
          (body', generator3) <- go generator2 bodyEnv body
          Right
            ( ELetNode (ResolvedTermReference details name) rhs' body',
              generator3
            )
        ELamAnnNode (RawTermReference name) ty body -> do
          let (ref, generator1) = freshLocalRef name generator0
              details = LocalId ref
          (body', generator2) <- go generator1 (Map.insert name details env) body
          Right
            ( ELamAnnNode (ResolvedTermReference details name) ty body',
              generator2
            )
        EAnn inner ty -> do
          (inner', generator1) <- go generator0 env inner
          Right (EAnn inner' ty, generator1)
        EExactAnn inner ty exactTy -> do
          (inner', generator1) <- go generator0 env inner
          Right (EExactAnn inner' ty exactTy, generator1)
        EExactLamNode (RawTermReference name) ty body -> do
          let (ref, generator1) = freshLocalRef name generator0
              details = LocalId ref
          (body', generator2) <- go generator1 (Map.insert name details env) body
          Right
            ( EExactLamNode (ResolvedTermReference details name) ty body',
              generator2
            )
        ECoerceConst ty -> Right (ECoerceConst ty, generator0)
        EExactCoerceConst ty exactTy -> Right (EExactCoerceConst ty exactTy, generator0)

    mentionsFree :: VarName -> Expr 'RawTermReferences s ty0 -> Bool
    mentionsFree needle =
      goFree Set.empty
      where
        goFree :: Set.Set VarName -> Expr 'RawTermReferences s ty0 -> Bool
        goFree bound expr =
          case expr of
            EVarNode (RawTermReference name) ->
              name == needle && Set.notMember name bound
            ELit {} -> False
            ELamNode (RawTermReference name) body ->
              goFree (Set.insert name bound) body
            EApp fun arg ->
              goFree bound fun || goFree bound arg
            ELetNode (RawTermReference name) rhs body ->
              goFree bound rhs || goFree (Set.insert name bound) body
            ELamAnnNode (RawTermReference name) _ body ->
              goFree (Set.insert name bound) body
            EAnn inner _ -> goFree bound inner
            EExactAnn inner _ _ -> goFree bound inner
            EExactLamNode (RawTermReference name) _ body ->
              goFree (Set.insert name bound) body
            ECoerceConst {} -> False
            EExactCoerceConst {} -> False
