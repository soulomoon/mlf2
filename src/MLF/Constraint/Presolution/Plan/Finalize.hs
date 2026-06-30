{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{- Note [Scheme finalization]

This module implements the final stage of generalization planning: converting
the intermediate generalization plan into a fully-formed ElabScheme suitable
for elaboration.

'finalizeScheme' takes a FinalizeInput (populated by the generalization
planning pipeline) and performs:

1. Alias inlining — variables that bind the type root are inlined into the
   scheme body so the scheme type reflects the actual structure.
2. Canonical variable normalization — 'canonAllVars' alpha-renames all type
   variables to a canonical "v0, v1, ..." form so structural comparisons
   are order-independent.
3. Bound-alias collapsing — when a binder's bound type is structurally
   identical to the scheme body, the binder is collapsed to avoid
   redundant quantification.
4. Scheme normalization — 'simplifySchemeBindingsRefs' and 'promoteArrowAliasRefs'
   strip trivial ∀-binders and promote arrow-shaped aliases.
5. Variable renaming — canonical variables are mapped to fresh alpha names
   (a, b, c, ...) for human-readable output.
6. Free-variable validation — any free type variables not bound by the
   scheme indicate a scoping bug; 'SchemeFreeVars' is raised in that case.

The function returns (ElabScheme, subst') where subst' maps node IDs to
their final scheme-level binder refs, used by downstream Φ reconstruction.

Related thesis sections:
  - Section 8.2 — Reification and scheme construction (Fig 8.2.2, 8.2.3)
  - Section 7.6.1 — Generalized types and quantification
-}
{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.Finalize
  ( FinalizeInput (..),
    finalizeScheme,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.BindingUtil (firstGenAncestorFrom)
import MLF.Constraint.Presolution.Plan.Context (GaBindParents (..), GeneralizeEnv (..), traceGeneralize)
import MLF.Constraint.Presolution.Plan.Normalize
  ( containsForall,
    isBaseBound,
    isVarBound,
    promoteArrowAliasRefs,
    simplifySchemeBindingsRefs,
  )
import MLF.Constraint.Types.Graph
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Types (mapBoundType)
import MLF.Reify.TypeOps
  ( freeTypeVarRefsFrom,
    freeTypeVarRefsType,
    splitForallsRefs,
    stripForallsType,
    substTypeSimpleRef,
  )
import MLF.Types.Elab
  ( BoundType,
    ElabScheme,
    ElabType,
    TypeBinderRef,
    Ty (..),
    TyIF (..),
    cataIx,
    mkElabSchemeWithRefs,
    renameTypeBinderRef,
    typeBinderIdentityFromNode,
    typeBinderRefFromIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
    tyToElab,
  )
import MLF.Util.ElabError (ElabError (..))
import MLF.Util.Names (alphaName)
import Text.Read (readMaybe)

-- | Inputs needed to finalize a generalized scheme.
data FinalizeInput p = FinalizeInput
  { fiEnv :: GeneralizeEnv p,
    fiConstraint :: Constraint p,
    fiCanonical :: NodeId -> NodeId,
    fiBindParents :: BindParents,
    fiScopeRootC :: NodeRef,
    fiTypeRoot :: NodeId,
    fiTypeRootC :: NodeId,
    fiScopeGen :: Maybe GenNodeId,
    fiFirstGenAncestorGa :: NodeRef -> Maybe GenNodeId,
    fiBindParentsGa :: Maybe (GaBindParents p),
    fiSolvedToBasePref :: IntMap.IntMap NodeId,
    fiGammaAlias :: IntMap.IntMap Int,
    fiNamedUnderGaSet :: IntSet.IntSet,
    fiOrderedBinderRefs :: [(Int, TypeBinderRef)],
    fiBindings :: [(TypeBinderRef, Maybe BoundType)],
    fiSubst :: IntMap.IntMap TypeBinderRef,
    fiTyRaw :: ElabType
  }

finalizeScheme :: FinalizeInput p -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
finalizeScheme FinalizeInput {..} =
  let env = fiEnv
      constraint = fiConstraint
      canonical = fiCanonical
      typeRoot = fiTypeRoot
      scopeGen = fiScopeGen
      firstGenAncestorGa = fiFirstGenAncestorGa
      mbBindParentsGa = fiBindParentsGa
      solvedToBasePrefPlan = fiSolvedToBasePref
      gammaAliasPlan = fiGammaAlias
      namedUnderGaSetPlan = fiNamedUnderGaSet
      orderedBinderRefs = fiOrderedBinderRefs
      bindings = fiBindings
      subst = fiSubst
      ty0Raw = fiTyRaw
      originalBinderRef nidInt ref =
        case IntMap.lookup nidInt subst of
          Just substRef -> substRef
          Nothing ->
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (canonical (NodeId nidInt)))
              (typeBinderRefName ref)
      originalBinderRefs =
        [ (nidInt, originalBinderRef nidInt ref)
          | (nidInt, ref) <- orderedBinderRefs
        ]
      aliasToTypeRootRefs =
        [ ref
          | (nidInt, ref) <- originalBinderRefs,
            let nid = NodeId nidInt,
            Just bnd <- [VarStore.lookupVarBound constraint (canonical nid)],
            canonical bnd == canonical typeRoot
        ]
      bindingMatchesRef ref (bindingRef, _) =
        typeBinderRefsSameIdentity bindingRef ref
      lookupBindingRef ref binds =
        [ mb
          | binding@(_, mb) <- binds,
            bindingMatchesRef ref binding
        ]
      inlineAliasBinder :: ElabType -> [(TypeBinderRef, Maybe BoundType)] -> (ElabType, [(TypeBinderRef, Maybe BoundType)])
      inlineAliasBinder ty binds = case ty of
        TVarRef ref
          | refMember ref aliasToTypeRootRefs ->
              case lookupBindingRef ref binds of
                Just bnd : _
                  | not (isVarBound bnd),
                    not (isBaseBound bnd) ->
                      (tyToElab bnd, filter (not . bindingMatchesRef ref) binds)
                _ -> (ty, binds)
        _ -> (ty, binds)
      (ty0RawAlias, bindingsAlias) = inlineAliasBinder ty0Raw bindings
      canonAllVars ty =
        let (ty', _freeEnv, _n) = go [] [] (0 :: Int) ty
         in ty'
        where
          freshCanonRef n ref =
            renameTypeBinderRef ("v" ++ show n) ref

          lookupCanonRef ref =
            lookupRefNameRef ref

          go boundEnv freeEnv n tyInput = case tyInput of
            TVarRef ref ->
              case lookupCanonRef ref boundEnv of
                Just ref' -> (TVarRef ref', freeEnv, n)
                Nothing ->
                  case lookupCanonRef ref freeEnv of
                    Just ref' -> (TVarRef ref', freeEnv, n)
                    Nothing ->
                      let ref' = freshCanonRef n ref
                       in (TVarRef ref', (ref, ref') : freeEnv, n + 1)
            TBaseWithIdentity identity b -> (TBaseWithIdentity identity b, freeEnv, n)
            TBottom -> (TBottom, freeEnv, n)
            TArrow a b ->
              let (a', free1, n1) = go boundEnv freeEnv n a
                  (b', free2, n2) = go boundEnv free1 n1 b
               in (TArrow a' b', free2, n2)
            TConWithIdentity identity c (arg :| args) ->
              let (arg', free1, n1) = go boundEnv freeEnv n arg
                  (argsRev, free2, n2) =
                    foldl
                      ( \(acc, freeAcc, nAcc) a ->
                          let (a', free', n') = go boundEnv freeAcc nAcc a
                           in (a' : acc, free', n')
                      )
                      ([], free1, n1)
                      args
                  args' = reverse argsRev
               in (TConWithIdentity identity c (arg' :| args'), free2, n2)
            TVarAppRef ref (arg :| args) ->
              let (headTy, free1, n1) = go boundEnv freeEnv n (TVarRef ref)
                  ref' = case headTy of
                    TVarRef refHead -> refHead
                    _ -> ref
                  (arg', freeArg, nArg) = go boundEnv free1 n1 arg
                  (argsRev, free2, n2) =
                    foldl
                      ( \(acc, freeAcc, nAcc) a ->
                          let (a', free', n') = go boundEnv freeAcc nAcc a
                           in (a' : acc, free', n')
                      )
                      ([], freeArg, nArg)
                      args
                  args' = reverse argsRev
               in (TVarAppRef ref' (arg' :| args'), free2, n2)
            TForallRef ref mb body ->
              let ref' = freshCanonRef n ref
                  n1 = n + 1
                  (mb', free1, n2) =
                    case mb of
                      Nothing -> (Nothing, freeEnv, n1)
                      Just bnd ->
                        let (bnd', free', n') = goBound boundEnv freeEnv n1 bnd
                         in (Just bnd', free', n')
                  (body', free2, n3) = go ((ref, ref') : boundEnv) free1 n2 body
               in (TForallRef ref' mb' body', free2, n3)
            TMuRef ref body ->
              let ref' = freshCanonRef n ref
                  n1 = n + 1
                  (body', free1, n2) = go ((ref, ref') : boundEnv) freeEnv n1 body
               in (TMuRef ref' body', free1, n2)

          goBound boundEnv freeEnv n bound = case bound of
            TArrow a b ->
              let (a', free1, n1) = go boundEnv freeEnv n a
                  (b', free2, n2) = go boundEnv free1 n1 b
               in (TArrow a' b', free2, n2)
            TConWithIdentity identity c (arg :| args) ->
              let (arg', free1, n1) = go boundEnv freeEnv n arg
                  (argsRev, free2, n2) =
                    foldl
                      ( \(acc, freeAcc, nAcc) a ->
                          let (a', free', n') = go boundEnv freeAcc nAcc a
                           in (a' : acc, free', n')
                      )
                      ([], free1, n1)
                      args
                  args' = reverse argsRev
               in (TConWithIdentity identity c (arg' :| args'), free2, n2)
            TVarAppRef ref (arg :| args) ->
              let (headTy, free1, n1) = go boundEnv freeEnv n (TVarRef ref)
                  ref' = case headTy of
                    TVarRef refHead -> refHead
                    _ -> ref
                  (arg', freeArg, nArg) = go boundEnv free1 n1 arg
                  (argsRev, free2, n2) =
                    foldl
                      ( \(acc, freeAcc, nAcc) a ->
                          let (a', free', n') = go boundEnv freeAcc nAcc a
                           in (a' : acc, free', n')
                      )
                      ([], freeArg, nArg)
                      args
                  args' = reverse argsRev
               in (TVarAppRef ref' (arg' :| args'), free2, n2)
            TBaseWithIdentity identity b -> (TBaseWithIdentity identity b, freeEnv, n)
            TBottom -> (TBottom, freeEnv, n)
            TForallRef ref mb body ->
              let ref' = freshCanonRef n ref
                  n1 = n + 1
                  (mb', free1, n2) =
                    case mb of
                      Nothing -> (Nothing, freeEnv, n1)
                      Just bnd ->
                        let (bnd', free', n') = goBound boundEnv freeEnv n1 bnd
                         in (Just bnd', free', n')
                  (body', free2, n3) = go ((ref, ref') : boundEnv) free1 n2 body
               in (TForallRef ref' mb' body', free2, n3)
            TMuRef ref body ->
              let ref' = freshCanonRef n ref
                  n1 = n + 1
                  (body', free1, n2) = go ((ref, ref') : boundEnv) freeEnv n1 body
               in (TMuRef ref' body', free1, n2)
      replaceAlias boundNorm ref = goReplace
        where
          goReplace ty
            | canonAllVars ty == boundNorm = TVarRef ref
            | otherwise =
                case ty of
                  TArrow a b -> TArrow (goReplace a) (goReplace b)
                  TConWithIdentity identity c args -> TConWithIdentity identity c (fmap goReplace args)
                  TVarAppRef headRef args -> TVarAppRef headRef (fmap goReplace args)
                  TForallRef binderRef mb body ->
                    TForallRef binderRef (fmap (mapBoundType goReplace) mb) (goReplace body)
                  TMuRef binderRef body -> TMuRef binderRef (goReplace body)
                  _ -> ty
      stripAliasForall ty = case ty of
        TForallRef ref (Just bound) body
          | TVarRef bodyRef <- body,
            typeBinderRefsSameIdentity ref bodyRef ->
              stripAliasForall (tyToElab bound)
          | otherwise ->
              TForallRef ref (Just (stripAliasForallBound bound)) (stripAliasForall body)
        TForallRef ref Nothing body ->
          TForallRef ref Nothing (stripAliasForall body)
        TArrow a b -> TArrow (stripAliasForall a) (stripAliasForall b)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap stripAliasForall args)
        TVarAppRef ref args -> TVarAppRef ref (fmap stripAliasForall args)
        TMuRef ref body -> TMuRef ref (stripAliasForall body)
        _ -> ty
      stripAliasForallBound bound = case bound of
        TArrow a b -> TArrow (stripAliasForall a) (stripAliasForall b)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap stripAliasForall args)
        TVarAppRef ref args -> TVarAppRef ref (fmap stripAliasForall args)
        TBaseWithIdentity _ _ -> bound
        TBottom -> bound
        TForallRef ref mb body ->
          let mb' = fmap stripAliasForallBound mb
              body' = stripAliasForall body
           in TForallRef ref mb' body'
        TMuRef ref body -> TMuRef ref (stripAliasForall body)
      collapseBoundAliases binds ty =
        foldr
          ( \(v, mbBound) acc ->
              case mbBound of
                Nothing -> acc
                Just bound ->
                  let boundTy = tyToElab bound
                      boundCore = stripForallsType bound
                      boundTyNorm = canonAllVars boundTy
                   in if isVarBound boundCore
                        then acc
                        else
                          let boundNorm = canonAllVars boundCore
                           in if canonAllVars acc == boundNorm
                                || canonAllVars acc == boundTyNorm
                                then acc
                                else replaceAlias boundNorm v acc
          )
          ty
          binds
      normalizeScheme tyRaw binds =
        let tyAdjusted0 =
              case (binds, tyRaw) of
                ((bindingRef, mb) : _, TForallRef ref mb' body)
                  | typeBinderRefsSameIdentity bindingRef ref && mb == mb' -> body
                _ -> tyRaw
            tyAdjusted =
              case stripForallsType tyAdjusted0 of
                TVarRef ref ->
                  case lookupBindingRef ref binds of
                    Just bound : _
                      | containsForall (tyToElab bound) -> tyToElab bound
                    _ -> tyAdjusted0
                _ -> tyAdjusted0
            tyAliased = stripAliasForall (collapseBoundAliases binds tyAdjusted)
         in traceGeneralize
              env
              ( "generalizeAt: ty0Raw="
                  ++ show tyAliased
                  ++ " subst="
                  ++ show subst
                  ++ " bindings="
                  ++ show binds
              )
              (tyAliased, binds)
      (ty0RawAdjusted, bindingsAdjusted) = normalizeScheme ty0RawAlias bindingsAlias
      nameForId k = "t" ++ show k
      substRefsByRawName =
        [ (nameForId k, ref)
          | (k, ref) <- IntMap.toList subst
        ]
      isOwnedStructuredAliasBinder nidInt =
        let nid = canonical (NodeId nidInt)
         in case VarStore.lookupVarBound constraint nid of
              Just bnd0 ->
                let bnd = canonical bnd0
                    binderIsTypeRoot = nid == canonical typeRoot
                    boundOwnedByBinder =
                      case IntMap.lookup (nodeRefKey (typeRef bnd)) fiBindParents of
                        Just (TypeRef parent, _) -> canonical parent == nid
                        _ -> False
                    boundIsStructured =
                      case lookupNodeIn (cNodes constraint) bnd of
                        Just TyArrow {} -> True
                        Just TyCon {} -> True
                        Just TyVarApp {} -> True
                        Just TyForall {} -> True
                        Just TyMu {} -> True
                        Just TyExp {} -> True
                        _ -> False
                 in not binderIsTypeRoot && boundOwnedByBinder && boundIsStructured
              Nothing -> False
      namedBinderRefs =
        [ ref
          | (nidInt, ref) <- IntMap.toList subst,
            IntSet.member nidInt namedUnderGaSetPlan,
            not (isOwnedStructuredAliasBinder nidInt)
        ]
      renameVars = cataIx alg
        where
          parseRigidName v = do
            digits <- stripPrefix "__rigid" v
            readMaybe digits
          renameFromSubst v = case renameRefFromSubstName v of
            Just ref -> typeBinderRefName ref
            Nothing ->
              case parseRigidName v of
                Just nid ->
                  let keyC = getNodeId (canonical (NodeId nid))
                      aliasKey = IntMap.findWithDefault keyC keyC gammaAliasPlan
                   in maybe v typeBinderRefName (IntMap.lookup aliasKey subst)
                Nothing -> v
          renameRefFromSubstName v = case lookup v substRefsByRawName of
            Just ref -> Just ref
            Nothing ->
              case parseRigidName v of
                Just nid ->
                  let keyC = getNodeId (canonical (NodeId nid))
                      aliasKey = IntMap.findWithDefault keyC keyC gammaAliasPlan
                   in IntMap.lookup aliasKey subst
                Nothing -> Nothing
          renameRefFromSubst ref =
            case renameRefFromSubstName (typeBinderRefName ref) of
              Just ref' -> ref'
              Nothing -> renameTypeBinderRef (renameFromSubst (typeBinderRefName ref)) ref
          alg :: TyIF i Ty -> Ty i
          alg ty = case ty of
            TVarIFRef ref -> TVarRef (renameRefFromSubst ref)
            TArrowIF a b -> TArrow a b
            TConIFWithIdentity identity c args -> TConWithIdentity identity c args
            TVarAppIFRef ref args -> TVarAppRef (renameRefFromSubst ref) args
            TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
            TBottomIF -> TBottom
            TForallIFRef ref mb body -> TForallRef (renameRefFromSubst ref) mb body
            TMuIFRef ref body -> TMuRef (renameRefFromSubst ref) body
      ty0 = renameVars ty0RawAdjusted
      inlineBaseBounds = False
      bindingsAdjustedRefs =
        bindingsAdjusted
      (bindingsNorm0Refs, tyNorm0) =
        simplifySchemeBindingsRefs inlineBaseBounds namedBinderRefs bindingsAdjustedRefs ty0
      (bindingsNorm1Refs, tyNorm1) = promoteArrowAliasRefs bindingsNorm0Refs tyNorm0
      (bindingsNormRefs, tyNorm) = (bindingsNorm1Refs, tyNorm1)
      bindingsNorm =
        [ (typeBinderRefName ref, mb)
          | (ref, mb) <- bindingsNormRefs
        ]
      quantifiedRefs = map fst (fst (splitForallsRefs tyNorm))
      usedRefs =
        unionTypeRefs
          ( quantifiedRefs
              : freeTypeVarRefsFrom [] tyNorm
              : [freeTypeVarRefsType b | (_, Just b) <- bindingsNormRefs]
          )
      usedNames = Set.fromList (map typeBinderRefName usedRefs)
      bindingsFinalRefs =
        filter
          ( \(ref, _) ->
              refMember ref usedRefs || refMember ref namedBinderRefs
          )
          bindingsNormRefs
      bindingsFinalRefs' =
        let dropRedundant (ref, mb) =
              not (refMember ref usedRefs)
                && case mb of
                  Nothing -> True
                  Just bnd ->
                    let freeBound = freeTypeVarRefsType bnd
                        boundMentionsSelf = refMember ref freeBound
                        boundIsSimple = isVarBound bnd || isBaseBound bnd
                        boundIsBody = tyToElab bnd == tyNorm
                     in not boundMentionsSelf && (boundIsSimple || boundIsBody)
         in filter (not . dropRedundant) bindingsFinalRefs
      bindingsFinal' =
        [ (typeBinderRefName ref, mb)
          | (ref, mb) <- bindingsFinalRefs'
        ]
      aliasBounds =
        [ (typeBinderRefName ref, bound)
          | (ref, Just bound) <- bindingsFinalRefs',
            isVarBound bound
        ]
      renameTypeVars :: ElabType -> ElabType
      renameTypeVars = cataIx alg
        where
          renameRefFromMap ref =
            renameTypeBinderRef (renameRefName ref) ref
          alg :: TyIF i Ty -> Ty i
          alg ty = case ty of
            TVarIFRef ref -> TVarRef (renameRefFromMap ref)
            TArrowIF a b -> TArrow a b
            TConIFWithIdentity identity c args -> TConWithIdentity identity c args
            TVarAppIFRef ref args -> TVarAppRef (renameRefFromMap ref) args
            TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
            TBottomIF -> TBottom
            TForallIFRef ref mb body -> TForallRef (renameRefFromMap ref) mb body
            TMuIFRef ref body -> TMuRef (renameRefFromMap ref) body
      renamePairs =
        [ (ref, alphaName idx 0)
          | (idx, (ref, _)) <- zip [0 ..] bindingsFinalRefs'
        ]
      renameMap =
        Map.fromList
          [ (typeBinderRefName ref, newName)
            | (ref, newName) <- renamePairs
          ]
      renameName name = Map.findWithDefault name name renameMap
      renameRefName ref =
        case lookupRefName ref renamePairs of
          Just newName -> newName
          Nothing -> renameName (typeBinderRefName ref)
      bindingsRenamedRefs =
        [ (renameTypeBinderRef (renameRefName ref) ref, fmap (mapBoundType renameTypeVars) mb)
          | (ref, mb) <- bindingsFinalRefs'
        ]
      bindingsRenamed =
        [ (typeBinderRefName ref, mb)
          | (ref, mb) <- bindingsRenamedRefs
        ]
      tyRenamed = renameTypeVars tyNorm
      traceFinal =
        traceGeneralize
          env
          ( "generalizeAt: tyNorm="
              ++ show tyNorm
              ++ " usedNames="
              ++ show (Set.toList usedNames)
              ++ " bindingsNorm="
              ++ show bindingsNorm
              ++ " bindingsFinal="
              ++ show bindingsFinal'
              ++ " bindingsRenamed="
              ++ show bindingsRenamed
          )

      usedRefsRenamed =
        unionTypeRefs
          ( freeTypeVarRefsFrom [] tyRenamed
              : [freeTypeVarRefsType b | (_, Just b) <- bindingsRenamedRefs]
          )
      boundNames = Set.fromList (map fst bindingsRenamed)
      boundRefs = map fst bindingsRenamedRefs
      missingRefsRaw =
        filter
          (\ref -> not (refMember ref boundRefs))
          usedRefsRenamed
      aliasAllowed ref =
        case typeBinderRefNode ref of
          Just nid ->
            let keyC = getNodeId (canonical nid)
                aliasKey = case IntMap.lookup keyC gammaAliasPlan of
                  Just repKey -> repKey
                  Nothing -> keyC
             in case IntMap.lookup aliasKey subst of
                  Just substRef -> Set.member (renameName (typeBinderRefName substRef)) boundNames
                  Nothing -> False
          Nothing -> False
      missingRefsRaw' = filter (not . aliasAllowed) missingRefsRaw
      missingRefs =
        case scopeGen of
          Nothing -> missingRefsRaw'
          Just gid ->
            let refNodeId =
                  typeBinderRefNode
                underScope ref =
                  case refNodeId ref of
                    Just nidRef@(NodeId nid) ->
                      let underSolved =
                            firstGenAncestorGa (typeRef nidRef) == Just gid
                          underBase =
                            case mbBindParentsGa of
                              Just ga ->
                                case IntMap.lookup nid solvedToBasePrefPlan of
                                  Just baseN ->
                                    firstGenAncestorFrom (gaBindParentsBase ga) (TypeRef baseN) == Just gid
                                  Nothing -> underSolved
                              Nothing -> underSolved
                       in underBase
                    Nothing -> True
             in filter underScope missingRefsRaw'
      keepRefs = map fst bindingsRenamedRefs
      subst' =
        IntMap.filter (`refMember` keepRefs) $
          IntMap.map (\ref -> renameTypeBinderRef (renameRefName ref) ref) subst
      finalize missing =
        if null missing
          then pure (mkElabSchemeWithRefs bindingsRenamedRefs tyRenamed, subst')
          else
            let synthPairs =
                  zip missing [alphaName idx 0 | idx <- [length bindingsRenamed ..]]
                renameResidual ty =
                  foldl
                    ( \acc (oldRef, new) ->
                        substTypeSimpleRef oldRef (TVarRef (renameTypeBinderRef new oldRef)) acc
                    )
                    ty
                    synthPairs
                tySynth = renameResidual tyRenamed
                bindingsSynthRefs =
                  [ (ref, fmap (mapBoundType renameResidual) mb)
                    | (ref, mb) <- bindingsRenamedRefs
                  ]
                    ++ [(renameTypeBinderRef new oldRef, Nothing) | (oldRef, new) <- synthPairs]
             in pure (mkElabSchemeWithRefs bindingsSynthRefs tySynth, subst')
   in traceFinal $ case aliasBounds of
        [] -> finalize missingRefs
        _ ->
          Left $
            ValidationFailed
              [ "alias bounds survived scheme finalization: "
                  ++ show (map fst aliasBounds)
              ]

refMember :: TypeBinderRef -> [TypeBinderRef] -> Bool
refMember ref = any (typeBinderRefsSameIdentity ref)

insertTypeRef :: TypeBinderRef -> [TypeBinderRef] -> [TypeBinderRef]
insertTypeRef ref refs
  | refMember ref refs = refs
  | otherwise = ref : refs

unionTypeRefs :: [[TypeBinderRef]] -> [TypeBinderRef]
unionTypeRefs = foldr (foldr insertTypeRef) []

lookupRefName :: TypeBinderRef -> [(TypeBinderRef, String)] -> Maybe String
lookupRefName _ [] = Nothing
lookupRefName ref ((candidate, name) : rest)
  | typeBinderRefsSameIdentity ref candidate = Just name
  | otherwise = lookupRefName ref rest

lookupRefNameRef :: TypeBinderRef -> [(TypeBinderRef, TypeBinderRef)] -> Maybe TypeBinderRef
lookupRefNameRef _ [] = Nothing
lookupRefNameRef ref ((candidate, renamed) : rest)
  | typeBinderRefsSameIdentity ref candidate = Just renamed
  | otherwise = lookupRefNameRef ref rest
