{- |
Module      : MLF.Constraint.Canonicalizer
Description : Stable canonicalization for redirects and union-find

This module provides a first-class canonicalizer that combines redirect
chasing with union-find canonicalization. It is idempotent (canonicalize twice
== once) and safe in the presence of redirect cycles by selecting a
stable representative.
-}
module MLF.Constraint.Canonicalizer (
    Canonicalizer,
    canonicalizeNode,
    canonicalizeRef,
    canonicalizerFrom,
    makeCanonicalizer,
    chaseRedirectsStable
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Canonicalize (canonicalRef)
import MLF.Constraint.Types.Graph (NodeId(..), NodeRef, getNodeId)
import qualified MLF.Util.UnionFind as UnionFind

-- | Canonicalization helper that is stable under repeated application.
newtype Canonicalizer = Canonicalizer { canonicalizeNode :: NodeId -> NodeId }

-- | Canonicalize a node reference, leaving gen refs untouched.
canonicalizeRef :: Canonicalizer -> NodeRef -> NodeRef
canonicalizeRef canon = canonicalRef (canonicalizeNode canon)

-- | Wrap a canonicalization function in a Canonicalizer.
-- Caller is responsible for ensuring stability/idempotence.
canonicalizerFrom :: (NodeId -> NodeId) -> Canonicalizer
canonicalizerFrom = Canonicalizer

-- | Build a canonicalizer that chases redirects before applying union-find,
-- and then stabilizes the result.
makeCanonicalizer :: IntMap.IntMap NodeId -> IntMap.IntMap NodeId -> Canonicalizer
makeCanonicalizer unionFind redirects =
    let redirectStable = chaseRedirectsStable redirects
        step nid = UnionFind.frWith unionFind (redirectStable nid)
    in Canonicalizer (stableChase step)

-- | Chase redirects until stable, selecting a deterministic representative
-- if a redirect cycle is encountered.
chaseRedirectsStable :: IntMap.IntMap NodeId -> NodeId -> NodeId
chaseRedirectsStable redirects = stableChase (redirectStep redirects)

redirectStep :: IntMap.IntMap NodeId -> NodeId -> NodeId
redirectStep redirects nid =
    case IntMap.lookup (getNodeId nid) redirects of
        Just target -> target
        Nothing -> nid

-- | Iterate a step function until it is stable, detecting cycles and
-- returning a deterministic representative (the smallest node id) if needed.
stableChase :: (NodeId -> NodeId) -> NodeId -> NodeId
stableChase step start = go IntSet.empty start
  where
    go seen current =
        let next = step current
        in if next == current
            then current
            else if IntSet.member (getNodeId next) seen
                then cycleRepresentative next
                else go (IntSet.insert (getNodeId current) seen) next

    cycleRepresentative cycleStart = goCycle cycleStart cycleStart
      where
        goCycle minNode current =
            let next = step current
                minNode' = minById minNode next
            in if next == cycleStart
                then minNode'
                else goCycle minNode' next

    minById a b =
        if getNodeId a <= getNodeId b
            then a
            else b
