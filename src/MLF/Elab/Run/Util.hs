module MLF.Elab.Run.Util (
    chaseRedirects
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types (NodeId(..), getNodeId)

-- | Chase redirects through the map until stable or missing.
chaseRedirects :: IntMap.IntMap NodeId -> NodeId -> NodeId
chaseRedirects redirects nid =
    let go seen n = case IntMap.lookup (getNodeId n) redirects of
            Just n'
                | n' == n -> n
                | IntSet.member (getNodeId n') seen -> n
                | otherwise -> go (IntSet.insert (getNodeId n') seen) n'
            Nothing -> n
    in go (IntSet.singleton (getNodeId nid)) nid
