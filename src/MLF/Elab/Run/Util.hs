module MLF.Elab.Run.Util (
    chaseRedirects,
    makeCanonicalizer,
    canonicalizeWitness,
    canonicalizeTrace,
    canonicalizeExpansion,
    firstShow
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), fromListInterior, toListInterior)
import MLF.Constraint.Types
    ( BoundRef(..)
    , EdgeWitness(..)
    , Expansion(..)
    , ForallSpec(..)
    , InstanceOp(..)
    , InstanceStep(..)
    , InstanceWitness(..)
    , NodeId(..)
    , getNodeId
    )
import qualified MLF.Util.UnionFind as UnionFind

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

-- | Build a canonicalizer that applies redirects before union-find canonicalization.
makeCanonicalizer :: IntMap.IntMap NodeId -> IntMap.IntMap NodeId -> (NodeId -> NodeId)
makeCanonicalizer unionFind redirects =
    let canonical = UnionFind.frWith unionFind
    in canonical . chaseRedirects redirects

canonicalizeWitness :: (NodeId -> NodeId) -> EdgeWitness -> EdgeWitness
canonicalizeWitness canon w =
    let canonOp op = case op of
            OpGraft a b -> OpGraft (canon a) (canon b)
            OpMerge a b -> OpMerge (canon a) (canon b)
            OpRaise n -> OpRaise (canon n)
            OpWeaken n -> OpWeaken (canon n)
            OpRaiseMerge a b -> OpRaiseMerge (canon a) (canon b)
        canonStep step = case step of
            StepOmega op -> StepOmega (canonOp op)
            StepIntro -> StepIntro
        InstanceWitness ops = ewWitness w
    in w
        { ewLeft = canon (ewLeft w)
        , ewRight = canon (ewRight w)
        , ewRoot = canon (ewRoot w)
        , ewSteps = map canonStep (ewSteps w)
        , ewWitness = InstanceWitness (map canonOp ops)
        }

canonicalizeTrace :: (NodeId -> NodeId) -> EdgeTrace -> EdgeTrace
canonicalizeTrace canon tr =
    let canonPair (a, b) = (canon a, canon b)
        canonInterior =
            fromListInterior
                [ canon i
                | i <- toListInterior (etInterior tr)
                ]
        canonCopyMap =
            CopyMapping $
                IntMap.fromListWith min
                    [ ( getNodeId (canon (NodeId k))
                      , canon v
                      )
                    | (k, v) <- IntMap.toList (getCopyMapping (etCopyMap tr))
                    ]
    in tr
        { etRoot = canon (etRoot tr)
        , etBinderArgs = map canonPair (etBinderArgs tr)
        , etInterior = canonInterior
        , etCopyMap = canonCopyMap
        }

canonicalizeExpansion :: (NodeId -> NodeId) -> Expansion -> Expansion
canonicalizeExpansion canon expn = case expn of
    ExpIdentity -> ExpIdentity
    ExpForall specs ->
        let canonBound bnd = case bnd of
                BoundNode nid -> BoundNode (canon nid)
                BoundBinder ix -> BoundBinder ix
            canonSpec spec =
                spec
                    { fsBounds =
                        map
                            (fmap canonBound)
                            (fsBounds spec)
                    }
        in ExpForall (NE.map canonSpec specs)
    ExpInstantiate args -> ExpInstantiate (map canon args)
    ExpCompose es -> ExpCompose (NE.map (canonicalizeExpansion canon) es)

-- | Convert Either with Showable error to Either String
firstShow :: Show e => Either e a -> Either String a
firstShow = either (Left . show) Right
