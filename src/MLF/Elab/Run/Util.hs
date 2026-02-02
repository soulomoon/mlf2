module MLF.Elab.Run.Util (
    chaseRedirects,
    makeCanonicalizer,
    canonicalizeWitness,
    canonicalizeTrace,
    canonicalizeExpansion,
    firstShow
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Canonicalizer (Canonicalizer, canonicalizeNode, chaseRedirectsStable)
import qualified MLF.Constraint.Canonicalizer as Canonicalizer
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), fromListInterior, toListInterior)
import MLF.Constraint.Types.Graph (NodeId(..), getNodeId)
import MLF.Constraint.Types.Witness
    ( BoundRef(..)
    , EdgeWitness(..)
    , Expansion(..)
    , ForallSpec(..)
    , InstanceOp(..)
    , InstanceStep(..)
    , InstanceWitness(..)
    )

-- | Chase redirects through the map until stable or missing.
chaseRedirects :: IntMap.IntMap NodeId -> NodeId -> NodeId
chaseRedirects = chaseRedirectsStable

-- | Build a canonicalizer that applies redirects before union-find canonicalization.
makeCanonicalizer :: IntMap.IntMap NodeId -> IntMap.IntMap NodeId -> Canonicalizer
makeCanonicalizer = Canonicalizer.makeCanonicalizer

canonicalizeWitness :: Canonicalizer -> EdgeWitness -> EdgeWitness
canonicalizeWitness canon w =
    let canonNode = canonicalizeNode canon
        canonOp op = case op of
            OpGraft a b -> OpGraft (canonNode a) (canonNode b)
            OpMerge a b -> OpMerge (canonNode a) (canonNode b)
            OpRaise n -> OpRaise (canonNode n)
            OpWeaken n -> OpWeaken (canonNode n)
            OpRaiseMerge a b -> OpRaiseMerge (canonNode a) (canonNode b)
        canonStep step = case step of
            StepOmega op -> StepOmega (canonOp op)
            StepIntro -> StepIntro
        InstanceWitness ops = ewWitness w
    in w
        { ewLeft = canonNode (ewLeft w)
        , ewRight = canonNode (ewRight w)
        , ewRoot = canonNode (ewRoot w)
        , ewSteps = map canonStep (ewSteps w)
        , ewWitness = InstanceWitness (map canonOp ops)
        }

canonicalizeTrace :: Canonicalizer -> EdgeTrace -> EdgeTrace
canonicalizeTrace canon tr =
    let canonNode = canonicalizeNode canon
        canonPair (a, b) = (canonNode a, canonNode b)
        canonInterior =
            fromListInterior
                [ canonNode i
                | i <- toListInterior (etInterior tr)
                ]
        canonCopyMap =
            CopyMapping $
                IntMap.fromListWith min
                    [ ( getNodeId (canonNode (NodeId k))
                      , canonNode v
                      )
                    | (k, v) <- IntMap.toList (getCopyMapping (etCopyMap tr))
                    ]
    in tr
        { etRoot = canonNode (etRoot tr)
        , etBinderArgs = map canonPair (etBinderArgs tr)
        , etInterior = canonInterior
        , etCopyMap = canonCopyMap
        }

canonicalizeExpansion :: Canonicalizer -> Expansion -> Expansion
canonicalizeExpansion canon expn =
    let canonNode = canonicalizeNode canon
    in case expn of
        ExpIdentity -> ExpIdentity
        ExpForall specs ->
            let canonBound bnd = case bnd of
                    BoundNode nid -> BoundNode (canonNode nid)
                    BoundBinder ix -> BoundBinder ix
                canonSpec spec =
                    spec
                        { fsBounds =
                            map
                                (fmap canonBound)
                                (fsBounds spec)
                        }
            in ExpForall (NE.map canonSpec specs)
        ExpInstantiate args -> ExpInstantiate (map canonNode args)
        ExpCompose es -> ExpCompose (NE.map (canonicalizeExpansion canon) es)

-- | Convert Either with Showable error to Either String
firstShow :: Show e => Either e a -> Either String a
firstShow = either (Left . show) Right
