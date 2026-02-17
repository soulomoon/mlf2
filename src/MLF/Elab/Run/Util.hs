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
import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Constraint.Types.Witness
    ( BoundRef(..)
    , EdgeWitness(..)
    , Expansion(..)
    , ForallSpec(..)
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
        -- Contract: preserve source-domain provenance (`ewSteps`, `ewWitness`)
        -- and canonicalize only structural lookup fields.
    in w
        { ewLeft = canonNode (ewLeft w)
        , ewRight = canonNode (ewRight w)
        , ewRoot = canonNode (ewRoot w)
        , ewSteps = ewSteps w
        , ewWitness = ewWitness w
        }

canonicalizeTrace :: Canonicalizer -> EdgeTrace -> EdgeTrace
canonicalizeTrace canon tr =
    let canonNode = canonicalizeNode canon
        -- Contract: preserve source-domain provenance in `etBinderArgs`,
        -- `etInterior`, and `etCopyMap` for Φ/Omega. Preserve replay-hint
        -- metadata in `etBinderReplayHints`. `etRoot` is structural and may be
        -- canonicalized for solved-graph lookup.
    in tr
        { etRoot = canonNode (etRoot tr)
        , etBinderArgs = etBinderArgs tr
        , etInterior = etInterior tr
        , etBinderReplayHints = etBinderReplayHints tr
        , etCopyMap = etCopyMap tr
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
