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
import MLF.Constraint.Presolution.Rewrite (canonicalizeTrace, canonicalizeWitness)
import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Constraint.Types.Witness
    ( BoundRef(..)
    , Expansion(..)
    , ForallSpec(..)
    )

-- | Chase redirects through the map until stable or missing.
chaseRedirects :: IntMap.IntMap NodeId -> NodeId -> NodeId
chaseRedirects = chaseRedirectsStable

-- | Build a canonicalizer that applies redirects before union-find canonicalization.
makeCanonicalizer :: IntMap.IntMap NodeId -> IntMap.IntMap NodeId -> Canonicalizer
makeCanonicalizer = Canonicalizer.makeCanonicalizer

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
