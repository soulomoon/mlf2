module MLF.Constraint.Presolution.Construction
    ( RawExpansionConstruction
    , emptyRawExpansionConstruction
    , mkRawExpansionConstruction
    , combineRawExpansionConstructions
    , rawExpansionConstructionParents
    , rawExpansionConstructionArgumentKeys
    , rawExpansionConstructionSemanticMetaKeys
    ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , BindParents
    , NodeId(..)
    , nodeRefKey
    , typeRef
    )

-- | The exact binding edits committed by one atomic expansion construction.
--
-- This artifact deliberately stays in the destination-ID domain that existed
-- when chi_e was built.  Generalization later projects several such artifacts
-- through the final quotient to recover elaboration construction placement.
-- This is binding-reset authority, not a snapshot of the solver's live Rebind
-- tree.
--
-- The constructor is private: candidate roles must name flexible type children
-- in the same committed parent map.  Non-role support edits may be rigid; they
-- remain part of the exact path model without becoming Gamma candidates.
data RawExpansionConstruction = RawExpansionConstruction
    { recParents :: !BindParents
    , recArgumentKeys :: !IntSet.IntSet
    , recSemanticMetaKeys :: !IntSet.IntSet
    }
    deriving (Eq, Show)

emptyRawExpansionConstruction :: RawExpansionConstruction
emptyRawExpansionConstruction =
    RawExpansionConstruction
        { recParents = IntMap.empty
        , recArgumentKeys = IntSet.empty
        , recSemanticMetaKeys = IntSet.empty
        }

mkRawExpansionConstruction
    :: BindParents
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> Either String RawExpansionConstruction
mkRawExpansionConstruction parents argumentKeys semanticMetaKeys = do
    validateRole "argument" argumentKeys
    validateRole "semantic meta" semanticMetaKeys
    pure
        RawExpansionConstruction
            { recParents = parents
            , recArgumentKeys = argumentKeys
            , recSemanticMetaKeys = semanticMetaKeys
            }
  where
    validateRole label keys =
        case
            [ (NodeId key, parent)
            | key <- IntSet.toAscList keys
            , let parent =
                    IntMap.lookup
                        (nodeRefKey (typeRef (NodeId key)))
                        parents
            , case parent of
                Just (_, BindFlex) -> False
                _ -> True
            ]
          of
            [] -> Right ()
            invalid ->
                Left
                    ( "raw expansion construction has "
                        ++ label
                        ++ " roles without flexible committed type parents: "
                        ++ show invalid
                    )

-- | Combine construction edits emitted by consecutive steps of one edge
-- expansion.  A shared child must carry exactly the same placement: choosing
-- either side on conflict would erase construction authority.
combineRawExpansionConstructions
    :: RawExpansionConstruction
    -> RawExpansionConstruction
    -> Either String RawExpansionConstruction
combineRawExpansionConstructions first second = do
    let firstParents = recParents first
        secondParents = recParents second
        conflicts =
            [ (childKey, firstPlacement, secondPlacement)
            | (childKey, firstPlacement) <- IntMap.toAscList firstParents
            , Just secondPlacement <- [IntMap.lookup childKey secondParents]
            , firstPlacement /= secondPlacement
            ]
    case conflicts of
        [] ->
            mkRawExpansionConstruction
                (IntMap.union firstParents secondParents)
                (IntSet.union
                    (recArgumentKeys first)
                    (recArgumentKeys second)
                )
                (IntSet.union
                    (recSemanticMetaKeys first)
                    (recSemanticMetaKeys second)
                )
        _ ->
            Left
                ( "conflicting composed raw expansion construction parents: "
                    ++ show conflicts
                )

rawExpansionConstructionParents :: RawExpansionConstruction -> BindParents
rawExpansionConstructionParents = recParents

rawExpansionConstructionArgumentKeys
    :: RawExpansionConstruction
    -> IntSet.IntSet
rawExpansionConstructionArgumentKeys = recArgumentKeys

rawExpansionConstructionSemanticMetaKeys
    :: RawExpansionConstruction
    -> IntSet.IntSet
rawExpansionConstructionSemanticMetaKeys = recSemanticMetaKeys
