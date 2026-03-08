module SolvedFacadeTestUtil (
    mkTestSolved,
    solvedFromSnapshot,
    classMembers,
    originalNode,
    originalBindParent,
    wasOriginalBinder,
    validateOriginalCanonicalAgreement
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Finalize (presolutionViewFromSnapshot, stepSolvedFromPresolutionView)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph
    ( BindFlag
    , Constraint
    , NodeId(..)
    , NodeRef
    , TyNode(..)
    , getNodeId
    , tnId
    )

mkTestSolved :: Constraint -> IntMap.IntMap NodeId -> Solved.Solved
mkTestSolved constraint uf =
    stepSolvedFromPresolutionView (presolutionViewFromSnapshot constraint uf)

solvedFromSnapshot :: IntMap.IntMap NodeId -> Constraint -> Solved.Solved
solvedFromSnapshot uf constraint =
    mkTestSolved constraint uf

classMembers :: Solved.Solved -> NodeId -> [NodeId]
classMembers solved nid =
    [ tnId node
    | node <- NodeAccess.allNodes (Solved.originalConstraint solved)
    , Solved.canonical solved (tnId node) == Solved.canonical solved nid
    ]

originalNode :: Solved.Solved -> NodeId -> Maybe TyNode
originalNode solved nid =
    NodeAccess.lookupNode (Solved.originalConstraint solved) nid

originalBindParent :: Solved.Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
originalBindParent solved ref =
    NodeAccess.lookupBindParent (Solved.originalConstraint solved) ref

wasOriginalBinder :: Solved.Solved -> NodeId -> Bool
wasOriginalBinder solved nid =
    any isForall (classMembers solved nid)
  where
    isForall member =
        case originalNode solved member of
            Just TyForall {} -> True
            _ -> False

validateOriginalCanonicalAgreement :: Solved.Solved -> [String]
validateOriginalCanonicalAgreement solved =
    [ "Node " ++ show (getNodeId nid) ++ ": original=" ++ show origTag
        ++ " canonical=" ++ show canonTag
    | node <- NodeAccess.allNodes (Solved.originalConstraint solved)
    , let nid = tnId node
          nidC = Solved.canonical solved nid
          origNode' = originalNode solved nid
          canonNode = NodeAccess.lookupNode (Solved.canonicalConstraint solved) nidC
          origTag = fmap tnTag origNode'
          canonTag = fmap tnTag canonNode
    , origTag /= canonTag
    ]
  where
    tnTag :: TyNode -> String
    tnTag TyVar{}    = "var"
    tnTag TyBottom{} = "bottom"
    tnTag TyArrow{}  = "arrow"
    tnTag TyBase{}   = "base"
    tnTag TyCon{}    = "con"
    tnTag TyForall{} = "forall"
    tnTag TyExp{}    = "exp"
