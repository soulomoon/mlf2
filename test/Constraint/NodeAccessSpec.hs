{-# LANGUAGE DataKinds #-}

module Constraint.NodeAccessSpec (spec) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import MLF.Constraint.NodeAccess qualified as NodeAccess
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    BindFlag (..),
    BindingError (..),
    Constraint (..),
    ExpVarId (..),
    GenNode (..),
    GenNodeId (..),
    NodeId (..),
    TyNode (..),
    fromListGen,
    genRef,
    nodeRefKey,
    typeRef,
  )
import MLF.Constraint.Types.Phase (Phase (Raw))
import SpecUtil (emptyConstraint, nodeMapFromList)
import Test.Hspec

sampleNodes :: [TyNode]
sampleNodes =
  [ TyVar (NodeId 0) (Just (NodeId 1)),
    TyBase (NodeId 1) (BaseTy "Int"),
    TyArrow (NodeId 2) (NodeId 0) (NodeId 1),
    TyForall (NodeId 3) (NodeId 2),
    TyBottom (NodeId 4),
    TyCon (NodeId 5) (BaseTy "Box") (NodeId 1 :| [NodeId 4]),
    TyVarApp (NodeId 6) (NodeId 0) (NodeId 1 :| []),
    TyExp (NodeId 7) (ExpVarId 0) (NodeId 2),
    TyMu (NodeId 8) (NodeId 2)
  ]

sampleGen :: GenNode
sampleGen = GenNode (GenNodeId 0) [NodeId 3]

sampleConstraint :: Constraint 'Raw
sampleConstraint =
  emptyConstraint
    { cNodes = nodeMapFromList (zip [0 ..] sampleNodes),
      cBindParents =
        IntMap.fromList
          [ (nodeRefKey (typeRef (NodeId 0)), (typeRef (NodeId 3), BindFlex)),
            (nodeRefKey (typeRef (NodeId 1)), (typeRef (NodeId 2), BindRigid)),
            (nodeRefKey (genRef (GenNodeId 0)), (typeRef (NodeId 3), BindFlex))
          ],
      cGenNodes = fromListGen [(GenNodeId 0, sampleGen)]
    }

spec :: Spec
spec = describe "MLF.Constraint.NodeAccess" $ do
  describe "node and gen-node lookup" $ do
    it "returns nodes through Maybe and Either APIs" $ do
      NodeAccess.lookupNode sampleConstraint (NodeId 1)
        `shouldBe` Just (TyBase (NodeId 1) (BaseTy "Int"))
      NodeAccess.lookupNode sampleConstraint (NodeId 99) `shouldBe` Nothing
      NodeAccess.lookupNodeSafe sampleConstraint (NodeId 1)
        `shouldBe` Right (TyBase (NodeId 1) (BaseTy "Int"))
      NodeAccess.lookupNodeSafe sampleConstraint (NodeId 99)
        `shouldBe` Left "Node not found: 99"

    it "returns gen nodes through Maybe and Either APIs" $ do
      NodeAccess.lookupGenNode sampleConstraint (GenNodeId 0)
        `shouldBe` Just sampleGen
      NodeAccess.lookupGenNode sampleConstraint (GenNodeId 99) `shouldBe` Nothing
      NodeAccess.lookupGenNodeSafe sampleConstraint (GenNodeId 0)
        `shouldBe` Right sampleGen
      NodeAccess.lookupGenNodeSafe sampleConstraint (GenNodeId 99)
        `shouldBe` Left "Gen node not found: 99"

    it "applies canonicalizers before lookup" $ do
      NodeAccess.lookupNodeCanon (const (NodeId 1)) sampleConstraint (NodeId 99)
        `shouldBe` Just (TyBase (NodeId 1) (BaseTy "Int"))
      NodeAccess.lookupGenNodeCanon (const (GenNodeId 0)) sampleConstraint (GenNodeId 99)
        `shouldBe` Just sampleGen

    it "reports binding-tree errors for required lookups" $ do
      NodeAccess.requireNode sampleConstraint (NodeId 1)
        `shouldBe` Right (TyBase (NodeId 1) (BaseTy "Int"))
      NodeAccess.requireNode sampleConstraint (NodeId 99)
        `shouldBe` Left (InvalidBindingTree "Node not found: 99")
      NodeAccess.requireGenNode sampleConstraint (GenNodeId 0)
        `shouldBe` Right sampleGen
      NodeAccess.requireGenNode sampleConstraint (GenNodeId 99)
        `shouldBe` Left (InvalidBindingTree "Gen node not found: 99")

  describe "node classification and structural access" $ do
    it "classifies each supported node family and treats missing nodes as false" $ do
      NodeAccess.isVar sampleConstraint (NodeId 0) `shouldBe` True
      NodeAccess.isVar sampleConstraint (NodeId 1) `shouldBe` False
      NodeAccess.isForall sampleConstraint (NodeId 3) `shouldBe` True
      NodeAccess.isForall sampleConstraint (NodeId 99) `shouldBe` False
      NodeAccess.isArrow sampleConstraint (NodeId 2) `shouldBe` True
      NodeAccess.isBase sampleConstraint (NodeId 1) `shouldBe` True
      NodeAccess.isBottom sampleConstraint (NodeId 4) `shouldBe` True
      NodeAccess.isBottom sampleConstraint (NodeId 99) `shouldBe` False

    it "reads structural children with and without variable bounds" $ do
      NodeAccess.lookupStructuralChildren sampleConstraint (NodeId 2)
        `shouldBe` Just [NodeId 0, NodeId 1]
      NodeAccess.lookupStructuralChildren sampleConstraint (NodeId 5)
        `shouldBe` Just [NodeId 1, NodeId 4]
      NodeAccess.lookupStructuralChildren sampleConstraint (NodeId 6)
        `shouldBe` Just [NodeId 0, NodeId 1]
      NodeAccess.lookupStructuralChildren sampleConstraint (NodeId 99)
        `shouldBe` Nothing
      NodeAccess.lookupStructuralChildrenWithBounds sampleConstraint (NodeId 0)
        `shouldBe` Just [NodeId 1]
      NodeAccess.lookupStructuralChildrenWithBounds sampleConstraint (NodeId 7)
        `shouldBe` Just [NodeId 2]

  describe "bound and binding-parent lookup" $ do
    it "distinguishes variable bounds, non-variables, and missing nodes" $ do
      NodeAccess.lookupVarBound sampleConstraint (NodeId 0) `shouldBe` Just (NodeId 1)
      NodeAccess.lookupVarBound sampleConstraint (NodeId 1) `shouldBe` Nothing
      NodeAccess.lookupVarBound sampleConstraint (NodeId 99) `shouldBe` Nothing
      NodeAccess.lookupVarBoundSafe sampleConstraint (NodeId 0)
        `shouldBe` Right (Just (NodeId 1))
      NodeAccess.lookupVarBoundSafe sampleConstraint (NodeId 1)
        `shouldBe` Left "Node is not a variable: 1"
      NodeAccess.lookupVarBoundSafe sampleConstraint (NodeId 99)
        `shouldBe` Left "Node not found: 99"

    it "returns type and gen binding parents with explicit missing-parent errors" $ do
      NodeAccess.lookupBindParent sampleConstraint (typeRef (NodeId 0))
        `shouldBe` Just (typeRef (NodeId 3), BindFlex)
      NodeAccess.lookupBindParentSafe sampleConstraint (genRef (GenNodeId 0))
        `shouldBe` Right (typeRef (NodeId 3), BindFlex)
      NodeAccess.lookupBindParent sampleConstraint (typeRef (NodeId 99))
        `shouldBe` Nothing
      NodeAccess.lookupBindParentSafe sampleConstraint (typeRef (NodeId 99))
        `shouldBe` Left ("No binding parent for node: " ++ show (nodeRefKey (typeRef (NodeId 99))))

  describe "batch and collection accessors" $ do
    it "looks up only present nodes or substitutes a default for missing nodes" $ do
      let fallback = TyBottom (NodeId 99)
      NodeAccess.lookupNodes sampleConstraint [NodeId 0, NodeId 99, NodeId 1]
        `shouldBe` [TyVar (NodeId 0) (Just (NodeId 1)), TyBase (NodeId 1) (BaseTy "Int")]
      NodeAccess.lookupNodesWithDefault sampleConstraint fallback [NodeId 99, NodeId 1]
        `shouldBe` [fallback, TyBase (NodeId 1) (BaseTy "Int")]

    it "returns stable node and gen-node collections" $ do
      NodeAccess.allNodes sampleConstraint `shouldBe` sampleNodes
      NodeAccess.allGenNodes sampleConstraint `shouldBe` [sampleGen]
      NodeAccess.allNodeIds sampleConstraint `shouldBe` IntSet.fromList [0 .. 8]
      NodeAccess.allGenNodeIds sampleConstraint `shouldBe` IntSet.singleton 0
