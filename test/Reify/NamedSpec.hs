module Reify.NamedSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Constraint.Finalize (presolutionViewFromSnapshot)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution
import MLF.Reify.Named (namedNodes, softenedBindParentsUnder)
import SpecUtil (emptyConstraint, nodeMapFromList)
import Test.Hspec

spec :: Spec
spec = describe "MLF.Reify.Named" $ do
  describe "namedNodes" $ do
    it "returns empty set for an empty constraint" $
      let c =
            emptyConstraint
              { cGenNodes = fromListGen [(GenNodeId 0, GenNode (GenNodeId 0) [])]
              }
          pv = presolutionViewFromSnapshot c IntMap.empty
       in namedNodes pv `shouldBe` Right IntSet.empty

    it "includes a TyVar directly under a gen root" $
      let varN = NodeId 1
          genId = GenNodeId 0
          nodes =
            nodeMapFromList
              [ (getNodeId varN, TyVar {tnId = varN, tnBound = Nothing})
              ]
          genNodes = fromListGen [(genId, GenNode genId [varN])]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef varN), (genRef genId, BindFlex))
              ]
          c =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = genNodes
              }
          pv = presolutionViewFromSnapshot c IntMap.empty
       in case namedNodes pv of
            Right s -> IntSet.member (getNodeId varN) s `shouldBe` True
            Left err -> expectationFailure ("namedNodes failed: " ++ show err)

    it "excludes a TyArrow directly under a gen root" $
      let arrowN = NodeId 1
          domN = NodeId 2
          codN = NodeId 3
          genId = GenNodeId 0
          nodes =
            nodeMapFromList
              [ (getNodeId arrowN, TyArrow {tnId = arrowN, tnDom = domN, tnCod = codN}),
                (getNodeId domN, TyVar {tnId = domN, tnBound = Nothing}),
                (getNodeId codN, TyVar {tnId = codN, tnBound = Nothing})
              ]
          genNodes = fromListGen [(genId, GenNode genId [arrowN])]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef arrowN), (genRef genId, BindFlex)),
                (nodeRefKey (typeRef domN), (typeRef arrowN, BindFlex)),
                (nodeRefKey (typeRef codN), (typeRef arrowN, BindFlex))
              ]
          c =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = genNodes
              }
          pv = presolutionViewFromSnapshot c IntMap.empty
       in case namedNodes pv of
            Right s -> IntSet.member (getNodeId arrowN) s `shouldBe` False
            Left err -> expectationFailure ("namedNodes failed: " ++ show err)

    it "excludes a TyVar not directly under a gen root" $
      let parentN = NodeId 1
          childVarN = NodeId 2
          genId = GenNodeId 0
          nodes =
            nodeMapFromList
              [ (getNodeId parentN, TyArrow {tnId = parentN, tnDom = childVarN, tnCod = childVarN}),
                (getNodeId childVarN, TyVar {tnId = childVarN, tnBound = Nothing})
              ]
          genNodes = fromListGen [(genId, GenNode genId [parentN])]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef parentN), (genRef genId, BindFlex)),
                (nodeRefKey (typeRef childVarN), (typeRef parentN, BindFlex))
              ]
          c =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = genNodes
              }
          pv = presolutionViewFromSnapshot c IntMap.empty
       in case namedNodes pv of
            Right s -> IntSet.member (getNodeId childVarN) s `shouldBe` False
            Left err -> expectationFailure ("namedNodes failed: " ++ show err)

  describe "softenedBindParentsUnder" $ do
    it "leaves non-weakened bind flags unchanged" $
      let varN = NodeId 1
          genId = GenNodeId 0
          nodes =
            nodeMapFromList
              [ (getNodeId varN, TyVar {tnId = varN, tnBound = Nothing})
              ]
          genNodes = fromListGen [(genId, GenNode genId [varN])]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef varN), (genRef genId, BindRigid))
              ]
          c =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = genNodes,
                cWeakenedVars = IntSet.empty
              }
       in case softenedBindParentsUnder id c of
            Right bp ->
              case IntMap.lookup (nodeRefKey (typeRef varN)) bp of
                Just (_, flag) -> flag `shouldBe` BindRigid
                Nothing -> expectationFailure "Expected entry in bind parents"
            Left err -> expectationFailure ("softenedBindParentsUnder failed: " ++ show err)

    it "softens a weakened rigid var to flex" $
      let varN = NodeId 1
          genId = GenNodeId 0
          nodes =
            nodeMapFromList
              [ (getNodeId varN, TyVar {tnId = varN, tnBound = Nothing})
              ]
          genNodes = fromListGen [(genId, GenNode genId [varN])]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef varN), (genRef genId, BindRigid))
              ]
          c =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = genNodes,
                cWeakenedVars = IntSet.singleton (getNodeId varN)
              }
       in case softenedBindParentsUnder id c of
            Right bp ->
              case IntMap.lookup (nodeRefKey (typeRef varN)) bp of
                Just (_, flag) -> flag `shouldBe` BindFlex
                Nothing -> expectationFailure "Expected entry in bind parents"
            Left err -> expectationFailure ("softenedBindParentsUnder failed: " ++ show err)

    it "does not soften a non-weakened rigid var" $
      let varA = NodeId 1
          varB = NodeId 2
          genId = GenNodeId 0
          nodes =
            nodeMapFromList
              [ (getNodeId varA, TyVar {tnId = varA, tnBound = Nothing}),
                (getNodeId varB, TyVar {tnId = varB, tnBound = Nothing})
              ]
          genNodes = fromListGen [(genId, GenNode genId [varA, varB])]
          bindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef varA), (genRef genId, BindRigid)),
                (nodeRefKey (typeRef varB), (genRef genId, BindRigid))
              ]
          c =
            emptyConstraint
              { cNodes = nodes,
                cBindParents = bindParents,
                cGenNodes = genNodes,
                cWeakenedVars = IntSet.singleton (getNodeId varA)
              }
       in case softenedBindParentsUnder id c of
            Right bp ->
              case IntMap.lookup (nodeRefKey (typeRef varB)) bp of
                Just (_, flag) -> flag `shouldBe` BindRigid
                Nothing -> expectationFailure "Expected entry in bind parents"
            Left err -> expectationFailure ("softenedBindParentsUnder failed: " ++ show err)
