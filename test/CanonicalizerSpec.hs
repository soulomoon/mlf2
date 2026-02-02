{-# LANGUAGE ScopedTypeVariables #-}
module CanonicalizerSpec (spec) where

import Control.Monad (forM)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Test.Hspec
import Test.QuickCheck

import MLF.Constraint.Canonicalizer
import MLF.Constraint.Presolution (CopyMapping(..), EdgeTrace(..), fromListInterior, toListInterior)
import MLF.Constraint.Types.Graph (EdgeId(..), NodeId(..), getNodeId)
import MLF.Constraint.Types.Witness
    ( BoundRef(..)
    , EdgeWitness(..)
    , Expansion(..)
    , ForallSpec(..)
    , InstanceOp(..)
    , InstanceStep(..)
    , InstanceWitness(..)
    )
import MLF.Elab.Run.Util (canonicalizeExpansion, canonicalizeTrace, canonicalizeWitness)

newtype UnionFindMap = UnionFindMap (IntMap.IntMap NodeId)
    deriving (Show)

newtype RedirectMap = RedirectMap (IntMap.IntMap NodeId)
    deriving (Show)

instance Arbitrary UnionFindMap where
    arbitrary = UnionFindMap <$> genUnionFindMap

instance Arbitrary RedirectMap where
    arbitrary = RedirectMap <$> genRedirectMap

spec :: Spec
spec = describe "MLF.Constraint.Canonicalizer" $ do
    it "redirect cycles pick a stable representative" $ do
        let redirects = IntMap.fromList
                [ (1, NodeId 2)
                , (2, NodeId 1)
                ]
            canon = makeCanonicalizer IntMap.empty redirects
        canonicalizeNode canon (NodeId 1) `shouldBe` NodeId 1
        canonicalizeNode canon (NodeId 2) `shouldBe` NodeId 1
        chaseRedirectsStable redirects (NodeId 1) `shouldBe` NodeId 1
        chaseRedirectsStable redirects (NodeId 2) `shouldBe` NodeId 1

    it "canonicalization is idempotent" $ property $
        \(UnionFindMap uf) (RedirectMap redirects) ->
            let canon = makeCanonicalizer uf redirects
                nodes = nodeDomain uf redirects
            in all
                (\nid ->
                    let canonN = canonicalizeNode canon nid
                    in canonicalizeNode canon canonN == canonN
                )
                nodes

    describe "witness/trace/expansion canonicalization" $ do
        let redirects = IntMap.fromList
                [ (1, NodeId 2)
                , (2, NodeId 2)
                , (3, NodeId 4)
                , (4, NodeId 4)
                ]
            canon = makeCanonicalizer IntMap.empty redirects
            canonNode = canonicalizeNode canon

        it "canonicalizes edge witnesses" $ do
            let ops0 = [OpMerge (NodeId 1) (NodeId 2), OpWeaken (NodeId 3)]
                steps0 =
                    [ StepOmega (OpGraft (NodeId 1) (NodeId 3))
                    , StepIntro
                    , StepOmega (OpRaise (NodeId 2))
                    ]
                w0 = EdgeWitness
                    { ewEdgeId = EdgeId 0
                    , ewLeft = NodeId 1
                    , ewRight = NodeId 2
                    , ewRoot = NodeId 3
                    , ewSteps = steps0
                    , ewWitness = InstanceWitness ops0
                    }
                w1 = canonicalizeWitness canon w0
                mapOp op = case op of
                    OpGraft a b -> OpGraft (canonNode a) (canonNode b)
                    OpMerge a b -> OpMerge (canonNode a) (canonNode b)
                    OpRaise n -> OpRaise (canonNode n)
                    OpWeaken n -> OpWeaken (canonNode n)
                    OpRaiseMerge a b -> OpRaiseMerge (canonNode a) (canonNode b)
                mapStep step = case step of
                    StepOmega op -> StepOmega (mapOp op)
                    StepIntro -> StepIntro
            ewLeft w1 `shouldBe` canonNode (ewLeft w0)
            ewRight w1 `shouldBe` canonNode (ewRight w0)
            ewRoot w1 `shouldBe` canonNode (ewRoot w0)
            ewSteps w1 `shouldBe` map mapStep steps0
            let InstanceWitness ops1 = ewWitness w1
            ops1 `shouldBe` map mapOp ops0

        it "canonicalizes edge traces and expansions" $ do
            let tr0 = EdgeTrace
                    { etRoot = NodeId 1
                    , etBinderArgs = [(NodeId 1, NodeId 3), (NodeId 2, NodeId 4)]
                    , etInterior = fromListInterior [NodeId 1, NodeId 2, NodeId 3]
                    , etCopyMap = CopyMapping (IntMap.fromList [(1, NodeId 2), (3, NodeId 4)])
                    }
                tr1 = canonicalizeTrace canon tr0
                interior1 = IntSet.fromList (map getNodeId (toListInterior (etInterior tr1)))
                interiorExpected =
                    IntSet.fromList (map (getNodeId . canonNode) (toListInterior (etInterior tr0)))
            etRoot tr1 `shouldBe` canonNode (etRoot tr0)
            etBinderArgs tr1
                `shouldBe` map (\(a, b) -> (canonNode a, canonNode b)) (etBinderArgs tr0)
            interior1 `shouldBe` interiorExpected
            let CopyMapping cmap = etCopyMap tr1
            IntMap.lookup (getNodeId (canonNode (NodeId 1))) cmap
                `shouldBe` Just (canonNode (NodeId 2))
            IntMap.lookup (getNodeId (canonNode (NodeId 3))) cmap
                `shouldBe` Just (canonNode (NodeId 4))

            let spec0 = ForallSpec
                    { fsBinderCount = 1
                    , fsBounds = [Just (BoundNode (NodeId 1)), Just (BoundBinder 0)]
                    }
                exp0 =
                    ExpCompose (ExpForall (spec0 NE.:| []) NE.:| [ExpInstantiate [NodeId 3]])
                exp1 = canonicalizeExpansion canon exp0
            exp1
                `shouldBe`
                    ExpCompose
                        ( ExpForall
                            ( ForallSpec
                                { fsBinderCount = 1
                                , fsBounds =
                                    [ Just (BoundNode (canonNode (NodeId 1)))
                                    , Just (BoundBinder 0)
                                    ]
                                }
                                NE.:|
                                    []
                            )
                            NE.:|
                                [ ExpInstantiate [canonNode (NodeId 3)]
                                ]
                        )

nodeDomain :: IntMap.IntMap NodeId -> IntMap.IntMap NodeId -> [NodeId]
nodeDomain uf redirects =
    let keys = IntMap.keys uf ++ IntMap.keys redirects
        values = map getNodeId (IntMap.elems uf ++ IntMap.elems redirects)
        maxKey = maximum (0 : keys ++ values)
    in map NodeId [0 .. maxKey]

genUnionFindMap :: Gen (IntMap.IntMap NodeId)
genUnionFindMap = do
    size <- chooseInt (0, 8)
    if size == 0
        then pure IntMap.empty
        else do
            let keys = [0 .. size - 1]
            parents <- mapM (\k -> chooseInt (0, k)) keys
            pure (IntMap.fromList (zip keys (map NodeId parents)))

genRedirectMap :: Gen (IntMap.IntMap NodeId)
genRedirectMap = do
    size <- chooseInt (0, 8)
    if size == 0
        then pure IntMap.empty
        else do
            entries <- forM [0 .. size - 1] $ \k -> do
                include <- arbitrary
                if include
                    then do
                        target <- chooseInt (0, size - 1)
                        pure (Just (k, NodeId target))
                    else pure Nothing
            pure (IntMap.fromList (catMaybes entries))
