{-# LANGUAGE ScopedTypeVariables #-}
module CanonicalizerSpec (spec) where

import Control.Monad (forM, forM_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NE
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Test.Hspec
import Test.QuickCheck

import MLF.Constraint.Canonicalizer
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.TestSupport (CopyMapping(..), fromListInterior)
import MLF.Constraint.Types.Graph (EdgeId(..), NodeId(..))
import MLF.Constraint.Types.Witness
    ( BoundRef(..)
    , EdgeWitness(..)
    , Expansion(..)
    , ForallSpec(..)
    , InstanceOp(..)
    , InstanceWitness(..)
    , ReplayContract(..)
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
    it "canonicalizer guard: canonicalizeRef stays retired" $ do
        src <- readFile "src/MLF/Constraint/Canonicalizer.hs"
        forM_
            [ "canonicalizeRef,"
            , "canonicalizeRef ::"
            ] $ \marker ->
                src `shouldSatisfy` (not . isInfixOf marker)

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

        -- Source-ID contract: keep witness/trace provenance in source domain.
        -- Canonicalization is only for structural lookup fields.
        it "canonicalizes witness structural fields and preserves source-domain witness ops" $ do
            let ops0 = [OpMerge (NodeId 1) (NodeId 2), OpWeaken (NodeId 3)]
                w0 = EdgeWitness
                    { ewEdgeId = EdgeId 0
                    , ewLeft = NodeId 1
                    , ewRight = NodeId 2
                    , ewRoot = NodeId 3
                    , ewForallIntros = 1
                    , ewWitness = InstanceWitness ops0
                    }
                w1 = canonicalizeWitness canon w0
            ewLeft w1 `shouldBe` canonNode (ewLeft w0)
            ewRight w1 `shouldBe` canonNode (ewRight w0)
            ewRoot w1 `shouldBe` canonNode (ewRoot w0)
            ewForallIntros w1 `shouldBe` 1
            let InstanceWitness ops1 = ewWitness w1
            ops1 `shouldBe` ops0

        it "canonicalizes trace root and preserves source-domain provenance fields" $ do
            let tr0 = EdgeTrace
                    { etRoot = NodeId 1
                    , etBinderArgs = [(NodeId 1, NodeId 3), (NodeId 2, NodeId 4)]
                    , etInterior = fromListInterior [NodeId 1, NodeId 2, NodeId 3]
                    , etBinderReplayMap = mempty
                    , etCopyMap = CopyMapping (IntMap.fromList [(1, NodeId 2), (3, NodeId 4)])
                    , etReplayContract = ReplayContractNone
                    }
                tr1 = canonicalizeTrace canon tr0
            etRoot tr1 `shouldBe` canonNode (etRoot tr0)
            etBinderArgs tr1 `shouldBe` etBinderArgs tr0
            etInterior tr1 `shouldBe` etInterior tr0
            let CopyMapping cmap = etCopyMap tr1
            cmap `shouldBe` IntMap.fromList [(1, NodeId 2), (3, NodeId 4)]

        it "canonicalizes expansions" $ do
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
