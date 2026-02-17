module Phi.IdentityBridgeSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Test.Hspec

import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , CopyMapping(..)
    , InteriorNodes(..)
    )
import MLF.Elab.Phi.IdentityBridge

-- | Trivial canonical function: identity.
idCanonical :: NodeId -> NodeId
idCanonical = id

-- | Canonical that merges node 20 -> node 10.
mergeCanonical :: NodeId -> NodeId
mergeCanonical (NodeId 20) = NodeId 10
mergeCanonical nid = nid

-- | Helper to build a minimal EdgeTrace.
mkTrace :: [(Int, Int)] -> [(Int, Int)] -> EdgeTrace
mkTrace binderArgs copyPairs =
    EdgeTrace
        { etRoot = NodeId 0
        , etBinderArgs = [(NodeId b, NodeId a) | (b, a) <- binderArgs]
        , etInterior = InteriorNodes IntSet.empty
        , etBinderReplayHints = mempty
        , etCopyMap = CopyMapping (IntMap.fromList [(k, NodeId v) | (k, v) <- copyPairs])
        }

spec :: Spec
spec = describe "IdentityBridge" $ do

    -- ---------------------------------------------------------------
    -- Source key de-duplication
    -- ---------------------------------------------------------------
    describe "sourceKeysForNode de-duplication" $ do
        it "returns a single key when raw == canonical and no copies" $ do
            let ib = mkIdentityBridge idCanonical Nothing IntMap.empty
            sourceKeysForNode ib (NodeId 5) `shouldBe` [5]

        it "de-duplicates when canonical collapses two ids" $ do
            let ib = mkIdentityBridge mergeCanonical Nothing IntMap.empty
            -- Node 20 canonicalises to 10; raw=20, canon=10 -> two distinct keys
            let keys = sourceKeysForNode ib (NodeId 20)
            -- Both 20 and 10 should appear, each exactly once
            length keys `shouldBe` 2
            IntSet.fromList keys `shouldBe` IntSet.fromList [10, 20]

        it "de-duplicates copy-map reverse entries" $ do
            -- copy map: 30 -> NodeId 5 (source 30 was copied to 5)
            let copyMap = IntMap.fromList [(30, NodeId 5)]
                ib = mkIdentityBridge idCanonical Nothing copyMap
                keys = sourceKeysForNode ib (NodeId 5)
            -- raw=5, canon=5, reverseCopy gives [30]
            IntSet.fromList keys `shouldBe` IntSet.fromList [5, 30]
            -- No duplicates
            length keys `shouldBe` length (IntSet.toList (IntSet.fromList keys))

    -- ---------------------------------------------------------------
    -- Trace-order priority over numeric order
    -- ---------------------------------------------------------------
    describe "trace-order priority" $ do
        it "ranks keys by trace binder order, not numeric order" $ do
            -- Trace has binders [50, 10, 30] in that order
            let tr = mkTrace [(50, 100), (10, 101), (30, 102)] []
                ib = mkIdentityBridge idCanonical (Just tr) IntMap.empty
            -- For node 50: raw=50, canon=50 -> keys=[50], trace-order ix=0
            -- For node 10: raw=10, canon=10 -> keys=[10], trace-order ix=1
            -- For node 30: raw=30, canon=30 -> keys=[30], trace-order ix=2
            -- sourceKeysForNode should return them in trace order
            sourceKeysForNode ib (NodeId 50) `shouldBe` [50]
            sourceKeysForNode ib (NodeId 10) `shouldBe` [10]
            sourceKeysForNode ib (NodeId 30) `shouldBe` [30]

        it "sorts multiple source keys by trace order" $ do
            -- Trace: binder 20 at ix=0, binder 10 at ix=1
            -- Canonical merges 20 -> 10
            -- So for node 20: raw=20, canon=10, traceCanon for canon 10 = [20, 10]
            -- After dedup: [20, 10], sorted by trace order: 20 (ix=0), 10 (ix=1)
            let tr = mkTrace [(20, 200), (10, 201)] []
                ib = mkIdentityBridge mergeCanonical (Just tr) IntMap.empty
                keys = sourceKeysForNode ib (NodeId 20)
            -- 20 has trace-order 0, 10 has trace-order 1 -> 20 first
            case keys of
                (k:_) -> k `shouldBe` 20
                []     -> expectationFailure "expected non-empty keys"

        it "puts non-trace keys after trace keys" $ do
            -- Trace only mentions binder 10 (ix=0)
            -- Copy map: 30 -> NodeId 10 (so reverseCopy for canon 10 includes 30)
            let tr = mkTrace [(10, 100)] []
                copyMap = IntMap.fromList [(30, NodeId 10)]
                ib = mkIdentityBridge idCanonical (Just tr) copyMap
                keys = sourceKeysForNode ib (NodeId 10)
            -- raw=10, canon=10, reverseCopy=[30], reverseTrace=[10]
            -- After dedup: [10, 30]
            -- 10 has trace-order 0, 30 has maxBound -> 10 first
            case keys of
                (k:_) -> k `shouldBe` 10
                []     -> expectationFailure "expected non-empty keys"
            keys `shouldSatisfy` (30 `elem`)

    -- ---------------------------------------------------------------
    -- Canonical alias fallback ordering
    -- ---------------------------------------------------------------
    describe "canonical alias fallback" $ do
        it "includes inverse copy map entries" $ do
            -- invCopyMap: if copyMap has 40 -> NodeId 5, then inv has 5 -> NodeId 40
            let copyMap = IntMap.fromList [(40, NodeId 5)]
                ib = mkIdentityBridge idCanonical Nothing copyMap
                keys = sourceKeysForNode ib (NodeId 5)
            -- raw=5, canon=5, invRaw for 5 -> Just 40, reverseCopy for 5 -> [40]
            -- After dedup: [5, 40]
            IntSet.fromList keys `shouldBe` IntSet.fromList [5, 40]

        it "falls back to numeric order when no trace" $ do
            let copyMap = IntMap.fromList [(40, NodeId 5), (30, NodeId 5)]
                ib = mkIdentityBridge idCanonical Nothing copyMap
                keys = sourceKeysForNode ib (NodeId 5)
            -- No trace -> all keys get (maxBound, key) rank -> sorted by key ascending
            -- After dedup: [5, 40, 30] or similar, sorted by numeric key
            let traceAbsentKeys = filter (/= 5) keys
            traceAbsentKeys `shouldBe` [30, 40]  -- numeric ascending

    -- ---------------------------------------------------------------
    -- sourceBinderKeysForNode
    -- ---------------------------------------------------------------
    describe "sourceBinderKeysForNode" $ do
        it "filters to binder key set membership" $ do
            let copyMap = IntMap.fromList [(30, NodeId 10)]
                ib = mkIdentityBridge idCanonical Nothing copyMap
                binderKeys = IntSet.fromList [10]
                keys = sourceBinderKeysForNode ib binderKeys (NodeId 10)
            keys `shouldBe` [10]

        it "returns empty when no source key is a binder" $ do
            let ib = mkIdentityBridge idCanonical Nothing IntMap.empty
                binderKeys = IntSet.fromList [99]
            sourceBinderKeysForNode ib binderKeys (NodeId 5) `shouldBe` []

    -- ---------------------------------------------------------------
    -- isBinderNode
    -- ---------------------------------------------------------------
    describe "isBinderNode" $ do
        it "returns True when a source key is in the binder set" $ do
            let ib = mkIdentityBridge idCanonical Nothing IntMap.empty
                binderKeys = IntSet.fromList [5]
            isBinderNode ib binderKeys (NodeId 5) `shouldBe` True

        it "returns True via canonical alias" $ do
            -- Node 20 canonicalises to 10; binder set has 10
            let ib = mkIdentityBridge mergeCanonical Nothing IntMap.empty
                binderKeys = IntSet.fromList [10]
            isBinderNode ib binderKeys (NodeId 20) `shouldBe` True

        it "returns False when no source key matches" $ do
            let ib = mkIdentityBridge idCanonical Nothing IntMap.empty
                binderKeys = IntSet.fromList [99]
            isBinderNode ib binderKeys (NodeId 5) `shouldBe` False

    -- ---------------------------------------------------------------
    -- lookupBinderIndex
    -- ---------------------------------------------------------------
    describe "lookupBinderIndex" $ do

        it "exact source-key match at spine position 0 returns Just 0" $ do
            let ib = mkIdentityBridge idCanonical Nothing IntMap.empty
                binderKeys = IntSet.fromList [10]
                spine = [Just (NodeId 10)]
            lookupBinderIndex ib binderKeys spine (NodeId 10) `shouldBe` Just 0

        it "exact match beats canonical alias match" $ do
            -- canonical: 30 -> 10, 20 -> 10
            -- binderKeys = {20, 30}, target = NodeId 30
            -- targetKeys = [30] (only 30 in binderKeys ∩ sourceKeys(30))
            -- Spine pos 0 (node 20): alias match only (canonical 30 == canonical 20 == 10)
            -- Spine pos 1 (node 30): exact match (30 in sourceKeys(30))
            -- Exact (matchClass 0) at pos 1 beats alias (matchClass 1) at pos 0
            let threeWayCanonical (NodeId 30) = NodeId 10
                threeWayCanonical (NodeId 20) = NodeId 10
                threeWayCanonical nid = nid
                ib = mkIdentityBridge threeWayCanonical Nothing IntMap.empty
                binderKeys = IntSet.fromList [20, 30]
                spine = [Just (NodeId 20), Just (NodeId 30)]
            lookupBinderIndex ib binderKeys spine (NodeId 30) `shouldBe` Just 1

        it "deterministic tie-break by trace order when two positions have exact matches" $ do
            -- Trace: binder 20 at ix=0, binder 10 at ix=1
            -- Copy map: 20 -> NodeId 10 (so sourceKeys(10) includes both 10 and 20)
            -- Target = NodeId 10, targetKeys = [10, 20]
            -- Spine pos 0 (node 10): exact matches [10, 20], chooseBestKey -> 20 (trace ix=0)
            -- Spine pos 1 (node 20): exact match [20], key 20
            -- Rank: (0, (0,20), 0) vs (0, (0,20), 1) -> pos 0 wins by index
            let tr = mkTrace [(20, 200), (10, 201)] [(20, 10)]
                copyMap = IntMap.fromList [(20, NodeId 10)]
                ib = mkIdentityBridge idCanonical (Just tr) copyMap
                binderKeys = IntSet.fromList [10, 20]
                spine = [Just (NodeId 10), Just (NodeId 20)]
            lookupBinderIndex ib binderKeys spine (NodeId 10) `shouldBe` Just 0

        it "no match returns Nothing" $ do
            -- Target is a binder node but no spine position shares its key.
            let ib = mkIdentityBridge idCanonical Nothing IntMap.empty
                binderKeys = IntSet.fromList [10, 50]
                spine = [Just (NodeId 50), Nothing]
            lookupBinderIndex ib binderKeys spine (NodeId 10) `shouldBe` Nothing

        it "non-binder node returns Nothing (isBinderNode gate)" $ do
            let ib = mkIdentityBridge idCanonical Nothing IntMap.empty
                binderKeys = IntSet.fromList [99]
                spine = [Just (NodeId 5)]
            lookupBinderIndex ib binderKeys spine (NodeId 5) `shouldBe` Nothing
