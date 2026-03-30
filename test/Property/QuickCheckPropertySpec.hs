{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Property.QuickCheckPropertySpec (spec) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
-- Binding tree validation
import MLF.Binding.Tree (checkBindingTree)
-- Canonicalizer
import MLF.Constraint.Canonicalizer (canonicalizeNode, makeCanonicalizer)
-- Constraint graph types
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    BindFlag (..),
    Constraint (..),
    NodeId (..),
    NodeRef (..),
    TyNode (..),
    getNodeId,
    lookupGen,
    lookupNodeIn,
    nodeRefKey,
  )
import MLF.Constraint.Unify.Decompose (decomposeUnifyChildren)
-- Reification type operations
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarsType, substTypeCapture)
-- Elaboration types
import MLF.Types.Elab (BoundType, ElabType, Ty (..))
-- Test utilities
import SpecUtil (bindParentsFromPairs, emptyConstraint, nodeMapFromList, rootedConstraint)
import Test.Hspec
import Test.QuickCheck

-- ============================================================================
-- Generators
-- ============================================================================

-- | Sized generator for well-formed elaboration types (ElabType = Ty 'AllowVar).
genElabType :: Int -> Gen ElabType
genElabType n
  | n <= 0 =
      oneof
        [ TVar <$> elements ["a", "b", "c", "d", "e"],
          TBase . BaseTy <$> elements ["int", "bool"],
          pure TBottom
        ]
  | otherwise =
      oneof
        [ TVar <$> elements ["a", "b", "c", "d", "e"],
          TBase . BaseTy <$> elements ["int", "bool"],
          pure TBottom,
          TArrow <$> genElabType half <*> genElabType half,
          TForall
            <$> elements ["x", "y", "z"]
            <*> oneof [pure Nothing, Just <$> genBoundType half]
            <*> genElabType half,
          TMu <$> elements ["r", "s"] <*> genElabType half
        ]
  where
    half = n `div` 2

-- | Sized generator for bound types (BoundType = Ty 'NoTopVar).
-- Cannot contain top-level TVar.
genBoundType :: Int -> Gen BoundType
genBoundType n
  | n <= 0 =
      oneof
        [ TBase . BaseTy <$> elements ["int", "bool"],
          pure TBottom
        ]
  | otherwise =
      oneof
        [ TBase . BaseTy <$> elements ["int", "bool"],
          pure TBottom,
          TArrow <$> genElabType half <*> genElabType half,
          TForall
            <$> elements ["x", "y", "z"]
            <*> oneof [pure Nothing, Just <$> genBoundType half]
            <*> genElabType half,
          TMu <$> elements ["r", "s"] <*> genElabType half
        ]
  where
    half = n `div` 2

-- | Shrink ElabType by reducing to subterms.
shrinkElabType :: ElabType -> [ElabType]
shrinkElabType (TArrow a b) = [a, b]
shrinkElabType (TForall _ _ body) = [body]
shrinkElabType (TMu _ body) = [body]
shrinkElabType _ = []

-- ============================================================================
-- Canonicalization generators (duplicated from CanonicalizerSpec)
-- ============================================================================

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
      entries <-
        mapM
          ( \k -> do
              include <- arbitrary
              if include
                then do
                  target <- chooseInt (0, size - 1)
                  pure (Just (k, NodeId target))
                else pure Nothing
          )
          [0 .. size - 1]
      pure (IntMap.fromList (catMaybes entries))

nodeDomain :: IntMap.IntMap NodeId -> IntMap.IntMap NodeId -> [NodeId]
nodeDomain uf redirects =
  let keys = IntMap.keys uf ++ IntMap.keys redirects
      values = map getNodeId (IntMap.elems uf ++ IntMap.elems redirects)
      maxKey = maximum (0 : keys ++ values)
   in map NodeId [0 .. maxKey]

-- ============================================================================
-- Binding tree generators (adapted from BindingSpec)
-- ============================================================================

-- | Generate a valid binding constraint with n nodes.
-- Builds a chain of TyForall nodes: node 0 -> node 1 -> ... -> node (n-2) -> var(n-1)
-- Each child is bound to its structural parent.
genValidBindingConstraint :: Int -> Gen Constraint
genValidBindingConstraint n
  | n <= 0 = pure (rootedConstraint emptyConstraint)
  | n == 1 =
      pure $
        rootedConstraint
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [(0, TyVar {tnId = NodeId 0, tnBound = Nothing})]
            }
  | otherwise = do
      -- Generate a chain: forall(0) -> forall(1) -> ... -> forall(n-2) -> var(n-1)
      let nodes =
            nodeMapFromList $
              [(i, TyForall (NodeId i) (NodeId (i + 1))) | i <- [0 .. n - 2]]
                ++ [(n - 1, TyVar {tnId = NodeId (n - 1), tnBound = Nothing})]
      -- Generate binding parents: node i has parent i-1 (node 0 is root)
      flags <- vectorOf (n - 1) (elements [BindFlex, BindRigid])
      let bindParents =
            bindParentsFromPairs
              [ (NodeId i, NodeId (i - 1), flag)
              | (i, flag) <- zip [1 .. n - 1] flags
              ]
      pure $
        rootedConstraint
          emptyConstraint
            { cNodes = nodes,
              cBindParents = bindParents
            }

-- ============================================================================
-- Unification symmetry generators
-- ============================================================================

-- | Generate an arbitrary pair of TyNode values.
genTyNodePair :: Gen (TyNode, TyNode)
genTyNodePair = oneof [genSameHeadTyNodePair, genDifferentHeadTyNodePair]

-- | Generate a pair of TyNode values with the same head constructor.
genSameHeadTyNodePair :: Gen (TyNode, TyNode)
genSameHeadTyNodePair =
  oneof
    [ -- Both TyArrow
      do
        d1 <- genNodeId
        c1 <- genNodeId
        d2 <- genNodeId
        c2 <- genNodeId
        pure (TyArrow (NodeId 100) d1 c1, TyArrow (NodeId 101) d2 c2),
      -- Both TyForall
      do
        b1 <- genNodeId
        b2 <- genNodeId
        pure (TyForall (NodeId 100) b1, TyForall (NodeId 101) b2),
      -- Both TyBase (same base)
      do
        b <- elements [BaseTy "int", BaseTy "bool"]
        pure (TyBase (NodeId 100) b, TyBase (NodeId 101) b),
      -- Both TyBottom
      pure (TyBottom (NodeId 100), TyBottom (NodeId 101)),
      -- Both TyMu
      do
        b1 <- genNodeId
        b2 <- genNodeId
        pure (TyMu (NodeId 100) b1, TyMu (NodeId 101) b2)
    ]
  where
    genNodeId :: Gen NodeId
    genNodeId = NodeId <$> chooseInt (0, 50)

-- | Generate a pair of TyNode values with different head constructors.
genDifferentHeadTyNodePair :: Gen (TyNode, TyNode)
genDifferentHeadTyNodePair = do
  let nid1 = NodeId 100
      nid2 = NodeId 101
  let nodes =
        [ TyArrow nid1 (NodeId 0) (NodeId 1),
          TyForall nid1 (NodeId 0),
          TyBase nid1 (BaseTy "int"),
          TyBottom nid1,
          TyMu nid1 (NodeId 0),
          TyVar {tnId = nid1, tnBound = Nothing}
        ]
  idx1 <- chooseInt (0, length nodes - 1)
  idx2 <- chooseInt (0, length nodes - 2)
  let n1 = nodes !! idx1
      -- Ensure different index by shifting if >= idx1
      idx2' = if idx2 >= idx1 then idx2 + 1 else idx2
      nodes2 =
        [ TyArrow nid2 (NodeId 2) (NodeId 3),
          TyForall nid2 (NodeId 2),
          TyBase nid2 (BaseTy "bool"),
          TyBottom nid2,
          TyMu nid2 (NodeId 2),
          TyVar {tnId = nid2, tnBound = Nothing}
        ]
      n2 = nodes2 !! idx2'
  pure (n1, n2)

-- ============================================================================
-- Spec
-- ============================================================================

spec :: Spec
spec = describe "Property.QuickCheckProperty" $ do
  reificationProperties
  canonicalizationProperties
  bindingTreeProperties
  unificationSymmetryProperties

-- ============================================================================
-- Deliverable (a): Reification round-trip well-formedness
-- ============================================================================

reificationProperties :: Spec
reificationProperties = describe "Reification round-trip well-formedness" $ do
  it "substTypeCapture preserves free-variable well-formedness" $
    property $
      forAllShrink (sized genElabType) shrinkElabType $ \ty ->
        forAll (elements ["a", "b", "c", "d", "e"]) $ \v ->
          forAllShrink (sized genElabType) shrinkElabType $ \s ->
            let result = substTypeCapture v s ty
                fvResult = freeTypeVarsType result
                expected = Set.union (Set.delete v (freeTypeVarsType ty)) (freeTypeVarsType s)
             in fvResult `Set.isSubsetOf` expected

  it "alphaEqType is reflexive" $
    property $
      forAllShrink (sized genElabType) shrinkElabType $ \ty ->
        alphaEqType ty ty === True

  it "alphaEqType is symmetric" $
    property $
      forAllShrink (sized genElabType) shrinkElabType $ \ty1 ->
        forAllShrink (sized genElabType) shrinkElabType $ \ty2 ->
          alphaEqType ty1 ty2 === alphaEqType ty2 ty1

  it "substituting a variable not free in a type is identity" $
    property $
      forAllShrink (sized genElabType) shrinkElabType $ \ty ->
        forAllShrink (sized genElabType) shrinkElabType $ \s ->
          let used = freeTypeVarsType ty
              fresh = case filter (`Set.notMember` used) ["q0", "q1", "q2", "q3", "q4"] of
                (v : _) -> v
                [] -> "q99" -- unreachable: only 5 var names in genElabType
           in substTypeCapture fresh s ty === ty

-- ============================================================================
-- Deliverable (b): Canonicalization idempotency
-- ============================================================================

canonicalizationProperties :: Spec
canonicalizationProperties = describe "Canonicalization idempotency" $ do
  it "canonicalization is idempotent (makeCanonicalizer)" $
    withMaxSuccess 200 $
      property $
        forAll genUnionFindMap $ \uf ->
          forAll genRedirectMap $ \redirects ->
            let canon = makeCanonicalizer uf redirects
                nodes = nodeDomain uf redirects
             in all
                  ( \nid ->
                      let c = canonicalizeNode canon nid
                       in canonicalizeNode canon c == c
                  )
                  nodes

  it "canonicalization is stable (triple-apply)" $
    withMaxSuccess 200 $
      property $
        forAll genUnionFindMap $ \uf ->
          forAll genRedirectMap $ \redirects ->
            let canon = makeCanonicalizer uf redirects
                nodes = nodeDomain uf redirects
             in all
                  ( \nid ->
                      let c1 = canonicalizeNode canon nid
                          c2 = canonicalizeNode canon c1
                          c3 = canonicalizeNode canon c2
                       in c1 == c2 && c2 == c3
                  )
                  nodes

  it "canonicalization is deterministic" $
    withMaxSuccess 200 $
      property $
        forAll genUnionFindMap $ \uf ->
          forAll genRedirectMap $ \redirects ->
            let canon1 = makeCanonicalizer uf redirects
                canon2 = makeCanonicalizer uf redirects
                nodes = nodeDomain uf redirects
             in all (\nid -> canonicalizeNode canon1 nid == canonicalizeNode canon2 nid) nodes

-- ============================================================================
-- Deliverable (c): Binding tree invariant preservation
-- ============================================================================

bindingTreeProperties :: Spec
bindingTreeProperties = describe "Binding tree invariant preservation" $ do
  it "genValidBindingConstraint always produces valid binding trees" $
    property $
      forAll (choose (1, 20)) $ \n -> ioProperty $ do
        c <- generate (genValidBindingConstraint n)
        case checkBindingTree c of
          Right () -> pure (property True)
          Left err -> pure (counterexample (show err) (property False))

  it "every child in binding tree has a valid parent node" $
    property $
      forAll (choose (1, 20)) $ \n -> ioProperty $ do
        c <- generate (genValidBindingConstraint n)
        let bp = cBindParents c
            nodeExists :: NodeRef -> Bool
            nodeExists (TypeRef nid) =
              case lookupNodeIn (cNodes c) nid of
                Just _ -> True
                Nothing -> False
            nodeExists (GenRef gid) =
              case lookupGen gid (cGenNodes c) of
                Just _ -> True
                Nothing -> False
            allValid =
              all
                (\(parentRef, _flag) -> nodeExists parentRef)
                (IntMap.elems bp)
        pure
          ( counterexample
              ("BindParents: " ++ show bp)
              (property allValid)
          )

  it "binding tree has no cycles" $
    property $
      forAll (choose (1, 20)) $ \n -> ioProperty $ do
        c <- generate (genValidBindingConstraint n)
        let bp = cBindParents c
            terminates :: Int -> Bool
            terminates start = go IntSet.empty start
              where
                go visited key
                  | IntSet.member key visited = False -- cycle!
                  | otherwise = case IntMap.lookup key bp of
                      Nothing -> True -- root
                      Just (parent, _) -> go (IntSet.insert key visited) (nodeRefKey parent)
            allTerminate = all terminates (IntMap.keys bp)
        pure
          ( counterexample
              ("BindParents: " ++ show bp)
              (property allTerminate)
          )

  it "rootedConstraint preserves binding tree validity" $
    property $
      forAll (choose (1, 15)) $ \n -> ioProperty $ do
        c <- generate (genValidBindingConstraint n)
        -- genValidBindingConstraint already calls rootedConstraint,
        -- but re-wrapping should be safe
        let rc = rootedConstraint c
        case checkBindingTree rc of
          Right () -> pure (property True)
          Left err -> pure (counterexample ("rooted constraint invalid: " ++ show err) (property False))

-- ============================================================================
-- Deliverable (d): Unification symmetry
-- ============================================================================

unificationSymmetryProperties :: Spec
unificationSymmetryProperties = describe "Unification symmetry" $ do
  it "decomposeUnifyChildren is symmetric" $
    property $
      forAll genTyNodePair $ \(n1, n2) ->
        case (decomposeUnifyChildren n1 n2, decomposeUnifyChildren n2 n1) of
          (Right edges1, Right edges2) ->
            length edges1 === length edges2
          (Left _, Left _) -> property True
          _ -> property False

  it "decomposeUnifyChildren succeeds for same-head nodes" $
    property $
      forAll genSameHeadTyNodePair $ \(n1, n2) ->
        case decomposeUnifyChildren n1 n2 of
          Right _ -> property True
          Left err -> counterexample (show err) (property False)

  it "decomposeUnifyChildren fails for different-head nodes" $
    property $
      forAll genDifferentHeadTyNodePair $ \(n1, n2) ->
        case decomposeUnifyChildren n1 n2 of
          Left _ -> property True
          Right edges -> counterexample ("unexpected success: " ++ show edges) (property False)
