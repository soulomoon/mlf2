{-# LANGUAGE GADTs #-}
module ExpansionMinimalitySpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck
    ( Gen
    , counterexample
    , elements
    , forAll
    , frequency
    , property
    , sized
    , withMaxSuccess
    )

import MLF.Constraint.Presolution (PresolutionResult(..))
import MLF.Constraint.Types.Witness (Expansion(..))
import MLF.Frontend.Syntax (Expr(..), Lit(..), SurfaceExpr)
import SpecUtil (runToPresolutionDefault)

spec :: Spec
spec = describe "Expansion minimality" $ do
    it "pipeline expansions are structurally minimal" $
        property $
            withMaxSuccess 200 $
                forAll genSurfaceExpr $ \expr ->
                    case runToPresolutionDefault Set.empty expr of
                        Left _ -> property True
                        Right pres ->
                            let exps = IntMap.elems (prEdgeExpansions pres)
                                bad = filter (not . isMinimal) exps
                            in counterexample
                                ("Non-minimal expansions: " ++ show bad)
                                (null bad)

    it "identity expression gets ExpIdentity" $
        case runToPresolutionDefault Set.empty (ELam "x" (EVar "x")) of
            Left err -> expectationFailure $ "pipeline: " ++ err
            Right pres ->
                all isIdentityExp (IntMap.elems (prEdgeExpansions pres))
                    `shouldBe` True

    it "let-polymorphic use gets ExpInstantiate" $ do
        let expr = ELet "id" (ELam "x" (EVar "x"))
                    (EApp (EVar "id") (ELit (LInt 1)))
        case runToPresolutionDefault Set.empty expr of
            Left err -> expectationFailure $ "pipeline: " ++ err
            Right pres ->
                any isInstantiateExp (IntMap.elems (prEdgeExpansions pres))
                    `shouldBe` True

-- | An expansion is minimal if compose has ≥2 parts.
isMinimal :: Expansion -> Bool
isMinimal ExpIdentity = True
isMinimal (ExpInstantiate _) = True
isMinimal (ExpForall _) = True
isMinimal (ExpCompose exps) = length exps >= 2

isIdentityExp :: Expansion -> Bool
isIdentityExp ExpIdentity = True
isIdentityExp _ = False

isInstantiateExp :: Expansion -> Bool
isInstantiateExp (ExpInstantiate _) = True
isInstantiateExp _ = False

-- | Generator: small surface expressions covering lambda, app, let, lit.
genSurfaceExpr :: Gen SurfaceExpr
genSurfaceExpr = sized go
  where
    vars :: [String]
    vars = ["x", "y", "z"]

    go :: Int -> Gen SurfaceExpr
    go n
        | n <= 0 = frequency
            [ (3, ELit . LInt <$> elements [0..5])
            , (2, ELit . LBool <$> elements [True, False])
            , (2, EVar <$> elements vars)
            ]
        | otherwise =
            let half = n `div` 2
            in frequency
                [ (3, ELit . LInt <$> elements [0..5])
                , (2, EVar <$> elements vars)
                , (2, ELam <$> elements vars <*> go (n - 1))
                , (2, EApp <$> go half <*> go half)
                , (1, ELet <$> elements vars <*> go half <*> go half)
                ]
