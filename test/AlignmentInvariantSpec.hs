module AlignmentInvariantSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Set (Set)
import qualified Data.Set as Set

import MLF.Constraint.Types.Graph
    ( TyNode(..), NodeId(..), getNodeId, toListNode, cNodes )
import MLF.Constraint.Presolution (PresolutionResult(..), EdgeTrace(..))
import MLF.Constraint.Types.Witness (EdgeWitness(..), InstanceWitness(..), InstanceOp(..))
import qualified MLF.Constraint.Solved as Solved
import MLF.Frontend.Syntax (SurfaceExpr(..))
import SpecUtil

spec :: Spec
spec = describe "Thesis alignment invariants" $ do
    pure ()
