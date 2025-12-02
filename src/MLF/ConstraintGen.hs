module MLF.ConstraintGen (
    ConstraintError (..),
    ConstraintResult (..),
    generateConstraints
) where

import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import MLF.Syntax
import MLF.Types

-- | Errors that can surface during constraint generation.
data ConstraintError
    = UnknownVariable VarName
    deriving (Eq, Show)

-- | Successful constraint generation returns the full constraint graph and the
-- root 'NodeId' that represents the program's type.
data ConstraintResult = ConstraintResult
    { crConstraint :: Constraint
    , crRoot :: NodeId
    }
    deriving (Eq, Show)

data Binding = Binding
    { bindingNode :: NodeId
    , bindingLevel :: GNodeId
    }

type Env = Map VarName Binding

data BuildState = BuildState
    { bsNextNode :: !Int
    , bsNextGNode :: !Int
    , bsNextExpVar :: !Int
    , bsNextEdge :: !Int
    , bsNodes :: !(IntMap TyNode)
    , bsGNodes :: !(IntMap GNode)
    , bsForest :: ![GNodeId]
    , bsInstEdges :: ![InstEdge]
    , bsUnifyEdges :: ![UnifyEdge]
    , bsRootLevel :: !GNodeId
    }

type ConstraintM = StateT BuildState (Except ConstraintError)

runConstraintM :: ConstraintM a -> BuildState -> Either ConstraintError (a, BuildState)
runConstraintM action st = runExcept (runStateT action st)

generateConstraints :: Expr -> Either ConstraintError ConstraintResult
generateConstraints expr = do
    let initialState = mkInitialState
        rootLevel = bsRootLevel initialState
    (rootNode, finalState) <- runConstraintM (buildExpr Map.empty rootLevel expr) initialState
    let constraint = buildConstraint finalState
    pure ConstraintResult
        { crConstraint = constraint
        , crRoot = rootNode
        }

mkInitialState :: BuildState
mkInitialState = BuildState
    { bsNextNode = 0
    , bsNextGNode = 1
    , bsNextExpVar = 0
    , bsNextEdge = 0
    , bsNodes = IntMap.empty
    , bsGNodes = IntMap.singleton (intFromG rootLevel) rootNode
    , bsForest = [rootLevel]
    , bsInstEdges = []
    , bsUnifyEdges = []
    , bsRootLevel = rootLevel
    }
  where
    rootLevel = GNodeId 0
    rootNode = GNode
        { gnodeId = rootLevel
        , gParent = Nothing
        , gBinds = []
        , gChildren = []
        }

buildConstraint :: BuildState -> Constraint
buildConstraint st = Constraint
    { cGForest = bsForest st
    , cGNodes = bsGNodes st
    , cNodes = bsNodes st
    , cInstEdges = reverse (bsInstEdges st)
    , cUnifyEdges = reverse (bsUnifyEdges st)
    }

buildExpr :: Env -> GNodeId -> Expr -> ConstraintM NodeId
buildExpr env level expr = case expr of
    EVar name -> lookupVar env name
    ELit lit -> allocBase (baseFor lit)
    ELam param body -> do
        argNode <- allocVar level
        let env' = Map.insert param (Binding argNode level) env
        bodyNode <- buildExpr env' level body
        allocArrow argNode bodyNode
    EApp fun arg -> do
        funNode <- buildExpr env level fun
        argNode <- buildExpr env level arg
        resultNode <- allocVar level
        arrowNode <- allocArrow argNode resultNode
        addInstEdge funNode arrowNode
        pure resultNode
    ELet name rhs body -> do
        childLevel <- newChildLevel (Just level)
        rhsNode <- buildExpr env childLevel rhs
        schemeNode <- allocExpNode rhsNode
        let env' = Map.insert name (Binding schemeNode level) env
        buildExpr env' level body

lookupVar :: Env -> VarName -> ConstraintM NodeId
lookupVar env name = case Map.lookup name env of
    Just binding -> pure (bindingNode binding)
    Nothing -> throwError (UnknownVariable name)

baseFor :: Lit -> BaseTy
baseFor lit = BaseTy $ case lit of
    LInt _ -> "Int"
    LBool _ -> "Bool"
    LString _ -> "String"

allocVar :: GNodeId -> ConstraintM NodeId
allocVar level = do
    nid <- freshNodeId
    let node = TyVar
            { tnId = nid
            , tnLevel = level
            }
    insertNode node
    attachVar level nid
    pure nid

allocBase :: BaseTy -> ConstraintM NodeId
allocBase base = do
    nid <- freshNodeId
    insertNode TyBase
        { tnId = nid
        , tnBase = base
        }
    pure nid

allocArrow :: NodeId -> NodeId -> ConstraintM NodeId
allocArrow domNode codNode = do
    nid <- freshNodeId
    insertNode TyArrow
        { tnId = nid
        , tnDom = domNode
        , tnCod = codNode
        }
    pure nid

allocExpNode :: NodeId -> ConstraintM NodeId
allocExpNode bodyNode = do
    expVar <- freshExpVarId
    nid <- freshNodeId
    insertNode TyExp
        { tnId = nid
        , tnExpVar = expVar
        , tnBody = bodyNode
        }
    pure nid

newChildLevel :: Maybe GNodeId -> ConstraintM GNodeId
newChildLevel parent = do
    gid <- freshGNodeId
    let node = GNode
            { gnodeId = gid
            , gParent = parent
            , gBinds = []
            , gChildren = []
            }
    modify' $ \st ->
        st { bsGNodes = IntMap.insert (intFromG gid) node (bsGNodes st)
           , bsForest = case parent of
                Nothing -> gid : bsForest st
                Just _ -> bsForest st
           }
    case parent of
        Nothing -> pure ()
        Just p -> modify' $ \st ->
            st { bsGNodes = IntMap.adjust (\gn -> gn { gChildren = gid : gChildren gn }) (intFromG p) (bsGNodes st) }
    pure gid

attachVar :: GNodeId -> NodeId -> ConstraintM ()
attachVar level nodeId = modify' $ \st ->
    st { bsGNodes = IntMap.adjust (\gn -> gn { gBinds = nodeId : gBinds gn }) (intFromG level) (bsGNodes st) }

addInstEdge :: NodeId -> NodeId -> ConstraintM ()
addInstEdge left right = do
    eid <- freshEdgeId
    let edge = InstEdge (EdgeId eid) left right
    modify' $ \st -> st { bsInstEdges = edge : bsInstEdges st }

freshNodeId :: ConstraintM NodeId
freshNodeId = do
    next <- gets bsNextNode
    modify' $ \st -> st { bsNextNode = next + 1 }
    pure (NodeId next)

freshGNodeId :: ConstraintM GNodeId
freshGNodeId = do
    next <- gets bsNextGNode
    modify' $ \st -> st { bsNextGNode = next + 1 }
    pure (GNodeId next)

freshExpVarId :: ConstraintM ExpVarId
freshExpVarId = do
    next <- gets bsNextExpVar
    modify' $ \st -> st { bsNextExpVar = next + 1 }
    pure (ExpVarId next)

freshEdgeId :: ConstraintM Int
freshEdgeId = do
    next <- gets bsNextEdge
    modify' $ \st -> st { bsNextEdge = next + 1 }
    pure next

insertNode :: TyNode -> ConstraintM ()
insertNode node = modify' $ \st ->
    st { bsNodes = IntMap.insert (intFromNode (tnId node)) node (bsNodes st) }

intFromNode :: NodeId -> Int
intFromNode (NodeId x) = x

intFromG :: GNodeId -> Int
intFromG (GNodeId x) = x
