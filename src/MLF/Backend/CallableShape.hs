{-# LANGUAGE LambdaCase #-}

{- |
Module      : MLF.Backend.CallableShape
Description : Private callable-shape owner for backend direct-vs-closure heads

This private module centralizes the shared callable-head classification used by
`MLF.Backend.IR`, `MLF.Backend.Convert`, and `MLF.Backend.LLVM.Lower`.
`MLF.Backend.IR` remains the single executable backend IR seam; this module
owns only the shared direct-vs-closure head datatypes and classifier, while
backend-expression destructuring stays in the IR-owned adapter instance.
-}
module MLF.Backend.CallableShape
  ( BackendCallableBindingKind (..),
    BackendCallableRef,
    backendCallableRef,
    backendCallableRefIdentity,
    backendCallableRefName,
    backendCallableRefMatches,
    BackendCallableHead (..),
    BackendCallableAlternative (..),
    BackendCallableExpr (..),
    BackendCallableExprView (..),
    backendCallableHead,
  )
where

import MLF.Types.Identity (IdDetails, UniqueIdentity, idDetailsSameIdentity)

data BackendCallableBindingKind
  = BackendCallableBindingDirect
  | BackendCallableBindingClosure
  | BackendCallableBindingUnknown
  deriving (Eq, Show)

data BackendCallableIdentity
  = BackendCallableTermIdentity IdDetails
  | BackendCallableClosureIdentity UniqueIdentity
  deriving (Show)

data BackendCallableRef
  = BackendCallableRef (Maybe BackendCallableIdentity) String
  deriving (Show)

instance Eq BackendCallableRef where
  left == right =
    backendCallableRefMatches left right

backendCallableRef :: Maybe IdDetails -> String -> BackendCallableRef
backendCallableRef mbIdentity name =
  BackendCallableRef (BackendCallableTermIdentity <$> mbIdentity) name

backendCallableClosureRef :: Maybe UniqueIdentity -> String -> BackendCallableRef
backendCallableClosureRef mbIdentity name =
  BackendCallableRef (BackendCallableClosureIdentity <$> mbIdentity) name

backendCallableRefName :: BackendCallableRef -> String
backendCallableRefName =
  \case
    BackendCallableRef _ name -> name

backendCallableRefIdentity :: BackendCallableRef -> Maybe IdDetails
backendCallableRefIdentity =
  \case
    BackendCallableRef (Just (BackendCallableTermIdentity identity)) _ -> Just identity
    BackendCallableRef {} -> Nothing

backendCallableRefMatches :: BackendCallableRef -> BackendCallableRef -> Bool
backendCallableRefMatches (BackendCallableRef leftIdentity leftName) (BackendCallableRef rightIdentity rightName) =
  case (leftIdentity, rightIdentity) of
    (Just (BackendCallableTermIdentity left), Just (BackendCallableTermIdentity right)) ->
      idDetailsSameIdentity left right
    (Just (BackendCallableTermIdentity {}), Nothing) ->
      False
    (Nothing, Just (BackendCallableTermIdentity {})) ->
      False
    (Just (BackendCallableClosureIdentity left), Just (BackendCallableClosureIdentity right)) ->
      left == right
    (Just (BackendCallableClosureIdentity {}), Nothing) ->
      False
    (Nothing, Just (BackendCallableClosureIdentity {})) ->
      False
    (Nothing, Nothing) ->
      leftName == rightName
    _ ->
      False

data BackendCallableHead
  = BackendDirectCallableHead (Maybe BackendCallableRef)
  | BackendClosureCallableHead BackendCallableRef
  | BackendUnknownCallableHead
  deriving (Eq, Show)

data BackendCallableAlternative expr = BackendCallableAlternative
  { backendCallableAltBinders :: [BackendCallableRef],
    backendCallableAltClosureBinders :: [BackendCallableRef],
    backendCallableAltBody :: expr
  }

data BackendCallableExprView expr
  = BackendCallableVar (Maybe IdDetails) String
  | BackendCallableLam
  | BackendCallableClosure (Maybe UniqueIdentity) String
  | BackendCallableTyAbs expr
  | BackendCallableTyApp expr
  | BackendCallableLet (Maybe IdDetails) String expr expr
  | BackendCallableCase [BackendCallableAlternative expr]
  | BackendCallableOpaque

class BackendCallableExpr expr where
  backendCallableExprView :: expr -> BackendCallableExprView expr

backendCallableHead :: BackendCallableExpr expr => (Maybe IdDetails -> String -> BackendCallableBindingKind) -> expr -> BackendCallableHead
backendCallableHead resolve0 =
  go resolve0
  where
    go resolve expr =
      case backendCallableExprView expr of
        BackendCallableVar mbIdentity name ->
          case resolve mbIdentity name of
            BackendCallableBindingDirect ->
              BackendDirectCallableHead (Just (backendCallableRef mbIdentity name))
            BackendCallableBindingClosure ->
              BackendClosureCallableHead (backendCallableRef mbIdentity name)
            BackendCallableBindingUnknown ->
              BackendUnknownCallableHead
        BackendCallableLam ->
          BackendDirectCallableHead Nothing
        BackendCallableClosure entryIdentity entryName ->
          BackendClosureCallableHead (backendCallableClosureRef entryIdentity entryName)
        BackendCallableTyAbs body ->
          go resolve body
        BackendCallableTyApp fun ->
          go resolve fun
        BackendCallableLet mbIdentity name rhs body ->
          go (extendBindingKind resolve mbIdentity name (go resolve rhs)) body
        BackendCallableCase alternatives ->
          collapseCallableHeads
            [ go (extendPatternBindingKinds binders closureBinders resolve) body
            | BackendCallableAlternative binders closureBinders body <- alternatives
            ]
        BackendCallableOpaque ->
          BackendUnknownCallableHead

    extendBindingKind resolve mbIdentity name headShape localIdentity localName
      | backendCallableRefMatches (backendCallableRef mbIdentity name) (backendCallableRef localIdentity localName) =
          callableBindingKindForHead headShape
      | otherwise =
          resolve localIdentity localName

    extendPatternBindingKinds binders closureBinders resolve localIdentity name
      | any (callableBinderMatches localIdentity name) closureBinders =
          BackendCallableBindingClosure
      | any (callableBinderMatches localIdentity name) binders =
          BackendCallableBindingDirect
      | otherwise =
          resolve localIdentity name

callableBinderMatches :: Maybe IdDetails -> String -> BackendCallableRef -> Bool
callableBinderMatches localIdentity localName binder =
  backendCallableRefMatches binder (backendCallableRef localIdentity localName)

callableBindingKindForHead :: BackendCallableHead -> BackendCallableBindingKind
callableBindingKindForHead =
  \case
    BackendDirectCallableHead _ ->
      BackendCallableBindingDirect
    BackendClosureCallableHead _ ->
      BackendCallableBindingClosure
    BackendUnknownCallableHead ->
      BackendCallableBindingUnknown

collapseCallableHeads :: [BackendCallableHead] -> BackendCallableHead
collapseCallableHeads heads
  | all isClosureHead heads =
      BackendClosureCallableHead (firstClosureHeadRef heads)
  | all isDirectHead heads =
      BackendDirectCallableHead (firstDirectHeadRef heads)
  | otherwise =
      BackendUnknownCallableHead
  where
    isClosureHead =
      \case
        BackendClosureCallableHead _ -> True
        _ -> False

    isDirectHead =
      \case
        BackendDirectCallableHead _ -> True
        _ -> False

firstClosureHeadRef :: [BackendCallableHead] -> BackendCallableRef
firstClosureHeadRef =
  go
  where
    go [] =
      backendCallableClosureRef Nothing "__mlfp_unknown_closure_head"
    go (BackendClosureCallableHead ref : _) =
      ref
    go (_ : rest) =
      go rest

firstDirectHeadRef :: [BackendCallableHead] -> Maybe BackendCallableRef
firstDirectHeadRef =
  go
  where
    go [] =
      Nothing
    go (BackendDirectCallableHead (Just ref) : _) =
      Just ref
    go (_ : rest) =
      go rest
