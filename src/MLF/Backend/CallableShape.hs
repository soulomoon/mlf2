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
    BackendCallableReferenceMode (..),
    BackendCallableRef,
    backendCallableRef,
    backendCallableRefIdentity,
    backendCallableRefName,
    backendCallableRefMatchesWith,
    backendCallableRefMatches,
    BackendCallableHead (..),
    BackendCallableAlternative (..),
    BackendCallableExpr (..),
    BackendCallableExprView (..),
    backendCallableHeadWith,
    backendCallableHead,
  )
where

import MLF.Types.Identity (IdDetails, UniqueIdentity, idDetailsSameIdentity)

data BackendCallableBindingKind
  = BackendCallableBindingDirect
  | BackendCallableBindingClosure
  | BackendCallableBindingUnknown
  deriving (Eq, Show)

data BackendCallableReferenceMode
  = BackendCallableIdentityOnly
  | BackendCallableMetadataLight
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
    backendCallableRefMatchesWith BackendCallableMetadataLight left right

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

backendCallableRefMatchesWith :: BackendCallableReferenceMode -> BackendCallableRef -> BackendCallableRef -> Bool
backendCallableRefMatchesWith mode (BackendCallableRef leftIdentity leftName) (BackendCallableRef rightIdentity rightName) =
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
      case mode of
        BackendCallableIdentityOnly -> False
        BackendCallableMetadataLight -> leftName == rightName
    _ ->
      False

backendCallableRefMatches :: BackendCallableRef -> BackendCallableRef -> Bool
backendCallableRefMatches =
  backendCallableRefMatchesWith BackendCallableIdentityOnly

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
  backendCallableExprViewWith :: BackendCallableReferenceMode -> expr -> BackendCallableExprView expr

backendCallableHead :: BackendCallableExpr expr => (Maybe IdDetails -> String -> BackendCallableBindingKind) -> expr -> BackendCallableHead
backendCallableHead =
  backendCallableHeadWith BackendCallableIdentityOnly

backendCallableHeadWith :: BackendCallableExpr expr => BackendCallableReferenceMode -> (Maybe IdDetails -> String -> BackendCallableBindingKind) -> expr -> BackendCallableHead
backendCallableHeadWith mode resolve0 =
  go resolve0
  where
    go resolve expr =
      case backendCallableExprViewWith mode expr of
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
          collapseCallableHeadsWith mode
            [ go (extendPatternBindingKinds binders closureBinders resolve) body
            | BackendCallableAlternative binders closureBinders body <- alternatives
            ]
        BackendCallableOpaque ->
          BackendUnknownCallableHead

    extendBindingKind resolve mbIdentity name headShape localIdentity localName
      | backendCallableRefMatchesWith mode (backendCallableRef mbIdentity name) (backendCallableRef localIdentity localName) =
          callableBindingKindForHead headShape
      | otherwise =
          resolve localIdentity localName

    extendPatternBindingKinds binders closureBinders resolve localIdentity name
      | any (callableBinderMatches mode localIdentity name) closureBinders =
          BackendCallableBindingClosure
      | any (callableBinderMatches mode localIdentity name) binders =
          BackendCallableBindingDirect
      | otherwise =
          resolve localIdentity name

callableBinderMatches :: BackendCallableReferenceMode -> Maybe IdDetails -> String -> BackendCallableRef -> Bool
callableBinderMatches mode localIdentity localName binder =
  backendCallableRefMatchesWith mode binder (backendCallableRef localIdentity localName)

callableBindingKindForHead :: BackendCallableHead -> BackendCallableBindingKind
callableBindingKindForHead =
  \case
    BackendDirectCallableHead _ ->
      BackendCallableBindingDirect
    BackendClosureCallableHead _ ->
      BackendCallableBindingClosure
    BackendUnknownCallableHead ->
      BackendCallableBindingUnknown

collapseCallableHeadsWith :: BackendCallableReferenceMode -> [BackendCallableHead] -> BackendCallableHead
collapseCallableHeadsWith _ [] =
  BackendClosureCallableHead unknownClosureHeadRef
collapseCallableHeadsWith mode heads
  | all isClosureHead heads =
      BackendClosureCallableHead (sameClosureHeadRef mode heads)
  | all isDirectHead heads =
      BackendDirectCallableHead (sameDirectHeadRef mode heads)
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

sameClosureHeadRef :: BackendCallableReferenceMode -> [BackendCallableHead] -> BackendCallableRef
sameClosureHeadRef mode heads =
  case [ref | BackendClosureCallableHead ref <- heads] of
    ref : rest
      | all (backendCallableRefMatchesWith mode ref) rest -> ref
    _ -> unknownClosureHeadRef

sameDirectHeadRef :: BackendCallableReferenceMode -> [BackendCallableHead] -> Maybe BackendCallableRef
sameDirectHeadRef mode heads =
  case [ref | BackendDirectCallableHead ref <- heads] of
    ref : rest
      | all (directHeadRefMatches mode ref) rest -> ref
    _ -> Nothing

directHeadRefMatches :: BackendCallableReferenceMode -> Maybe BackendCallableRef -> Maybe BackendCallableRef -> Bool
directHeadRefMatches mode (Just left) (Just right) =
  backendCallableRefMatchesWith mode left right
directHeadRefMatches _ Nothing Nothing =
  True
directHeadRefMatches _ _ _ =
  False

unknownClosureHeadRef :: BackendCallableRef
unknownClosureHeadRef =
  backendCallableClosureRef Nothing "__mlfp_unknown_closure_head"
