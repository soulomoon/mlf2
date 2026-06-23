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
    backendCallableRefName,
    BackendCallableHead (..),
    BackendCallableAlternative (..),
    BackendCallableExpr (..),
    BackendCallableExprView (..),
    backendCallableHead,
  )
where

import MLF.Types.Identity (IdDetails, idDetailsSameIdentity)

data BackendCallableBindingKind
  = BackendCallableBindingDirect
  | BackendCallableBindingClosure
  | BackendCallableBindingUnknown
  deriving (Eq, Show)

type BackendCallableRef = (Maybe IdDetails, String)

backendCallableRefName :: BackendCallableRef -> String
backendCallableRefName = snd

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
  | BackendCallableClosure String
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
              BackendDirectCallableHead (Just (mbIdentity, name))
            BackendCallableBindingClosure ->
              BackendClosureCallableHead (mbIdentity, name)
            BackendCallableBindingUnknown ->
              BackendUnknownCallableHead
        BackendCallableLam ->
          BackendDirectCallableHead Nothing
        BackendCallableClosure entryName ->
          BackendClosureCallableHead (Nothing, entryName)
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
      | callableNameMatches mbIdentity name localIdentity localName =
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
callableBinderMatches localIdentity localName (binderIdentity, binderName) =
  callableNameMatches binderIdentity binderName localIdentity localName

callableNameMatches :: Maybe IdDetails -> String -> Maybe IdDetails -> String -> Bool
callableNameMatches (Just left) _ (Just right) _ =
  idDetailsSameIdentity left right
callableNameMatches Nothing leftName Nothing rightName =
  leftName == rightName
callableNameMatches _ _ _ _ =
  False

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
      (Nothing, "__mlfp_unknown_closure_head")
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
