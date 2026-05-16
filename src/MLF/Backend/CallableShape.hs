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
    BackendCallableHead (..),
    BackendCallableAlternative (..),
    BackendCallableExpr (..),
    BackendCallableExprView (..),
    backendCallableHead,
  )
where

import qualified Data.Set as Set

data BackendCallableBindingKind
  = BackendCallableBindingDirect
  | BackendCallableBindingClosure
  | BackendCallableBindingUnknown
  deriving (Eq, Show)

data BackendCallableHead
  = BackendDirectCallableHead (Maybe String)
  | BackendClosureCallableHead String
  | BackendUnknownCallableHead
  deriving (Eq, Show)

data BackendCallableAlternative expr = BackendCallableAlternative
  { backendCallableAltBinders :: Set.Set String,
    backendCallableAltClosureBinders :: Set.Set String,
    backendCallableAltBody :: expr
  }

data BackendCallableExprView expr
  = BackendCallableVar String
  | BackendCallableLam
  | BackendCallableClosure String
  | BackendCallableTyAbs expr
  | BackendCallableTyApp expr
  | BackendCallableLet String expr expr
  | BackendCallableCase [BackendCallableAlternative expr]
  | BackendCallableOpaque

class BackendCallableExpr expr where
  backendCallableExprView :: expr -> BackendCallableExprView expr

backendCallableHead :: BackendCallableExpr expr => (String -> BackendCallableBindingKind) -> expr -> BackendCallableHead
backendCallableHead resolve0 =
  go resolve0
  where
    go resolve expr =
      case backendCallableExprView expr of
        BackendCallableVar name ->
          case resolve name of
            BackendCallableBindingDirect ->
              BackendDirectCallableHead (Just name)
            BackendCallableBindingClosure ->
              BackendClosureCallableHead name
            BackendCallableBindingUnknown ->
              BackendUnknownCallableHead
        BackendCallableLam ->
          BackendDirectCallableHead Nothing
        BackendCallableClosure entryName ->
          BackendClosureCallableHead entryName
        BackendCallableTyAbs body ->
          go resolve body
        BackendCallableTyApp fun ->
          go resolve fun
        BackendCallableLet name rhs body ->
          go (extendBindingKind resolve name (go resolve rhs)) body
        BackendCallableCase alternatives ->
          collapseCallableHeads
            [ go (extendPatternBindingKinds binders closureBinders resolve) body
            | BackendCallableAlternative binders closureBinders body <- alternatives
            ]
        BackendCallableOpaque ->
          BackendUnknownCallableHead

    extendBindingKind resolve name headShape localName
      | localName == name =
          callableBindingKindForHead headShape
      | otherwise =
          resolve localName

    extendPatternBindingKinds binders closureBinders resolve name
      | Set.member name closureBinders =
          BackendCallableBindingClosure
      | Set.member name binders =
          BackendCallableBindingDirect
      | otherwise =
          resolve name

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
      BackendClosureCallableHead (firstClosureHeadName heads)
  | all isDirectHead heads =
      BackendDirectCallableHead (firstDirectHeadName heads)
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

firstClosureHeadName :: [BackendCallableHead] -> String
firstClosureHeadName =
  go
  where
    go [] =
      "__mlfp_unknown_closure_head"
    go (BackendClosureCallableHead name : _) =
      name
    go (_ : rest) =
      go rest

firstDirectHeadName :: [BackendCallableHead] -> Maybe String
firstDirectHeadName =
  go
  where
    go [] =
      Nothing
    go (BackendDirectCallableHead (Just name) : _) =
      Just name
    go (_ : rest) =
      go rest
