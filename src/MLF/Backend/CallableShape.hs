{-# LANGUAGE LambdaCase #-}

{- |
Module      : MLF.Backend.CallableShape
Description : Private callable-shape owner for backend direct-vs-closure heads

This private module owns the shared direct-vs-closure reference and head
datatypes. `MLF.Backend.IR` classifies its own executable expression type
directly.
-}
module MLF.Backend.CallableShape
  ( BackendCallableBindingKind (..),
    BackendCallableRef,
    backendCallableRef,
    backendCallableClosureRef,
    backendCallableRefIdentity,
    backendCallableRefName,
    backendCallableRefMatches,
    BackendCallableHead (..),
  )
where

import MLF.Types.Identity (IdDetails, UniqueIdentity, idDetailsSameIdentity)

data BackendCallableBindingKind
  = BackendCallableBindingDirect
  | BackendCallableBindingClosure
  | BackendCallableBindingUnknown
  deriving (Eq, Show)

data BackendCallableRef
  = BackendCallableTermRef IdDetails String
  | BackendCallableClosureRef UniqueIdentity String
  deriving (Show)

instance Eq BackendCallableRef where
  left == right =
    backendCallableRefMatches left right

backendCallableRef :: IdDetails -> String -> BackendCallableRef
backendCallableRef =
  BackendCallableTermRef

backendCallableClosureRef :: UniqueIdentity -> String -> BackendCallableRef
backendCallableClosureRef =
  BackendCallableClosureRef

backendCallableRefName :: BackendCallableRef -> String
backendCallableRefName =
  \case
    BackendCallableTermRef _ name -> name
    BackendCallableClosureRef _ name -> name

backendCallableRefIdentity :: BackendCallableRef -> Maybe IdDetails
backendCallableRefIdentity =
  \case
    BackendCallableTermRef identity _ -> Just identity
    BackendCallableClosureRef {} -> Nothing

backendCallableRefMatches :: BackendCallableRef -> BackendCallableRef -> Bool
backendCallableRefMatches left right =
  case (left, right) of
    (BackendCallableTermRef leftIdentity _, BackendCallableTermRef rightIdentity _) ->
      idDetailsSameIdentity leftIdentity rightIdentity
    (BackendCallableClosureRef leftIdentity _, BackendCallableClosureRef rightIdentity _) ->
      leftIdentity == rightIdentity
    _ -> False

data BackendCallableHead
  = BackendDirectCallableHead (Maybe BackendCallableRef)
  | BackendClosureCallableHead (Maybe BackendCallableRef)
  | BackendUnknownCallableHead
  deriving (Eq, Show)
