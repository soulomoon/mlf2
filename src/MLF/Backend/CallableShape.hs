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
    backendCallableRefMatchesWith,
    backendCallableRefMatches,
    BackendCallableHead (..),
  )
where

import MLF.Types.Identity (IdDetails, UniqueIdentity, idDetailsSameIdentity)
import MLF.Types.Reference (ReferenceMode (..), referenceMatchesWith)

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
    backendCallableRefMatchesWith MetadataLight left right

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

backendCallableRefMatchesWith :: ReferenceMode -> BackendCallableRef -> BackendCallableRef -> Bool
backendCallableRefMatchesWith mode (BackendCallableRef leftIdentity leftName) (BackendCallableRef rightIdentity rightName) =
  referenceMatchesWith sameCallableIdentity mode leftIdentity leftName rightIdentity rightName

sameCallableIdentity :: BackendCallableIdentity -> BackendCallableIdentity -> Bool
sameCallableIdentity (BackendCallableTermIdentity left) (BackendCallableTermIdentity right) =
  idDetailsSameIdentity left right
sameCallableIdentity (BackendCallableClosureIdentity left) (BackendCallableClosureIdentity right) =
  left == right
sameCallableIdentity _ _ =
  False

backendCallableRefMatches :: BackendCallableRef -> BackendCallableRef -> Bool
backendCallableRefMatches =
  backendCallableRefMatchesWith IdentityOnly

data BackendCallableHead
  = BackendDirectCallableHead (Maybe BackendCallableRef)
  | BackendClosureCallableHead (Maybe BackendCallableRef)
  | BackendUnknownCallableHead
  deriving (Eq, Show)
