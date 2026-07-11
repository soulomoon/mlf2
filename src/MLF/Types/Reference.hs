module MLF.Types.Reference
  ( ReferenceMode (..),
    referenceMatchesWith,
  )
where

data ReferenceMode
  = IdentityOnly
  | MetadataLight
  deriving (Eq, Show)

referenceMatchesWith :: (identity -> identity -> Bool) -> ReferenceMode -> Maybe identity -> String -> Maybe identity -> String -> Bool
referenceMatchesWith sameIdentity _ (Just left) _ (Just right) _ =
  sameIdentity left right
referenceMatchesWith _ MetadataLight Nothing leftName Nothing rightName =
  leftName == rightName
referenceMatchesWith _ IdentityOnly Nothing _ Nothing _ =
  False
referenceMatchesWith _ _ _ _ _ _ =
  False
