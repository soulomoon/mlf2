module MLF.Elab.Inst.TestSupport
  ( substBinderAtOccurrencesWithFreshDeclarationCopiesForTest,
  )
where

import MLF.Elab.Inst
  ( substBinderAtOccurrencesWithFreshDeclarationCopies,
  )
import MLF.Elab.Types (ElabType, TypeBinderRef)
import MLF.Types.Identity (IdentityGenerator)

substBinderAtOccurrencesWithFreshDeclarationCopiesForTest
  :: IdentityGenerator
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> (IdentityGenerator, ElabType)
substBinderAtOccurrencesWithFreshDeclarationCopiesForTest =
  substBinderAtOccurrencesWithFreshDeclarationCopies
