module MLF.Frontend.Program.Checked.Internal
  ( CheckedProgram (..),
  )
where

import MLF.Elab.Types (ResolvedVar)
import MLF.Frontend.Program.Types (CheckedModule, ResolvedProgram)

-- | Identity-complete checked-program payload. Production construction is
-- owned by 'MLF.Frontend.Program.Checked'; this constructor exists only for
-- the owner module and explicit test-support fixtures.
data CheckedProgram = CheckedProgram
  { checkedProgramModulesInternal :: [CheckedModule],
    checkedProgramMainResolvedVarInternal :: ResolvedVar,
    checkedProgramResolvedInternal :: ResolvedProgram
  }
  deriving (Eq, Show)
