module MLF.Elab.Reduce.TestSupport
  ( collectApplicationSpineThroughHeadTypeRedexes,
    normalizeCheckedTypeRedexesForTest,
  )
where

import MLF.Elab.Reduce
  ( collectApplicationSpineThroughHeadTypeRedexes,
    normalizeCheckedTypeRedexesWithEnv,
  )
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types (XmlfTerm)

normalizeCheckedTypeRedexesForTest :: XmlfTerm -> XmlfTerm
normalizeCheckedTypeRedexesForTest =
  normalizeCheckedTypeRedexesWithEnv TypeCheck.emptyEnv
