module MLF.Backend.IR.Production.Internal
  ( ProductionBackendProgram,
    productionBackendProgramFromValidated,
    productionBackendProgramIR,
  )
where

import MLF.Backend.IR.Types (BackendProgram)

-- | Capability owned by the production backend boundary. The raw projection
-- is intentionally confined to the lowering owner and explicit test support.
newtype ProductionBackendProgram = ProductionBackendProgram BackendProgram
  deriving (Show)

productionBackendProgramFromValidated :: BackendProgram -> ProductionBackendProgram
productionBackendProgramFromValidated =
  ProductionBackendProgram

productionBackendProgramIR :: ProductionBackendProgram -> BackendProgram
productionBackendProgramIR (ProductionBackendProgram program) =
  program
