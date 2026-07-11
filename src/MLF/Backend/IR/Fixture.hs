module MLF.Backend.IR.Fixture
  ( BackendProgramFixture,
    backendProgramFixture,
    validateBackendProgramFixture,
  )
where

import MLF.Backend.IR
  ( BackendProgram,
    BackendValidationError,
    validateBackendProgramMetadataLight,
  )

-- | Explicit metadata-light backend input. Production conversion and LLVM
-- lowering never accept this capability.
newtype BackendProgramFixture = BackendProgramFixture BackendProgram

backendProgramFixture :: BackendProgram -> BackendProgramFixture
backendProgramFixture =
  BackendProgramFixture

validateBackendProgramFixture :: BackendProgramFixture -> Either BackendValidationError ()
validateBackendProgramFixture (BackendProgramFixture program) =
  validateBackendProgramMetadataLight program
