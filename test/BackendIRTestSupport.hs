module BackendIRTestSupport
  ( validateBackendProgramFixture,
  )
where

import MLF.Backend.IR
  ( BackendProgram,
    BackendValidationError,
  )
import qualified MLF.Backend.IR.Fixture as Fixture

validateBackendProgramFixture :: BackendProgram -> Either BackendValidationError ()
validateBackendProgramFixture =
  Fixture.validateBackendProgramFixture . Fixture.backendProgramFixture
