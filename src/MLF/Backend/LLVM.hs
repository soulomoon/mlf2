{- |
Module      : MLF.Backend.LLVM
Description : Real LLVM IR backend facade for checked .mlfp programs
-}
module MLF.Backend.LLVM
  ( BackendLLVMError (..),
    renderCheckedProgramLLVM,
    renderCheckedProgramNativeLLVM,
    renderBackendProgramLLVM,
    renderBackendProgramNativeLLVM,
    renderBackendLLVMError,
  )
where

import Data.Bifunctor (first)

import MLF.Backend.Convert
  ( BackendConversionError,
    convertCheckedProgram,
  )
import MLF.Backend.IR (BackendProgram)
import qualified MLF.Backend.LLVM.Lower as Lower
import MLF.Backend.LLVM.Ppr (renderLLVMModule)
import MLF.Frontend.Program.Types (CheckedProgram)

renderCheckedProgramLLVM :: CheckedProgram -> Either BackendLLVMError String
renderCheckedProgramLLVM checked =
  first BackendLLVMConversionFailed (convertCheckedProgram checked)
    >>= renderBackendProgramLLVM

renderCheckedProgramNativeLLVM :: CheckedProgram -> Either BackendLLVMError String
renderCheckedProgramNativeLLVM checked =
  first BackendLLVMConversionFailed (convertCheckedProgram checked)
    >>= renderBackendProgramNativeLLVM

renderBackendProgramLLVM :: BackendProgram -> Either BackendLLVMError String
renderBackendProgramLLVM program =
  first BackendLLVMLoweringFailed (renderLLVMModule <$> Lower.lowerBackendProgram program)

renderBackendProgramNativeLLVM :: BackendProgram -> Either BackendLLVMError String
renderBackendProgramNativeLLVM program =
  first BackendLLVMLoweringFailed (renderLLVMModule <$> Lower.lowerBackendProgramNative program)

data BackendLLVMError
  = BackendLLVMConversionFailed BackendConversionError
  | BackendLLVMLoweringFailed Lower.BackendLLVMError
  deriving (Eq, Show)

renderBackendLLVMError :: BackendLLVMError -> String
renderBackendLLVMError err =
  case err of
    BackendLLVMConversionFailed conversionErr ->
      "Backend LLVM conversion failed: " ++ show conversionErr
    BackendLLVMLoweringFailed loweringErr ->
      Lower.renderBackendLLVMError loweringErr
