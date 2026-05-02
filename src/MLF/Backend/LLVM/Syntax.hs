{- |
Module      : MLF.Backend.LLVM.Syntax
Description : Small LLVM IR syntax used by the backend lowerer

This is intentionally a narrow, GHC-style LLVM model: it contains only the
LLVM surface the mlf2 backend currently emits, not a general-purpose LLVM
binding.
-}
module MLF.Backend.LLVM.Syntax
  ( LLVMModule (..),
    LLVMDeclaration (..),
    LLVMGlobal (..),
    LLVMFunction (..),
    LLVMParameter (..),
    LLVMBasicBlock (..),
    LLVMInstruction (..),
    LLVMExpression (..),
    LLVMTerminator (..),
    LLVMType (..),
    LLVMOperand (..),
  )
where

data LLVMModule = LLVMModule
  { llvmModuleGlobals :: [LLVMGlobal],
    llvmModuleDeclarations :: [LLVMDeclaration],
    llvmModuleFunctions :: [LLVMFunction]
  }
  deriving (Eq, Show)

data LLVMDeclaration = LLVMDeclaration
  { llvmDeclarationName :: String,
    llvmDeclarationReturnType :: LLVMType,
    llvmDeclarationParameters :: [LLVMType],
    llvmDeclarationVarArgs :: Bool
  }
  deriving (Eq, Show)

data LLVMGlobal = LLVMStringGlobal
  { llvmGlobalName :: String,
    llvmGlobalBytes :: String
  }
  deriving (Eq, Show)

data LLVMFunction = LLVMFunction
  { llvmFunctionName :: String,
    llvmFunctionPrivate :: Bool,
    llvmFunctionReturnType :: LLVMType,
    llvmFunctionParameters :: [LLVMParameter],
    llvmFunctionBlocks :: [LLVMBasicBlock]
  }
  deriving (Eq, Show)

data LLVMParameter = LLVMParameter
  { llvmParameterType :: LLVMType,
    llvmParameterName :: String
  }
  deriving (Eq, Show)

data LLVMBasicBlock = LLVMBasicBlock
  { llvmBlockLabel :: String,
    llvmBlockInstructions :: [LLVMInstruction],
    llvmBlockTerminator :: LLVMTerminator
  }
  deriving (Eq, Show)

data LLVMInstruction
  = LLVMAssign String LLVMType LLVMExpression
  | LLVMStore LLVMType LLVMOperand LLVMOperand
  | LLVMComment String
  deriving (Eq, Show)

data LLVMExpression
  = LLVMCall String [(LLVMType, LLVMOperand)]
  | LLVMCallVarArgs String [LLVMType] [(LLVMType, LLVMOperand)]
  | LLVMCallOperand LLVMOperand [(LLVMType, LLVMOperand)]
  | LLVMAnd LLVMOperand LLVMOperand
  | LLVMICmpEq LLVMOperand LLVMOperand
  | LLVMICmpUgt LLVMOperand LLVMOperand
  | LLVMZext LLVMOperand LLVMType
  | LLVMAlloca LLVMType LLVMOperand
  | LLVMGetElementPtr LLVMType LLVMOperand [(LLVMType, LLVMOperand)]
  | LLVMLoad LLVMType LLVMOperand
  | LLVMPhi LLVMType [(LLVMOperand, String)]
  deriving (Eq, Show)

data LLVMTerminator
  = LLVMRet LLVMType LLVMOperand
  | LLVMBr String
  | LLVMSwitch LLVMType LLVMOperand String [(Integer, String)]
  | LLVMUnreachable
  deriving (Eq, Show)

data LLVMType
  = LLVMVoid
  | LLVMInt Int
  | LLVMPtr
  | LLVMArray Int LLVMType
  deriving (Eq, Ord, Show)

data LLVMOperand
  = LLVMLocal LLVMType String
  | LLVMGlobalRef LLVMType String
  | LLVMIntLiteral Int Integer
  | LLVMNull
  deriving (Eq, Show)
