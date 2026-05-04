module MLF.Backend.LLVM.Lower.Emit
  ( emitAssign,
    emitGep,
    emitInstruction,
    emitStore,
    finishCurrentBlock,
    freshBlock,
    freshLocal,
    sanitizeBlockLabel,
    startBlock,
  )
where

import Control.Monad.State.Strict (get, gets, modify)
import Data.Char (isAlphaNum)
import MLF.Backend.LLVM.Lower.Types
import MLF.Backend.LLVM.Syntax

emitGep :: String -> LLVMOperand -> Int -> LowerM LLVMOperand
emitGep prefix base offset =
  emitAssign prefix LLVMPtr (LLVMGetElementPtr (LLVMInt 8) base [(LLVMInt 64, LLVMIntLiteral 64 (toInteger offset))])

emitStore :: LLVMType -> LLVMOperand -> LLVMOperand -> LowerM ()
emitStore ty value pointer =
  emitInstruction (LLVMStore ty value pointer)

emitAssign :: String -> LLVMType -> LLVMExpression -> LowerM LLVMOperand
emitAssign prefix ty expr = do
  name <- freshLocal prefix
  emitInstruction (LLVMAssign name ty expr)
  pure (LLVMLocal ty name)

emitInstruction :: LLVMInstruction -> LowerM ()
emitInstruction instruction =
  modify $ \state0 ->
    state0 {fsCurrentInstructions = fsCurrentInstructions state0 ++ [instruction]}

freshLocal :: String -> LowerM String
freshLocal prefix = do
  index0 <- gets fsNextLocal
  modify $ \state0 -> state0 {fsNextLocal = index0 + 1}
  pure ("__llvm." ++ prefix ++ "." ++ show index0)

freshBlock :: String -> LowerM String
freshBlock prefix = do
  index0 <- gets fsNextBlock
  modify $ \state0 -> state0 {fsNextBlock = index0 + 1}
  pure (sanitizeBlockLabel prefix ++ "." ++ show index0)

sanitizeBlockLabel :: String -> String
sanitizeBlockLabel =
  map sanitizeChar
  where
    sanitizeChar char
      | isAlphaNum char = char
      | otherwise = '.'

finishCurrentBlock :: LLVMTerminator -> LowerM ()
finishCurrentBlock terminator = do
  state0 <- get
  let block =
        LLVMBasicBlock
          { llvmBlockLabel = fsCurrentLabel state0,
            llvmBlockInstructions = fsCurrentInstructions state0,
            llvmBlockTerminator = terminator
          }
  modify $ \state1 ->
    state1
      { fsCurrentInstructions = [],
        fsCompletedBlocks = block : fsCompletedBlocks state1
      }

startBlock :: String -> LowerM ()
startBlock label =
  modify $ \state0 ->
    state0
      { fsCurrentLabel = label,
        fsCurrentInstructions = []
      }
