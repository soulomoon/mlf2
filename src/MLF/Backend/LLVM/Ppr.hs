{- |
Module      : MLF.Backend.LLVM.Ppr
Description : Pretty-printer for the backend LLVM syntax
-}
module MLF.Backend.LLVM.Ppr
  ( renderLLVMModule,
    renderLLVMType,
    renderLLVMOperand,
    renderLLVMGlobalName,
    renderLLVMLocalName,
  )
where

import Data.Char (isAlphaNum, ord)
import Data.List (intercalate)
import Numeric (showHex)

import MLF.Backend.LLVM.Syntax

renderLLVMModule :: LLVMModule -> String
renderLLVMModule llvmModule =
  renderLines $
    ["; mlf2 LLVM backend v0", "source_filename = \"mlf2\""]
      ++ blankSeparated
        [ map renderLLVMGlobal (llvmModuleGlobals llvmModule),
          map renderLLVMDeclaration (llvmModuleDeclarations llvmModule),
          blankSeparated (map renderLLVMFunctionLines (llvmModuleFunctions llvmModule))
        ]

renderLines :: [String] -> String
renderLines [] = ""
renderLines lines0 =
  intercalate "\n" lines0 ++ "\n"

blankSeparated :: [[String]] -> [String]
blankSeparated =
  go . filter (not . null)
  where
    go [] = []
    go [section] = section
    go (section : rest) = section ++ [""] ++ go rest

renderLLVMGlobal :: LLVMGlobal -> String
renderLLVMGlobal (LLVMStringGlobal name value) =
  renderLLVMGlobalName name
    ++ " = private unnamed_addr constant "
    ++ renderLLVMType (LLVMArray (stringByteLength value + 1) (LLVMInt 8))
    ++ " c\""
    ++ concatMap renderLLVMStringChar value
    ++ "\\00\""

renderLLVMDeclaration :: LLVMDeclaration -> String
renderLLVMDeclaration declaration =
  "declare "
    ++ renderLLVMType (llvmDeclarationReturnType declaration)
    ++ " "
    ++ renderLLVMGlobalName (llvmDeclarationName declaration)
    ++ "("
    ++ intercalate ", " (map renderLLVMType (llvmDeclarationParameters declaration))
    ++ ")"

renderLLVMFunctionLines :: LLVMFunction -> [String]
renderLLVMFunctionLines function =
    [ "define "
        ++ privateText
        ++ renderLLVMType (llvmFunctionReturnType function)
        ++ " "
        ++ renderLLVMGlobalName (llvmFunctionName function)
        ++ "("
        ++ intercalate ", " (map renderLLVMParameter (llvmFunctionParameters function))
        ++ ") {"
    ]
      ++ concatMap renderLLVMBasicBlock (llvmFunctionBlocks function)
      ++ ["}"]
  where
    privateText
      | llvmFunctionPrivate function = "private "
      | otherwise = ""

renderLLVMParameter :: LLVMParameter -> String
renderLLVMParameter parameter =
  renderLLVMType (llvmParameterType parameter)
    ++ " "
    ++ renderLLVMLocalName (llvmParameterName parameter)

renderLLVMBasicBlock :: LLVMBasicBlock -> [String]
renderLLVMBasicBlock block =
  [renderLLVMLabelDefinition (llvmBlockLabel block)]
    ++ map (("  " ++) . renderLLVMInstruction) (llvmBlockInstructions block)
    ++ ["  " ++ renderLLVMTerminator (llvmBlockTerminator block)]

renderLLVMInstruction :: LLVMInstruction -> String
renderLLVMInstruction instruction =
  case instruction of
    LLVMAssign name ty expr ->
      renderLLVMLocalName name ++ " = " ++ renderLLVMExpression ty expr
    LLVMStore ty value pointer ->
      "store "
        ++ renderLLVMType ty
        ++ " "
        ++ renderLLVMOperand value
        ++ ", ptr "
        ++ renderLLVMOperand pointer
    LLVMComment comment ->
      "; " ++ comment

renderLLVMExpression :: LLVMType -> LLVMExpression -> String
renderLLVMExpression resultTy expression =
  case expression of
    LLVMCall name args ->
      "call "
        ++ renderLLVMType resultTy
        ++ " "
        ++ renderLLVMGlobalName name
        ++ "("
        ++ intercalate ", " (map renderLLVMArgument args)
        ++ ")"
    LLVMGetElementPtr elementTy base indexes ->
      "getelementptr "
        ++ renderLLVMType elementTy
        ++ ", ptr "
        ++ renderLLVMOperand base
        ++ concatMap renderLLVMIndex indexes
    LLVMLoad ty pointer ->
      "load "
        ++ renderLLVMType ty
        ++ ", ptr "
        ++ renderLLVMOperand pointer
    LLVMPhi ty incoming ->
      "phi "
        ++ renderLLVMType ty
        ++ " "
        ++ intercalate ", " (map renderLLVMIncoming incoming)

renderLLVMArgument :: (LLVMType, LLVMOperand) -> String
renderLLVMArgument (ty, operand) =
  renderLLVMType ty ++ " " ++ renderLLVMOperand operand

renderLLVMIndex :: (LLVMType, LLVMOperand) -> String
renderLLVMIndex (ty, operand) =
  ", " ++ renderLLVMType ty ++ " " ++ renderLLVMOperand operand

renderLLVMIncoming :: (LLVMOperand, String) -> String
renderLLVMIncoming (operand, label) =
  "[ " ++ renderLLVMOperand operand ++ ", " ++ renderLLVMLabelRef label ++ " ]"

renderLLVMTerminator :: LLVMTerminator -> String
renderLLVMTerminator terminator =
  case terminator of
    LLVMRet ty operand ->
      "ret " ++ renderLLVMType ty ++ " " ++ renderLLVMOperand operand
    LLVMBr label ->
      "br label " ++ renderLLVMLabelRef label
    LLVMSwitch ty operand defaultLabel targets ->
      "switch "
        ++ renderLLVMType ty
        ++ " "
        ++ renderLLVMOperand operand
        ++ ", label "
        ++ renderLLVMLabelRef defaultLabel
        ++ " [ "
        ++ intercalate " " (map renderSwitchTarget targets)
        ++ " ]"
    LLVMUnreachable ->
      "unreachable"

renderSwitchTarget :: (Integer, String) -> String
renderSwitchTarget (tag, label) =
  "i64 " ++ show tag ++ ", label " ++ renderLLVMLabelRef label

renderLLVMType :: LLVMType -> String
renderLLVMType ty =
  case ty of
    LLVMVoid -> "void"
    LLVMInt width -> "i" ++ show width
    LLVMPtr -> "ptr"
    LLVMArray count elementTy ->
      "[" ++ show count ++ " x " ++ renderLLVMType elementTy ++ "]"

renderLLVMOperand :: LLVMOperand -> String
renderLLVMOperand operand =
  case operand of
    LLVMLocal _ name -> renderLLVMLocalName name
    LLVMGlobalRef _ name -> renderLLVMGlobalName name
    LLVMIntLiteral 1 0 -> "false"
    LLVMIntLiteral 1 1 -> "true"
    LLVMIntLiteral _ value -> show value
    LLVMNull -> "null"

renderLLVMGlobalName :: String -> String
renderLLVMGlobalName name =
  "@\"" ++ escapeQuoted name ++ "\""

renderLLVMLocalName :: String -> String
renderLLVMLocalName name =
  "%\"" ++ escapeQuoted name ++ "\""

renderLLVMLabelDefinition :: String -> String
renderLLVMLabelDefinition label
  | isBareLabel label = label ++ ":"
  | otherwise = "\"" ++ escapeQuoted label ++ "\":"

renderLLVMLabelRef :: String -> String
renderLLVMLabelRef label
  | isBareLabel label = "%" ++ label
  | otherwise = "%\"" ++ escapeQuoted label ++ "\""

isBareLabel :: String -> Bool
isBareLabel [] = False
isBareLabel value =
  all isBareLabelChar value

isBareLabelChar :: Char -> Bool
isBareLabelChar char =
  isAlphaNum char || char == '_' || char == '.' || char == '$'

escapeQuoted :: String -> String
escapeQuoted =
  concatMap escapeChar
  where
    escapeChar '"' = "\\22"
    escapeChar '\\' = "\\5C"
    escapeChar '\n' = "\\0A"
    escapeChar '\r' = "\\0D"
    escapeChar '\t' = "\\09"
    escapeChar char = [char]

renderLLVMStringChar :: Char -> String
renderLLVMStringChar char
  | code >= 32 && code <= 126 && char /= '"' && char /= '\\' =
      [char]
  | otherwise =
      "\\" ++ twoHex code
  where
    code = ord char

stringByteLength :: String -> Int
stringByteLength =
  length

twoHex :: Int -> String
twoHex value =
  case map toUpperHex (showHex value "") of
    [digit] -> ['0', digit]
    digits -> digits

toUpperHex :: Char -> Char
toUpperHex char =
  case char of
    'a' -> 'A'
    'b' -> 'B'
    'c' -> 'C'
    'd' -> 'D'
    'e' -> 'E'
    'f' -> 'F'
    _ -> char
