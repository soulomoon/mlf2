{-# LANGUAGE LambdaCase #-}

{- |
Module      : MLF.Backend.Text
Description : Deterministic LLVM-like text for typed backend IR

This module is the first textual lowering boundary after "MLF.Backend.Convert".
It renders the supported functional subset of the typed backend IR and rejects
backend-only control/data nodes explicitly so downstream experiments can inspect
backend output without treating unsupported constructs as silently lowered.
-}
module MLF.Backend.Text
    ( BackendTextError (..)
    , renderCheckedProgramBackendText
    , renderBackendProgram
    , renderBackendModule
    , renderBackendBinding
    , renderBackendExpr
    , renderBackendType
    , renderBackendTextError
    ) where

import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set

import MLF.Backend.Convert
    ( BackendConversionError
    , convertCheckedProgram
    )
import MLF.Backend.IR
    ( BackendBinding (..)
    , BackendConstructor (..)
    , BackendData (..)
    , BackendExpr (..)
    , BackendModule (..)
    , BackendProgram (..)
    , BackendType (..)
    , BackendTypeBinder (..)
    , BackendValidationError
    , validateBackendProgram
    )
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Program.Types (CheckedProgram)
import MLF.Frontend.Syntax (Lit (..))

data BackendTextError
    = BackendTextConversionFailed BackendConversionError
    | BackendTextValidationFailed BackendValidationError
    | BackendTextUnsupportedNode String String
    deriving (Eq, Show)

data RenderEnv = RenderEnv
    { renderGlobalNames :: Set.Set String
    }

renderCheckedProgramBackendText :: CheckedProgram -> Either BackendTextError String
renderCheckedProgramBackendText checked =
    first BackendTextConversionFailed (convertCheckedProgram checked)
        >>= renderBackendProgram

renderBackendProgram :: BackendProgram -> Either BackendTextError String
renderBackendProgram program =
    case validateBackendProgram program of
        Left err -> Left (BackendTextValidationFailed err)
        Right () -> do
            moduleLines <- traverse (renderBackendModuleLines env) (backendProgramModules program)
            pure $
                unlines $
                    [ "; mlf2 backend-text v0"
                    , "; entry " ++ renderGlobal (backendProgramMain program)
                    ]
                        ++ concat moduleLines
  where
    env =
        RenderEnv
            { renderGlobalNames =
                Set.fromList
                    [ backendBindingName binding
                    | backendModule <- backendProgramModules program
                    , binding <- backendModuleBindings backendModule
                    ]
            }

renderBackendModule :: BackendModule -> Either BackendTextError String
renderBackendModule backendModule =
    unlines <$> renderBackendModuleLines env backendModule
  where
    env =
        RenderEnv
            { renderGlobalNames =
                Set.fromList (map backendBindingName (backendModuleBindings backendModule))
            }

renderBackendBinding :: BackendBinding -> Either BackendTextError String
renderBackendBinding binding =
    unlines <$> renderBackendBindingLines env binding
  where
    env = RenderEnv {renderGlobalNames = Set.singleton (backendBindingName binding)}

renderBackendExpr :: BackendExpr -> Either BackendTextError String
renderBackendExpr =
    renderExpr RenderEnv {renderGlobalNames = Set.empty} "expression" 0

renderBackendModuleLines :: RenderEnv -> BackendModule -> Either BackendTextError [String]
renderBackendModuleLines env backendModule = do
    bindingSections <- traverse (renderBackendBindingLines env) (backendModuleBindings backendModule)
    let dataSections = map renderBackendDataLines (backendModuleData backendModule)
        sections = dataSections ++ bindingSections
        indentedSections = map (map ("  " ++)) sections
    pure $
        [ ""
        , "module " ++ renderGlobal (backendModuleName backendModule) ++ " {"
        ]
            ++ concat (intersperse [""] indentedSections)
            ++ ["}"]

renderBackendDataLines :: BackendData -> [String]
renderBackendDataLines backendData =
    [ "type "
        ++ renderGlobal (backendDataName backendData)
        ++ renderPlainTypeParameters (backendDataParameters backendData)
        ++ " {"
    ]
        ++ map renderConstructorLine (backendDataConstructors backendData)
        ++ ["}"]

renderConstructorLine :: BackendConstructor -> String
renderConstructorLine constructor =
    "  ctor "
        ++ renderGlobal (backendConstructorName constructor)
        ++ renderBackendTypeBinders (backendConstructorForalls constructor)
        ++ "("
        ++ intercalate ", " (map renderBackendType (backendConstructorFields constructor))
        ++ ") -> "
        ++ renderBackendType (backendConstructorResult constructor)

renderBackendBindingLines :: RenderEnv -> BackendBinding -> Either BackendTextError [String]
renderBackendBindingLines env binding = do
    renderedBody <- renderExpr env context 0 bodyExpr
    let typeParamsText = renderBackendTypeBinderPairs typeParams
        paramsText = intercalate ", " (map renderValueParam valueParams)
        returnTy = backendExprType bodyExpr
    pure
        [ bindingComment
        , "define "
            ++ renderGlobal (backendBindingName binding)
            ++ typeParamsText
            ++ "("
            ++ paramsText
            ++ ") -> "
            ++ renderBackendType returnTy
            ++ " {"
        , "  ret " ++ renderBackendType returnTy ++ " " ++ renderedBody
        , "}"
        ]
  where
    context = "binding " ++ show (backendBindingName binding)
    (typeParams, afterTypeParams) = collectTypeParams (backendBindingExpr binding)
    (valueParams, bodyExpr) = collectValueParams afterTypeParams
    bindingComment
        | backendBindingExportedAsMain binding = "; exported main"
        | otherwise = "; binding"

collectTypeParams :: BackendExpr -> ([(String, Maybe BackendType)], BackendExpr)
collectTypeParams =
    \case
        BackendTyAbs _ name mbBound body ->
            let (params, core) = collectTypeParams body
             in ((name, mbBound) : params, core)
        expr -> ([], expr)

collectValueParams :: BackendExpr -> ([(String, BackendType)], BackendExpr)
collectValueParams =
    \case
        BackendLam _ name paramTy body ->
            let (params, core) = collectValueParams body
             in ((name, paramTy) : params, core)
        expr -> ([], expr)

renderValueParam :: (String, BackendType) -> String
renderValueParam (name, ty) =
    renderLocal name ++ " : " ++ renderBackendType ty

renderExpr :: RenderEnv -> String -> Int -> BackendExpr -> Either BackendTextError String
renderExpr env context prec =
    \case
        BackendVar _ name ->
            Right (renderVariable env name)
        BackendLit _ lit ->
            Right (renderLiteral lit)
        BackendLam _ name paramTy body -> do
            bodyText <- renderExpr env context 0 body
            Right $
                paren (prec > 0) $
                    "fn ("
                        ++ renderLocal name
                        ++ " : "
                        ++ renderBackendType paramTy
                        ++ ") -> "
                        ++ bodyText
        BackendApp _ fun arg -> do
            funText <- renderExpr env context 2 fun
            argText <- renderExpr env context 0 arg
            Right $
                paren (prec > 1) $
                    "call "
                        ++ funText
                        ++ "("
                        ++ argText
                        ++ ")"
        BackendLet _ name bindingTy rhs body -> do
            rhsText <- renderExpr env context 0 rhs
            bodyText <- renderExpr env context 0 body
            Right $
                paren (prec > 0) $
                    "let "
                        ++ renderLocal name
                        ++ " : "
                        ++ renderBackendType bindingTy
                        ++ " = "
                        ++ rhsText
                        ++ "; "
                        ++ bodyText
        BackendTyAbs _ name mbBound body -> do
            bodyText <- renderExpr env context 0 body
            Right $
                paren (prec > 0) $
                    "type.fn <"
                        ++ renderBackendTypeBinder (name, mbBound)
                        ++ "> -> "
                        ++ bodyText
        BackendTyApp _ fun argTy -> do
            funText <- renderExpr env context 2 fun
            Right $
                paren (prec > 1) $
                    "type.call "
                        ++ funText
                        ++ " ["
                        ++ renderBackendType argTy
                        ++ "]"
        BackendConstruct {} ->
            Left (BackendTextUnsupportedNode context "construct")
        BackendCase {} ->
            Left (BackendTextUnsupportedNode context "case")
        BackendRoll {} ->
            Left (BackendTextUnsupportedNode context "roll")
        BackendUnroll {} ->
            Left (BackendTextUnsupportedNode context "unroll")

renderVariable :: RenderEnv -> String -> String
renderVariable env name
    | Set.member name (renderGlobalNames env) = renderGlobal name
    | otherwise = renderLocal name

renderBackendType :: BackendType -> String
renderBackendType =
    \case
        BTVar name -> renderTypeVar name
        BTArrow dom cod -> "fn (" ++ renderBackendType dom ++ ") -> " ++ renderBackendType cod
        BTBase (BaseTy "Int") -> "i64"
        BTBase (BaseTy "Bool") -> "i1"
        BTBase (BaseTy "String") -> "ptr.str"
        BTBase (BaseTy name) -> renderTypeConstructor name
        BTCon (BaseTy name) args ->
            renderTypeConstructor name ++ "<" ++ intercalate ", " (map renderBackendType (toListNE args)) ++ ">"
        BTForall name mbBound body ->
            "forall <" ++ renderBackendTypeBinder (name, mbBound) ++ ">. " ++ renderBackendType body
        BTMu name body ->
            "mu <" ++ renderTypeVar name ++ ">. " ++ renderBackendType body
        BTBottom -> "bottom"

renderBackendTypeBinders :: [BackendTypeBinder] -> String
renderBackendTypeBinders binders =
    renderBackendTypeBinderPairs [(name, mbBound) | BackendTypeBinder name mbBound <- binders]

renderBackendTypeBinderPairs :: [(String, Maybe BackendType)] -> String
renderBackendTypeBinderPairs [] = ""
renderBackendTypeBinderPairs binders =
    "<" ++ intercalate ", " (map renderBackendTypeBinder binders) ++ ">"

renderPlainTypeParameters :: [String] -> String
renderPlainTypeParameters [] = ""
renderPlainTypeParameters params =
    "<" ++ intercalate ", " (map renderTypeVar params) ++ ">"

renderBackendTypeBinder :: (String, Maybe BackendType) -> String
renderBackendTypeBinder (name, mbBound) =
    renderTypeVar name ++ maybe "" ((" >= " ++) . renderBackendType) mbBound

renderLiteral :: Lit -> String
renderLiteral =
    \case
        LInt n -> show n
        LBool True -> "true"
        LBool False -> "false"
        LString value -> show value

renderBackendTextError :: BackendTextError -> String
renderBackendTextError =
    \case
        BackendTextConversionFailed err ->
            "Backend text conversion failed: " ++ show err
        BackendTextValidationFailed err ->
            "Backend text validation failed: " ++ show err
        BackendTextUnsupportedNode context node ->
            "Unsupported backend text node at " ++ context ++ ": " ++ node

renderGlobal :: String -> String
renderGlobal name =
    "@\"" ++ escapeQuoted name ++ "\""

renderLocal :: String -> String
renderLocal name =
    "%\"" ++ escapeQuoted name ++ "\""

renderTypeVar :: String -> String
renderTypeVar name =
    "$\"" ++ escapeQuoted name ++ "\""

renderTypeConstructor :: String -> String
renderTypeConstructor name
    | not (null name) && all isBareSymbolChar name = "%" ++ name
    | otherwise = "%\"" ++ escapeQuoted name ++ "\""

isBareSymbolChar :: Char -> Bool
isBareSymbolChar char =
    isAlphaNum char || char == '_' || char == '.'

escapeQuoted :: String -> String
escapeQuoted =
    concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar char = [char]

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) =
    x : xs

paren :: Bool -> String -> String
paren True value =
    "(" ++ value ++ ")"
paren False value =
    value
