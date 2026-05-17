{-# LANGUAGE GADTs #-}

module MLF.Frontend.Pretty.Program
    ( prettyProgram
    ) where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import MLF.Frontend.Pretty (prettyEmlfType)
import MLF.Frontend.Syntax.Program
import MLF.Frontend.Syntax (Lit (..), SrcTy (..), SrcType)
import MLF.Frontend.TypeLevel
    ( TypeFamilyDecl (..)
    , TypeFamilyEquation (..)
    , TypeLevelKind (..)
    , TypeLevelPattern (..)
    , TypeLevelTy (..)
    )

prettyProgram :: Program -> String
prettyProgram (Program modules0) = intercalate "\n\n" (map prettyModule modules0)

prettyModule :: Module -> String
prettyModule mod0 =
    unlines
        [ "module " ++ moduleName mod0 ++ maybe "" prettyExports (moduleExports mod0) ++ " {"
        , indent (unlines (map prettyImport (moduleImports mod0) ++ map prettyDecl (moduleDecls mod0)))
        , "}"
        ]

prettyExports :: [ExportItem] -> String
prettyExports items = " export (" ++ intercalate ", " (map prettyExportItem items) ++ ")"

prettyExportItem :: ExportItem -> String
prettyExportItem item = case item of
    ExportValue name -> name
    ExportType name -> name
    ExportTypeWithConstructors name -> name ++ "(..)"

prettyImport :: Import -> String
prettyImport imp =
    "import "
        ++ importModuleName imp
        ++ maybe "" (" as " ++) (importAlias imp)
        ++ maybe "" (\items -> " exposing (" ++ intercalate ", " (map prettyExportItem items) ++ ")") (importExposing imp)
        ++ ";"

prettyDecl :: Decl -> String
prettyDecl decl = case decl of
    DeclClass classDecl -> prettyClassDecl classDecl
    DeclInstance instanceDecl -> prettyInstanceDecl instanceDecl
    DeclData dataDecl -> prettyDataDecl dataDecl
    DeclTypeFamily familyDecl -> prettyTypeFamilyDecl familyDecl
    DeclDef defDecl -> prettyDefDecl defDecl

prettyClassDecl :: ClassDecl -> String
prettyClassDecl classDecl =
    unlines
        [ "class "
            ++ prettySuperclassPrefix (classDeclSuperclasses classDecl)
            ++ classDeclName classDecl
            ++ concatMap ((" " ++) . prettyTypeParam) (NE.toList (classDeclParams classDecl))
            ++ prettyFundeps (classDeclFundeps classDecl)
            ++ " {"
        , indent (concatMap prettyMethodSig (classDeclMethods classDecl))
        , "}"
        ]

prettyMethodSig :: MethodSig -> String
prettyMethodSig methodSig = methodSigName methodSig ++ " : " ++ prettyConstrainedType (methodSigType methodSig) ++ ";\n"

prettyInstanceDecl :: InstanceDecl -> String
prettyInstanceDecl instanceDecl =
    unlines
        [ "instance "
            ++ prettyConstraintsPrefix (instanceDeclConstraints instanceDecl)
            ++ instanceDeclClass instanceDecl
            ++ concatMap ((" " ++) . prettyClassArgument) (NE.toList (instanceDeclTypes instanceDecl))
            ++ " {"
        , indent (concatMap prettyMethodDef (instanceDeclMethods instanceDecl))
        , "}"
        ]

prettyMethodDef :: MethodDef -> String
prettyMethodDef methodDef = methodDefName methodDef ++ " = " ++ prettyExpr (methodDefExpr methodDef) ++ ";\n"

prettyDataDecl :: DataDecl -> String
prettyDataDecl dataDecl =
    unlines
        [ "data " ++ unwords (dataDeclName dataDecl : map prettyTypeParam (dataDeclParams dataDecl)) ++ " ="
        , indent (intercalate "\n| " (map prettyConstructorDecl (dataDeclConstructors dataDecl)))
              ++ prettyDeriving (dataDeclDeriving dataDecl)
              ++ ";"
        ]

prettyTypeFamilyDecl :: TypeFamilyDecl -> String
prettyTypeFamilyDecl familyDecl =
    unlines
        [ "type family "
            ++ familyDeclName familyDecl
            ++ concatMap ((" " ++) . prettyTypeFamilyParam) (familyDeclParams familyDecl)
            ++ " :: "
            ++ prettyTypeLevelKind (familyDeclResultKind familyDecl)
            ++ " where {"
        , indent (concatMap (prettyTypeFamilyEquation (familyDeclName familyDecl)) (familyDeclEquations familyDecl))
        , "}"
        ]

prettyTypeFamilyParam :: (String, TypeLevelKind) -> String
prettyTypeFamilyParam (name, kind)
    | kind == TLKType = name
    | otherwise = "(" ++ name ++ " :: " ++ prettyTypeLevelKind kind ++ ")"

prettyTypeFamilyEquation :: TypeName -> TypeFamilyEquation -> String
prettyTypeFamilyEquation familyName equation =
    familyName
        ++ concatMap ((" " ++) . prettyTypeLevelPatternArg) (familyEquationPatterns equation)
        ++ " = "
        ++ prettyTypeLevelType (familyEquationRhs equation)
        ++ ";\n"

prettyTypeParam :: TypeParam -> String
prettyTypeParam param
    | typeParamIsFirstOrder param = typeParamName param
    | otherwise = "(" ++ typeParamName param ++ " :: " ++ prettyKind (typeParamKind param) ++ ")"

prettyKind :: SrcKind -> String
prettyKind = go 0
  where
    go :: Int -> SrcKind -> String
    go prec kind0 =
        case kind0 of
            KType -> "*"
            KArrow dom cod ->
                paren (prec > 0) (go 1 dom ++ " -> " ++ go 0 cod)

prettyTypeLevelKind :: TypeLevelKind -> String
prettyTypeLevelKind = go 0
  where
    go :: Int -> TypeLevelKind -> String
    go prec kind0 =
        case kind0 of
            TLKType -> "*"
            TLKVar name -> name
            TLKArrow dom cod ->
                paren (prec > 0) (go 1 dom ++ " -> " ++ go 0 cod)

prettyTypeLevelType :: TypeLevelTy -> String
prettyTypeLevelType = go 0
  where
    go :: Int -> TypeLevelTy -> String
    go prec ty =
        case ty of
            TLTVar name -> name
            TLTCon name -> name
            TLTArrow dom cod ->
                paren (prec > 1) (go 2 dom ++ " -> " ++ go 1 cod)
            TLTLam name kind body ->
                let (binders, tailBody) = collectTypeLevelLams body
                    binderText = unwords (prettyTypeLevelLamBinder (name, kind) : map prettyTypeLevelLamBinder binders)
                 in paren (prec > 0) ("Λ" ++ binderText ++ ". " ++ go 0 tailBody)
            TLTApp fun arg ->
                paren (prec > 2) (go 2 fun ++ " " ++ prettyTypeLevelArg arg)
            TLTFamilyApp name args ->
                paren (prec > 2) (unwords (name : map prettyTypeLevelArg args))

    prettyTypeLevelArg :: TypeLevelTy -> String
    prettyTypeLevelArg ty =
        case ty of
            TLTVar {} -> go 3 ty
            TLTCon {} -> go 3 ty
            _ -> "(" ++ go 0 ty ++ ")"

    collectTypeLevelLams :: TypeLevelTy -> ([(String, TypeLevelKind)], TypeLevelTy)
    collectTypeLevelLams ty =
        case ty of
            TLTLam name kind body ->
                let (rest, tailBody) = collectTypeLevelLams body
                 in ((name, kind) : rest, tailBody)
            _ -> ([], ty)

prettyTypeLevelLamBinder :: (String, TypeLevelKind) -> String
prettyTypeLevelLamBinder (name, kind)
    | kind == TLKType = name
    | otherwise = "(" ++ name ++ " :: " ++ prettyTypeLevelKind kind ++ ")"

prettyTypeLevelPattern :: TypeLevelPattern -> String
prettyTypeLevelPattern pattern0 =
    case pattern0 of
        TLPVar name -> name
        TLPCon name patterns -> unwords (name : map prettyTypeLevelPatternArg patterns)

prettyTypeLevelPatternArg :: TypeLevelPattern -> String
prettyTypeLevelPatternArg pattern0 =
    case pattern0 of
        TLPVar {} -> prettyTypeLevelPattern pattern0
        TLPCon _ [] -> prettyTypeLevelPattern pattern0
        _ -> "(" ++ prettyTypeLevelPattern pattern0 ++ ")"

prettyConstructorDecl :: ConstructorDecl -> String
prettyConstructorDecl ctor = constructorDeclName ctor ++ " : " ++ prettyEmlfType (constructorDeclType ctor)

prettyDeriving :: [ClassName] -> String
prettyDeriving [] = ""
prettyDeriving classes0 = "\nderiving " ++ intercalate ", " classes0

prettyDefDecl :: DefDecl -> String
prettyDefDecl defDecl =
    "def "
        ++ defDeclName defDecl
        ++ " : "
        ++ prettyConstrainedType (defDeclType defDecl)
        ++ " = "
        ++ prettyExpr (defDeclExpr defDecl)
        ++ ";"

prettyConstrainedType :: ConstrainedType -> String
prettyConstrainedType (ConstrainedType constraints ty) =
    prettyConstraintsPrefix constraints ++ prettyEmlfType ty

prettySuperclassPrefix :: [ClassConstraint] -> String
prettySuperclassPrefix [] = ""
prettySuperclassPrefix [constraint] = prettyClassConstraint constraint ++ " => "
prettySuperclassPrefix constraints =
    "(" ++ intercalate ", " (map prettyClassConstraint constraints) ++ ") => "

prettyFundeps :: [FunctionalDependency] -> String
prettyFundeps [] = ""
prettyFundeps fundeps =
    " | " ++ intercalate ", " (map prettyFundep fundeps)

prettyFundep :: FunctionalDependency -> String
prettyFundep fundep =
    unwords (NE.toList (fundepDeterminers fundep))
        ++ " → "
        ++ unwords (NE.toList (fundepDetermined fundep))

prettyConstraintsPrefix :: [ClassConstraint] -> String
prettyConstraintsPrefix [] = ""
prettyConstraintsPrefix [constraint] = prettyClassConstraint constraint ++ " => "
prettyConstraintsPrefix constraints =
    "(" ++ intercalate ", " (map prettyClassConstraint constraints) ++ ") => "

prettyClassConstraint :: ClassConstraint -> String
prettyClassConstraint constraint =
    constraintClassName constraint
        ++ concatMap ((" " ++) . prettyClassArgument) (NE.toList (constraintTypes constraint))

prettyClassArgument :: SrcType -> String
prettyClassArgument ty =
    case ty of
        STBase {} -> prettyEmlfType ty
        STVar {} -> prettyEmlfType ty
        STBottom -> prettyEmlfType ty
        _ -> "(" ++ prettyEmlfType ty ++ ")"

prettyExpr :: Expr -> String
prettyExpr expr = go 0 expr
  where
    go :: Int -> Expr -> String
    go prec term = case term of
        EVar name -> name
        ELit lit -> prettyLit lit
        ELam param body -> paren (prec > 0) ("λ" ++ prettyParam param ++ " " ++ go 0 body)
        EApp fun arg -> paren (prec > 1) (go 1 fun ++ " " ++ goArg arg)
        ELet name ann rhs body ->
            paren (prec > 0)
                ( "let "
                    ++ name
                    ++ maybe "" ((" : " ++) . prettyEmlfType) ann
                    ++ " = "
                    ++ go 0 rhs
                    ++ " in "
                    ++ go 0 body
                )
        EAnn inner ty -> paren True (go 0 inner ++ " : " ++ prettyEmlfType ty)
        ECase scrutinee alts ->
            paren (prec > 0)
                ( "case "
                    ++ go 0 scrutinee
                    ++ " of { "
                    ++ intercalate "; " (map prettyAlt alts)
                    ++ " }"
                )

    goArg :: Expr -> String
    goArg term = case term of
        EVar {} -> go 2 term
        ELit {} -> go 2 term
        EAnn {} -> go 2 term
        _ -> "(" ++ go 0 term ++ ")"

prettyParam :: Param -> String
prettyParam param =
    "(" ++ paramName param ++ maybe "" ((" : " ++) . prettyEmlfType) (paramType param) ++ ")"

prettyAlt :: Alt -> String
prettyAlt alt = prettyPattern (altPattern alt) ++ " -> " ++ prettyExpr (altExpr alt)

prettyPattern :: Pattern -> String
prettyPattern pat = case pat of
    PatCtor ctor patterns -> unwords (ctor : map prettyPatternArg patterns)
    PatVar name -> name
    PatWildcard -> "_"
    PatAnn inner ty -> "(" ++ prettyPattern inner ++ " : " ++ prettyEmlfType ty ++ ")"

prettyPatternArg :: Pattern -> String
prettyPatternArg pat = case pat of
    PatVar {} -> prettyPattern pat
    PatWildcard -> prettyPattern pat
    PatCtor _ [] -> prettyPattern pat
    _ -> "(" ++ prettyPattern pat ++ ")"

prettyLit :: Lit -> String
prettyLit lit = case lit of
    LInt i -> show i
    LBool b -> if b then "true" else "false"
    LString s -> show s

indent :: String -> String
indent = unlines . map ("  " ++) . lines

paren :: Bool -> String -> String
paren True s = "(" ++ s ++ ")"
paren False s = s
