module MLF.Frontend.Pretty.Program
    ( prettyProgram
    ) where

import Data.List (intercalate)
import MLF.Frontend.Pretty (prettyEmlfType)
import MLF.Frontend.Syntax.Program
import MLF.Frontend.Syntax (Lit (..))

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
    DeclDef defDecl -> prettyDefDecl defDecl

prettyClassDecl :: ClassDecl -> String
prettyClassDecl classDecl =
    unlines
        [ "class " ++ classDeclName classDecl ++ " " ++ classDeclParam classDecl ++ " {"
        , indent (concatMap prettyMethodSig (classDeclMethods classDecl))
        , "}"
        ]

prettyMethodSig :: MethodSig -> String
prettyMethodSig methodSig = methodSigName methodSig ++ " : " ++ prettyConstrainedType (methodSigType methodSig) ++ ";\n"

prettyInstanceDecl :: InstanceDecl -> String
prettyInstanceDecl instanceDecl =
    unlines
        [ "instance " ++ prettyConstraintsPrefix (instanceDeclConstraints instanceDecl) ++ instanceDeclClass instanceDecl ++ " " ++ prettyEmlfType (instanceDeclType instanceDecl) ++ " {"
        , indent (concatMap prettyMethodDef (instanceDeclMethods instanceDecl))
        , "}"
        ]

prettyMethodDef :: MethodDef -> String
prettyMethodDef methodDef = methodDefName methodDef ++ " = " ++ prettyExpr (methodDefExpr methodDef) ++ ";\n"

prettyDataDecl :: DataDecl -> String
prettyDataDecl dataDecl =
    unlines
        [ "data " ++ unwords (dataDeclName dataDecl : dataDeclParams dataDecl) ++ " ="
        , indent (intercalate "\n| " (map prettyConstructorDecl (dataDeclConstructors dataDecl)))
              ++ prettyDeriving (dataDeclDeriving dataDecl)
              ++ ";"
        ]

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

prettyConstraintsPrefix :: [ClassConstraint] -> String
prettyConstraintsPrefix [] = ""
prettyConstraintsPrefix [constraint] = prettyClassConstraint constraint ++ " => "
prettyConstraintsPrefix constraints =
    "(" ++ intercalate ", " (map prettyClassConstraint constraints) ++ ") => "

prettyClassConstraint :: ClassConstraint -> String
prettyClassConstraint constraint =
    constraintClassName constraint ++ " " ++ prettyEmlfType (constraintType constraint)

prettyExpr :: Expr -> String
prettyExpr expr = go 0 expr
  where
    go :: Int -> Expr -> String
    go prec term = case term of
        EVar name -> name
        ELit lit -> prettyLit lit
        ELam param body -> paren (prec > 0) ("\\" ++ prettyParam param ++ " " ++ go 0 body)
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
