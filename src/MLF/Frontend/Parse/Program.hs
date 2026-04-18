module MLF.Frontend.Parse.Program
    ( ProgramParseError
    , renderProgramParseError
    , parseRawProgram
    ) where

import Control.Monad (void)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import MLF.Frontend.Syntax.Program
import MLF.Frontend.Syntax (SrcTy (..), SrcType, mkSrcBound)
import MLF.Parse.Common
    ( Parser
    , bottomTok
    , forallTok
    , geTok
    , lambdaTok
    , lowerIdent
    , pLit
    , parens
    , sc
    , symbol
    , upperIdent
    )
import MLF.Parse.Type (TypeParserConfig (..), parseArrowTypeWith)
import Text.Megaparsec
    ( ParseErrorBundle
    , between
    , choice
    , eof
    , errorBundlePretty
    , many
    , optional
    , parse
    , sepBy
    , sepBy1
    , some
    , try
    , (<|>)
    )

newtype ProgramParseError = ProgramParseError (ParseErrorBundle String Void)
    deriving (Eq, Show)

renderProgramParseError :: ProgramParseError -> String
renderProgramParseError (ProgramParseError err) = errorBundlePretty err

parseRawProgram :: String -> Either ProgramParseError Program
parseRawProgram input =
    case parse (sc *> pProgram <* eof) "<mlf-program>" input of
        Left err -> Left (ProgramParseError err)
        Right program -> Right program

reservedWords :: Set String
reservedWords =
    Set.fromList
        [ "case"
        , "class"
        , "data"
        , "def"
        , "deriving"
        , "export"
        , "exposing"
        , "forall"
        , "import"
        , "in"
        , "instance"
        , "let"
        , "module"
        , "of"
        , "true"
        , "false"
        ]

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semi :: Parser String
semi = symbol ";"

commaSep :: Parser a -> Parser [a]
commaSep item = item `sepBy` symbol ","

programTypeConfig :: TypeParserConfig SrcType (Maybe SrcType)
programTypeConfig =
    TypeParserConfig
        { tpcForallTok = forallTok
        , tpcGeTok = geTok
        , tpcSymbol = symbol
        , tpcParens = parens
        , tpcLowerIdent = lowerIdent reservedWords
        , tpcUpperIdent = upperIdent reservedWords
        , tpcBottomTok = bottomTok
        , tpcMkVar = STVar
        , tpcMkArrow = STArrow
        , tpcMkBase = STBase
        , tpcMkCon = STCon
        , tpcMkForall = \v mb body -> STForall v (fmap mkSrcBound mb) body
        , tpcMkBottom = STBottom
        , tpcBoundedBinder = \pTy ->
            parens $ do
                v <- lowerIdent reservedWords
                geTok
                bound <- pTy
                pure (v, Just bound)
        , tpcUnboundedBinder = do
            v <- lowerIdent reservedWords
            pure (v, Nothing)
        , tpcForallBinders = \pTy -> do
            binders <- some (try ((tpcBoundedBinder programTypeConfig) pTy) <|> tpcUnboundedBinder programTypeConfig)
            void (optional (symbol "."))
            pure binders
        }

pType :: Parser SrcType
pType = try pProgramForall <|> try pProgramMu <|> parseArrowTypeWith programTypeConfig pType
  where
    pProgramForall = do
        forallTok
        binders <- tpcForallBinders programTypeConfig pType
        body <- pType
        pure (foldr (\(v, bnd) acc -> tpcMkForall programTypeConfig v bnd acc) body binders)

    pProgramMu = do
        void (symbol "μ" <|> symbol "mu")
        v <- lowerIdent reservedWords
        void (symbol ".")
        STMu v <$> pType

pProgram :: Parser Program
pProgram = Program <$> some pModule

pModule :: Parser Module
pModule = do
    void (symbol "module")
    name <- upperIdent reservedWords
    exports <- optional pExportList
    (imports, decls) <- braces $ do
        imports <- many (try pImport)
        decls <- many pDecl
        pure (imports, decls)
    pure Module
        { moduleName = name
        , moduleExports = exports
        , moduleImports = imports
        , moduleDecls = decls
        }

pExportList :: Parser [ExportItem]
pExportList = do
    void (symbol "export")
    parens (commaSep pExportItem)

pExportItem :: Parser ExportItem
pExportItem =
    try pExportTypeWithConstructors
        <|> try (ExportType <$> upperIdent reservedWords)
        <|> (ExportValue <$> lowerIdent reservedWords)
  where
    pExportTypeWithConstructors = do
        tyName <- upperIdent reservedWords
        void (symbol "(")
        void (symbol "..")
        void (symbol ")")
        pure (ExportTypeWithConstructors tyName)

pImport :: Parser Import
pImport = do
    void (symbol "import")
    name <- upperIdent reservedWords
    exposing <- optional $ do
        void (symbol "exposing")
        parens (commaSep pExportItem)
    void semi
    pure Import
        { importModuleName = name
        , importExposing = exposing
        }

pDecl :: Parser Decl
pDecl =
    choice
        [ DeclClass <$> try pClassDecl
        , DeclInstance <$> try pInstanceDecl
        , DeclData <$> try pDataDecl
        , DeclDef <$> pDefDecl
        ]

pClassDecl :: Parser ClassDecl
pClassDecl = do
    void (symbol "class")
    className <- upperIdent reservedWords
    classParam <- lowerIdent reservedWords
    methods <- braces (many pMethodSig)
    pure ClassDecl
        { classDeclName = className
        , classDeclParam = classParam
        , classDeclMethods = methods
        }

pMethodSig :: Parser MethodSig
pMethodSig = do
    name <- lowerIdent reservedWords
    void (symbol ":")
    ty <- pType
    void semi
    pure MethodSig
        { methodSigName = name
        , methodSigType = ty
        }

pInstanceDecl :: Parser InstanceDecl
pInstanceDecl = do
    void (symbol "instance")
    className <- upperIdent reservedWords
    instTy <- pType
    methods <- braces (many pMethodDef)
    pure InstanceDecl
        { instanceDeclClass = className
        , instanceDeclType = instTy
        , instanceDeclMethods = methods
        }

pMethodDef :: Parser MethodDef
pMethodDef = do
    name <- lowerIdent reservedWords
    void (symbol "=")
    body <- pExpr
    void semi
    pure MethodDef
        { methodDefName = name
        , methodDefExpr = body
        }

pDataDecl :: Parser DataDecl
pDataDecl = do
    void (symbol "data")
    dataName <- upperIdent reservedWords
    params <- many (lowerIdent reservedWords)
    void (symbol "=")
    ctors <- pConstructorDecl `sepBy1` symbol "|"
    derivingClasses <- maybe [] id <$> optional pDerivingClause
    void semi
    pure DataDecl
        { dataDeclName = dataName
        , dataDeclParams = params
        , dataDeclConstructors = ctors
        , dataDeclDeriving = derivingClasses
        }

pConstructorDecl :: Parser ConstructorDecl
pConstructorDecl = do
    ctorName <- upperIdent reservedWords
    void (symbol ":")
    ctorTy <- pType
    pure ConstructorDecl
        { constructorDeclName = ctorName
        , constructorDeclType = ctorTy
        }

pDerivingClause :: Parser [ClassName]
pDerivingClause = do
    void (symbol "deriving")
    commaSep (upperIdent reservedWords)

pDefDecl :: Parser DefDecl
pDefDecl = do
    void (symbol "def")
    name <- lowerIdent reservedWords
    void (symbol ":")
    ty <- pType
    void (symbol "=")
    body <- pExpr
    void semi
    pure DefDecl
        { defDeclName = name
        , defDeclType = ty
        , defDeclExpr = body
        }

pExpr :: Parser Expr
pExpr = choice [try pLet, try pLambda, try pCase, pAnnOrApp]

pLet :: Parser Expr
pLet = do
    void (symbol "let")
    name <- lowerIdent reservedWords
    ann <- optional (symbol ":" *> pType)
    void (symbol "=")
    rhs <- pExpr
    void (symbol "in")
    body <- pExpr
    pure (ELet name ann rhs body)

pLambda :: Parser Expr
pLambda = do
    lambdaTok
    param <- pParam
    body <- pExpr
    pure (ELam param body)

pParam :: Parser Param
pParam = try pParenParam <|> pBareParam
  where
    pParenParam = parens $ do
        name <- lowerIdent reservedWords
        ann <- optional (symbol ":" *> pType)
        pure Param { paramName = name, paramType = ann }

    pBareParam = do
        name <- lowerIdent reservedWords
        ann <- optional (symbol ":" *> pType)
        _ <- optional (symbol ".")
        pure Param { paramName = name, paramType = ann }

pCase :: Parser Expr
pCase = do
    void (symbol "case")
    scrutinee <- pExpr
    void (symbol "of")
    alts <- braces (pAlt `sepBy1` semi)
    pure (ECase scrutinee alts)

pAlt :: Parser Alt
pAlt = do
    pat <- pPattern
    void (symbol "->")
    body <- pExpr
    pure Alt { altPattern = pat, altExpr = body }

pPattern :: Parser Pattern
pPattern =
    choice
        [ PatWildcard <$ symbol "_"
        , try $ do
            ctor <- upperIdent reservedWords
            binders <- many (lowerIdent reservedWords)
            pure (PatCtor ctor binders)
        , PatVar <$> lowerIdent reservedWords
        ]

pAnnOrApp :: Parser Expr
pAnnOrApp = do
    headExpr <- pApp
    ann <- optional (symbol ":" *> pType)
    pure (maybe headExpr (EAnn headExpr) ann)

pApp :: Parser Expr
pApp = do
    fun <- pAtom
    args <- many pAtom
    pure (foldl EApp fun args)

pAtom :: Parser Expr
pAtom =
    choice
        [ parens pExpr
        , ELit <$> pLit
        , EVar <$> upperIdent reservedWords
        , EVar <$> lowerIdent reservedWords
        ]
