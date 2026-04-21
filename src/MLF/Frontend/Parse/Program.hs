module MLF.Frontend.Parse.Program
    ( ProgramParseError
    , renderProgramParseError
    , parseLocatedProgram
    , parseLocatedProgramWithFile
    , parseRawProgram
    ) where

import Control.Monad (void)
import qualified Data.Map.Strict as Map
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
    , getSourcePos
    , many
    , optional
    , parse
    , sepBy
    , sepBy1
    , some
    , try
    , (<|>)
    )
import qualified Text.Megaparsec.Pos as MP
import Text.Megaparsec.Pos (unPos)

newtype ProgramParseError = ProgramParseError (ParseErrorBundle String Void)
    deriving (Eq, Show)

renderProgramParseError :: ProgramParseError -> String
renderProgramParseError (ProgramParseError err) = errorBundlePretty err

parseRawProgram :: String -> Either ProgramParseError Program
parseRawProgram input = locatedProgram <$> parseLocatedProgram input

parseLocatedProgram :: String -> Either ProgramParseError LocatedProgram
parseLocatedProgram = parseLocatedProgramWithFile "<mlf-program>"

parseLocatedProgramWithFile :: FilePath -> String -> Either ProgramParseError LocatedProgram
parseLocatedProgramWithFile path input =
    case parse (sc *> pLocatedProgram <* eof) path input of
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
        , "as"
        , "true"
        , "false"
        ]

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semi :: Parser String
semi = symbol ";"

commaSep :: Parser a -> Parser [a]
commaSep item = item `sepBy` symbol ","

qualifiedUpperIdent :: Parser String
qualifiedUpperIdent =
    try
        ( do
            qualifier <- upperIdent reservedWords
            void (symbol ".")
            name <- upperIdent reservedWords
            pure (qualifier ++ "." ++ name)
        )
        <|> upperIdent reservedWords

qualifiedLowerIdent :: Parser String
qualifiedLowerIdent =
    try
        ( do
            qualifier <- upperIdent reservedWords
            void (symbol ".")
            name <- lowerIdent reservedWords
            pure (qualifier ++ "." ++ name)
        )
        <|> lowerIdent reservedWords

programTypeConfig :: TypeParserConfig SrcType (Maybe SrcType)
programTypeConfig =
    TypeParserConfig
        { tpcForallTok = forallTok
        , tpcGeTok = geTok
        , tpcSymbol = symbol
        , tpcParens = parens
        , tpcLowerIdent = lowerIdent reservedWords
        , tpcUpperIdent = qualifiedUpperIdent
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

pLocatedConstrainedType :: Parser (ConstrainedType, ProgramSpanIndex)
pLocatedConstrainedType =
    try
        ( do
            (constraints, spans) <- pLocatedConstraintList
            void (symbol "=>")
            ty <- pType
            pure (ConstrainedType constraints ty, spans)
        )
        <|> do
            ty <- pType
            pure (unconstrainedType ty, emptyProgramSpanIndex)

pLocatedClassConstraint :: Parser (ClassConstraint, ProgramSpanIndex)
pLocatedClassConstraint = do
    (className, classSpan) <- withSpan qualifiedUpperIdent
    ty <- pType
    pure
        ( ClassConstraint
            { constraintClassName = className
            , constraintType = ty
            }
        , singletonClassSpan className classSpan
        )

pLocatedConstraintList :: Parser ([ClassConstraint], ProgramSpanIndex)
pLocatedConstraintList = do
    constraints <-
        try (parens (commaSep pLocatedClassConstraint))
            <|> fmap (: []) pLocatedClassConstraint
    pure (map fst constraints, mergeSpanIndexes (map snd constraints))

pLocatedProgram :: Parser LocatedProgram
pLocatedProgram = do
    modules <- some pLocatedModule
    let program = Program (map fst modules)
        spanIndex = foldr appendProgramSpanIndex emptyProgramSpanIndex (map snd modules)
    pure LocatedProgram { locatedProgram = program, locatedProgramSpans = spanIndex }

withSpan :: Parser a -> Parser (a, SourceSpan)
withSpan parser = do
    start <- getSourcePos
    value <- parser
    end <- getSourcePos
    pure (value, sourceSpanFromPositions start end)

sourceSpanFromPositions :: MP.SourcePos -> MP.SourcePos -> SourceSpan
sourceSpanFromPositions start end =
    SourceSpan
        { sourceFile = MP.sourceName start
        , sourceStart = sourcePositionFromMegaparsec start
        , sourceEnd = sourcePositionFromMegaparsec end
        }

sourcePositionFromMegaparsec :: MP.SourcePos -> SourcePosition
sourcePositionFromMegaparsec pos =
    SourcePosition
        (unPos (MP.sourceLine pos))
        (unPos (MP.sourceColumn pos))

singletonModuleSpan :: ModuleName -> SourceSpan -> ProgramSpanIndex
singletonModuleSpan name span0 =
    emptyProgramSpanIndex { spanModules = Map.singleton name span0 }

singletonImportSpan :: ModuleName -> SourceSpan -> ProgramSpanIndex
singletonImportSpan name span0 =
    emptyProgramSpanIndex { spanImports = Map.singleton name [span0] }

singletonImportAliasSpan :: ModuleName -> SourceSpan -> ProgramSpanIndex
singletonImportAliasSpan name span0 =
    emptyProgramSpanIndex { spanImportAliases = Map.singleton name [span0] }

singletonImportItemSpan :: String -> SourceSpan -> ProgramSpanIndex
singletonImportItemSpan name span0 =
    emptyProgramSpanIndex { spanImportItems = Map.singleton name [span0] }

singletonValueSpan :: ValueName -> SourceSpan -> ProgramSpanIndex
singletonValueSpan name span0 =
    emptyProgramSpanIndex { spanValues = Map.singleton name [span0] }

singletonTypeSpan :: TypeName -> SourceSpan -> ProgramSpanIndex
singletonTypeSpan name span0 =
    emptyProgramSpanIndex { spanTypes = Map.singleton name [span0] }

singletonConstructorSpan :: ConstructorName -> SourceSpan -> ProgramSpanIndex
singletonConstructorSpan name span0 =
    emptyProgramSpanIndex { spanConstructors = Map.singleton name [span0] }

singletonClassSpan :: ClassName -> SourceSpan -> ProgramSpanIndex
singletonClassSpan name span0 =
    emptyProgramSpanIndex { spanClasses = Map.singleton name [span0] }

mergeSpanIndexes :: [ProgramSpanIndex] -> ProgramSpanIndex
mergeSpanIndexes = foldr appendProgramSpanIndex emptyProgramSpanIndex

pLocatedModule :: Parser (Module, ProgramSpanIndex)
pLocatedModule = do
    start <- getSourcePos
    void (symbol "module")
    name <- upperIdent reservedWords
    exports <- optional pExportList
    (imports, decls) <- braces $ do
        imports <- many (try pLocatedImport)
        decls <- many pLocatedDecl
        pure (imports, decls)
    end <- getSourcePos
    let moduleSpan = sourceSpanFromPositions start end
        module0 =
            Module
                { moduleName = name
                , moduleExports = exports
                , moduleImports = map fst imports
                , moduleDecls = map fst decls
                }
        spanIndex =
            singletonModuleSpan name moduleSpan
                `appendProgramSpanIndex` mergeSpanIndexes (map snd imports)
                `appendProgramSpanIndex` mergeSpanIndexes (map snd decls)
    pure (module0, spanIndex)

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

pLocatedImport :: Parser (Import, ProgramSpanIndex)
pLocatedImport = do
    void (symbol "import")
    (name, nameSpan) <- withSpan (upperIdent reservedWords)
    alias <- optional $ do
        void (symbol "as")
        withSpan (upperIdent reservedWords)
    exposing <- optional $ do
        void (symbol "exposing")
        parens (commaSep pLocatedExportItem)
    void semi
    let import0 =
            Import
                { importModuleName = name
                , importAlias = fst <$> alias
                , importExposing = fmap (map fst) exposing
                }
        spans =
            singletonImportSpan name nameSpan
                `appendProgramSpanIndex` maybe emptyProgramSpanIndex (uncurry singletonImportAliasSpan) alias
                `appendProgramSpanIndex` mergeSpanIndexes (maybe [] (map snd) exposing)
    pure (import0, spans)

pLocatedExportItem :: Parser (ExportItem, ProgramSpanIndex)
pLocatedExportItem = do
    (item, span0) <- withSpan pExportItem
    pure (item, singletonImportItemSpan (exportItemName item) span0)

exportItemName :: ExportItem -> String
exportItemName item =
    case item of
        ExportValue name -> name
        ExportType name -> name
        ExportTypeWithConstructors name -> name

pLocatedDecl :: Parser (Decl, ProgramSpanIndex)
pLocatedDecl =
    choice
        [ fmap (\(decl, spans) -> (DeclClass decl, spans)) (try pLocatedClassDecl)
        , fmap (\(decl, spans) -> (DeclInstance decl, spans)) (try pLocatedInstanceDecl)
        , fmap (\(decl, spans) -> (DeclData decl, spans)) (try pLocatedDataDecl)
        , fmap (\(decl, spans) -> (DeclDef decl, spans)) pLocatedDefDecl
        ]

pLocatedClassDecl :: Parser (ClassDecl, ProgramSpanIndex)
pLocatedClassDecl = do
    start <- getSourcePos
    void (symbol "class")
    className <- qualifiedUpperIdent
    classParam <- lowerIdent reservedWords
    methods <- braces (many pLocatedMethodSig)
    end <- getSourcePos
    let classSpan = sourceSpanFromPositions start end
        decl =
            ClassDecl
                { classDeclName = className
                , classDeclParam = classParam
                , classDeclMethods = map fst methods
                }
        spans =
            singletonClassSpan className classSpan
                `appendProgramSpanIndex` mergeSpanIndexes (map snd methods)
    pure (decl, spans)

pLocatedMethodSig :: Parser (MethodSig, ProgramSpanIndex)
pLocatedMethodSig = do
    start <- getSourcePos
    name <- lowerIdent reservedWords
    void (symbol ":")
    (ty, constraintSpans) <- pLocatedConstrainedType
    void semi
    end <- getSourcePos
    let span0 = sourceSpanFromPositions start end
        sig =
            MethodSig
                { methodSigName = name
                , methodSigType = ty
                }
    pure
        ( sig
        , singletonValueSpan name span0
            `appendProgramSpanIndex` constraintSpans
        )

pLocatedInstanceDecl :: Parser (InstanceDecl, ProgramSpanIndex)
pLocatedInstanceDecl = do
    void (symbol "instance")
    constraints <- optional $ try $ do
        constraints0 <- pLocatedConstraintList
        void (symbol "=>")
        pure constraints0
    (className, classSpan) <- withSpan qualifiedUpperIdent
    instTy <- pType
    methods <- braces (many pLocatedMethodDef)
    let decl =
            InstanceDecl
                { instanceDeclConstraints = maybe [] fst constraints
                , instanceDeclClass = className
                , instanceDeclType = instTy
                , instanceDeclMethods = map fst methods
                }
        spans =
            maybe emptyProgramSpanIndex snd constraints
                `appendProgramSpanIndex` singletonClassSpan className classSpan
                `appendProgramSpanIndex` mergeSpanIndexes (map snd methods)
    pure (decl, spans)

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

pLocatedMethodDef :: Parser (MethodDef, ProgramSpanIndex)
pLocatedMethodDef = do
    (methodDef, span0) <- withSpan pMethodDef
    pure (methodDef, singletonValueSpan (methodDefName methodDef) span0)

pLocatedDataDecl :: Parser (DataDecl, ProgramSpanIndex)
pLocatedDataDecl = do
    start <- getSourcePos
    void (symbol "data")
    dataName <- upperIdent reservedWords
    params <- many (lowerIdent reservedWords)
    void (symbol "=")
    ctors <- pLocatedConstructorDecl `sepBy1` symbol "|"
    derivingClasses <- maybe [] id <$> optional pDerivingClause
    void semi
    end <- getSourcePos
    let dataSpan = sourceSpanFromPositions start end
        decl =
            DataDecl
                { dataDeclName = dataName
                , dataDeclParams = params
                , dataDeclConstructors = map fst ctors
                , dataDeclDeriving = derivingClasses
                }
        spans =
            singletonTypeSpan dataName dataSpan
                `appendProgramSpanIndex` mergeSpanIndexes (map snd ctors)
    pure (decl, spans)

pConstructorDecl :: Parser ConstructorDecl
pConstructorDecl = do
    ctorName <- upperIdent reservedWords
    void (symbol ":")
    ctorTy <- pType
    pure ConstructorDecl
        { constructorDeclName = ctorName
        , constructorDeclType = ctorTy
        }

pLocatedConstructorDecl :: Parser (ConstructorDecl, ProgramSpanIndex)
pLocatedConstructorDecl = do
    (ctor, span0) <- withSpan pConstructorDecl
    pure (ctor, singletonConstructorSpan (constructorDeclName ctor) span0)

pDerivingClause :: Parser [ClassName]
pDerivingClause = do
    void (symbol "deriving")
    commaSep qualifiedUpperIdent

pLocatedDefDecl :: Parser (DefDecl, ProgramSpanIndex)
pLocatedDefDecl = do
    start <- getSourcePos
    void (symbol "def")
    name <- lowerIdent reservedWords
    void (symbol ":")
    (ty, constraintSpans) <- pLocatedConstrainedType
    void (symbol "=")
    body <- pExpr
    void semi
    end <- getSourcePos
    let span0 = sourceSpanFromPositions start end
        def =
            DefDecl
                { defDeclName = name
                , defDeclType = ty
                , defDeclExpr = body
                }
    pure
        ( def
        , singletonValueSpan name span0
            `appendProgramSpanIndex` constraintSpans
        )

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
pPattern = pPatternAnn

pPatternAnn :: Parser Pattern
pPatternAnn = do
    pat <- pPatternApp
    ann <- optional (symbol ":" *> pType)
    pure (maybe pat (PatAnn pat) ann)

pPatternApp :: Parser Pattern
pPatternApp =
    try
        ( do
            ctor <- qualifiedUpperIdent
            args <- many pPatternAtom
            pure (PatCtor ctor args)
        )
        <|> pPatternAtom

pPatternAtom :: Parser Pattern
pPatternAtom =
    choice
        [ PatWildcard <$ symbol "_"
        , parens pPattern
        , PatCtor <$> qualifiedUpperIdent <*> pure []
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
        , EVar <$> qualifiedLowerIdent
        , EVar <$> qualifiedUpperIdent
        ]
