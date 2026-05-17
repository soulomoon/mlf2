module MLF.Frontend.Parse.Program
    ( ProgramParseError
    , renderProgramParseError
    , parseLocatedProgram
    , parseLocatedProgramWithFile
    , parseRawProgram
    ) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isLower)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import MLF.Frontend.Syntax (SrcTy (..), SrcType, mkSrcBound)
import MLF.Frontend.Syntax.Program
import MLF.Frontend.TypeLevel
    ( TypeFamilyDecl (..)
    , TypeFamilyEquation (..)
    , TypeLevelKind (..)
    , TypeLevelPattern (..)
    , TypeLevelTy (..)
    )
import MLF.Parse.Common
    ( Parser
    , canonicalBigLambdaTok
    , canonicalBottomTok
    , canonicalForallTok
    , canonicalGeTok
    , canonicalLambdaTok
    , lexeme
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
    , satisfy
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
        , "mu"
        , "of"
        , "as"
        , "bottom"
        , "true"
        , "false"
        , "family"
        , "type"
        , "where"
        ]

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

programMuTok :: Parser ()
programMuTok = void (symbol "μ")

semi :: Parser String
semi = symbol ";"

commaSep :: Parser a -> Parser [a]
commaSep item = item `sepBy` symbol ","

commaSep1 :: Parser a -> Parser [a]
commaSep1 item = item `sepBy1` symbol ","

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
        { tpcForallTok = canonicalForallTok
        , tpcTypeLambdaTok = canonicalBigLambdaTok
        , tpcGeTok = canonicalGeTok
        , tpcSymbol = symbol
        , tpcParens = parens
        , tpcLowerIdent = lowerIdent reservedWords
        , tpcUpperIdent = qualifiedUpperIdent
        , tpcBottomTok = canonicalBottomTok
        , tpcMkVar = STVar
        , tpcMkArrow = STArrow
        , tpcMkBase = STBase
        , tpcMkCon = STCon
        , tpcMkVarApp = \v args -> Just (STVarApp v args)
        , tpcMkTypeLam = \v body -> Just (STTyLam v body)
        , tpcMkTypeApp = \fun arg -> Just (STTyApp fun arg)
        , tpcMkForall = \v mb body -> STForall v (fmap mkSrcBound mb) body
        , tpcMkBottom = STBottom
        , tpcBoundedBinder = \pTy ->
            parens $ do
                v <- lowerIdent reservedWords
                canonicalGeTok
                bound <- pTy
                pure (v, Just bound)
        , tpcUnboundedBinder = do
            v <- lowerIdent reservedWords
            pure (v, Nothing)
        , tpcForallBinders = \pTy -> do
            binders <- some (try ((tpcBoundedBinder programTypeConfig) pTy) <|> tpcUnboundedBinder programTypeConfig)
            void (symbol ".")
            pure binders
        }

pType :: Parser SrcType
pType = try pProgramForall <|> try pProgramMu <|> parseArrowTypeWith programTypeConfig pType
  where
    pProgramForall = do
        canonicalForallTok
        binders <- tpcForallBinders programTypeConfig pType
        body <- pType
        pure (foldr (\(v, bnd) acc -> tpcMkForall programTypeConfig v bnd acc) body binders)

    pProgramMu = do
        programMuTok
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
        <|> try
            ( do
                canonicalForallTok
                vars <- some (lowerIdent reservedWords)
                void (symbol ".")
                (constraints, spans) <- pLocatedConstraintList
                void (symbol "=>")
                ty <- pType
                let wrappedTy = foldr (\v acc -> STForall v Nothing acc) ty vars
                pure (ConstrainedType constraints wrappedTy, spans)
            )
        <|> do
            ty <- pType
            pure (unconstrainedType ty, emptyProgramSpanIndex)

pLocatedClassConstraint :: Parser (ClassConstraint, ProgramSpanIndex)
pLocatedClassConstraint = do
    (className, classSpan) <- withSpan qualifiedUpperIdent
    tys <- some pClassArgument
    pure
        ( ClassConstraint
            { constraintClassName = className
            , constraintTypes = NE.fromList tys
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

singletonExportItemSpan :: String -> SourceSpan -> ProgramSpanIndex
singletonExportItemSpan name span0 =
    emptyProgramSpanIndex { spanExportItems = Map.singleton name [span0] }

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
    exports <- optional pLocatedExportList
    (imports, decls) <- braces $ do
        imports <- many (try pLocatedImport)
        decls <- many pLocatedDecl
        pure (imports, decls)
    end <- getSourcePos
    let moduleSpan = sourceSpanFromPositions start end
        module0 =
            Module
                { moduleName = name
                , moduleExports = fmap fst exports
                , moduleImports = map fst imports
                , moduleDecls = map fst decls
                }
        spanIndex =
            singletonModuleSpan name moduleSpan
                `appendProgramSpanIndex` maybe emptyProgramSpanIndex snd exports
                `appendProgramSpanIndex` mergeSpanIndexes (map snd imports)
                `appendProgramSpanIndex` mergeSpanIndexes (map snd decls)
    pure (module0, spanIndex)

pLocatedExportList :: Parser ([ExportItem], ProgramSpanIndex)
pLocatedExportList = do
    void (symbol "export")
    exports <- parens (commaSep pLocatedModuleExportItem)
    pure (map fst exports, mergeSpanIndexes (map snd exports))

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

pLocatedExportItemSpan :: Parser (ExportItem, SourceSpan)
pLocatedExportItemSpan = do
    (item, span0) <- withSpan pExportItem
    pure (item, span0)

pLocatedModuleExportItem :: Parser (ExportItem, ProgramSpanIndex)
pLocatedModuleExportItem = do
    (item, span0) <- pLocatedExportItemSpan
    pure (item, singletonExportItemSpan (exportItemName item) span0)

pLocatedExportItem :: Parser (ExportItem, ProgramSpanIndex)
pLocatedExportItem = do
    (item, span0) <- pLocatedExportItemSpan
    pure (item, singletonImportItemSpan (exportItemName item) span0)

pLocatedDecl :: Parser (Decl, ProgramSpanIndex)
pLocatedDecl =
    choice
        [ fmap (\(decl, spans) -> (DeclClass decl, spans)) (try pLocatedClassDecl)
        , fmap (\(decl, spans) -> (DeclInstance decl, spans)) (try pLocatedInstanceDecl)
        , fmap (\(decl, spans) -> (DeclTypeFamily decl, spans)) (try pLocatedTypeFamilyDecl)
        , fmap (\(decl, spans) -> (DeclData decl, spans)) (try pLocatedDataDecl)
        , fmap (\(decl, spans) -> (DeclDef decl, spans)) pLocatedDefDecl
        ]

pLocatedClassDecl :: Parser (ClassDecl, ProgramSpanIndex)
pLocatedClassDecl = do
    start <- getSourcePos
    void (symbol "class")
    superclasses <- optional $ try $ do
        constraints <- pLocatedConstraintList
        void (symbol "=>")
        pure constraints
    className <- qualifiedUpperIdent
    classParams <- some pTypeParam
    fundeps <- maybe [] id <$> optional pFunctionalDependencies
    methods <- braces (many pLocatedMethodSig)
    end <- getSourcePos
    let classSpan = sourceSpanFromPositions start end
        decl =
            ClassDecl
                { classDeclName = className
                , classDeclSuperclasses = maybe [] fst superclasses
                , classDeclParams = NE.fromList classParams
                , classDeclFundeps = fundeps
                , classDeclMethods = map fst methods
                }
        spans =
            singletonClassSpan className classSpan
                `appendProgramSpanIndex` maybe emptyProgramSpanIndex snd superclasses
                `appendProgramSpanIndex` mergeSpanIndexes (map snd methods)
    pure (decl, spans)

pFunctionalDependencies :: Parser [FunctionalDependency]
pFunctionalDependencies = do
    void (symbol "|")
    commaSep1 pFunctionalDependency

pFunctionalDependency :: Parser FunctionalDependency
pFunctionalDependency = do
    determiners <- some (lowerIdent reservedWords)
    void (symbol "→")
    determined <- some (lowerIdent reservedWords)
    pure
        FunctionalDependency
            { fundepDeterminers = NE.fromList determiners
            , fundepDetermined = NE.fromList determined
            }

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
    instTys <- some pClassArgument
    methods <- braces (many pLocatedMethodDef)
    let decl =
            InstanceDecl
                { instanceDeclConstraints = maybe [] fst constraints
                , instanceDeclClass = className
                , instanceDeclTypes = NE.fromList instTys
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

pClassArgument :: Parser SrcType
pClassArgument =
    choice
        [ parens pType
        , STBottom <$ canonicalBottomTok
        , STBase <$> qualifiedUpperIdent
        , STVar <$> lowerIdent reservedWords
        ]

pLocatedDataDecl :: Parser (DataDecl, ProgramSpanIndex)
pLocatedDataDecl = do
    start <- getSourcePos
    void (symbol "data")
    dataName <- upperIdent reservedWords
    params <- many pTypeParam
    void (symbol "=")
    ctors <- pLocatedConstructorDecl `sepBy1` symbol "|"
    derivingClause <- optional pLocatedDerivingClause
    void semi
    end <- getSourcePos
    let dataSpan = sourceSpanFromPositions start end
        decl =
            DataDecl
                { dataDeclName = dataName
                , dataDeclParams = params
                , dataDeclConstructors = map fst ctors
                , dataDeclDeriving = maybe [] fst derivingClause
                }
        spans =
            singletonTypeSpan dataName dataSpan
                `appendProgramSpanIndex` mergeSpanIndexes (map snd ctors)
                `appendProgramSpanIndex` maybe emptyProgramSpanIndex snd derivingClause
    pure (decl, spans)

pLocatedTypeFamilyDecl :: Parser (TypeFamilyDecl, ProgramSpanIndex)
pLocatedTypeFamilyDecl = do
    start <- getSourcePos
    void (symbol "type")
    void (symbol "family")
    familyName <- upperIdent reservedWords
    params <- many pTypeFamilyParam
    void (symbol "::")
    resultKind <- pTypeLevelKind
    void (symbol "where")
    equations <- braces (some (pTypeFamilyEquation familyName))
    end <- getSourcePos
    let familySpan = sourceSpanFromPositions start end
        decl =
            TypeFamilyDecl
                { familyDeclName = familyName
                , familyDeclParams = params
                , familyDeclResultKind = resultKind
                , familyDeclEquations = equations
                }
    pure (decl, singletonTypeSpan familyName familySpan)

pTypeFamilyParam :: Parser (String, TypeLevelKind)
pTypeFamilyParam =
    try
        ( parens $ do
            name <- lowerIdent reservedWords
            void (symbol "::")
            kind <- pTypeLevelKind
            pure (name, kind)
        )
        <|> do
            name <- lowerIdent reservedWords
            pure (name, TLKType)

pTypeFamilyEquation :: TypeName -> Parser TypeFamilyEquation
pTypeFamilyEquation familyName = do
    equationName <- upperIdent reservedWords
    if equationName == familyName
        then pure ()
        else fail ("expected type family equation for " ++ show familyName ++ ", got " ++ show equationName)
    patterns <- many pTypeLevelPatternAtom
    void (symbol "=")
    rhs <- pTypeLevelType
    void semi
    pure TypeFamilyEquation { familyEquationPatterns = patterns, familyEquationRhs = rhs }

pTypeLevelKind :: Parser TypeLevelKind
pTypeLevelKind = do
    lhs <- pTypeLevelKindAtom
    rhs <- optional (symbol "->" *> pTypeLevelKind)
    pure $ maybe lhs (TLKArrow lhs) rhs

pTypeLevelKindAtom :: Parser TypeLevelKind
pTypeLevelKindAtom =
    (TLKType <$ symbol "*")
        <|> parens pTypeLevelKind
        <|> (TLKVar <$> kindVarIdent)

kindVarIdent :: Parser String
kindVarIdent = lexeme $ try $ do
    first <- satisfy (\c -> isLower c || c == '_')
    rest <- many (satisfy (\c -> isAlphaNum c || c == '_' || c == '\''))
    let name = first : rest
    if Set.member name reservedWords
        then fail ("reserved word " ++ show name)
        else pure name

pTypeLevelType :: Parser TypeLevelTy
pTypeLevelType = try pTypeLevelLam <|> pTypeLevelArrow

pTypeLevelLam :: Parser TypeLevelTy
pTypeLevelLam = do
    canonicalBigLambdaTok
    binders <- some pTypeLevelLamBinder
    void (symbol ".")
    body <- pTypeLevelType
    pure (foldr (\(name, kind) acc -> TLTLam name kind acc) body binders)

pTypeLevelLamBinder :: Parser (String, TypeLevelKind)
pTypeLevelLamBinder =
    try
        ( parens $ do
            name <- lowerIdent reservedWords
            void (symbol "::")
            kind <- pTypeLevelKind
            pure (name, kind)
        )
        <|> do
            name <- lowerIdent reservedWords
            pure (name, TLKType)

pTypeLevelArrow :: Parser TypeLevelTy
pTypeLevelArrow = do
    lhs <- pTypeLevelApp
    rhs <- optional (symbol "->" *> pTypeLevelType)
    pure $ maybe lhs (TLTArrow lhs) rhs

pTypeLevelApp :: Parser TypeLevelTy
pTypeLevelApp = do
    headTy <- pTypeLevelAtom
    args <- many pTypeLevelAtom
    pure (foldl TLTApp headTy args)

pTypeLevelAtom :: Parser TypeLevelTy
pTypeLevelAtom =
    choice
        [ parens pTypeLevelType
        , TLTVar <$> lowerIdent reservedWords
        , TLTCon <$> qualifiedUpperIdent
        ]

pTypeLevelPattern :: Parser TypeLevelPattern
pTypeLevelPattern = try pTypeLevelPatternApp <|> pTypeLevelPatternAtom

pTypeLevelPatternApp :: Parser TypeLevelPattern
pTypeLevelPatternApp = do
    name <- qualifiedUpperIdent
    args <- many pTypeLevelPatternAtom
    pure (TLPCon name args)

pTypeLevelPatternAtom :: Parser TypeLevelPattern
pTypeLevelPatternAtom =
    choice
        [ parens pTypeLevelPattern
        , TLPVar <$> lowerIdent reservedWords
        , TLPCon <$> qualifiedUpperIdent <*> pure []
        ]

pTypeParam :: Parser TypeParam
pTypeParam =
    try pKindedTypeParam
        <|> (firstOrderTypeParam <$> lowerIdent reservedWords)
  where
    pKindedTypeParam =
        parens $ do
            name <- lowerIdent reservedWords
            void (symbol "::")
            TypeParam name <$> pKind

pKind :: Parser SrcKind
pKind = do
    lhs <- pKindAtom
    rhs <- optional (symbol "->" *> pKind)
    pure $ maybe lhs (KArrow lhs) rhs

pKindAtom :: Parser SrcKind
pKindAtom =
    (KType <$ symbol "*")
        <|> parens pKind

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

pLocatedDerivingClause :: Parser ([ClassName], ProgramSpanIndex)
pLocatedDerivingClause = do
    void (symbol "deriving")
    classes <- commaSep (withSpan qualifiedUpperIdent)
    pure
        ( map fst classes
        , mergeSpanIndexes
            [ singletonClassSpan className classSpan
            | (className, classSpan) <- classes
            ]
        )

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
    canonicalLambdaTok
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
