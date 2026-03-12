module MLF.Frontend.Parse (
    -- * Error types
    EmlfParseError,
    NormParseError (..),
    renderEmlfParseError,
    renderNormParseError,
    -- * Raw parser entrypoints (return SrcType / SurfaceExpr)
    parseRawEmlfExpr,
    parseRawEmlfType,
    -- * Normalized parser entrypoints (parse raw, then normalize)
    parseNormEmlfExpr,
    parseNormEmlfType,
) where

import Control.Monad (void)
import Data.Set (Set)
import Data.Void (Void)
import qualified Data.Set as Set
import MLF.Frontend.Normalize
    ( NormalizationError
    , normalizeExpr
    , normalizeType
    )
import MLF.Frontend.Syntax
    ( Expr (..)
    , NormSrcType
    , NormSurfaceExpr
    , SrcTy (..)
    , SrcType
    , SurfaceExpr
    , mkSrcBound
    )
import Text.Megaparsec
    ( ParseErrorBundle
    , choice
    , eof
    , errorBundlePretty
    , many
    , optional
    , parse
    , some
    , try
    , (<|>)
    )
import MLF.Parse.Common
    ( Parser
    , bottomTok
    , forallTok
    , geTok
    , lambdaTok
    , lowerIdent
    , parens
    , pLit
    , sc
    , symbol
    , upperIdent
    )
import MLF.Parse.Type (TypeParserConfig(..), parseArrowTypeWith)

newtype EmlfParseError = EmlfParseError (ParseErrorBundle String Void)
    deriving (Eq, Show)

-- | Errors from normalized parsing: either a parse error or a normalization error.
data NormParseError
    = NormParseErr EmlfParseError
    | NormNormErr NormalizationError
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Raw parser entrypoints
-- ---------------------------------------------------------------------------

-- | Parse a raw eMLF expression (returns 'SurfaceExpr' with 'SrcType' annotations).
parseRawEmlfExpr :: String -> Either EmlfParseError SurfaceExpr
parseRawEmlfExpr input =
    case parse (sc *> pExpr <* eof) "<emlf-expr>" input of
        Left err -> Left (EmlfParseError err)
        Right e -> Right e

-- | Parse a raw eMLF type (returns 'SrcType').
parseRawEmlfType :: String -> Either EmlfParseError SrcType
parseRawEmlfType input =
    case parse (sc *> pType <* eof) "<emlf-type>" input of
        Left err -> Left (EmlfParseError err)
        Right ty -> Right ty

-- ---------------------------------------------------------------------------
-- Normalized parser entrypoints (parse raw, then normalize)
-- ---------------------------------------------------------------------------

-- | Parse an eMLF expression and normalize all type annotations.
-- Returns 'NormSurfaceExpr' with 'NormSrcType' annotations.
parseNormEmlfExpr :: String -> Either NormParseError NormSurfaceExpr
parseNormEmlfExpr input = do
    raw <- mapLeft NormParseErr (parseRawEmlfExpr input)
    mapLeft NormNormErr (normalizeExpr raw)

-- | Parse an eMLF type and normalize it.
-- Returns 'NormSrcType' with alias bounds inlined.
parseNormEmlfType :: String -> Either NormParseError NormSrcType
parseNormEmlfType input = do
    raw <- mapLeft NormParseErr (parseRawEmlfType input)
    mapLeft NormNormErr (normalizeType raw)

-- ---------------------------------------------------------------------------
-- Error rendering
-- ---------------------------------------------------------------------------

renderEmlfParseError :: EmlfParseError -> String
renderEmlfParseError (EmlfParseError err) = errorBundlePretty err

-- | Render a 'NormParseError' to a human-readable string.
renderNormParseError :: NormParseError -> String
renderNormParseError (NormParseErr pe) = renderEmlfParseError pe
renderNormParseError (NormNormErr ne) = "Normalization error: " ++ show ne

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

reservedWords :: Set String
reservedWords =
    Set.fromList
        [ "let"
        , "in"
        , "mu"
        , "true"
        , "false"
        , "forall"
        ]

muTok :: Parser ()
muTok = void (symbol "μ" <|> symbol "mu")

frontendTypeConfig :: TypeParserConfig SrcType (Maybe SrcType)
frontendTypeConfig =
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
            binders <- some (try ((tpcBoundedBinder frontendTypeConfig) pTy) <|> tpcUnboundedBinder frontendTypeConfig)
            void (optional (symbol "."))
            pure binders
        }

pType :: Parser SrcType
pType = try pFrontendForall <|> try pFrontendMu <|> parseArrowTypeWith frontendTypeConfig pType
  where
    pFrontendForall = do
        forallTok
        binders <- tpcForallBinders frontendTypeConfig pType
        body <- pType
        pure (foldr (\(v, bnd) acc -> tpcMkForall frontendTypeConfig v bnd acc) body binders)

    pFrontendMu = do
        muTok
        v <- lowerIdent reservedWords
        void (symbol ".")
        STMu v <$> pType

pExpr :: Parser SurfaceExpr
pExpr = choice [try pLet, try pLambda, pAnnOrApp]

pLet :: Parser SurfaceExpr
pLet = do
    void (symbol "let")
    v <- lowerIdent reservedWords
    mAnn <- optional (symbol ":" *> pType)
    void (symbol "=")
    rhs <- pExpr
    void (symbol "in")
    body <- pExpr
    pure $ case mAnn of
        Nothing -> ELet v rhs body
        Just annTy -> ELet v (EAnn rhs annTy) body

pLambda :: Parser SurfaceExpr
pLambda = try pLambdaParen <|> pLambdaLegacy

pLambdaParen :: Parser SurfaceExpr
pLambdaParen = do
    lambdaTok
    (v, mTy) <- parens $ do
        name <- lowerIdent reservedWords
        mAnn <- optional (symbol ":" *> pType)
        pure (name, mAnn)
    body <- pExpr
    pure $ case mTy of
        Nothing -> ELam v body
        Just ty -> ELamAnn v ty body

pLambdaLegacy :: Parser SurfaceExpr
pLambdaLegacy = do
    lambdaTok
    v <- lowerIdent reservedWords
    mTy <- optional (symbol ":" *> pType)
    void (symbol ".")
    body <- pExpr
    pure $ case mTy of
        Nothing -> ELam v body
        Just ty -> ELamAnn v ty body

pAnnOrApp :: Parser SurfaceExpr
pAnnOrApp = do
    base <- pApp
    mTy <- optional (symbol ":" *> pType)
    pure $ maybe base (EAnn base) mTy

pApp :: Parser SurfaceExpr
pApp = do
    f <- pPostfix
    args <- many pPostfix
    pure (foldl EApp f args)

pPostfix :: Parser SurfaceExpr
pPostfix = pAtom

pAtom :: Parser SurfaceExpr
pAtom =
    choice
        [ pParenExpr
        , ELit <$> pLit
        , EVar <$> lowerIdent reservedWords
        , EVar <$> upperIdent reservedWords
        ]

pParenExpr :: Parser SurfaceExpr
pParenExpr = do
    void (symbol "(")
    e <- pExpr
    mTy <- optional (symbol ":" *> pType)
    void (symbol ")")
    pure $ case mTy of
        Nothing -> e
        Just ty -> EAnn e ty
