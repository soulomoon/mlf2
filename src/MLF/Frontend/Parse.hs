module MLF.Frontend.Parse (
    EmlfParseError,
    parseEmlfExpr,
    parseEmlfType,
    renderEmlfParseError
) where

import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import MLF.Frontend.Syntax
    ( Expr (..)
    , Lit (..)
    , SrcType (..)
    , SurfaceExpr
    )
import Text.Megaparsec
    ( Parsec
    , ParseErrorBundle
    , between
    , choice
    , eof
    , errorBundlePretty
    , many
    , optional
    , parse
    , satisfy
    , some
    , try
    , (<|>)
    )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

newtype EmlfParseError = EmlfParseError (ParseErrorBundle String Void)
    deriving (Eq, Show)

parseEmlfExpr :: String -> Either EmlfParseError SurfaceExpr
parseEmlfExpr input =
    case parse (sc *> pExpr <* eof) "<emlf-expr>" input of
        Left err -> Left (EmlfParseError err)
        Right e -> Right e

parseEmlfType :: String -> Either EmlfParseError SrcType
parseEmlfType input =
    case parse (sc *> pType <* eof) "<emlf-type>" input of
        Left err -> Left (EmlfParseError err)
        Right ty -> Right ty

renderEmlfParseError :: EmlfParseError -> String
renderEmlfParseError (EmlfParseError err) = errorBundlePretty err

sc :: Parser ()
sc =
    L.space
        C.space1
        (L.skipLineComment "--")
        (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reservedWords :: Set String
reservedWords =
    Set.fromList
        [ "let"
        , "in"
        , "true"
        , "false"
        , "forall"
        ]

identifier :: Parser String
identifier = lexeme $ try $ do
    x <- satisfy isIdentStart
    xs <- many (satisfy isIdentContinue)
    let name = x : xs
    if Set.member name reservedWords
        then fail ("reserved word " ++ show name)
        else pure name

lowerIdent :: Parser String
lowerIdent = try $ do
    name <- identifier
    case name of
        (c:_) | isAsciiLower c || c == '_' -> pure name
        _ -> fail ("expected lowercase identifier, got " ++ show name)

upperIdent :: Parser String
upperIdent = try $ do
    name <- identifier
    case name of
        (c:_) | isAsciiUpper c -> pure name
        _ -> fail ("expected uppercase identifier, got " ++ show name)

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isAsciiLower c || isAsciiUpper c

isIdentContinue :: Char -> Bool
isIdentContinue c =
    isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c == '_'
        || c == '\''

forallTok :: Parser ()
forallTok = void (symbol "∀" <|> symbol "forall")

lambdaTok :: Parser ()
lambdaTok = void (symbol "λ" <|> symbol "\\")

geTok :: Parser ()
geTok = void (symbol "⩾" <|> symbol ">=")

bottomTok :: Parser ()
bottomTok = void (symbol "⊥" <|> symbol "_|_" <|> symbol "bottom")

pType :: Parser SrcType
pType = try pForallType <|> pArrowType

pForallType :: Parser SrcType
pForallType = try $ do
    forallTok
    binders <- some pForallBinder
    void (optional (symbol "."))
    body <- pType
    pure (foldr (\(v, mb) acc -> STForall v mb acc) body binders)

pForallBinder :: Parser (String, Maybe SrcType)
pForallBinder =
    try
        (parens $ do
            v <- lowerIdent
            geTok
            bound <- pType
            pure (v, Just bound))
        <|> do
            v <- lowerIdent
            pure (v, Nothing)

pArrowType :: Parser SrcType
pArrowType = do
    lhs <- pTypeApp
    rhs <- optional (symbol "->" *> pType)
    pure $ case rhs of
        Nothing -> lhs
        Just r -> STArrow lhs r

pTypeApp :: Parser SrcType
pTypeApp = do
    headTy <- pTypeAtom
    case headTy of
        Left conName -> do
            args <- many pTypeArg
            pure $ case args of
                [] -> STBase conName
                (arg:rest) -> STCon conName (arg :| rest)
        Right ty -> pure ty

pTypeArg :: Parser SrcType
pTypeArg = pTypeAtom >>= \case
    Left conName -> pure (STBase conName)
    Right ty -> pure ty

pTypeAtom :: Parser (Either String SrcType)
pTypeAtom =
    choice
        [ Left <$> upperIdent
        , Right . STVar <$> lowerIdent
        , Right STBottom <$ bottomTok
        , Right <$> parens pType
        ]

pExpr :: Parser SurfaceExpr
pExpr = choice [try pLet, try pLambda, pAnnOrApp]

pLet :: Parser SurfaceExpr
pLet = do
    void (symbol "let")
    v <- lowerIdent
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
        name <- lowerIdent
        mAnn <- optional (symbol ":" *> pType)
        pure (name, mAnn)
    body <- pExpr
    pure $ case mTy of
        Nothing -> ELam v body
        Just ty -> ELamAnn v ty body

pLambdaLegacy :: Parser SurfaceExpr
pLambdaLegacy = do
    lambdaTok
    v <- lowerIdent
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
        , EVar <$> lowerIdent
        , EVar <$> upperIdent
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

pLit :: Parser Lit
pLit =
    choice
        [ LBool True <$ symbol "true"
        , LBool False <$ symbol "false"
        , LString <$> pString
        , LInt <$> lexeme (L.signed sc L.decimal)
        ]

pString :: Parser String
pString = lexeme (C.char '"' *> manyTillChar <* C.char '"')
  where
    manyTillChar = many L.charLiteral
