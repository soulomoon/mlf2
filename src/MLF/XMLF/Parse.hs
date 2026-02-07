{-# LANGUAGE LambdaCase #-}
module MLF.XMLF.Parse (
    XmlfParseError,
    parseXmlfType,
    parseXmlfComp,
    parseXmlfTerm,
    renderXmlfParseError
) where

import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import MLF.Frontend.Syntax (Lit (..))
import MLF.XMLF.Syntax
    ( XmlfComp (..)
    , XmlfTerm (..)
    , XmlfType (..)
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
    , try
    , (<|>)
    )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

newtype XmlfParseError = XmlfParseError (ParseErrorBundle String Void)
    deriving (Eq, Show)

parseXmlfType :: String -> Either XmlfParseError XmlfType
parseXmlfType input =
    case parse (sc *> pType <* eof) "<xmlf-type>" input of
        Left err -> Left (XmlfParseError err)
        Right ty -> Right ty

parseXmlfComp :: String -> Either XmlfParseError XmlfComp
parseXmlfComp input =
    case parse (sc *> pComp <* eof) "<xmlf-comp>" input of
        Left err -> Left (XmlfParseError err)
        Right comp -> Right comp

parseXmlfTerm :: String -> Either XmlfParseError XmlfTerm
parseXmlfTerm input =
    case parse (sc *> pTerm <* eof) "<xmlf-term>" input of
        Left err -> Left (XmlfParseError err)
        Right tm -> Right tm

renderXmlfParseError :: XmlfParseError -> String
renderXmlfParseError (XmlfParseError err) = errorBundlePretty err

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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

reservedWords :: Set String
reservedWords =
    Set.fromList
        [ "let"
        , "in"
        , "true"
        , "false"
        , "forall"
        , "epsilon"
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

bigLambdaTok :: Parser ()
bigLambdaTok = void (symbol "Λ" <|> symbol "Lambda")

geTok :: Parser ()
geTok = void (symbol "⩾" <|> symbol ">=")

bottomTok :: Parser ()
bottomTok = void (symbol "⊥" <|> symbol "_|_" <|> symbol "bottom")

botCompTok :: Parser ()
botCompTok = void (symbol "⊲")

hypCompTok :: Parser ()
hypCompTok = void (symbol "⊳")

pType :: Parser XmlfType
pType = try pForallType <|> pArrowType

pForallType :: Parser XmlfType
pForallType = try $ do
    forallTok
    firstBinder <- pForallBinder
    restBinders <- optional . try $ do
        more <- many pForallBinder
        void (symbol ".")
        pure more
    case restBinders of
        Nothing -> void (optional (symbol "."))
        Just _ -> pure ()
    body <- pType
    let binders = case restBinders of
            Nothing -> [firstBinder]
            Just more -> firstBinder : more
    pure (foldr (\(v, b) acc -> XTForall v b acc) body binders)

pForallBinder :: Parser (String, XmlfType)
pForallBinder =
    try
        (parens $ do
            v <- lowerIdent
            geTok
            bound <- pType
            pure (v, bound))
        <|> do
            v <- lowerIdent
            pure (v, XTBottom)

pArrowType :: Parser XmlfType
pArrowType = do
    lhs <- pTypeApp
    rhs <- optional (symbol "->" *> pType)
    pure $ case rhs of
        Nothing -> lhs
        Just r -> XTArrow lhs r

pTypeApp :: Parser XmlfType
pTypeApp = do
    headTy <- pTypeAtom
    case headTy of
        Left conName -> do
            args <- many pTypeArg
            pure $ case args of
                [] -> XTBase conName
                (arg:rest) -> XTCon conName (arg :| rest)
        Right ty -> pure ty

pTypeArg :: Parser XmlfType
pTypeArg = pTypeAtom >>= \case
    Left conName -> pure (XTBase conName)
    Right ty -> pure ty

pTypeAtom :: Parser (Either String XmlfType)
pTypeAtom =
    choice
        [ Left <$> upperIdent
        , Right . XTVar <$> lowerIdent
        , Right XTBottom <$ bottomTok
        , Right <$> parens pType
        ]

pComp :: Parser XmlfComp
pComp = do
    c0 <- pCompAtom
    rest <- many (symbol ";" *> pCompAtom)
    pure (foldl XCSeq c0 rest)

pCompAtom :: Parser XmlfComp
pCompAtom =
    choice
        [ parens pComp
        , try pCompInner
        , try pCompOuter
        , try pCompLegacyApp
        , pCompIntro
        , pCompElim
        , pCompId
        , try pCompHypLegacy
        , try pCompHyp
        , try pCompBot
        , pCompTypeSugar
        ]

pCompId :: Parser XmlfComp
pCompId = XCId <$ (symbol "ε" <|> symbol "epsilon" <|> symbol "1")

pCompIntro :: Parser XmlfComp
pCompIntro = XCIntro <$ symbol "O"

pCompElim :: Parser XmlfComp
pCompElim = XCElim <$ symbol "N"

pCompBot :: Parser XmlfComp
pCompBot = do
    botCompTok
    XCBot <$> pType

pCompHyp :: Parser XmlfComp
pCompHyp = do
    v <- lowerIdent
    hypCompTok
    pure (XCHyp v)

pCompHypLegacy :: Parser XmlfComp
pCompHypLegacy = do
    void (symbol "!")
    XCHyp <$> lowerIdent

pCompInner :: Parser XmlfComp
pCompInner = do
    forallTok
    c <- parens $ do
        geTok
        pComp
    pure (XCInner c)

pCompOuter :: Parser XmlfComp
pCompOuter = do
    forallTok
    (v, c) <- do
        v <- parens $ do
            name <- lowerIdent
            geTok
            pure name
        comp <- pCompAtom
        pure (v, comp)
    pure (XCOuter v c)

pCompLegacyApp :: Parser XmlfComp
pCompLegacyApp = do
    ty <- between (symbol "⟨") (symbol "⟩") pType
    pure (compFromType ty)

pCompTypeSugar :: Parser XmlfComp
pCompTypeSugar = compFromType <$> pArrowType

compFromType :: XmlfType -> XmlfComp
compFromType ty = XCSeq (XCInner (XCBot ty)) XCElim

pTerm :: Parser XmlfTerm
pTerm = choice [try pLet, try pLam, try pTyAbs, pApp]

pLet :: Parser XmlfTerm
pLet = do
    void (symbol "let")
    v <- lowerIdent
    void (symbol "=")
    rhs <- pTerm
    void (symbol "in")
    body <- pTerm
    pure (XLet v rhs body)

pLam :: Parser XmlfTerm
pLam = try pLamParen <|> pLamLegacy

pLamParen :: Parser XmlfTerm
pLamParen = do
    lambdaTok
    (v, ty) <- parens $ do
        name <- lowerIdent
        void (symbol ":")
        annTy <- pType
        pure (name, annTy)
    body <- pTerm
    pure (XLam v ty body)

pLamLegacy :: Parser XmlfTerm
pLamLegacy = do
    lambdaTok
    v <- lowerIdent
    void (symbol ":")
    ty <- pType
    void (symbol ".")
    body <- pTerm
    pure (XLam v ty body)

pTyAbs :: Parser XmlfTerm
pTyAbs = try pTyAbsParen <|> pTyAbsLegacy

pTyAbsParen :: Parser XmlfTerm
pTyAbsParen = do
    bigLambdaTok
    (v, bound) <- parens $ do
        name <- lowerIdent
        geTok
        b <- pType
        pure (name, b)
    body <- pTerm
    pure (XTyAbs v bound body)

pTyAbsLegacy :: Parser XmlfTerm
pTyAbsLegacy = do
    bigLambdaTok
    v <- lowerIdent
    void (symbol ".")
    body <- pTerm
    pure (XTyAbs v XTBottom body)

pApp :: Parser XmlfTerm
pApp = do
    f <- pPostfix
    args <- many pPostfix
    pure (foldl XApp f args)

pPostfix :: Parser XmlfTerm
pPostfix = do
    base <- pAtom
    insts <- many (brackets pComp)
    pure (foldl XTyInst base insts)

pAtom :: Parser XmlfTerm
pAtom =
    choice
        [ parens pTerm
        , XLit <$> pLit
        , XVar <$> lowerIdent
        , XVar <$> upperIdent
        ]

pLit :: Parser Lit
pLit =
    choice
        [ LBool True <$ symbol "true"
        , LBool False <$ symbol "false"
        , LString <$> pString
        , LInt <$> lexeme (L.signed sc L.decimal)
        ]

pString :: Parser String
pString = lexeme (C.char '"' *> many L.charLiteral <* C.char '"')
