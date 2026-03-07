module MLF.Parse.Common (
    Parser,
    sc,
    lexeme,
    symbol,
    parens,
    brackets,
    identifier,
    lowerIdent,
    upperIdent,
    forallTok,
    lambdaTok,
    bigLambdaTok,
    geTok,
    bottomTok,
    pLit,
    pString,
) where

import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import MLF.Frontend.Syntax (Lit(..))
import Text.Megaparsec
    ( Parsec
    , between
    , choice
    , many
    , satisfy
    , try
    , (<|>)
    )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

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

identifier :: Set String -> Parser String
identifier reservedWords = lexeme $ try $ do
    x <- satisfy isIdentStart
    xs <- many (satisfy isIdentContinue)
    let name = x : xs
    if Set.member name reservedWords
        then fail ("reserved word " ++ show name)
        else pure name

lowerIdent :: Set String -> Parser String
lowerIdent reservedWords = try $ do
    name <- identifier reservedWords
    case name of
        (c:_) | isAsciiLower c || c == '_' -> pure name
        _ -> fail ("expected lowercase identifier, got " ++ show name)

upperIdent :: Set String -> Parser String
upperIdent reservedWords = try $ do
    name <- identifier reservedWords
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
