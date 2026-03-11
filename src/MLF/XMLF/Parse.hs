{-# LANGUAGE LambdaCase #-}
module MLF.XMLF.Parse (
    XmlfParseError,
    parseXmlfType,
    parseXmlfComp,
    parseXmlfTerm,
    renderXmlfParseError
) where

import Control.Monad (void)
import Data.Set (Set)
import Data.Void (Void)
import qualified Data.Set as Set
import MLF.XMLF.Syntax
    ( XmlfComp (..)
    , XmlfTerm (..)
    , XmlfType (..)
    )
import Text.Megaparsec
    ( ParseErrorBundle
    , choice
    , eof
    , errorBundlePretty
    , many
    , optional
    , parse
    , try
    , (<|>)
    )
import MLF.Parse.Common
    ( Parser
    , bigLambdaTok
    , bottomTok
    , brackets
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
import MLF.Parse.Type (TypeParserConfig (..), parseArrowTypeWith)

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

reservedWords :: Set String
reservedWords =
    Set.fromList
        [ "let"
        , "in"
        , "mu"
        , "roll"
        , "unroll"
        , "true"
        , "false"
        , "forall"
        , "epsilon"
        ]

muTok :: Parser ()
muTok = void (symbol "μ" <|> symbol "mu")

botCompTok :: Parser ()
botCompTok = void (symbol "⊲")

hypCompTok :: Parser ()
hypCompTok = void (symbol "⊳")

xmlfTypeConfig :: TypeParserConfig XmlfType XmlfType
xmlfTypeConfig =
    TypeParserConfig
        { tpcForallTok = forallTok
        , tpcGeTok = geTok
        , tpcSymbol = symbol
        , tpcParens = parens
        , tpcLowerIdent = lowerIdent reservedWords
        , tpcUpperIdent = upperIdent reservedWords
        , tpcBottomTok = bottomTok
        , tpcMkVar = XTVar
        , tpcMkArrow = XTArrow
        , tpcMkBase = XTBase
        , tpcMkCon = XTCon
        , tpcMkForall = XTForall
        , tpcMkBottom = XTBottom
        , tpcBoundedBinder = \pTy ->
            parens $ do
                v <- lowerIdent reservedWords
                geTok
                bound <- pTy
                pure (v, bound)
        , tpcUnboundedBinder = do
            v <- lowerIdent reservedWords
            pure (v, XTBottom)
        , tpcForallBinders = \pTy -> do
            firstBinder <- try ((tpcBoundedBinder xmlfTypeConfig) pTy) <|> tpcUnboundedBinder xmlfTypeConfig
            restBinders <- optional . try $ do
                more <- many (try ((tpcBoundedBinder xmlfTypeConfig) pTy) <|> tpcUnboundedBinder xmlfTypeConfig)
                void (symbol ".")
                pure more
            case restBinders of
                Nothing -> void (optional (symbol "."))
                Just _ -> pure ()
            pure $ case restBinders of
                Nothing -> [firstBinder]
                Just more -> firstBinder : more
        }

pType :: Parser XmlfType
pType = try pForallType <|> try pMuType <|> pArrowType
  where
    pForallType = try $ do
        forallTok
        binders <- tpcForallBinders xmlfTypeConfig pType
        body <- pType
        pure (foldr (\(v, bnd) acc -> XTForall v bnd acc) body binders)

    pMuType = do
        muTok
        v <- lowerIdent reservedWords
        void (symbol ".")
        XTMu v <$> pType

    pArrowType = parseArrowTypeWith xmlfTypeConfig pType

pTypeNoTopForall :: Parser XmlfType
pTypeNoTopForall = try pMuType <|> parseArrowTypeWith xmlfTypeConfig pType
  where
    pMuType = do
        muTok
        v <- lowerIdent reservedWords
        void (symbol ".")
        XTMu v <$> pType

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
    v <- lowerIdent reservedWords
    hypCompTok
    pure (XCHyp v)

pCompHypLegacy :: Parser XmlfComp
pCompHypLegacy = do
    void (symbol "!")
    XCHyp <$> lowerIdent reservedWords

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
            name <- lowerIdent reservedWords
            geTok
            pure name
        comp <- pCompAtom
        pure (v, comp)
    pure (XCOuter v c)

pCompLegacyApp :: Parser XmlfComp
pCompLegacyApp = do
    ty <- do
        void (symbol "⟨")
        ty0 <- pType
        void (symbol "⟩")
        pure ty0
    pure (compFromType ty)

pCompTypeSugar :: Parser XmlfComp
pCompTypeSugar = compFromType <$> pTypeNoTopForall

compFromType :: XmlfType -> XmlfComp
compFromType ty = XCSeq (XCInner (XCBot ty)) XCElim

pTerm :: Parser XmlfTerm
pTerm = choice [try pLet, try pLam, try pTyAbs, pApp]

pLet :: Parser XmlfTerm
pLet = do
    void (symbol "let")
    v <- lowerIdent reservedWords
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
        name <- lowerIdent reservedWords
        void (symbol ":")
        annTy <- pType
        pure (name, annTy)
    body <- pTerm
    pure (XLam v ty body)

pLamLegacy :: Parser XmlfTerm
pLamLegacy = do
    lambdaTok
    v <- lowerIdent reservedWords
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
        name <- lowerIdent reservedWords
        geTok
        b <- pType
        pure (name, b)
    body <- pTerm
    pure (XTyAbs v bound body)

pTyAbsLegacy :: Parser XmlfTerm
pTyAbsLegacy = do
    bigLambdaTok
    v <- lowerIdent reservedWords
    void (symbol ".")
    body <- pTerm
    pure (XTyAbs v XTBottom body)

pApp :: Parser XmlfTerm
pApp = do
    f <- pPrefix
    args <- many pPrefix
    pure (foldl XApp f args)

pPrefix :: Parser XmlfTerm
pPrefix = choice [try pRoll, try pUnroll, pPostfix]

pRoll :: Parser XmlfTerm
pRoll = do
    void (symbol "roll")
    ty <- brackets pType
    body <- pPrefix
    pure (XRoll ty body)

pUnroll :: Parser XmlfTerm
pUnroll = do
    void (symbol "unroll")
    body <- pPrefix
    pure (XUnroll body)

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
        , XVar <$> lowerIdent reservedWords
        , XVar <$> upperIdent reservedWords
        ]
