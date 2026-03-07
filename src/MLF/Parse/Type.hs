{-# LANGUAGE LambdaCase #-}
module MLF.Parse.Type (
    TypeParserConfig(..),
    parseTypeWith,
    parseArrowTypeWith,
) where

import Data.List.NonEmpty (NonEmpty(..))
import MLF.Parse.Common (Parser)
import Text.Megaparsec (choice, many, optional, try, (<|>))

data TypeParserConfig ty bound = TypeParserConfig
    { tpcForallTok :: Parser ()
    , tpcGeTok :: Parser ()
    , tpcSymbol :: String -> Parser String
    , tpcParens :: Parser ty -> Parser ty
    , tpcLowerIdent :: Parser String
    , tpcUpperIdent :: Parser String
    , tpcBottomTok :: Parser ()
    , tpcMkVar :: String -> ty
    , tpcMkArrow :: ty -> ty -> ty
    , tpcMkBase :: String -> ty
    , tpcMkCon :: String -> NonEmpty ty -> ty
    , tpcMkForall :: String -> bound -> ty -> ty
    , tpcMkBottom :: ty
    , tpcBoundedBinder :: Parser ty -> Parser (String, bound)
    , tpcUnboundedBinder :: Parser (String, bound)
    , tpcForallBinders :: Parser ty -> Parser [(String, bound)]
    }

parseTypeWith :: TypeParserConfig ty bound -> Parser ty
parseTypeWith cfg = pType
  where
    pType = try pForallType <|> parseArrowTypeWith cfg pType

    pForallType = try $ do
        tpcForallTok cfg
        binders <- tpcForallBinders cfg pType
        body <- pType
        pure (foldr (\(v, bnd) acc -> tpcMkForall cfg v bnd acc) body binders)

parseArrowTypeWith :: TypeParserConfig ty bound -> Parser ty -> Parser ty
parseArrowTypeWith cfg pType = do
    lhs <- pTypeApp
    rhs <- optional (tpcSymbol cfg "->" *> pType)
    pure $ case rhs of
        Nothing -> lhs
        Just r -> tpcMkArrow cfg lhs r
  where
    pTypeApp = do
        headTy <- pTypeAtom
        case headTy of
            Left conName -> do
                args <- many pTypeArg
                pure $ case args of
                    [] -> tpcMkBase cfg conName
                    (arg:rest) -> tpcMkCon cfg conName (arg :| rest)
            Right ty -> pure ty

    pTypeArg = pTypeAtom >>= \case
        Left conName -> pure (tpcMkBase cfg conName)
        Right ty -> pure ty

    pTypeAtom =
        choice
            [ Left <$> tpcUpperIdent cfg
            , Right . tpcMkVar cfg <$> tpcLowerIdent cfg
            , Right (tpcMkBottom cfg) <$ tpcBottomTok cfg
            , Right <$> tpcParens cfg pType
            ]
