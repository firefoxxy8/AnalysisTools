{-# LANGUAGE RankNTypes #-}

module Lichen.Architecture.Lexer where

import Data.Aeson
import Data.Foldable()
import Data.Semigroup ((<>))
import qualified Data.List.NonEmpty as NE

import Text.Megaparsec
import Text.Megaparsec.ByteString

import Lichen.Architecture.Error
import Lichen.Architecture.Source

type Lexer a = forall s. (Source s) => s -> Erring [Positional a]

data TokenPosition = TokenPosition { startLine :: !Pos
                                   , endLine :: !Pos
                                   , startCol :: !Pos
                                   , endCol :: !Pos
                                   } deriving (Show, Eq, Ord)

data Positional a = Positional TokenPosition a deriving (Show, Eq)

instance Ord a => Ord (Positional a) where
    compare (Positional _ x) (Positional _ y) = compare x y

instance Functor Positional where
    fmap f (Positional p x) = Positional p $ f x

instance Show a => ToJSON (Positional a) where
    toJSON (Positional p x) = object [ "token" .= show x
                                 , "start_line" .= unPos (startLine p)
                                 , "end_line" .= unPos (endLine p)
                                 , "start_col" .= unPos (startCol p)
                                 , "end_col" .= unPos (endCol p)
                                 ]

wrap :: Foldable t => Parser (t a) -> b -> Parser (Positional b)
wrap p x = do
        pos <- NE.head . statePos <$> getParserState
        s <- p
        return $ Positional (TokenPosition (sourceLine pos) (sourceLine pos) (sourceColumn pos) (sourceColumn pos <> unsafePos (fromIntegral $ length s))) x

wrapid :: Foldable t => Parser (t a) -> Parser (Positional (t a))
wrapid p = do
        pos <- NE.head . statePos <$> getParserState
        s <- p
        return $ Positional (TokenPosition (sourceLine pos) (sourceLine pos) (sourceColumn pos) (sourceColumn pos <> unsafePos (fromIntegral $ length s))) s

reserved :: String -> Parser String
reserved s = try (string s >> notFollowedBy (alphaNumChar <|> char '_') >> pure s)

operator :: String -> Parser String
operator = try . string

-- Parse a C-style character literal. Ex: 'a', '@'.
charLit :: Parser String
charLit = char '\'' *> manyTill (noneOf ['\'']) (char '\'' <|> (eof >> pure ' '))

-- Parse a C-style string literal. Ex: "a", "hello, world".
strLit :: Parser String
strLit = char '\"' *> manyTill (noneOf ['"']) (char '\"' <|> (eof >> pure ' '))

quote :: String -> String
quote s = ('"':s) ++ ['"']

-- Parse a C-style identifier (letter or underscore followed by any number
-- of letters, digits, and underscores).
ident :: Parser String
ident = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
