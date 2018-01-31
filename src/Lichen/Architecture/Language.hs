{-# LANGUAGE GADTs, DeriveGeneric, StandaloneDeriving #-}

module Lichen.Architecture.Language where

import Data.Hashable
import Data.Aeson
import qualified Data.Text as T

import Control.Monad.Except

import Text.Read (readMaybe)

import Lichen.Architecture.Error
import Lichen.Architecture.Lexer
import Lichen.Architecture.Parser
import qualified Lichen.Implementation.Lexer.C as C
import qualified Lichen.Implementation.Lexer.Python as Python
import qualified Lichen.Implementation.Lexer.Java as Java
import qualified Lichen.Implementation.Lexer.Text as Text
import qualified Lichen.Implementation.Parser.Python as Python

-- Configuration for a given language. Should typically not need to be
-- modified, but can be overwritten in the case of unexpected instructor
-- use cases (non-typical file extensions, etc.).
-- Note: as of this writing (6/2017), the following lines break HLint. This
-- is an HLint bug, and can safely be ignored.
data Language where
    Language :: (Show a, Hashable a) => { extensions :: [FilePath]
                                        , readToken :: String -> Erring a
                                        , lexer :: Lexer a
                                        , parser :: Parser Node
                                        } -> Language
instance FromJSON Language where
    parseJSON (String s) = pure $ chooseLanguage languageDummy (Just $ T.unpack s)
    parseJSON _ = pure languageDummy

dummy :: T.Text -> a -> b -> Erring c
dummy t _ _ = throwError $ InvocationError t

smartRead :: Read a => String -> Erring a
smartRead s = case readMaybe s of Just t -> pure t
                                  Nothing -> throwError . InvalidTokenError $ T.pack s

languageDummy :: Language
languageDummy = Language []
                         (const $ pure ())
                         (dummy "No valid language specified")
                         (dummy "No valid language specified")

languageC :: Language
languageC = Language [".c", ".h", ".cpp", ".cc", ".cxx", ".hpp", ".C", ".H", ".CPP", ".CC", ".CXX", ".CPP"]
                     (smartRead :: String -> Erring C.Tok)
                     C.lex
                     (dummy "The C tooling does not currently support the requested feature")

languagePython :: Language
languagePython = Language [".py"]
                          (smartRead :: String -> Erring Python.Tok)
                          Python.lex Python.parse

languageJava :: Language
languageJava = Language [".java"]
                        (smartRead :: String -> Erring Java.Tok)
                        Java.lex
                        (dummy "The Java tooling does not currently support the requested feature")

languageText :: Language
languageText = Language [".txt"]
                        (pure . T.pack)
                        Text.lex
                        (dummy "Plain text cannot be parsed, so this feature is unavailable")

chooseLanguage :: Language -> Maybe String -> Language
chooseLanguage d Nothing = d
chooseLanguage _ (Just "C") = languageC
chooseLanguage _ (Just "c") = languageC
chooseLanguage _ (Just "Python") = languagePython
chooseLanguage _ (Just "python") = languagePython
chooseLanguage _ (Just "py") = languagePython
chooseLanguage _ (Just "java") = languageJava
chooseLanguage _ (Just "Java") = languageJava
chooseLanguage _ (Just "text") = languageText
chooseLanguage _ (Just "Text") = languageText
chooseLanguage _ (Just "txt") = languageText
chooseLanguage _ (Just "plaintext") = languageText
chooseLanguage _ (Just "plain") = languageText
chooseLanguage _ _ = languageDummy
