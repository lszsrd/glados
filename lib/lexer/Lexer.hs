{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/lexer/Lexer.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Lexer
-- Description : Generates a list of tokens from a given stream of bytes.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- Takes a stream of bytes and performs some checkups to extract every
-- __@'Token'@__ from it, based on pre-defined __@'Lexemes'@__.
--
-- * __@'Token'@__ is a way to break down a series of bytes into an individual,
-- small piece that represent something in the global context of the parsed
-- stream.
--
-- * __@'Lexemes'@__, however, are a list of predetermined @'Lexeme'@ that help
-- to determine a tokens among a bytes stream.
-------------------------------------------------------------------------------
module Lexer (
    -- * Basic types
    Stream
    , Lexeme
    , Lexemes
    , Token (..) -- '(..)' exports constructors

    -- * Lexemes
    , keywords
    , operators
    , delimiters

    -- * Parsing
    , parseAnyToken
    , parseToken
    , parseIdentifier
    , parseConstant
    , parseUInteger

    -- * Lexing
    , lexer
) where

import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf)

-- | Defines @'Stream'@ type as a string which represents a finite byte stream.
type Stream = String

-- | Defines @'Lexeme'@ type as a string representing a way to identify a
-- specific @'Token'@.
type Lexeme = String

-- | Defines @'Lexemes'@ type as a list of @'Lexeme'@, covering a set of
-- patterns to match for a @'Token'@ identification.

type Lexemes = [Lexeme]

-- | Defines a @'Token'@ which is bound by its corresponding representation in
-- the bytes @'Stream'@.
--
-- It can only be of one type and serves as a way to represent a series
-- of bytes in a more abstract way.
data Token
    = Keyword Lexeme
    -- ^ @'Keyword'@ is any lexeme defined in the @'keywords'@ list. It
    -- represents a reserved word used by LISP language.
    | Identifier Lexeme
    -- ^ @'Identifier'@ is a variable, function or lambda name built when no
    -- other @'Token'@ can be built from current @'Stream'@ and that is not
    -- a @'Constant'@ nor a @'Boolean'@.
    | Operator Lexeme
    -- ^ @'Operator'@ is any lexeme defined in the @operators@ list. It
    -- represents auto-bound functions used for arithmetic and comparison
    -- operations.
    | Delimiter Lexeme
    -- ^ @'Delimiter'@ is any lexeme defined in @delimiters@ list. It represents
    -- a character used to separate some other tokens.
    | Constant Integer
    -- ^ @'Constant'@ is any signed integerer representing a constant value.
    | Boolean Bool
    -- ^ @'Boolean'@ is either @'True'@ or @'False'@ and is represented by
    -- either '#t' for @'True'@ and '#f' for @'False'@ values.
    deriving (
        Show
        -- ^ Allows tokens to be displayed on standard/error outputs.
        , Eq
        -- ^ Allows tokens to be compared, needed for unit tests.
    )


-- | Defines @'keywords'@ as @define@, @lambda@ and @if@ patterns. Note that an
-- @'Identifier'@ __can__ start with any keyword as long as their are no spaces
-- between the keyword and the identifier.
keywords :: Lexemes
keywords = ["define ", "lambda ", "if "]

-- | Defines @'operators'@ as @+@, @-@, @*@, @/@, @<@, @>@ and
-- @eq?@ patterns.
operators :: Lexemes
operators = ["+", "-", "*", "/", ">", "<", "eq?"]

-- | Defines @'delimiters'@ as @(@ and @)@ patterns.
delimiters :: Lexemes
delimiters = ["(", ")"]


-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Token'@,
-- @'Stream'@).
--
-- This function __tries__ to parse any @'Token'@ from the current @'Stream'@
-- (either from @'keywords'@, @'operators'@ or @'delimiters'@) and returns
-- Nothing if no valid @'Token'@ is found.
parseAnyToken :: Stream -> Maybe (Token, Stream)
parseAnyToken [] = Nothing
parseAnyToken stream = case stream `parseToken` delimiters of
    Nothing -> case stream `parseToken` operators of
        Nothing -> case stream `parseToken` keywords of
            Nothing -> Nothing
            Just (lexeme, strip) -> Just (Keyword lexeme, strip)
        Just (lexeme, strip) -> Just (Operator lexeme, strip)
    Just (lexeme, strip) -> Just (Delimiter lexeme, strip)
    
-- | Takes a @'Stream'@ and @'Lexemes'@ as parameters and returns a __Maybe__
-- (@'Lexeme'@, @'Stream'@).
--
-- This function takes a @'Lexemes'@ and checks if any of its @'Lexeme'@
-- match the first bytes of the given @'Stream'@. If none of the @'Lexemes'@
-- were found at the __beginning__ of the stream, returns Nothing.
parseToken :: Stream -> Lexemes -> Maybe (Lexeme, Stream)
parseToken _ [] = Nothing
parseToken string (x: xs)
    | x `isPrefixOf` string = Just (x, drop (length x) string)
    | otherwise = parseToken string xs

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Lexeme'@,
-- @'Stream'@).
--
-- This functions takes a @'Stream'@ and __tries__ to parses it as a
-- @'Token'@ @Identifier@ and returns Nothing if it fails.
parseIdentifier :: Stream -> Maybe (Lexeme, Stream)
parseIdentifier [] = Nothing
parseIdentifier stream@(x: xs)
    | isSpace x = Nothing
    | isDigit x = Nothing   -- prevents variables starting with a digit to be
                            -- parsed as an Identifier
    | otherwise = case parseAnyToken stream of
        Nothing -> case parseIdentifier xs of
            Nothing -> Just ([x], xs)
            Just (lexeme, strip) -> Just (x: lexeme, strip)
        _ -> Nothing

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Token@,
-- @'Stream'@).
--
-- This functions takes a @'Stream'@ and __tries__ to parses it either a
-- @'Token'@ @Constant@ or @Boolean@ and returns Nothing if it fails.
parseConstant :: Stream -> Maybe (Token, Stream)
parseConstant [] = Nothing
parseConstant ('#': 't': x) = Just (Boolean True, x)
parseConstant ('#': 'f': x) = Just (Boolean False, x)
parseConstant stream@(x: xs)
    | x == '-' = case parseUInteger xs of
        Nothing -> Nothing
        Just (string, strip) -> Just (Constant (-read string :: Integer), strip)
    | otherwise = case parseUInteger stream of
        Nothing -> Nothing
        Just (string, strip) -> Just (Constant (read string :: Integer), strip)

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (String,
-- @'Stream'@).
--
-- This functions takes a @'Stream'@ and __tries__ to extracts a valid signed
-- Integer and returns Nothing if it fails.
parseUInteger :: Stream -> Maybe (String, Stream)
parseUInteger [] = Nothing
parseUInteger (x: xs)
    | isDigit x = case parseUInteger xs of
        Nothing -> Just ([x], xs)
        Just (y, strip) -> Just (x: y, strip)
    | otherwise = Nothing

-- | Takes a @'Stream'@ as parameter and returns a list of @'Token'@.
--
-- This functions parses the given stream and extracts every tokens to a list
-- that is returned.
--
-- A special case, not covered by lexemes, are comments, starting with a @'\\n'@
-- that let a user define a comment in its code that should be ignored. However,
-- @'Lexemes'@ from @'keywords'@, @'operators'@ or @'delimiters'@ are used to
-- generate the list of @'Token'@.
lexer :: Stream -> [Token]
lexer [] = []
lexer (';': stream) = lexer $ dropWhile (/= '\n') stream
lexer stream@(_: xs) = case parseAnyToken stream of
    Nothing -> case parseIdentifier stream of
        Nothing -> lexer xs
        Just (lexeme, strip) -> case parseConstant stream of
            Nothing -> Identifier lexeme: lexer strip
            Just (token, string) -> token: lexer string
    Just (token, strip) -> token: lexer strip
