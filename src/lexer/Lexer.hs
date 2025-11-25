{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Lexer.hs
-}

{-|
Module      : Lexer
Description : Transforms a String to a list of Tokens
License     : MIT
Maintainer  : laszlo.serdet@epitech.eu

Takes a String (also known as a `stream` of bytes) and performs some checkups
to pop off Tokens from it.
Tokens are a way to break down a full series of bytes into individual small
pieces that each represent something.
This lexer is designed to "lexe" LISP language and thus, it decomposes into
groups:
    - Keyword       => a reserved word of the language
    - Identifier    => a variable or function name
    - Operator      => a default bounded function, mostly arithmetic ones
    - Delimiter     => since LISP heavely uses parenthesis, it only represents
                       opening and closing ones
    - Constant      => a value bounded to an Identifier
-}
module Lexer (Token (..), lexer) where

import Data.Char (isSpace, isNumber, isDigit)
import Data.List (isPrefixOf)

type Stream                 = String
type Lexeme                 = String
type Lexemes                = [Lexeme]

data Token
    = Keyword               Lexeme  -- define, lambda, if
    | Identifier            Lexeme  -- variable and/or function names
    | Operator              Lexeme  -- +, -, *, /, eq? => auto bound functions
    | Delimiter             Lexeme  -- (, )
    | Constant              Integer -- Signed Integer but as String
    deriving (Show)

keywords :: Lexemes
keywords                    = ["define", "lambda", "if"]
operators :: Lexemes
operators                   = ["+", "-", "*", "/", ">", "<", "eq?"]
delimiters :: Lexemes
delimiters                  = ["(", ")"]


-- Checks if current stream starts with **ANY** token
hasAnyToken :: Stream -> Maybe (Token, Stream)
hasAnyToken []              = Nothing
hasAnyToken stream          = case stream `parseToken` delimiters of
    Nothing                     -> case stream `parseToken` operators of
        Nothing                     -> case stream `parseToken` keywords of
            Nothing                     -> Nothing
            Just (lexeme, strip)        -> Just (Keyword lexeme, strip)
        Just (lexeme, strip)        -> Just (Operator lexeme, strip)
    Just (lexeme, strip)        -> Just (Delimiter lexeme, strip)


-- Checks if a given String starts with on of the given Lexemes
parseToken :: Stream -> Lexemes -> Maybe (Lexeme, Stream)
parseToken _ []             = Nothing
parseToken string (x: xs)
    | x `isPrefixOf` string = Just (x, drop (length x) string)
    | otherwise             = parseToken string xs


-- Try to parse an Token::Identifier
parseIdentifier :: Stream -> Maybe (Lexeme, Stream)
parseIdentifier []          = Nothing
parseIdentifier stream@(x: xs)
    | isSpace x             = Nothing
    | otherwise             = case hasAnyToken stream of
        Nothing                 -> case parseIdentifier xs of
            Nothing                 -> Just ([x], xs)
            Just (lexeme, strip)    -> Just (x: lexeme, strip)
        _                       -> Nothing


-- Try to parse a Token::Constant
parseConstant :: Stream -> Maybe (Token, Stream)
parseConstant []            = Nothing
parseConstant ('#': x: xs)
    | x == 't'              = Just (Constant 1, xs)
    | x == 'f'              = Just (Constant 0, xs)
parseConstant ('-': x)      = case parseUInteger x of
    Nothing                     -> Nothing
    Just (integer, stream)      -> Just (Constant (-read integer :: Integer), stream)
parseConstant stream@(x: _)
    | isNumber x            = case parseUInteger stream of
        Nothing                 -> Nothing
        Just (integer, string)  -> Just (Constant (read integer :: Integer), string)
    | otherwise             = Nothing


-- Try to parse an UNSIGNED Integer
parseUInteger :: Stream -> Maybe (String, Stream)
parseUInteger []            = Nothing
parseUInteger (x: xs)
    | isDigit x             = case parseUInteger xs of
        Nothing                     -> Just ([x], xs)
        Just (y, strip)             -> Just (x: y, strip)
    | otherwise             = Nothing


-- Translates a String to a list of Token
lexer :: Stream -> [Token]
lexer []                    = []
lexer (';': stream)         = lexer $ dropWhile (/= '\n') stream
lexer stream@(_: xs)        = case hasAnyToken stream of
    Nothing                     -> case parseIdentifier stream of
        Nothing                     -> lexer xs
        Just (lexeme, strip)        -> case parseConstant stream of
            Nothing                     -> Identifier lexeme: lexer strip
            Just (token, string)        -> token: lexer string
    Just (token, strip)         -> token: lexer strip

{- getTokenList :: String -> IO [Token]
getTokenList buffer = do
    evalLexer <- try (evaluate (lexer buffer))
        :: IO (Either SomeException [Token])
    case evalLexer of
        Left err -> printError (show err)
        Right tokenList -> return tokenList -}
