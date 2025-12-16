{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/rizz/Lexer.hs
-}

-- Builds tokens list from input
module Lexer (
    -- * Basic type
    Stream

    -- * Parsing helper functions
    , parseStringLiteral
    , parseSCharSequence
    , parseSChar
    , parseDecimalConstant
    , parseFloatingConstant
    , parseDigitSequence
    , parseCharacterConstant
    , parseChar
    , parseEscapeSequence -- unused
    , parseIdentifier
    , parseNonDigit
    , parseDigit

    -- * Token parsing helper functions
    , parseBooleanConstant
    , parseKeyword
    , parseLiteral
    , parseConstant
    , parsePunctuator

    -- * Tokens generating functions
    , lexerWrapper
    , lexer
) where

import Data.Char (isDigit, isLetter, isAscii)
import Data.List (isPrefixOf)
import Control.Applicative ((<|>))

import Token

type Stream = String

parseStringLiteral :: String -> Maybe (String, Int, Stream)
parseStringLiteral ('"': x) = case parseSCharSequence x of
    Just (x, y, ('"': z)) -> Just (x, 2 + y, z)
    _ -> Nothing
parseStringLiteral _ = Nothing

parseSCharSequence :: String -> Maybe (String, Int, Stream)
parseSCharSequence stream = case parseSChar stream of
    Nothing -> Nothing
    Just (x, y, z) -> case parseSCharSequence z of
        Nothing -> Just ([x], y, z)
        Just (x', y', z') -> Just ([x] ++ x', y + y', z')

parseSChar :: String -> Maybe (Char, Int, Stream)
parseSChar ('"': _) = Nothing
parseSChar ('\n': _) = Nothing
parseSChar (x: xs)
    | isAscii x = Just (x, 1, xs)
parseSChar _ = Nothing

parseDecimalConstant :: String -> Maybe (String, Int, Stream)
parseDecimalConstant stream = case parseDigit stream of
    Nothing -> Nothing
    Just (x, y, z) -> case parseDigit z of
        Nothing -> case parseDecimalConstant z of
            Nothing -> Just ([x], y, z)
            Just (xs, ys, zs) -> Just (x: xs, y + ys, zs)
        Just (xs, ys, zs) -> case parseDecimalConstant zs of
            Nothing -> Just (x: [xs], y + ys, zs)
            Just (xs', ys', zs') -> Just (x: xs: xs', y + ys + ys', zs')

parseFloatingConstant :: String -> Maybe (String, Int, Stream)
parseFloatingConstant stream = do
    (x, y, '.': z) <- parseDigitSequence stream
    (x', y', z') <- parseDigitSequence z
    Just (x ++ "." ++ x', y + 1 + y', z')

parseDigitSequence :: String -> Maybe (String, Int, Stream)
parseDigitSequence stream = case parseDigit stream of
    Nothing -> Nothing
    Just (x, y, z) -> case parseDigitSequence z of
        Nothing -> Just ([x], y, z)
        Just (x', y', z') -> Just ([x] ++ x', y + y', z')

parseCharacterConstant :: String -> Maybe (Char, Int, Stream)
parseCharacterConstant ('\'': x: '\'': xs) = case parseChar [x] of
    Nothing -> Nothing
    Just (x, _, _) -> Just (x, 3, xs)
parseCharacterConstant _ = Nothing

parseChar :: String -> Maybe (Char, Int, Stream)
parseChar ('\'': _) = Nothing
parseChar ('\n': _) = Nothing
parseChar (x: xs)
    | isAscii x = Just (x, 1, xs)
parseChar _ = Nothing

parseEscapeSequence :: String -> Maybe (Char, Int, Stream)
parseEscapeSequence (' ': x) = Just (' ', 1, x)
parseEscapeSequence ('\a': x) = Just ('\a', 1, x)
parseEscapeSequence ('\b': x) = Just ('\b', 1, x)
parseEscapeSequence ('\f': x) = Just ('\f', 1, x)
parseEscapeSequence ('\n': x) = Just ('\n', 1, x)
parseEscapeSequence ('\r': x) = Just ('\r', 1, x)
parseEscapeSequence ('\t': x) = Just ('\t', 1, x)
parseEscapeSequence ('\v': x) = Just ('\v', 1, x)
parseEscapeSequence ('\'': x) = Just ('\'', 1, x)
parseEscapeSequence ('\"': x) = Just ('\"', 1, x)
parseEscapeSequence ('\\': x) = Just ('\\', 1, x)
parseEscapeSequence _ = Nothing

parseIdentifier :: String -> Maybe (Identifier, Int, Stream)
parseIdentifier stream = case parseNonDigit stream of
    Nothing -> Nothing
    Just (x, y, z) -> case parseDigit z of
        Nothing -> case parseIdentifier z of
            Nothing -> Just ([x], y, z)
            Just (xs, ys, zs) -> Just (x: xs, y + ys, zs)
        Just (xs, ys, zs) -> case parseIdentifier zs of
            Nothing -> Just (x: [xs], y + ys, zs)
            Just (xs', ys', zs') -> Just (x: xs: xs', y + ys + ys', zs')

parseNonDigit :: String -> Maybe (Char, Int, Stream)
parseNonDigit ('_': xs) = Just ('_', 1, xs)
parseNonDigit stream@(x: xs)
    | isLetter x = Just (x, 1, xs)
parseNonDigit _ = Nothing

parseDigit :: String -> Maybe (Char, Int, Stream)
parseDigit (x: xs)
    | isDigit x = Just (x, 1, xs)
parseDigit _ = Nothing

-- handmade, not in BNF
parseBooleanConstant :: String -> Maybe (Token, Int, Stream)
parseBooleanConstant x
    | "True" `isPrefixOf` x = Just (Literal (BoolLiteral True), 4, drop 4 x)
    | "False" `isPrefixOf` x = Just (Literal (BoolLiteral False), 5, drop 5 x)
    | otherwise = Nothing

parseKeyword :: String -> Maybe (Token, Int, Stream)
parseKeyword ('B': 'o': 'o': 'l' : x) = Just (Keyword Bool, 4, x)
parseKeyword ('C': 'h': 'a': 'r' : x) = Just (Keyword Char, 4, x)
parseKeyword ('I': 'n': 't': x) = Just (Keyword Int, 3, x)
parseKeyword ('F': 'l': 'o': 'a': 't': x) = Just (Keyword Float, 5, x)
parseKeyword ('D': 'o': 'u': 'b': 'l': 'e': x) = Just (Keyword Double, 6, x)
parseKeyword ('f': 'n': x) = Just (Keyword Fn, 2, x)
parseKeyword ('i': 'f': x) = Just (Keyword If, 2, x)
parseKeyword ('e': 'l': 's': 'e': x) = Just (Keyword Else, 4, x)
parseKeyword ('w': 'h': 'i': 'l': 'e': x) = Just (Keyword While, 5, x)
parseKeyword ('f': 'o': 'r': 'e': 'a': 'c': 'h': x) = Just (Keyword Foreach, 7, x)
parseKeyword ('f': 'o': 'r': x) = Just (Keyword For, 3, x)
parseKeyword ('r': 'e': 't': x) = Just (Keyword Ret, 3, x)
parseKeyword _ = Nothing

parseLiteral :: String -> Maybe (Token, Int, Stream)
parseLiteral stream = case parseBooleanConstant stream of
    Nothing -> case parseCharacterConstant stream of
        Nothing -> case parseFloatingConstant stream of
            Nothing -> case parseDecimalConstant stream of
                Nothing -> Nothing
                Just (x, y, z) -> Just (Literal (IntLiteral (read x :: Integer)), y, z)
            Just (x, y, z) -> Just (Literal (FloatLiteral (read x :: Float)), y, z)
        Just (x, y, z) -> Just (Literal (CharLiteral x), y, z)
    Just (x, y, z) -> Just (x, y, z)

parseConstant :: String -> Maybe (Token, Int, Stream)
parseConstant stream = case parseBooleanConstant stream of
    Nothing -> case parseCharacterConstant stream of
        Nothing -> case parseFloatingConstant stream of
            Nothing -> case parseDecimalConstant stream of
                Nothing -> Nothing
                Just (x, y, z) -> Just (Literal (IntLiteral (read x :: Integer)), y, z)
            Just (x, y, z) -> Just (Literal (FloatLiteral (read x :: Float)), y, z)
        Just (x, y, z) -> Just (Literal (CharLiteral x), y, z)
    Just (x, y, z) -> Just (x, y, z)

parsePunctuator :: String -> Maybe (Token, Int, Stream)
parsePunctuator ('[': x) = Just (Punctuator (SBracket OpenSBracket), 1, x)
parsePunctuator (']': x) = Just (Punctuator (SBracket CloseSBracket), 1, x)
parsePunctuator ('(': x) = Just (Punctuator (RBracket OpenRBracket), 1, x)
parsePunctuator (')': x) = Just (Punctuator (RBracket CloseRBracket), 1, x)
parsePunctuator ('{': x) = Just (Punctuator (CBracket OpenCBracket), 1, x)
parsePunctuator ('}': x) = Just (Punctuator (CBracket CloseCBracket), 1, x)
parsePunctuator ('.': x) = Just (Punctuator Dot, 1, x)
parsePunctuator ('-': '>': x) = Just (Punctuator Arrow, 2, x)
parsePunctuator ('+': '+': x) = Just (Punctuator (UnaryOp IdentIncrement), 2, x)
parsePunctuator ('-': '-': x) = Just (Punctuator (UnaryOp IdentDecrement), 2, x)
parsePunctuator ('*': '=': x) = Just (Punctuator (AssignOp MulEqual), 2, x)
parsePunctuator ('/': '=': x) = Just (Punctuator (AssignOp DivEqual), 2, x)
parsePunctuator ('%': '=': x) = Just (Punctuator (AssignOp ModEqual), 2, x)
parsePunctuator ('+': '=': x) = Just (Punctuator (AssignOp AddEqual), 2, x)
parsePunctuator ('-': '=': x) = Just (Punctuator (AssignOp SubEqual), 2, x)
parsePunctuator ('<': '=': x) = Just (Punctuator (BinaryOp LEq), 2, x)
parsePunctuator ('>': '=': x) = Just (Punctuator (BinaryOp GEq), 2, x)
parsePunctuator ('=': '=': x) = Just (Punctuator (BinaryOp Eq), 2, x)
parsePunctuator ('!': '=': x) = Just (Punctuator (BinaryOp NEq), 2, x)
parsePunctuator ('&': '&': x) = Just (Punctuator (BinaryOp And), 2, x)
parsePunctuator ('|': '|': x) = Just (Punctuator (BinaryOp Or), 2, x)
parsePunctuator ('*': x) = Just (Punctuator (BinaryOp Mul), 1, x)
parsePunctuator ('+': x) = Just (Punctuator (BinaryOp Add), 1, x)
parsePunctuator ('-': x) = Just (Punctuator (BinaryOp Sub), 1, x)
parsePunctuator ('/': x) = Just (Punctuator (BinaryOp Div), 1, x)
parsePunctuator ('%': x) = Just (Punctuator (BinaryOp Mod), 1, x)
parsePunctuator ('<': x) = Just (Punctuator (BinaryOp Lt), 1, x)
parsePunctuator ('>': x) = Just (Punctuator (BinaryOp Gt), 1, x)
parsePunctuator (':': x) = Just (Punctuator Colon, 1, x)
parsePunctuator (';': x) = Just (Punctuator Semicolon, 1, x)
parsePunctuator (',': x) = Just (Punctuator Comma, 1, x)
parsePunctuator ('=': x) = Just (Punctuator Equal, 1, x)
parsePunctuator _ = Nothing

lexerWrapper :: String -> (Int, Int) -> [(Token, (Int, Int))]
lexerWrapper [] _ = []
lexerWrapper ('#': x) (l, _) = lexerWrapper (dropWhile (\x -> x /= '\n') x) (l + 1, 1)
lexerWrapper ('\n': x) (l, _) = lexerWrapper x (l + 1, 1)
lexerWrapper stream@(_:xs) (l, c) =
    case  parseKeyword stream
      <|> fmap (\(x,y,z) -> (Identifier x, y, z)) (parseIdentifier stream)
      <|> parseLiteral stream
      <|> parsePunctuator stream of
        Just (tok, len, rest) ->
            (tok, (l, c)) : lexerWrapper rest (l, c + len)
        Nothing -> case parseEscapeSequence stream of
            Just _  -> lexerWrapper xs (l, c + 1)
            Nothing -> error stream

lexer :: String -> [(Token, (Int, Int))]
lexer stream = lexerWrapper stream (1, 1)
