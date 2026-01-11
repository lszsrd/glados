{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/lisp/Lexer.hs
-}

module Lexer (
    lexer
    , lexerWrapper
    , parseAtom
    , parseBool
    , parseInteger
    , parseFloat
    , parseIdentifier
    , parseOperator
) where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isLetter, isSpace)
import Data.List (isPrefixOf)

import Format (fError)

import Tokens

lexer :: Stream -> Either String Tokens
lexer stream = lexerWrapper stream stream (1, 1)

unexpectedChar :: String -> String
unexpectedChar lexeme = "unexpected token '" ++ lexeme ++ "'"

lexerWrapper :: Stream -> Stream -> (Int, Int) -> Either String Tokens
lexerWrapper _ [] _ = Right []
lexerWrapper begin ('\n': xs) (l, _) = lexerWrapper begin xs (l + 1, 1)
lexerWrapper begin (x: xs) (l, c)
    | isSpace x = lexerWrapper begin xs (l, c + 1)
lexerWrapper begin stream (l, c) = case parseRBracket stream of
    Nothing -> case parseAtom stream of
        Nothing -> Left $ fError begin (l, c) (length tok) (unexpectedChar tok)
            where tok = head $ words stream
        Just (x, y, z) -> case lexerWrapper begin z (l, c + y) of
            Left e -> Left e
            Right tokens -> Right $ (Atom x, (l, c)): tokens
    Just (x, y, z) -> case lexerWrapper begin z (l, c + y) of
        Left e -> Left e
        Right tokens -> Right $ (RBracket x, (l, c)): tokens

parseRBracket :: Stream -> Maybe (RBracket, Int, Stream)
parseRBracket ('(': x) = Just (Open, 1, x)
parseRBracket (')': x) = Just (Close, 1, x)
parseRBracket _ = Nothing

parseAtom :: Stream -> Maybe (Atom, Int, Stream)
parseAtom stream = case parseOperator stream
    <|> parseFloat stream
    <|> parseInteger stream
    <|> parseBool stream
    <|> parseIdentifier stream of
        Nothing -> Nothing
        Just (x, y, z) -> Just (x, y, z)

parseBool :: Stream -> Maybe (Atom, Int, Stream)
parseBool ('#': 'f': x) = Just (Bool False, 2, x)
parseBool ('#': 't': x) = Just (Bool True, 2, x)
parseBool _ = Nothing

parseInteger :: Stream -> Maybe (Atom, Int, Stream)
parseInteger ('-': x) = case parseInteger x of
    Just (Integer y, y', z) -> Just (Integer (- y), 1 + y', z)
    _ -> Nothing
parseInteger stream = case takeWhile isDigit stream of
    [] -> Nothing
    x -> Just (Integer (read x :: Integer), length x, drop (length x) stream)

parseFloat :: Stream -> Maybe (Atom, Int, Stream)
parseFloat ('-': x) = case parseFloat x of
    Just (Float y, y', z) -> Just (Float (- y), 1 + y', z)
    _ -> Nothing
parseFloat xs = case takeWhile isDigit xs of
    x -> case drop (length x) xs of
        ('.': y) -> case takeWhile isDigit y of
            [] -> Nothing
            z -> Just (Float (read f :: Float), length z, drop (length f) xs)
                where f = x ++ "." ++ z
        _ -> Nothing

parseIdentifier :: Stream -> Maybe (Atom, Int, Stream)
parseIdentifier stream = case takeWhile isLetter stream of
    [] -> Nothing
    x -> Just (Identifier x, length x, drop (length x) stream)

hparseOperator :: Stream -> String -> Operator -> Maybe (Atom, Int, Stream)
hparseOperator stream x operator = case drop (length x) stream of
    [] -> Just (Operator operator, length x, [])
    stream'@(z: _) -> if isSpace z || z == '(' || z == ')'
        then Just (Operator operator, length x, stream')
        else Nothing

parseOperator :: Stream -> Maybe (Atom, Int, Stream)
parseOperator ('+': x) = Just (Operator Add, 1, x)
parseOperator ('-': x) = Just (Operator Sub, 1, x)
parseOperator ('*': x) = Just (Operator Mul, 1, x)
parseOperator ('<': x) = Just (Operator Lt, 1, x)
parseOperator stream
    | "define" `isPrefixOf` stream = hparseOperator stream "define" Define
    | "lambda" `isPrefixOf` stream = hparseOperator stream "lambda" Lambda
    | "div" `isPrefixOf` stream = hparseOperator stream "div" Div
    | "mod" `isPrefixOf` stream = hparseOperator stream "mod" Mod
    | "eq?" `isPrefixOf` stream = hparseOperator stream "eq?" Eq
    | "if" `isPrefixOf` stream = hparseOperator stream "if" If
    | otherwise = Nothing
