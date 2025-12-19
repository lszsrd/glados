{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/Lexer.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Lexer
-- Description : Performs lexical analysis of a stream of bytes and extracts a list of rizz tokens.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- Takes a stream of bytes and performs some checkups, based on a BNF grammar, to extract every __@'Token'@__ from it.
--
-- If an unexpected character is found or a multi line comment block is not closed, the @'lexer'@ function returns an error message using @'fError'@ function.
-------------------------------------------------------------------------------
module Lexer (
    -- * BNF definition
    -- $bnf

    -- * Lexical analysis
    lexer
    , lexerWrapper

    -- * Tokens parsing
    , parseBooleanConstant
    , parseKeyword
    , parseLiteral
    , parsePunctuator

    -- * Lexemes parsing
    , parseMultiLineComment
    , parseStringLiteral
    , parseSCharSequence
    , parseSChar
    , parseDecimalConstant
    , parseFloatingConstant
    , parseDigitSequence
    , parseCharacterConstant
    , parseChar
    , parseEscapeSequence
    , parseIdentifier
    , parseNonDigit
    , parseDigit
) where

import Control.Applicative ((<|>))
import Data.Char (isAscii, isLetter, isDigit)
import Data.List (isPrefixOf)

import Tokens
import Format (fError)

-- $bnf
-- For the full rizz lexical syntax definition, see the [BNF definition](https://github.com/lszsrd/glados/blob/main/docs/BNF/rizz.md) here.

-- | Takes a @'Stream'@ as parameter and returns a __Either__ @'String'@ [(@'Token'@, (@'Data.Int'@, @'Data.Int'@))].
--
-- On success, this function returns a list of (@'Token'@, @'Data.Int'@) in which each token has a tuple of integers representing the token's line and column position.
--
-- On failure, this function returns a pretty formatted error message.
lexer :: Stream -> Either String [(Token, (Int, Int))]
lexer stream = lexerWrapper stream stream (1, 1)

-- coding style helper function, do not export it nor document it
unexpectedChar :: String -> String
unexpectedChar lexeme = "unexpected character '" ++ lexeme ++ "'"

-- | Takes a @'Stream'@ and a (@'Data.Int'@, @'Data.Int'@) as parameters and returns a __Either__ @'String'@ [(@'Token'@, (@'Data.Int'@, @'Data.Int'@))].
--
-- Note that the input @'Data.Int'@ tuple represents the stream's location starting index for both line and column.
--
-- On success, this function returns a list of (@'Token'@, @'Data.Int'@) in which each token has a tuple of integers representing the token's line and column position.
--
-- On failure, this function returns a pretty formatted error message.
lexerWrapper :: Stream -> Stream -> (Int, Int)
    -> Either String [(Token, (Int, Int))]
lexerWrapper _ [] _ = Right []
lexerWrapper begin ('/': x: xs) (l, c)
    | x == '/' = lexerWrapper begin (dropWhile (/= '\n') xs) (l, c)
    | x == '*' = case parseMultiLineComment begin xs (l, c + 2) (l, c + 2) of
        Left error -> Left error
        Right (stream, pos) -> lexerWrapper begin stream pos
lexerWrapper begin ('\n': xs) (l, _) = lexerWrapper begin xs (l + 1, 1)
lexerWrapper begin stream@(x:xs) (l, c) = case parseKeyword stream
      <|> fmap (\(x,y,z) -> (Identifier x, y, z)) (parseIdentifier stream)
      <|> parseLiteral stream
      <|> parsePunctuator stream of
        Just (tok, len, rest) -> case lexerWrapper begin rest (l, c + len) of
            Left error -> Left error
            Right tokens -> Right $ (tok, (l, c)): tokens
        Nothing -> case parseEscapeSequence stream of
            Just _  -> lexerWrapper begin xs (l, c + 1)
            Nothing -> Left $ fError begin (l, c) 1 (unexpectedChar [x])

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Token'@, @'Data.Int'@, @'Stream'@) if the stream starts with a @'BoolLiteral'@.
--
-- On success, this function returns a tuple made of the parsed @'BoolLiteral'@, the @'Lexeme'@ length and the input stream stripped of the parsed @'Token'@.
parseBooleanConstant :: Stream -> Maybe (Token, Int, Stream)
parseBooleanConstant x
    | "True" `isPrefixOf` x = Just (Literal (BoolLiteral True), 4, drop 4 x)
    | "False" `isPrefixOf` x = Just (Literal (BoolLiteral False), 5, drop 5 x)
    | otherwise = Nothing

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Token'@, @'Data.Int'@, @'Stream'@) if the stream starts with a @'Keyword'@.
--
-- On success, this function returns a tuple made of the parsed @'Keyword'@, the @'Lexeme'@ length and the input stream stripped of the parsed @'Token'@.
parseKeyword :: Stream -> Maybe (Token, Int, Stream)
parseKeyword ('B': 'o': 'o': 'l' : x) = Just (Keyword Bool, 4, x)
parseKeyword ('C': 'h': 'a': 'r' : x) = Just (Keyword Char, 4, x)
parseKeyword ('I': 'n': 't': x) = Just (Keyword Int, 3, x)
parseKeyword ('F': 'l': 'o': 'a': 't': x) = Just (Keyword Float, 5, x)
parseKeyword ('D': 'o': 'u': 'b': 'l': 'e': x) = Just (Keyword Double, 6, x)
parseKeyword ('f': 'n': x) = Just (Keyword Fn, 2, x)
parseKeyword ('i': 'f': x) = Just (Keyword If, 2, x)
parseKeyword ('e': 'l': 's': 'e': x) = Just (Keyword Else, 4, x)
parseKeyword ('w': 'h': 'i': 'l': 'e': x) = Just (Keyword While, 5, x)
parseKeyword ('f': 'o': 'r': 'e': 'a': 'c': 'h': x) = 
    Just (Keyword Foreach, 7, x)
parseKeyword ('f': 'o': 'r': x) = Just (Keyword For, 3, x)
parseKeyword ('r': 'e': 't': x) = Just (Keyword Ret, 3, x)
parseKeyword _ = Nothing

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Token'@, @'Data.Int'@, @'Stream'@) if the stream starts with a @'Literal'@.
--
-- On success, this function returns a tuple made of the parsed @'Literal'@, the @'Lexeme'@ length and the input stream stripped of the parsed @'Token'@.
parseLiteral :: Stream -> Maybe (Token, Int, Stream)
parseLiteral stream =
      parseBooleanConstant stream
  <|> fmap (\(x,y,z) -> (Literal (CharLiteral x), y, z))
        (parseCharacterConstant stream)
  <|> fmap (\(x,y,z) -> (Literal (FloatLiteral (read x)), y, z))
        (parseFloatingConstant stream)
  <|> fmap (\(x,y,z) -> (Literal (IntLiteral (read x)), y, z))
        (parseDecimalConstant stream)

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Token'@, @'Data.Int'@, @'Stream'@) if the stream starts with a @'Punctuator'@.
--
-- On success, this function returns a tuple made of the parsed @'Punctuator'@, the @'Lexeme'@ length and the input stream stripped of the parsed @'Token'@.
parsePunctuator :: Stream -> Maybe (Token, Int, Stream)
parsePunctuator ('[': x) = Just (Punctuator (SBracket OpenSBracket), 1, x)
parsePunctuator (']': x) = Just (Punctuator (SBracket CloseSBracket), 1, x)
parsePunctuator ('(': x) = Just (Punctuator (RBracket OpenRBracket), 1, x)
parsePunctuator (')': x) = Just (Punctuator (RBracket CloseRBracket), 1, x)
parsePunctuator ('{': x) = Just (Punctuator (CBracket OpenCBracket), 1, x)
parsePunctuator ('}': x) = Just (Punctuator (CBracket CloseCBracket), 1, x)
parsePunctuator ('.': x) = Just (Punctuator Dot, 1, x)
parsePunctuator ('-': '>': x) = Just (Punctuator Arrow, 2, x)
parsePunctuator ('+': '+': x)
    = Just (Punctuator (UnaryOp IdentIncrement), 2, x)
parsePunctuator ('-': '-': x)
    = Just (Punctuator (UnaryOp IdentDecrement), 2, x)
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
parsePunctuator ('=': x) = Just (Punctuator (AssignOp Equal), 1, x)
parsePunctuator _ = Nothing

-- | Takes a 2 @'Stream'@ (stream's start and stream's current position) and 2 (@'Data.Int'@, @'Data.Int'@) (starting position and current position) as parameters and returns a __Either__ @'String'@ (@'Stream'@, (@'Data.Int'@, @'Data.Int'@)).
--
-- On success, this function returns a tuple made of the parsed comment block, enclosed in @\`\\*\`@ and @\`*/\`@ and the new @'Stream'@'s current position. On failure, this function returns a pretty formatted error message.
parseMultiLineComment :: Stream -> Stream -> (Int, Int) -> (Int, Int)
    -> Either String (Stream, (Int, Int))
parseMultiLineComment begin [] _ y
    = Left $ fError begin y 2 "unterminated comment block, missing '\\*'"
parseMultiLineComment begin ('/': '*': x) y _ = Left $ fError begin y 2
    "unterminated comment block upon creation of a new one"
parseMultiLineComment begin ('\n': x) (l, c) y
    = parseMultiLineComment begin x (l + 1, 1) y
parseMultiLineComment begin ('*': '/': xs) (l, c) _ = Right (xs, (l, c + 2))
parseMultiLineComment begin (_: x) (l, c) y
    = parseMultiLineComment begin x (l, c + 1) y

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Lexeme'@, @'Prelude.Int'@, @'Stream'@) if the stream starts with a \<string-literal\>.
--
-- This function is a wrapper around @'parseSCharSequence'@ that checks whether the string literal starts and ends with the @\`"\`@ character.
-- As such, it returns the same components and increases the total length by 2, taking into account both @\`"\`@ characters.
parseStringLiteral :: Stream -> Maybe (Lexeme, Int, Stream)
parseStringLiteral ('"': x) = case parseSCharSequence x of
    Just (x, y, '"': z) -> Just (x, 2 + y, z)
    _ -> Nothing
parseStringLiteral _ = Nothing

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Lexeme'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<s-char-sequence\>.
--
-- On success, this function returns a tuple made of the parsed string literal, the string length and the input stream stripped of the parsed string literal.
--
-- This function differs from @'parseCharacterConstant'@ in that it accepts one or many characters and is surrounded by @\`"\`@ characters.
parseSCharSequence :: Stream -> Maybe (Lexeme, Int, Stream)
parseSCharSequence stream = case parseSChar stream of
    Nothing -> Nothing
    Just (x, y, z) -> case parseSCharSequence z of
        Nothing -> Just ([x], y, z)
        Just (x', y', z') -> Just (x: x', y + y', z')

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Data.Char'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<s-char\>.
--
-- On success, this function returns a tuple made of the parsed character, the character length (which is always 1) and the input stream stripped of the parsed character.
--
-- This function differs from @'parseChar'@ as it fails if the character @\`"\`@ is not escaped.
parseSChar :: Stream -> Maybe (Char, Int, Stream)
parseSChar ('"': _) = Nothing
parseSChar ('\\': _) = Nothing
parseSChar ('\n': _) = Nothing
parseSChar (x: xs)
    | isAscii x = Just (x, 1, xs)
parseSChar _ = Nothing

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Lexeme'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<decimal-constant\>.
--
-- On success, this function returns a tuple made of the decimal string representation, the parsed integer length and the input stream stripped of the parsed integer.
parseDecimalConstant :: Stream -> Maybe (Lexeme, Int, Stream)
parseDecimalConstant stream = case parseDigit stream of
    Nothing -> Nothing
    Just (x, y, z) -> case parseDigit z of
        Nothing -> case parseDecimalConstant z of
            Nothing -> Just ([x], y, z)
            Just (xs, ys, zs) -> Just (x: xs, y + ys, zs)
        Just (xs, ys, zs) -> case parseDecimalConstant zs of
            Nothing -> Just (x: [xs], y + ys, zs)
            Just (xs', ys', zs') -> Just (x: xs: xs', y + ys + ys', zs')

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Lexeme'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<floating-constant\>.
--
-- On success, this function returns a tuple made of the float string representation, the parsed float length (including the dot separator) and the input stream stripped of the parsed float.
parseFloatingConstant :: Stream -> Maybe (Lexeme, Int, Stream)
parseFloatingConstant stream = do
    (x, y, '.': z) <- parseDigitSequence stream
    (x', y', z') <- parseDigitSequence z
    Just (x ++ "." ++ x', y + 1 + y', z')

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Lexeme'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<digit-sequence\>.
--
-- On success, this function returns a tuple made of the digit string representation, the parsed digit length and the input stream stripped of the parsed digit.
parseDigitSequence :: Stream -> Maybe (Lexeme, Int, Stream)
parseDigitSequence stream = case parseDigit stream of
    Nothing -> Nothing
    Just (x, y, z) -> case parseDigitSequence z of
        Nothing -> Just ([x], y, z)
        Just (x', y', z') -> Just (x: x', y + y', z')

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Data.Char'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<character-constant\>.
--
-- On success, this function returns a tuple made of the parsed string literal, the string length and the input stream stripped of the parsed string literal.
--
-- This function differs from @'parseSCharSequence'@ in that it accepts only one character and is surrounded by @\`\'\`@ characters.
parseCharacterConstant :: Stream -> Maybe (Char, Int, Stream)
parseCharacterConstant ('\'': x: '\'': xs) = case parseChar [x] of
    Nothing -> Nothing
    Just (x, _, _) -> Just (x, 3, xs)
parseCharacterConstant _ = Nothing

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Data.Char'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<s-char\>.
--
-- On success, this function returns a tuple made of the parsed character, the character length (which is always 1) and the input stream stripped of the parsed character.
--
-- This function differs from @'parseSChar'@ as it fails if the character @\`\'\`@ is not escaped.
parseChar :: Stream -> Maybe (Char, Int, Stream)
parseChar ('\'': _) = Nothing
parseChar ('\\': _) = Nothing
parseChar ('\n': _) = Nothing
parseChar (x: xs)
    | isAscii x = Just (x, 1, xs)
parseChar _ = Nothing

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Data.Char'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<escape-sequence\>.
--
-- On success, this function returns a tuple made of the parsed character, the character length (which is always 1) and the input stream stripped of the parsed character.
parseEscapeSequence :: Stream -> Maybe (Char, Int, Stream)
parseEscapeSequence (' ': x) = Just (' ', 1, x)
parseEscapeSequence ('\t': x) = Just ('\t', 1, x)
parseEscapeSequence ('\a': x) = Just ('\a', 1, x)
parseEscapeSequence ('\b': x) = Just ('\b', 1, x)
parseEscapeSequence ('\f': x) = Just ('\f', 1, x)
parseEscapeSequence ('\n': x) = Just ('\n', 1, x)
parseEscapeSequence ('\r': x) = Just ('\r', 1, x)
parseEscapeSequence ('\v': x) = Just ('\v', 1, x)
parseEscapeSequence ('\'': x) = Just ('\'', 1, x)
parseEscapeSequence ('\"': x) = Just ('\"', 1, x)
parseEscapeSequence ('\\': x) = Just ('\\', 1, x)
parseEscapeSequence _ = Nothing

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Data.Char'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<identifier\>.
--
-- On success, this function returns a tuple made of the parsed identifier, the identifier length and the input stream stripped of the identifier.
parseIdentifier :: Stream -> Maybe (Identifier, Int, Stream)
parseIdentifier stream = case parseNonDigit stream of
    Nothing -> Nothing
    Just (x, y, z) -> case parseDigit z of
        Nothing -> case parseIdentifier z of
            Nothing -> Just ([x], y, z)
            Just (xs, ys, zs) -> Just (x: xs, y + ys, zs)
        Just (xs, ys, zs) -> case parseIdentifier zs of
            Nothing -> Just (x: [xs], y + ys, zs)
            Just (xs', ys', zs') -> Just (x: xs: xs', y + ys + ys', zs')

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Data.Char'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<nondigit\>.
--
-- On success, this function returns a tuple made of the parsed character, the character length (which is always 1) and the input stream stripped of the parsed character.
parseNonDigit :: Stream -> Maybe (Char, Int, Stream)
parseNonDigit ('_': xs) = Just ('_', 1, xs)
parseNonDigit stream@(x: xs)
    | isLetter x = Just (x, 1, xs)
parseNonDigit _ = Nothing

-- | Takes a @'Stream'@ as parameter and returns a __Maybe__ (@'Data.Char'@, @'Data.Int'@, @'Stream'@) if the stream starts with a \<digit\>.
--
-- On success, this function returns a tuple made of the parsed digit (as a @'Data.Char'@), the digit length (which is always 1) and the input stream stripped of the parsed digit.
parseDigit :: Stream -> Maybe (Char, Int, Stream)
parseDigit (x: xs)
    | isDigit x = Just (x, 1, xs)
parseDigit _ = Nothing
