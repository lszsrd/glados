{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Parser.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Parser.hs
-- Description : Parse a tokens list send by the Lexer
--
-- License     : MIT
-- Maintainers : hugo.duda@epitech.eu, florian.grave@epitech.eu
--
-- Takes a List of __@'Token'@__ and tries to parse expressions and return it to a List.
--
-- * __@'Token'@__ is a way to break down a series of bytes into an individual,
-- small piece that represent something in the global context of the parsed
-- stream.
--
-- * __@'List'@__ is a list of Ast who is an abstract way to represent the whole code-base
-- effectively making it easier to interpret and compute
-- 
-------------------------------------------------------------------------------

module Parser(
    -- * Main function
    getAST,
    -- * Subfunction
    parsor,
    parseTopLevel
) where

import Lexer (Token(..))
import Control.Exception (try, SomeException, evaluate)
import Error
import AbstractTree

type Parser a   = [Token] -> Either ErrorT (a, [Token])

-- | Helper function needed by contructor who have to parse a list of Identifier
parseIdentifierList :: Parser [Identifier]
parseIdentifierList rest@(Lexer.Delimiter ")" : _) = Right ([], rest)
parseIdentifierList (Lexer.Identifier name : xs) = do
    (names, rest) <- parseIdentifierList xs
    Right (name : names, rest)
parseIdentifierList [] =
    Left $ ErrorT 0 "Unexpected end of input in lambda parameter list"
parseIdentifierList (tok : _) =
    Left $ ErrorT 0 ("Invalid token in lambda parameter list: " ++ show tok)

-- | Helper function needed by constructor who have to parse a list of Expr
parseArgs :: Parser [Expr]
parseArgs rest@(Lexer.Delimiter ")" : _) = Right ([], rest)
parseArgs tokens = do
    (arg, xs1) <- parseExpression tokens
    (args, xs2) <- parseArgs xs1
    Right (arg : args, xs2)

-- | Parse the Define keyword into a Identifier and a Expression
parseDefine :: Parser Ast
parseDefine (Lexer.Identifier name : xs) = do
    (expr, rest) <- parseExpression xs
    case rest of
        Lexer.Delimiter ")" : rest -> Right (Define name expr, rest)
        _ -> Left $ ErrorT 0 "Missing ')' after define"
parseDefine (Lexer.Delimiter "(": Lexer.Identifier id : xs) = do
    (expr, xs1) <- parseIdentifierList xs
    case xs1 of
        (Lexer.Delimiter ")" : xs2) -> do
            (body, xs3) <- parseExpression xs2
            case xs3 of
                Lexer.Delimiter ")" : rest -> Right (Define id (Lambda expr body), rest)
                _ -> Left $ ErrorT 0 "Missing ')' after lamdba body"
        _ -> Left $ ErrorT 0 "Missing ')' after lamdba param list"
parseDefine _ = Left $ ErrorT 0 "Invalid define"

-- | Parse the Lambda keyword into a list of Identifier and a Expr
parseLambda :: Parser Expr
parseLambda (Lexer.Delimiter "(" : tokens) = do
    (ids, xs1) <- parseIdentifierList tokens
    case xs1 of
        (Lexer.Delimiter ")" : xs2) -> do
            (body, xs3) <- parseExpression xs2
            case xs3 of
                Lexer.Delimiter ")" : rest -> Right (Lambda ids body, rest)
                _ -> Left $ ErrorT 0 "Missing ')' after lamdba body"
        _ -> Left $ ErrorT 0 "Missing ')' after lamdba param list"
parseLambda _ = Left $ ErrorT 0 "Missing '(' before lambda's params"

-- | Parse the If keyword into 3 Expr
parseIf :: Parser Expr
parseIf token = do
    (cond, xs1) <- parseExpression token
    (th, xs2) <- parseExpression xs1
    (el, xs3) <- parseExpression xs2
    case xs3 of
        Lexer.Delimiter ")" : rest -> Right (If cond th el, rest)
        _ -> Left $ ErrorT 0 "Missing ')' after if expression"

-- | Parse all Lisp application into a Expr and a list of Expr
parseCall :: Parser Expr
parseCall tokens = do
    (expr, xs1) <- parseExpression tokens
    (args, xs2) <- parseArgs xs1
    case xs2 of
        (Lexer.Delimiter ")" : rest) -> Right (Call expr args, rest)
        _ -> Left $ ErrorT 0 "Missing ')' after call expression"

-- | Parse all keyword except Define
parseSExpression :: Parser Expr
parseSExpression (Lexer.Keyword "lambda" : xs) = parseLambda xs
parseSExpression (Lexer.Keyword "if" : xs) = parseIf xs
parseSExpression expression = parseCall expression

-- | Parse all tokens into a Expr
parseExpression :: Parser Expr
parseExpression (Lexer.Constant c : xs) = Right (Int c, xs)
parseExpression (Lexer.Boolean b : xs) = Right (AbstractTree.Boolean b, xs)
parseExpression (Lexer.Identifier i : xs) = Right (Var i, xs)
parseExpression (Lexer.Operator o : xs) = Right (Var o, xs)
parseExpression (Lexer.Delimiter "(" : xs) = parseSExpression xs
parseExpression [] = Left $ ErrorT 0 "Unexpected end of input"
parseExpression (tok : _) = Left $ ErrorT 0 ("Unexpected token: " ++ show tok)

-- | Parse all tokens into a Ast
parseTopLevel :: Parser Ast
parseTopLevel (Delimiter "(" : Keyword "define" : ts) = parseDefine ts
parseTopLevel tokens = do
    (e, rest) <- parseExpression tokens
    pure (Expression e, rest)

-- | Parse all tokens into a List
parseList :: Parser List
parseList [] = Right ([], [])
parseList tokens = do
    (top, xs)         <- parseTopLevel tokens
    (tops, xsfinal)   <- parseList xs
    Right (top : tops, xsfinal)

-- | Parse all tokens into a Either ErrorT List
parsor :: [Lexer.Token] -> Either ErrorT List
parsor tokens =
    case parseList tokens of
        Left err         -> Left err
        Right (list, []) -> Right list
        Right (_, _)     -> Left $ ErrorT 0 "Can't parse all tokens"

-- | Get a list of Ast by a list of Token
getAST :: [Lexer.Token] -> IO List
getAST tkList =
    case parsor tkList of
        Left err -> printError err
        Right ast -> return ast
