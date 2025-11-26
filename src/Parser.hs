{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Parser.hs
-}

module Parser (getAST, Ast(..)) where

import Lexer (Token(..))
import Control.Exception (try, SomeException, evaluate)
import Error

type Identifier             = String


data Ast = Define Identifier Ast
    | FunctionCall Identifier [Ast]
    | IF Ast Ast Ast -- if expr then expr2 else expr3
    | Lambda [Ast] Ast
    | Cond [Ast] -- cond has a list of cond and execute until one is true
    | Variable Identifier
    | Constant Integer
    | Boolean Bool
    deriving Show

-- Parse and create the AST with the given TokenList 
parsor :: [Lexer.Token] -> Ast
parsor [] = throwErr $ Error.ErrorT { location = 0, message = "No input" } 
parsor _ = MT "passed"

getAST :: [Token] -> IO Ast
getAST tkList = do
    evalParser <- try (evaluate (parsor tkList))
        :: IO (Either SomeException Ast)
    case evalParser of
        Left err -> printError (show err)
        Right ast -> return ast
