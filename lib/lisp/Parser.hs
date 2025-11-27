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

type Identifier    = String

data Ast = ExprDefine  Identifier Ast
    |      ExprLambda  Identifier [Ast]   
    |      ExprIf      Ast Ast Ast
    |      ExprVar     Identifier
    |      ExprBool    Bool
    |      ExprInteger Integer
    |      ExprApp     Identifier [Ast]
    deriving Show

-- Parse and create the AST with the given TokenList
parsor :: [Lexer.Token] -> Ast
parsor [] = throwErr $ Error.ErrorT { location = 0, message = "No input" }
parsor _ = ExprVar "passed"

getAST :: [Lexer.Token] -> IO Ast
getAST tkList = do
    evalParser <- try (evaluate (parsor tkList))
        :: IO (Either SomeException Ast)
    case evalParser of
        Left err -> printError (show err)
        Right ast -> return ast
