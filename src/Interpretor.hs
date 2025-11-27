{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Parser.hs
-}

module Interpretor (interpretResult) where

import Parser (Ast(..))
import Control.Exception (try, SomeException, evaluate)
import Error

-- Ram through the AST and interpret it  
interpret :: Parser.Ast -> String
{- interpret (TODO _) = throwErr $ Error.ErrorT
    { location = 0, message = "No input" }  -}
interpret _ = "passed"

interpretResult :: Ast -> IO String
interpretResult ast = do
    evalParser <- try (evaluate (interpret ast))
        :: IO (Either SomeException String)
    case evalParser of
        Left err -> printError (show err)
        Right str -> return str
