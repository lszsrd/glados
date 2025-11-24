{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Lexer.hs
-}

module Lexer (Token (..), getTokenList) where

import Control.Exception (try, SomeException, evaluate)
import Error

data Token = Empty String
    | TODO Int
    deriving Show

-- lexer, responsible of translating code into Token List #TODO
lexer :: String -> [Token]
lexer "" = throwErr $ Error.ErrorT { location = 0, message = "No input" } 
lexer _ = [Empty "passed"]


getTokenList :: String -> IO [Token]
getTokenList buffer = do
    evalLexer <- try (evaluate (lexer buffer))
        :: IO (Either SomeException [Token])
    case evalLexer of
        Left err -> printError (show err)
        Right tokenList -> return tokenList
