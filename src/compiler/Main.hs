{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Main.hs
-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (stderr, hPutStrLn)

import Lexer (lexer)

-- source => lexical analysis (tokens) => syntactic analysis (parse tree) => semantic analysis (type checking)

main :: IO ()
main = do
    path <- getArgs
    buffer <- readFile $ head path
    case lexer buffer of
        Left e -> hPutStrLn stderr (head path ++ ":" ++ e)
        Right tokens -> print tokens
    exitSuccess
