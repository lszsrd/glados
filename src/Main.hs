{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Main.hs
-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Lexer (lexer)
-- import Parser (getAST)
import Interpretor (interpret)

-- get the buffer, if args, then try file, else read stdin
getBuffer :: [String] -> IO String
getBuffer [] = getContents
getBuffer (x:_) = return ("Not yet implemented + " ++ x)

-- Entrypoint of the program, Coordinate every parts
main :: IO ()
main = do
    args <- getArgs
    buffer <- getBuffer args
    putStrLn buffer
    let tokenList = lexer buffer
    print tokenList
    {- ast <- getAST tokenList
    res <- interpretResult ast
    putStr (show res) -}
    exitSuccess
