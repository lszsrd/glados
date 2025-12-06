{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Main.hs
-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))

import Lexer (lexer)
import Parser (getAST)
import Interpretor (interpret)
import AbstractTree
import Control.Exception (try, SomeException, evaluate)

-- get the buffer, if args, then try file, else read stdin
getBuffer :: [String] -> IO String
getBuffer [] = getContents
getBuffer ["-h"] = putStrLn
    ("GLaDOS: Generic Language and Data Operand Syntax.\n"
    ++ "\nusage:\n\t./glados <File> or ./glados < <File>\n"
    ++ "\nProgram takes as input a File or read content from stdin.\n"
    ++ "It then tries to Interpret it as Lisp-like language.\n\nExample:\n  "
    ++ "echo -e \"((lambda (foo bar) (+ foo bar) 2 4)\" | ./glados\n  >> 6")
    >> exitSuccess
getBuffer [x] = do
    res <- try (readFile x)
        :: IO (Either SomeException String)
    case res of
        Left e -> print e >> exitWith (ExitFailure 84)
        Right content -> return content

printResult :: Maybe Ast -> IO ()
printResult (Just (Expression a)) =
    case a of
        (Boolean True) -> putStrLn "#t"
        (Boolean False) -> putStrLn "#f"
        (Int a) -> print a
        _ -> putStrLn "#\\<procedure\\>"
printResult _ = putStrLn "Error happened: sorry not sorry"

-- Entrypoint of the program, Coordinate every parts
main :: IO ()
main = do
    args <- getArgs
    buffer <- getBuffer args
    let tokenList = lexer buffer
    ast <- getAST tokenList
    res <- try (evaluate (interpret ast []))
        :: IO (Either SomeException (Maybe Ast))
    case res of
        Left e -> print e >> exitWith (ExitFailure 84)
        Right content -> printResult content >> exitSuccess
