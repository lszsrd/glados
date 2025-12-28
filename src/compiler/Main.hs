{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Main.hs
-}

module Main where

import System.IO (stderr, hPutStrLn)
import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)

import Data.List (isPrefixOf)

import Lexer (lexer)
import Parser (parser)
import Bytecode (compileDecl)

import Arguments (parseArgs, printUsage)
import Compiler (compile)

main :: IO ()
main = do
    let extension = ".rz"
    progName <- getProgName
    args <- parseArgs extension <$> getArgs
    case args of
        Left e -> if "USAGE" `isPrefixOf` e then printUsage extension progName
            else hPutStrLn stderr (progName ++ e) >> exitFailure
        Right (opts, files) -> compile opts files lexer parser compileDecl
