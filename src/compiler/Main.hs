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

import Arguments (parseArgs)
import Compilation (compileFiles)

import Lexer (lexer)
import Parser (parser)

-- parse all args as files, parse all files to [Token] and compiles all
main :: IO ()
main = do
    progName <- getProgName
    args <- parseArgs <$> getArgs
    case args of
        Left e -> if "USAGE" `isPrefixOf` e
            then putStrLn $ "USAGE: " ++ progName ++ " [rizz files (.rz)]"
            else hPutStrLn stderr (progName ++ e) >> exitFailure
        Right files -> compileFiles lexer parser files
