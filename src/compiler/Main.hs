{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Main.hs
-}

module Main where

import System.Environment (getProgName, getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

import Data.List (isPrefixOf, group, sort)

import Rizz.Lexer (lexer)
import Rizz.Parser (parser)
import Bytecode (compileDecl)

import ParseArgs (parseArgs, printUsage)
import Compiler (compile)

main :: IO ()
main = do
    progName <- getProgName
    arguments <- parseArgs <$> getArgs
    case arguments of
        Left e -> if "USAGE" `isPrefixOf` e
            then printUsage progName
            else hPutStrLn stderr (progName ++ e) >> exitFailure
        Right (x, xs) ->
            compile x (map head . group . sort $ xs) lexer parser compileDecl
