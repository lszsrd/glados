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

import Format (error)

import ParseArgs (parseArgs, printUsage)
import Compiler (compile)

main :: IO ()
main = do
    x <- getProgName
    y <- parseArgs <$> getArgs
    case y of
        Left e -> if "USAGE" `isPrefixOf` e
            then printUsage x
            else hPutStrLn stderr (x ++ e) >> exitFailure
        Right (_, []) -> hPutStrLn stderr
            (x ++ ": " ++ Format.error ++ ": no input files") >> exitFailure
        Right (z, zs) -> compile z (map head . group . sort $ zs)
