{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Main.hs
-}

module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    args <- getArgs

    if null args
        then do
            hPutStrLn stderr "USAGE: ./glados [LISP file to interpret]"
            exitFailure
        else exitSuccess
