{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Main.hs
-}

module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    args <- getArgs

    if null args
        then hPutStrLn stderr "USAGE: ./glados [LISP file to interpret]"
        else exitSuccess
