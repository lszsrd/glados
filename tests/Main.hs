{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- tests/Main.hs
-}

module Main where

import Test.HUnit
import System.Exit (exitSuccess, exitFailure)
import LexerTests (lexerTestsIdentifier, lexerTestsUInteger, lexerTestsConstant)

main :: IO ()
main = do
    result <- runTestTT $ TestList [lexerTestsIdentifier, lexerTestsConstant, lexerTestsUInteger]
    if failures result > 0 then exitFailure else exitSuccess
