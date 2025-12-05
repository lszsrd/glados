{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- tests/Main.hs
-}

module Main where

import Test.HUnit
import System.Exit (exitSuccess, exitFailure)
import LexerTests (lexerTestsLexemes, lexerTestsLexer, lexerTestsAnyToken, lexerTestsToken, lexerTestsIdentifier, lexerTestsUInteger, lexerTestsConstant)
import InterpretorTests (interpretorTestsSimpleSubjet, interpretorTestsBuiltins, interpretorTestsEnv)

main :: IO ()
main = do
    result <- runTestTT $ TestList [lexerTestsLexemes, lexerTestsLexer, lexerTestsAnyToken, lexerTestsToken, lexerTestsIdentifier, lexerTestsConstant, lexerTestsUInteger]
    result2 <- runTestTT $ TestList [interpretorTestsSimpleSubjet, interpretorTestsBuiltins, interpretorTestsEnv]
    if failures result > 0 || failures result2 > 0 then exitFailure else exitSuccess
