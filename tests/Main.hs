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
import InterpretorTests (interpretorTestsSimpleSubjet, interpretorExtended, interpretorTestsBuiltins, interpretorTestsEnv)
import ParserTests (parserTestsSimple)

main :: IO ()
main = do
    result <- runTestTT $ TestList [lexerTestsLexemes, lexerTestsLexer, lexerTestsAnyToken, lexerTestsToken, lexerTestsIdentifier, lexerTestsConstant, lexerTestsUInteger]
    result2 <- runTestTT $ TestList [interpretorTestsSimpleSubjet, interpretorExtended, interpretorTestsBuiltins, interpretorTestsEnv]
    result3 <- runTestTT $ TestList [parserTestsSimple]
    if failures result > 0 || failures result2 > 0 then exitFailure else exitSuccess
