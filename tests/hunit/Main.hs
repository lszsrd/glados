{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- tests/hunit/Main.hs
-}

module Main where

import System.Exit (exitSuccess, exitFailure)

import Test.HUnit

import TestsLexer

runModuleTests :: String -> String -> Test -> IO ()
runModuleTests name path tests = do
    putStrLn $ "===================================" ++ replicate (length name + length path) '='
    putStrLn $ "Running unit tests for module \"\ESC[1;35m" ++ name ++ "\ESC[0m\" (" ++ path ++")\n"
    results <- runTestTT $ TestList [tests]
    putStrLn $ "===================================" ++ replicate (length name + length path) '='
    if failures results > 0 then exitFailure else return ()

main :: IO ()
main = do
    runModuleTests "Lexer" "frontend/rizz" TestsLexer.tests
