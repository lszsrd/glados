{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- tests/Main.hs
-}

module Main where

import Test.HUnit
import System.Exit (exitSuccess, exitFailure)

test1 :: Test
test1 = TestCase (assertEqual "Default test" 42 (21 + 21))

main :: IO ()
main = do
    result <- runTestTT $ TestList [test1, test2]
    if failures result > 0 then exitFailure else exitSuccess
