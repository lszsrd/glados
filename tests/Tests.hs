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
test1 = TestCase (assertEqual "should return 3" 3 (1 + 2))

test2 :: Test
test2 = TestCase (assertEqual "should return 3" 3 1)

main :: IO ()
main = do
    result <- runTestTT $ TestList [test1, test2]
    if failures result > 0 then exitFailure else exitSuccess
