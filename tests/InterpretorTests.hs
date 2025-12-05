{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- tests/LexerTests.hs
-}

module InterpretorTests (
    interpretorTestsInterpret
) where

import Test.HUnit
import AbstractTree
import Interpretor

interpretorTestsInterpret = TestList    [
    interpretEmptyList
    , interpretSimpleInt
    , interpretDefineVar 
    , interpretDefineNUse
    , interpretCallBuiltin
    , interpretLambda
                                        ]

interpretEmptyList = TestCase (assertEqual "interpret []" Nothing (interpret [] []) )
interpretSimpleInt = TestCase (assertEqual "interpret Int 2" (Just $ Expression (Int 2)) (interpret [Expression (Int 2)] []) )
interpretDefineVar = TestCase (assertEqual "interpret Define foo 2" Nothing (interpret [ Define "foo" (Int 2) ] []) )
interpretDefineNUse = TestCase (assertEqual "interpret Define foo 2, foo" (Just $ Expression (Int 2)) (interpret [Define "foo" (Int 2), Expression $ Var "foo"] []) )
interpretCallBuiltin = TestCase (assertEqual "interpret Call + 2 4" (Just $ Expression (Int 6)) (interpret [ Expression $ Call (Var "+") [Int 2, Int 4] ] []) )
interpretLambda = TestCase (assertEqual "interpret lambda a b ..." (Just $ Expression (Int 47)) (interpret [ Define "naouel" (Lambda ["a", "b"] (Call (Var "+") [Var "a", Var "b"]) ), Expression (Call (Var "naouel") [Int 1, Int 46])  ] []) )
