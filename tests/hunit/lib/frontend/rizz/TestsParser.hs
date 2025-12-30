{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- tests/hunit/lib/frontend/TestsParser.hs
-}

module TestsParser (
    tests
) where

import Test.HUnit

import Lexer
import Tokens
import Parser (parser)
import qualified Ast as A

runParse :: String -> Either String [A.Decl]
runParse s = do
    a <- lexer s
    parser a

tests = TestList                        [
    testsParser
                                        ]

testsParser = TestList                  [
    testParser1, testParser2, testParser3,
    testParser4
                                        ]

testParser1 = TestCase (assertEqual "parser []" (Right []) (parser []))
testParser2 = TestCase (assertEqual "parser \"if (y == 0) {}\"" (Right []) (runParse "if (y == 0) {}"))
testParser3 = TestCase (assertEqual "parser \"fn foo(Int: x, Int: y) -> Int\"" (Right []) (runParse "fn foo(Int: x, Int: y) -> Int"))
testParser4 = TestCase (assertEqual "parser \"Int x\"" (Right []) (runParse "Int x"))

testParser9 = TestCase (assertEqual "parser \"if /*\"" (Left "1:6: \ESC[1;31merror\ESC[0m: unterminated comment block, missing '\\*'\n    1 | if /*\n      |      \ESC[1;32m^~ here\ESC[0m") (runParse "if /*"))
testParser10 = TestCase (assertEqual "parser \"x @\"" (Left "1:3: \ESC[1;31merror\ESC[0m: unexpected character '@'\n    1 | x @\n      |   \ESC[1;32m^ here\ESC[0m") (runParse "x @"))
testParser11 = TestCase (assertEqual "parser \"/**/&\"" (Left "1:5: \ESC[1;31merror\ESC[0m: unexpected character '&'\n    1 | /**/&\n      |     \ESC[1;32m^ here\ESC[0m") (runParse "/**/&"))
testParser12 = TestCase (assertEqual "parser \"//\n&\"" (Left "2:1: \ESC[1;31merror\ESC[0m: unexpected character '&'\n    2 | &\n      | \ESC[1;32m^ here\ESC[0m") (runParse "//\n&"))
