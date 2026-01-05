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
import qualified Tokens as T
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
    testParser4, testParser5, testParser6,
    testParser7
                                        ]

testParser1 = TestCase (assertEqual "parser []" (Right []) (parser []))
testParser2 = TestCase (assertEqual "parser \"Int x\"" (Left "0:0 Expected AssignOp") (runParse "Int x"))
testParser3 = TestCase (assertEqual "parser \"fn foo(Int: x, Int: y) -> Int {}\""
    (Right [A.FunctionDecl "foo" [A.ParmVarDeclExpr A.Integer "x", A.ParmVarDeclExpr A.Integer "y"] (A.CompoundStmt []) (Just A.Integer)])
    (runParse "fn foo(Int: x, Int: y) -> Int {}"))
testParser4 = TestCase (assertEqual "parser \"(in func) if (y == 0) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.IfStmt (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclIdent "y")) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 0)))) (A.CompoundStmt []) Nothing]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { if (y == 0) {} }"))
testParser5 = TestCase (assertEqual "parser \"(in func) ret 0;\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.RetStmt (A.BinaryOpConst (A.ParmCallDeclLiteral (T.IntLiteral 0)))]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { ret 0; }"))
testParser6 = TestCase (assertEqual "parser \"(in func) while (0) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.WhileStmt (A.BinaryOpConst (A.ParmCallDeclLiteral (T.IntLiteral 0))) (A.CompoundStmt [])]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { while (0) {} }"))
testParser7 = TestCase (assertEqual "parser \"(in func) for (;;) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.ForStmt Nothing Nothing Nothing (A.CompoundStmt [])]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { for (;;) {} }"))

testParser9 = TestCase (assertEqual "parser \"if /*\"" (Left "1:6: \ESC[1;31merror\ESC[0m: unterminated comment block, missing '\\*'\n    1 | if /*\n      |      \ESC[1;32m^~ here\ESC[0m") (runParse "if /*"))
testParser10 = TestCase (assertEqual "parser \"x @\"" (Left "1:3: \ESC[1;31merror\ESC[0m: unexpected character '@'\n    1 | x @\n      |   \ESC[1;32m^ here\ESC[0m") (runParse "x @"))
testParser11 = TestCase (assertEqual "parser \"/**/&\"" (Left "1:5: \ESC[1;31merror\ESC[0m: unexpected character '&'\n    1 | /**/&\n      |     \ESC[1;32m^ here\ESC[0m") (runParse "/**/&"))
testParser12 = TestCase (assertEqual "parser \"//\n&\"" (Left "2:1: \ESC[1;31merror\ESC[0m: unexpected character '&'\n    2 | &\n      | \ESC[1;32m^ here\ESC[0m") (runParse "//\n&"))
