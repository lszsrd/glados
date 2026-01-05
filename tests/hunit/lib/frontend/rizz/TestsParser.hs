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
    testsParser,
    testPreciselyIf, testPreciselyFor
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
-- testParser8 = TestCase (assertEqual "parser \"(in func) foreach (foo : b) {}\""
--     (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt []) (Just A.Integer)])
--     (runParse "fn f(Int: a) -> Int { foreach (:) {} }"))


testPreciselyIf = TestList              [
    testIf1, testIf2, testIf3, testIf4
                                        ]

testIf1 = TestCase (assertEqual "parser \"(in func) if (foo(y)) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.IfStmt (A.BinaryOpConst (A.ParmCallDeclExpr (A.CallExprDecl "foo" [A.ParmCallDeclIdent "y"]))) (A.CompoundStmt []) Nothing]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { if (foo(y)) {} }"))
testIf2 = TestCase (assertEqual "parser \"(in func) if (0) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.IfStmt (A.BinaryOpConst (A.ParmCallDeclLiteral (T.IntLiteral 0))) (A.CompoundStmt []) Nothing]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { if (0) {} }"))
testIf3 = TestCase (assertEqual "parser \"(in func) if ((4 > 2) == 1) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.IfStmt (A.BinaryOpExpr (A.BinaryOpParmBOp (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 4))) T.Gt (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 2))))) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 1)))) (A.CompoundStmt []) Nothing]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { if ((4 > 2) == 1) {} }"))
testIf4 = TestCase (assertEqual "parser \"(in func) if (foo(y) == 0) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.IfStmt (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclExpr (A.CallExprDecl "foo" [A.ParmCallDeclIdent "y"]))) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 0)))) (A.CompoundStmt []) Nothing]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { if (foo(y) == 0) {} }"))


testPreciselyFor = TestList             [
    testFor1, testFor2, testFor3, testFor4
                                        ]

testFor1 = TestCase (assertEqual "parser \"(in func) for (Int a = 10;;) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.ForStmt (Just (A.VarDeclStmt A.Integer "a" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 10)))) Nothing Nothing (A.CompoundStmt [])]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { for (Int a = 10;;) {} }"))
testFor2 = TestCase (assertEqual "parser \"(in func) for (Int a = 10; a < 12; a++) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.ForStmt (Just (A.VarDeclStmt A.Integer "a" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 10)))) (Just (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclIdent "a")) T.Lt (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 12))))) (Just (A.DeclAssignStmtUnary (A.UnaryOperatorExpr "a" T.IdentIncrement))) (A.CompoundStmt [])]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { for (Int a = 10; a < 12; a++) {} }"))
testFor3 = TestCase (assertEqual "parser \"(in func) for (; foo == 0; a--) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.ForStmt Nothing (Just (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclIdent "foo")) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 0))))) (Just (A.DeclAssignStmtUnary (A.UnaryOperatorExpr "a" T.IdentDecrement))) (A.CompoundStmt [])]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { for (; foo == 0; a--) {} }"))
testFor4 = TestCase (assertEqual "parser \"(in func) for (Char a = 'd'; a != 'z'; a++) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.ForStmt (Just (A.VarDeclStmt A.Character "a" T.Equal (A.ParmCallDeclLiteral (T.CharLiteral 'd')))) (Just (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclIdent "a")) T.NEq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.CharLiteral 'z'))))) (Just (A.DeclAssignStmtUnary (A.UnaryOperatorExpr "a" T.IdentIncrement))) (A.CompoundStmt [])]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { for (Char a = 'd'; a != 'z'; a++) {} }"))
