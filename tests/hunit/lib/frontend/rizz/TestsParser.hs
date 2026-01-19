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

import Rizz.Lexer
import qualified Rizz.Tokens as T
import Rizz.Parser (parser)
import qualified Rizz.Ast as A

runParse :: String -> Either String [A.Decl]
runParse s = do
    a <- lexer s
    parser a

tests = TestList                        [
    testsParser,
    testPreciselyIf, testPreciselyFor,
    testPreciselyHelper
                                        ]

testsParser = TestList                  [
    testParser1, testParser2, testParser3,
    testParser4, testParser5, testParser6,
    testParser7, testParser8, testParser9
                                        ]

testParser1 = TestCase (assertEqual "parser []" (Right []) (parser []))
testParser2 = TestCase (assertEqual "parser \"Int x\"" (Left "1:1 Expected AssignOp, got ") (runParse "Int x"))
testParser3 = TestCase (assertEqual "parser \"fn foo(Int: x, Int: y) {}\""
    (Right [A.FunctionDecl "foo" [A.ParmVarDeclExpr A.Integer "x", A.ParmVarDeclExpr A.Integer "y"] (A.CompoundStmt []) Nothing])
    (runParse "fn foo(Int: x, Int: y) {}"))
testParser4 = TestCase (assertEqual "parser \"(in func) if (y == 0) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt A.Integer "y" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 0))), A.IfStmt (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclIdent "y")) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 0)))) (A.CompoundStmt []) Nothing]) Nothing])
    (runParse "fn f(Int: a) { Int y = 0; if (y == 0) {} }"))
testParser5 = TestCase (assertEqual "parser \"(in func) ret 0;\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.RetStmt (Just (A.BinaryOpConst (A.ParmCallDeclLiteral (T.IntLiteral 0))))]) (Just A.Integer)])
    (runParse "fn f(Int: a) -> Int { ret 0; }"))
testParser6 = TestCase (assertEqual "parser \"(in func) while (0) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.WhileStmt (A.BinaryOpConst (A.ParmCallDeclLiteral (T.IntLiteral 0))) (A.CompoundStmt [])]) Nothing])
    (runParse "fn f(Int: a) { while (0) {} }"))
testParser7 = TestCase (assertEqual "parser \"(in func) for (;;) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.ForStmt Nothing Nothing Nothing (A.CompoundStmt [])]) Nothing])
    (runParse "fn f(Int: a)  { for (;;) {} }"))
testParser8 = TestCase (assertEqual "parser \"(in func) foreach (foo : b) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt (A.ListType A.Character) "h" T.Equal (A.ParmCallDeclLiteral (T.ListLiteral [T.CharLiteral 'h',T.CharLiteral 'e',T.CharLiteral 'l',T.CharLiteral 'l',T.CharLiteral 'o']))),A.ForeachStmt "le" "h" (A.CompoundStmt [])]) Nothing])
    (runParse "fn f(Int: a) { [Char] h = \"hello\"; foreach (le : h ) {} }"))
testParser9 = TestCase (assertEqual "parser \"(in func) Int a = (True == False) ? {0} : {1} )\""
    (Right [A.FunctionDecl "f" [] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt A.Integer "a" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 0))),A.IfStmt (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclLiteral (T.BoolLiteral True))) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.BoolLiteral False)))) (A.CompoundStmt [A.DeclStmt (A.DeclAssignStmtLiteral "a" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 1)))]) (Just (A.CompoundStmt [A.DeclStmt (A.DeclAssignStmtLiteral "a" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 5)))]))]) Nothing])
    (runParse "fn f() { Int a = 0; (True == False) ? {a = 1;} : {a = 5;} }"))


testPreciselyIf = TestList              [
    testIf1, testIf2, testIf3,
    testIf4, testIf5, testIf6,
    testIf7, testIf8 
                                        ]

testIf1 = TestCase (assertEqual "parser \"(in func) if (foo(y)) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt A.Integer "y" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 0))),A.IfStmt (A.BinaryOpConst (A.ParmCallDeclExpr (A.CallExprDecl "foo" [A.ParmCallDeclIdent "y"]))) (A.CompoundStmt []) Nothing]) Nothing])
    (runParse "fn f(Int: a) { Int y = 0; if (foo(y)) {} }"))
testIf2 = TestCase (assertEqual "parser \"(in func) if (0) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.IfStmt (A.BinaryOpConst (A.ParmCallDeclLiteral (T.IntLiteral 0))) (A.CompoundStmt []) Nothing]) Nothing])
    (runParse "fn f(Int: a) { if (0) {} }"))
testIf3 = TestCase (assertEqual "parser \"(in func) if ((4 > 2) == 1) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.SinglePrecision "a"] (A.CompoundStmt [A.IfStmt (A.BinaryOpExpr (A.BinaryOpParmBOp (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 4))) T.Gt (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 2))))) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 1)))) (A.CompoundStmt []) Nothing]) Nothing])
    (runParse "fn f(Float: a) { if ((4 > 2) == 1) {} }"))
testIf4 = TestCase (assertEqual "parser \"(in func) if (foo(y) == 0) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt A.Integer "y" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 0))),A.IfStmt (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclExpr (A.CallExprDecl "foo" [A.ParmCallDeclIdent "y"]))) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 0)))) (A.CompoundStmt []) Nothing]) Nothing])
    (runParse "fn f(Int: a) { Int y = 0; if (foo(y) == 0) {} }"))
testIf5 = TestCase (assertEqual "parser \"(in func) if (False) { foo(10); } else { foo(20); }\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Boolean "a"] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt A.Integer "y" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 0))),A.IfStmt (A.BinaryOpConst (A.ParmCallDeclLiteral (T.BoolLiteral False))) (A.CompoundStmt [A.CallExpr (A.CallExprDecl "foo" [A.ParmCallDeclLiteral (T.IntLiteral 10)])]) (Just (A.CompoundStmt [A.CallExpr (A.CallExprDecl "foo" [A.ParmCallDeclLiteral (T.IntLiteral 20)])]))]) Nothing])
    (runParse "fn f(Bool: a) { Int y = 0; if (False) { foo(10); } else { foo(20); }}"))
testIf6 = TestCase (assertEqual "parser \"(in func) if 4\""
    (Left "1:30 Expected '(', got 4")
    (runParse "fn f(Int: a) { Int y = 0; if 4 }"))
testIf7 = TestCase (assertEqual "parser \"(in func) if(4\""
    (Left "1:35 Expected ')', got '}'")
    (runParse "fn f(Int: a) { Float y = 0; if (4 }"))
testIf8 = TestCase (assertEqual "parser \"(in func) if (True) { continue; }\""
    (Left "1:40 break or continue outside loop scope.")
    (runParse "fn f(Int: a) { Bool y = False; if (y) {continue;}}"))

testPreciselyFor = TestList             [
    testFor1, testFor2, testFor3,
    testFor4, testFor5, testFor6
                                        ]

testFor1 = TestCase (assertEqual "parser \"(in func) for (Int a = 10;;) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "b"] (A.CompoundStmt [A.ForStmt (Just (A.VarDeclStmt A.Integer "a" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 10)))) Nothing Nothing (A.CompoundStmt [])]) Nothing])
    (runParse "fn f(Int: b) { for (Int a = 10;;) {} }"))
testFor2 = TestCase (assertEqual "parser \"(in func) for (Int a = 10; a < 12; a++) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "b"] (A.CompoundStmt [A.ForStmt (Just (A.VarDeclStmt A.Integer "a" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 10)))) (Just (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclIdent "a")) T.Lt (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 12))))) (Just (A.DeclAssignStmtUnary (A.UnaryOperatorExpr "a" T.IdentIncrement))) (A.CompoundStmt [])]) Nothing])
    (runParse "fn f(Int: b) { for (Int a = 10; a < 12; a++) {} }"))
testFor3 = TestCase (assertEqual "parser \"(in func) for (; a == 0; a--) {}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.ForStmt Nothing (Just (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclIdent "a")) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 0))))) (Just (A.DeclAssignStmtUnary (A.UnaryOperatorExpr "a" T.IdentDecrement))) (A.CompoundStmt [])]) Nothing])
    (runParse "fn f(Int: a) { for (; a == 0; a--) {} }"))
testFor4 = TestCase (assertEqual "parser \"(in func) for (; feur == 0; a-- {}\""
    (Left "1:35 Expected ')', got '{'")
    (runParse "fn f(Int: a) { for (; a == 0; a-- {} }"))
testFor5 = TestCase (assertEqual "parser \"(in func) for (; a == 0; a--) {continue; break;}\""
    (Right [A.FunctionDecl "f" [A.ParmVarDeclExpr A.Integer "a"] (A.CompoundStmt [A.ForStmt Nothing (Just (A.BinaryOpExpr (A.BinaryOpParm (A.ParmCallDeclIdent "a")) T.Eq (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 0))))) (Just (A.DeclAssignStmtUnary (A.UnaryOperatorExpr "a" T.IdentDecrement))) (A.CompoundStmt [A.LoopControlStmt T.Continue,A.LoopControlStmt T.Break])]) Nothing])
    (runParse "fn f(Int: a) { for (; a == 0; a--) {continue; break;} }"))
testFor6 = TestCase (assertEqual "parser \"(in func) for (Int a = 1; a == 0; a--) {}\""
    (Left "1:24 variable already exists \"a\"")
    (runParse "fn f(Int: a) { for (Int a = 0; a == 0; a--) {} }"))


testPreciselyHelper = TestList          [
    testH, testH2, testH3,
    testH4, testH5, testH6,
    testH7
                                        ]

testH = TestCase (assertEqual "parser \"(in func) [[Int]] a = [[1]]; a[0][0] = 1;\""
    (Right [A.FunctionDecl "f" [] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt (A.ListType (A.ListType A.Integer)) "a" T.Equal (A.ParmCallDeclLiteral (T.ListLiteral [T.ListLiteral [T.IntLiteral 1]]))),A.DeclStmt (A.DeclAssignStmtLiteral "ParmCallDeclIdx (ParmCallDeclIdx (ParmCallDeclIdent \"a\") (ParmCallDeclLiteral (IntLiteral 0))) (ParmCallDeclLiteral (IntLiteral 0))" T.Equal (A.ParmCallDeclLiteral (T.IntLiteral 1)))]) Nothing])
    (runParse "fn f() {[[Int]] a = [[1]]; a[0][0] = 1;}"))

testH2 = TestCase (assertEqual "parser \"(in func) Int a = 1 + 4 - 5 * foo(4);\""
    (Right [A.FunctionDecl "f" [] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt A.Integer "a" T.Equal (A.ParmCallBExpr (A.BinaryOpParm (A.ParmCallBExpr (A.BinaryOpParm (A.ParmCallBExpr (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 1))) T.Add (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 4))))) T.Sub (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 5))))) T.Mul (A.BinaryOpParm (A.ParmCallDeclExpr (A.CallExprDecl "foo" [A.ParmCallDeclLiteral (T.IntLiteral 4)])))))]) Nothing])
    (runParse "fn f() {Int a = ((1 + 4) - 5) * foo(4);}"))
testH3 = TestCase (assertEqual "parser \"(in func) Int a = 1 / 4 + 5 * foo(4 - 1 / 2);\""
    (Right [A.FunctionDecl "f" [] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt A.Integer "a" T.Equal (A.ParmCallBExpr (A.BinaryOpParm (A.ParmCallBExpr (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 1))) T.Div (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 4))))) T.Add (A.BinaryOpParm (A.ParmCallBExpr (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 5))) T.Mul (A.BinaryOpParm (A.ParmCallDeclExpr (A.CallExprDecl "foo" [A.ParmCallBExpr (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 4))) T.Sub (A.BinaryOpParm (A.ParmCallBExpr (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 1))) T.Div (A.BinaryOpParm (A.ParmCallDeclLiteral (T.IntLiteral 2)))))])))))))]) Nothing])
    (runParse "fn f() {Int a = 1 / 4 + 5 * foo(4 - 1 / 2);}"))
testH4 = TestCase (assertEqual "parser \"(in func) \""
    (Right [A.RecordDecl (A.RecordDeclExpr "Bar" [A.ParmVarDeclExpr A.Integer "j"]),A.RecordDecl (A.RecordDeclExpr "Foo" [A.ParmVarDeclExpr (A.Struct "Bar") "a",A.ParmVarDeclExpr A.Integer "b"]),A.FunctionDecl "f" [] (A.CompoundStmt [A.DeclVarExpr (A.VarDeclStmt (A.Struct "Foo") "a" T.Equal (A.ParmCallDeclList [A.ParmCallDeclList [A.ParmCallDeclLiteral (T.IntLiteral 1)],A.ParmCallDeclLiteral (T.IntLiteral 0)]))]) Nothing])
    (runParse "struct Bar {Int: j}  struct Foo {Bar: a, Int: b}  fn f() {Foo a = {{1}, 0};}"))
testH5 = TestCase (assertEqual "parser \"(in func) double def of A\""
    (Left "1:23 variable already exists \"a\"")
    (runParse "fn f(Int: a) {for (Int a = 0;;) {} }"))
testH6 = TestCase (assertEqual "parser \"(in func) assignation to function def;\""
    (Left "1:21 Cannot assign value to function definition")
    (runParse "fn foo () {} fn f() {foo = 0;}"))
testH7 = TestCase (assertEqual "parser \"(in func) Int a = foo(b);\""
    (Left "1:28 Cannot assign value to struct definition")
    (runParse "struct Bar {Int: i} fn f() {Bar = 0;}"))
