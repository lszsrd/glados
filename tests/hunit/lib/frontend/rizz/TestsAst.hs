{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- tests/hunit/lib/frontend/TestAst.hs
-}

module TestsAst (
    tests
) where

import Test.HUnit
import Rizz.Ast
import Rizz.Tokens

tests :: Test
tests = TestList
  [ testBuiltinTypeEq
  , testBuiltinTypeShow
  , testCompoundStmtEq
  , testStmtShow
  , testDeclEq
  , testDeclShow
  , testBig
  , testBig2
  , testStmt 
  , testTokenShowCoverage
  ]

testBuiltinTypeEq :: Test
testBuiltinTypeEq = TestCase $ do
  Boolean @=? Boolean
  False   @=? (Integer == Boolean)


testCompoundStmtEq :: Test
testCompoundStmtEq = TestCase $ do
  let stmt = RetStmt Nothing
  CompoundStmt [stmt] @=? CompoundStmt [stmt]

testDeclEq :: Test
testDeclEq = TestCase $ do
  let d1 = VarDecl (VarDeclStmt Boolean "x" Equal (ParmCallDeclLiteral (BoolLiteral True)))
      d2 = VarDecl (VarDeclStmt Boolean "x" Equal (ParmCallDeclLiteral (BoolLiteral True)))
  d1 @=? d2

testBuiltinTypeShow :: Test
testBuiltinTypeShow = TestCase $
  "Boolean" @=? show Boolean

testStmtShow :: Test
testStmtShow = TestCase $
  "RetStmt Nothing" @=? show (RetStmt Nothing)

testDeclShow :: Test
testDeclShow = TestCase $ do
  let decl =
        FunctionDecl
          "foo"
          [ParmVarDeclExpr Integer "x"]
          (CompoundStmt [])
          (Just Integer)

  "FunctionDecl \"foo\" [ParmVarDeclExpr Integer \"x\"] (CompoundStmt []) (Just Integer)"
    @=? show decl


testBig = TestCase $ do
    let a = [FunctionDecl "foo" [ParmVarDeclExpr Integer "x"] (CompoundStmt [DeclVarExpr (VarDeclStmt Integer "y" Equal (ParmCallDeclLiteral (IntLiteral 42))), IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "x")) Eq (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 0)))) (CompoundStmt [RetStmt (Just (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 0))))]) Nothing, RetStmt (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "y")) Div (BinaryOpParm (ParmCallDeclIdent "x"))))]) (Just Integer), FunctionDecl "bar" [ParmVarDeclExpr Integer "x"] (CompoundStmt [DeclVarExpr (VarDeclStmt Integer "y" Equal (ParmCallDeclLiteral (IntLiteral 1))),ForStmt Nothing (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "y")) Lt (BinaryOpParm (ParmCallDeclIdent "x")))) (Just (DeclAssignStmtUnary (UnaryOperatorExpr "y" IdentIncrement))) (CompoundStmt [IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclIdent "y"]))) Eq (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 0)))) (CompoundStmt [RetStmt (Just (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 0))))]) Nothing]),RetStmt (Just (BinaryOpConst (ParmCallDeclIdent "y")))]) (Just Integer), FunctionDecl "baz" [] (CompoundStmt [CallExpr (CallExprDecl "bar" [ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 100)])])]) Nothing]
    a @=? a
    show a @=? show a

testBig2 = TestCase $ do
    let a = [RecordDecl (RecordDeclExpr "bar2" [ParmVarDeclExpr Integer "i",ParmVarDeclExpr Integer "j"]),RecordDecl (RecordDeclExpr "bar" [ParmVarDeclExpr (Rizz.Ast.Struct "bar2") "i",ParmVarDeclExpr Character "c"]),RecordDecl (RecordDeclExpr "foo" [ParmVarDeclExpr (Rizz.Ast.Struct "bar") "b",ParmVarDeclExpr Integer "x",ParmVarDeclExpr Integer "j"]),FunctionDecl "babar" [ParmVarDeclExpr Integer "x"] (CompoundStmt [DeclVarExpr (VarDeclStmt Integer "y" Equal (ParmCallDeclLiteral (IntLiteral 5))),DeclVarExpr (VarDeclStmt (Rizz.Ast.Struct "foo") "f" Equal (ParmCallDeclList [ParmCallDeclList [ParmCallDeclList [ParmCallDeclLiteral (IntLiteral 1),ParmCallDeclLiteral (IntLiteral 2)],ParmCallDeclLiteral (CharLiteral 'c')],ParmCallDeclLiteral (IntLiteral 1),ParmCallDeclLiteral (IntLiteral 2)])),DeclStmt (DeclAssignStmtLiteral "f@b@i@i" Equal (ParmCallDeclLiteral (IntLiteral 1))),ForStmt (Just (VarDeclStmt Integer "q" Equal (ParmCallDeclExpr (CallExprDecl "feur" [ParmCallDeclIdent "y"])))) (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "feur" [ParmCallDeclIdent "y"]))) Lt (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 1))))) (Just (DeclAssignStmtUnary (UnaryOperatorExpr "y" IdentIncrement))) (CompoundStmt [IfStmt (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 0))) (CompoundStmt [RetStmt (Just (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 0))))]) Nothing,IfStmt (BinaryOpExpr (BinaryOpParmBOp (BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 4))) Gt (BinaryOpParm (ParmCallDeclLiteral(IntLiteral 2))))) Eq (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 0)))) (CompoundStmt [RetStmt (Just (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 1))))]) Nothing,IfStmt (BinaryOpConst (ParmCallDeclLiteral (BoolLiteral True))) (CompoundStmt [RetStmt (Just (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 12))))]) Nothing]),DeclStmt (DeclAssignStmtLiteral "y" Equal (ParmCallDeclExpr (CallExprDecl "feur" [ParmCallDeclIdent "y"]))),RetStmt (Just (BinaryOpConst (ParmCallDeclIdent "y")))]) (Just Integer),FunctionDecl "baz" [] (CompoundStmt [DeclVarExpr (VarDeclStmt (ListType Integer) "s" Equal (ParmCallDeclLiteral (ListLiteral [IntLiteral 1,IntLiteral 2,IntLiteral 56]))),DeclVarExpr (VarDeclStmt (Rizz.Ast.Struct "bar2") "ll" Equal (ParmCallDeclList [ParmCallDeclLiteral (IntLiteral 1),ParmCallDeclLiteral (IntLiteral 50)])),DeclVarExpr (VarDeclStmt Integer "foo2do" Equal (ParmCallDeclLiteral (IntLiteral 1))),IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "ll")) Eq (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 0)))) (CompoundStmt [RetStmt Nothing]) Nothing,DeclStmt (DeclAssignStmtLiteral "ll@i" Equal (ParmCallDeclLiteral (IntLiteral (-5)))),DeclStmt (DeclAssignStmtLiteral "ParmCallDeclIdx (ParmCallDeclIdent \"s\") (ParmCallDeclLiteral (IntLiteral 1))" Equal (ParmCallDeclLiteral (IntLiteral 4)))]) Nothing]
    a @=? a
    show a @=? show a

testStmt = TestCase $ do
    let a = CompoundStmt []
    a @=? a
    show a @=? show a


testTokenShowCoverage = TestCase $ do
  let tokens =
        [ Keyword Bool
        , Keyword Char
        , Keyword Int
        , Keyword Float
        , Keyword Double
        , Keyword Rizz.Tokens.Struct
        , Keyword Rizz.Tokens.String

        , Keyword Fn
        , Keyword If
        , Keyword Else
        , Keyword While
        , Keyword For
        , Keyword Foreach
        , Keyword Ret
        , Keyword Break
        , Keyword Continue

        , Identifier "foo"

        , Literal (BoolLiteral True)
        , Literal (BoolLiteral False)
        , Literal (CharLiteral 'x')
        , Literal (IntLiteral 42)
        , Literal (FloatLiteral 3.14)
        , Literal (ListLiteral [IntLiteral 1, IntLiteral 2])

        , Punctuator (SBracket OpenSBracket)
        , Punctuator (SBracket CloseSBracket)
        , Punctuator (RBracket OpenRBracket)
        , Punctuator (RBracket CloseRBracket)
        , Punctuator (CBracket OpenCBracket)
        , Punctuator (CBracket CloseCBracket)

        , Punctuator Dot
        , Punctuator Arrow

        , Punctuator (UnaryOp IdentIncrement)
        , Punctuator (UnaryOp IdentDecrement)

        , Punctuator (BinaryOp Mul)
        , Punctuator (BinaryOp Add)
        , Punctuator (BinaryOp Sub)
        , Punctuator (BinaryOp Div)
        , Punctuator (BinaryOp Mod)
        , Punctuator (BinaryOp Lt)
        , Punctuator (BinaryOp Gt)
        , Punctuator (BinaryOp LEq)
        , Punctuator (BinaryOp GEq)
        , Punctuator (BinaryOp Eq)
        , Punctuator (BinaryOp NEq)
        , Punctuator (BinaryOp And)
        , Punctuator (BinaryOp Or)

        , Punctuator Colon
        , Punctuator Semicolon
        , Punctuator Comma
        , Punctuator QMark

        , Punctuator (AssignOp Equal)
        , Punctuator (AssignOp MulEqual)
        , Punctuator (AssignOp DivEqual)
        , Punctuator (AssignOp ModEqual)
        , Punctuator (AssignOp AddEqual)
        , Punctuator (AssignOp SubEqual)
        ]

  map show tokens @=? map show tokens
