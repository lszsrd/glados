{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/rizz/Ast.hs
-}

module Ast (
    CmpdStmt                (..)
    , FunctionDecl          (..)
    , ParmVarDecl           (..)
    , VarDecl               (..)
    , ParmCallDecl          (..)
    , DeclStmt              (..)
    , BinaryOpParm          (..)
    , BinaryOpExpr          (..)
    , CallExprDecl          (..)
    , RetStmt               (..)
    , Stmt                  (..)
) where

import Token

data CmpdStmt
    = CmpdStmt [Stmt]

    deriving (
        Show
        , Eq
    )

data FunctionDecl
    = FunctionDecl (Maybe Type) Identifier [ParmVarDecl] CmpdStmt
    -- ^ FunctionDecl Nothing "foo" [ParmVarDecl Integer "x"] (CmpdStmt [])

    deriving (
        Show
        , Eq
    )


data ParmVarDecl
    = ParmVarDecl Type Identifier
    -- ^ ParmVarDecl Boolean "x"

    deriving (
        Show
        , Eq
    )

data VarDecl
    = VarDeclLiteral Type Identifier Literal
    | VarDeclIdent Type Identifier Identifier
    -- ^ WARNING: Check that the variable does not initialize itself!
    | VarDeclCall Type Identifier CallExprDecl

    deriving (
        Show
        , Eq
    )

data ParmCallDecl
    = ParmCallLiteral Literal
    | ParmCallIdent Identifier

    deriving (
        Show
        , Eq
    )

data DeclStmt
    = DeclStmtLiteral Identifier AssignOp Literal
    -- ^ DeclStmtLiteral "var" DivEqual (BoolLiteral True)
    | DeclStmtIdent Identifier AssignOp Identifier
    -- ^ DeclStmtIdent "var" MulEqual "bar"

    deriving (
        Show
        , Eq
    )

data BinaryOpParm
    = Ident Identifier
    | BinaryOp BinaryOpExpr
    | Constant Literal
    | CallExprRes CallExprDecl

    deriving (
        Show
        , Eq
    )

data BinaryOpExpr
    = BinaryOpExpr BinaryOpParm BinaryOp BinaryOpParm
    | BinaryOpConst Bool

    deriving (
        Show
        , Eq
    )

data CallExprDecl
    = CallExprDecl Identifier [ParmCallDecl]

    deriving (
        Show
        , Eq
    )

data RetStmt
    = RetLiteral Literal
    | RetIdent Identifier

    deriving (
        Show
        , Eq
    )

data Stmt
    = VarDecl VarDecl
    -- ^ VarDecl (VarDeclLiteral Boolean "foo" (BoolLiteral True))
    | DeclStmt DeclStmt
    -- ^ DeclStmt (DeclStmtLiteral "var" MulEqual (BoolLiteral True))
    | BinaryOperator BinaryOpExpr
    -- ^ BinaryOpExpr (Ident "x") Lt (Ident "y")
    | IfStmt BinaryOpExpr CmpdStmt (Maybe CmpdStmt)
    -- ^ IfStmt (BinaryOpExpr (CallExprRes (CallExprDecl "foo" [ParmCallLiteral (IntLiteral 42)])) Eq (Constant (IntLiteral 42))) (CmpdStmt []) (Just (CmpdStmt []))
    -- ^ IfStmt (BinaryOpConst True) (CmpdStmt []) Nothing
    | WhileStmt BinaryOpExpr CmpdStmt
    -- ^ WhileStmt (BinaryOpConst True) (CmpdStmt [])
    -- ^ WhileStmt (BinaryOpExpr (Constant (BoolLiteral True)) NEq (Constant (BoolLiteral True))) (CmpdStmt [])
    | ForStmt (Maybe VarDecl) (Maybe BinaryOpExpr) (Maybe DeclStmt) CmpdStmt
    -- ^ ForStmt (Just (VarDeclLiteral Boolean "x" (BoolLiteral True))) (Just (BinaryOpExpr (Ident "x") Lt (Ident "y"))) (Nothing) (CmpdStmt [])
    | ForeachStmt Identifier Identifier CmpdStmt
    -- ^ ForeachStmt "foo" "it" (CmpdStmt [])
    | CallExpr CallExprDecl
    -- ^ CallExpr (CallExprDecl "foo" [ParmCallLiteral (IntLiteral 42)])
    | UnaryOperator Identifier UnaryOp
    -- ^ UnaryOperator "x" IdentIncrement
    | RetStmt RetStmt
    -- ^ RetStmt (RetLiteral (BoolLiteral True))

    deriving (
        Show
        , Eq
    )
