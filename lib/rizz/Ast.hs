{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/rizz/Ast.hs
-}

module Ast (
    CompoundStmt            (..)
    , ParmVarDeclExpr       (..)
    , Decl                  (..) -- main data structure
    , ParmCallDecl          (..)
    , VarDeclStmt           (..)
    , DeclStmt              (..)
    , BinaryOpParm          (..)
    , BinaryOpExpr          (..)
    , CallExprDecl          (..)
    , Stmt                  (..) -- main data structure
) where

import Token

data CompoundStmt
    = CompoundStmt [Stmt]

    deriving (
        Show
        , Eq
    )

data ParmVarDeclExpr
    = ParmVarDeclExpr BuiltinType Identifier
    -- ^ ParmVarDecl Boolean "x"

    deriving (
        Show
        , Eq
    )

data Decl
    = FunctionDecl (Maybe BuiltinType) Identifier [ParmVarDeclExpr] CompoundStmt
    -- ^ FunctionDecl Nothing "foo" [ParmVarDeclExpr Integer "x"] (CompoundStmt [])
    | ParmVarDecl ParmVarDeclExpr
    -- ^ ParmVarDeclExpr Integer "foo"
    | VarDecl VarDeclStmt -- TODO (semantic analysis): Check that the variable does not initialize itself!
    -- ^ VarDecl (VarDeclStmt Boolean "foo" (ParmCallDeclLiteral (BoolLiteral True)))

    deriving (
        Show
        , Eq
    )

data ParmCallDecl
    = ParmCallDeclLiteral Literal
    | ParmCallDeclIdent Identifier
    | ParmCallDeclExpr CallExprDecl

    deriving (
        Show
        , Eq
    )

data VarDeclStmt
    = VarDeclStmt BuiltinType Identifier ParmCallDecl
    

    deriving (
        Show
        , Eq
    )

data DeclStmt
    = DeclStmtLiteral Identifier AssignOp ParmCallDecl
    -- ^ DeclStmtLiteral "var" DivEqual (ParmCallDeclLiteral (BoolLiteral True))

    deriving (
        Show
        , Eq
    )

data BinaryOpParm
    = BinaryOpParm ParmCallDecl
    | BinaryOpParmBOp BinaryOpExpr

    deriving (
        Show
        , Eq
    )

data BinaryOpExpr
    = BinaryOpExpr BinaryOpParm BinaryOp BinaryOpParm
    | BinaryOpConst ParmCallDecl

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

data Stmt
    = DeclStmt DeclStmt
    -- ^ DeclStmt (DeclStmtLiteral "var" MulEqual (ParmCallDeclLiteral (BoolLiteral True)))
    | UnaryOperator Identifier UnaryOp
    -- ^ UnaryOperator "x" IdentIncrement
    | BinaryOperator BinaryOpExpr
    -- ^ BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))
    | IfStmt BinaryOpExpr CompoundStmt (Maybe CompoundStmt)
    --                             ^
    --                             \-- else block that is OPTIONAL
    -- ^ IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)]))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))) (CompoundStmt []) (Just (CompoundStmt []))
    -- ^ IfStmt (BinaryOpConst (ParmCallDeclLiteral (BoolLiteral True))) (CompoundStmt []) Nothing
    | WhileStmt BinaryOpExpr CompoundStmt
    -- ^ WhileStmt (BinaryOpConst (ParmCallDeclLiteral (BoolLiteral True))) (CompoundStmt [])
    -- ^ WhileStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) NEq (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral False)))) (CompoundStmt [])
    | ForStmt (Maybe VarDeclStmt) (Maybe BinaryOpExpr) (Maybe DeclStmt) CompoundStmt
    -- ^ ForStmt (Just (VarDeclStmt Integer "i" (ParmCallDeclExpr (CallExprDecl "foo" [])))) (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "x")) Lt (BinaryOpParm (ParmCallDeclIdent "y")))) Nothing (CompoundStmt [])
    | ForeachStmt Identifier Identifier CompoundStmt
    -- ^ ForeachStmt "foo" "it" (CompoundStmt [])
    | CallExpr CallExprDecl
    -- ^ CallExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)])
    | RetStmt BinaryOpExpr
    -- ^ RetStmt (BinaryOpConst (ParmCallDeclIdent "foo"))

    deriving (
        Show
        , Eq
    )
