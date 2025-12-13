{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/rizz/Ast.hs
-}

module Ast (
    CmpdStmt                (..)
    , ParmVarDeclExpr       (..)
    , Decl                  (..) -- main type
    , ParmCallDecl          (..)
    , VarDeclStmt           (..)
    , DeclStmt              (..)
    , BinaryOpParm          (..)
    , BinaryOpExpr          (..)
    , CallExprDecl          (..)
    , Stmt                  (..) -- main type
) where

import Token

data CmpdStmt
    = CmpdStmt [Stmt]

    deriving (
        Show
        , Eq
    )

data ParmVarDeclExpr
    = ParmVarDeclExpr Type Identifier
    -- ^ ParmVarDecl Boolean "x"

    deriving (
        Show
        , Eq
    )

data Decl
    = FunctionDecl (Maybe Type) Identifier [ParmVarDeclExpr] CmpdStmt
    -- ^ FunctionDecl Nothing "foo" [ParmVarDeclExpr Integer "x"] (CmpdStmt [])
    | ParmVarDecl ParmVarDeclExpr
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
    = VarDeclStmt Type Identifier ParmCallDecl
    

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

data Stmt
    = DeclStmt DeclStmt
    -- ^ DeclStmt (DeclStmtLiteral "var" MulEqual (ParmCallDeclLiteral (BoolLiteral True)))
    | UnaryOperator Identifier UnaryOp
    -- ^ UnaryOperator "x" IdentIncrement
    | BinaryOperator BinaryOpExpr
    -- ^ BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))
    | IfStmt BinaryOpExpr CmpdStmt (Maybe CmpdStmt)
    --                             ^
    --                             \-- else block that is OPTIONAL
    -- ^ IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)]))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))) (CmpdStmt []) (Just (CmpdStmt [])
    -- ^ IfStmt (BinaryOpConst True) (CmpdStmt []) Nothing
    | WhileStmt BinaryOpExpr CmpdStmt
    -- ^ WhileStmt (BinaryOpConst True) (CmpdStmt [])
    -- ^ WhileStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) NEq (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral False)))) (CmpdStmt [])
    | ForStmt (Maybe VarDeclStmt) (Maybe BinaryOpExpr) (Maybe DeclStmt) CmpdStmt
    -- ^ ForStmt (Just (VarDeclStmt Integer "i" (ParmCallDeclExpr (CallExprDecl "foo" [])))) (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "x")) Lt (BinaryOpParm (ParmCallDeclIdent "y")))) Nothing (CmpdStmt [])
    | ForeachStmt Identifier Identifier CmpdStmt
    -- ^ ForeachStmt "foo" "it" (CmpdStmt [])
    | CallExpr CallExprDecl
    -- ^ CallExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)])
    | RetStmt ParmCallDecl
    -- ^ RetStmt (ParmCallDeclIdent "foo")

    deriving (
        Show
        , Eq
    )
