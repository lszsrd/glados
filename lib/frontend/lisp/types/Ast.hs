{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/lisp/types/Ast.hs
-}

module Ast (
    Identifier
    , Decl                  (..)
    , Expr                  (..)
    , Number                (..)
    , ArithOperator         (..)
    , CondOperator          (..)
    , BinaryOperator        (..)
) where

type Identifier = String

data Decl
    = Define Identifier Expr
    | Func Identifier [Identifier] Expr
    | Lambda [Identifier] Expr

    deriving (
        Show
        , Eq
    )

data Expr
    = Const Number
    | If CondOperator Expr Expr
    | Call Identifier [Expr]
    | BinaryOp BinaryOperator Expr Expr
    | Defun Decl

    deriving (
        Show
        , Eq
    )

data Number
    = Boolean Bool
    | Int Integer
    | SPrecision Float
    | Identifier Identifier

    deriving (
        Show
        , Eq
    )

data ArithOperator
    = OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod

    deriving (
        Show
        , Eq
    )

data CondOperator
    = OpLt
    | OpEq
    | OpBool Bool

    deriving (
        Show
        , Eq
    )

data BinaryOperator
    = ArithExpr ArithOperator
    | CondExpr CondOperator

    deriving (
        Show
        , Eq
    )
