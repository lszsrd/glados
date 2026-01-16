{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/lisp/types/Ast.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Ast
-- Description : Defines the abstract syntax tree structure for the lisp language.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- The abstract syntax tree serves to define how lisp tokens forms a valid grammar in order to represent a correct language expression.
--
--
-- === __Example__
-- Taking those simple functions, expressed in rizz code:
--
-- @(define foo (add a b)
--     (+ a b))
-- (define add
--     (lambda (a b)
--         (+ a b)))
-- (define deriv (lambda (expr var)
--     (if expr var 1)))@
-------------------------------------------------------------------------------
module Ast (
    -- * Simple atoms definitions
    Identifier
    , Decl                  (..)
    , Expr                  (..)
    , Number                (..)

    -- * Operators definitions
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
    | OpIdentifier Identifier

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
