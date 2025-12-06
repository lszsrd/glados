{-
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/AbstractTree.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : AbstractTree
-- Description : Abstract syntax tree (AST) definitions used by the lexer,
--               parser and interpreter. Provides compact, well-typed
--               representations for program source forms used across the
--               project (expressions, top-level forms and identifiers)
--
-- License     : MIT
-- Maintainers : maxence.pierre@epitech.eu, nathan.flachat@epitech.eu, laszlo.serdet@epitech.eu, hugo.duda@epitech.eu, florian.grave@epitech.eu
--
-- This module exports the core AST types:
--   * Identifier : alias for String used for names
--   * Expr       : expression language (literals, vars, calls, lambda, if)
--   * Ast        : top-level forms (define and expression)
--   * List       : convenience alias for lists of top-level forms
--
-- Keep these types small and stable: the interpreter and builtins expect
-- exactly these constructors (Int, Boolean, Var, Call, Lambda, If)
-------------------------------------------------------------------------------
module AbstractTree (
      Ast(..)
    , Identifier(..)
    , Expr(..)
    , List(..)
    ) where


-- | Short alias for identifiers used throughout the AST (variable and
-- function names). Kept as a separate alias to make signatures explicit
type Identifier     = String


-- | Expressions of the language
--
-- Constructors:
--
-- * 'Lambda' [Identifier] Expr
--     Anonymous function with a list of parameter names and a body expression
--     Example: (Lambda ["x","y"] (Call (Var "+") [Var "x", Var "y"]))
--
-- * 'If' Expr Expr Expr
--     Conditional expression: (If cond thenExpr elseExpr) The condition must
--     evaluate to a Boolean for reduction to succeed
--
-- * 'Call' Expr [Expr]
--     Function call: the head can be a 'Var' (named call) or another expression
--     (for anonymous lambda application) Arguments are expressions
--
-- * 'Var' Identifier
--     Variable or symbol reference
--
-- * 'Boolean' Bool
--     Boolean literal (#t / #f)
--
-- * 'Int' Integer
--     Integer literal
data Expr = Lambda          [Identifier] Expr
  |         If              Expr Expr Expr
  |         Call            Expr [Expr]
  |         Var             Identifier
  |         Boolean         Bool
  |         Int             Integer
  deriving (Show, Eq)


-- | Top-level AST nodes
--
-- * 'Define' Identifier Expr
--     Top-level definition binding an identifier to an expression Conventions:
--       - function definitions are typically stored as a Define name (Lambda params body)
--       - variable definitions can be Define name (Int) or Define name (Boolean )
--
-- * 'Expression' Expr
--     A top-level expression to evaluate
data Ast =  Define          Identifier Expr
  |         Expression      Expr
  deriving (Show, Eq) 

-- | Convenience alias for lists of top-level AST nodes
type List = [Ast]
