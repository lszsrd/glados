{-
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/AbstractTree.hs
-}

module AbstractTree (
      Ast(..)
    , Identifier(..)
    , Expr(..)
    , List(..)
    ) where

type Identifier     = String

data Expr = Lambda          [Identifier] Expr
  |         If              Expr Expr Expr
  |         Call            Expr [Expr]
  |         Var             Identifier
  |         Boolean         Bool
  |         Int             Integer
  deriving (Show, Eq)

data Ast =  Define          Identifier Expr
  |         Expression      Expr
  deriving (Show, Eq) 

type List = [Ast]
