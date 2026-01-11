{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/lisp/types/Tokens.hs
-}

module Tokens (
    Stream
    , Tokens

    , Token                 (..)
    , RBracket              (..)
    , Atom                  (..)
    , Operator              (..)
) where

type Stream = String

type Tokens = [(Token, (Int, Int))]

instance Show Token where
    show (RBracket Open) = "'('"
    show (RBracket Close) = "')'"
    show (Atom (Bool False)) = "'#f'"
    show (Atom (Bool True)) = "'#t'"
    show (Atom (Integer x)) = show x
    show (Atom (Float x)) = show x
    show (Atom (Identifier x)) = x
    show (Atom (Operator Define)) = "'define'"
    show (Atom (Operator Lambda)) = "'lambda'"
    show (Atom (Operator Add)) = "'+'"
    show (Atom (Operator Sub)) = "'-'"
    show (Atom (Operator Mul)) = "'*'"
    show (Atom (Operator Div)) = "'div'"
    show (Atom (Operator Mod)) = "'mod'"
    show (Atom (Operator Lt)) = "'<'"
    show (Atom (Operator Eq)) = "'eq?'"
    show (Atom (Operator If)) = "'if'"

data Token
    = RBracket RBracket
    | Atom Atom

    deriving (
        Eq
    )

data RBracket
    = Open
    | Close

    deriving (
        Show
        , Eq
    )

data Atom
    = Bool Bool
    | Integer Integer
    | Float Float
    | Identifier String
    | Operator Operator

    deriving (
        Show
        , Eq
    )

data Operator
    = Define
    | Lambda
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | Lt
    | If

    deriving (
        Show
        , Eq
    )
