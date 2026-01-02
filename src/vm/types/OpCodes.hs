{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/types/OpCodes.hs
-}

module OpCodes (
    Identifier
    , Label

    , Operand               (..)
    , OpCode                (..)
) where

type Identifier = String
type Label = String

data Operand
    = Bool Bool
    | Char Char
    | Integer Integer
    | Float Float
    | List [Operand]

    deriving (
        Show
        , Eq
    )

data OpCode
    = Nop
    | Call Identifier Integer
    | Load Identifier
    | Store Identifier
    | PushBool Operand
    | PushChar Operand
    | PushInt Operand
    | PushFloat Operand
    | Pop
    | JumpFalse Label
    | JumpTrue Label
    | Label Identifier
    | Mul
    | Add
    | Sub
    | Div
    | Mod
    | Lt
    | Gt
    | LEq
    | GEq
    | Eq
    | NEq
    | And
    | Or
    | Ret

    deriving (
        Show
        , Eq
    )
