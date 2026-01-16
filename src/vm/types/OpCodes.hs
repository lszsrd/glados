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
    
    , showList'
) where

type Identifier = String
type Label = String

instance Show Operand where
    show (Bool False) = "False"
    show (Bool True) = "True"
    show (Char x) = [x]
    show (Integer x) = show x
    show (Float x) = show x
    show (List x) = showList' x
    show (Struct x) = show x

data Operand
    = Bool Bool
    | Char Char
    | Integer Integer
    | Float Float
    | List [Operand]
    | Struct [Operand]

    deriving (
        Eq
        , Ord
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
    | PushList Int
    | Pop
    | Jump Label
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

showList' :: [Operand] -> String
showList' [] = []
showList' [x] = show x
showList' ((Char x): xs) = x: showList' xs
showList' (x: xs) = (" " ++ show x ++ ", ") ++ showList' xs
