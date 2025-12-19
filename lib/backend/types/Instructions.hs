{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/backend/types/Instructions.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Instructions
-- Description : Defines instructions that will be generated from the generator step.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
-------------------------------------------------------------------------------
module Instructions (
    Instruction
) where

-- fetch decode execute

data Value
    = Bool Bool
    | Integer Integer
    | Float Float

    deriving (
        Show
        , Eq
    )

data Registers = Registers  {
    rax :: Value
    , r1 :: Value
    , r2 :: Value
    , r3 :: Value
    , r4 :: Value
    , r5 :: Value
    , r6 :: Value
    , r7 :: Value
    , r8 :: Value
                            }
    deriving (
        Show
        , Eq
    )

type Environment = [(String, Value)]
type Stack = [Value]

-- variables are in env
-- constants are in stack
-- move/get values to registers to operate on them
-- when an operation is done (add, mul, ret, calling a function -> store the value in rax)

data Instruction
    = Nop
    -- ^ code #0 nop; do nothing
    | BPush -- push a boolean to the stack
    -- ^ code # ;
    | IPush -- push an integer to the stack
    -- ^ code # ;
    | FPush -- push a float to the stack
    -- ^ code # ;
    | Pop -- pop a value from stack and store in a reg
    -- ^ code # ; 
    | Mov -- copy a value from a reg to another
    -- ^ code # ;
    | Fetch -- get a var from env and store it in a register (fetch <name> <reg>)
    -- all jumps conditions bellow
    -- ^ code # ; 
    | JLt
    -- ^ code # ; 
    | JGt
    -- ^ code # ; 
    | JLEq
    -- ^ code # ; 
    | JGEq
    -- ^ code # ; 
    | JEq
    -- ^ code # ; 
    | Neg
    -- ^ code # ; 

    -- all operations (operate on registers and store result in rax)
    | Add
    -- ^ code # ; 
    | Sub
    -- ^ code # ; 
    | Mul
    -- ^ code # ; 
    | Div
    -- ^ code # ; 
    | Mod
    -- ^ code # ; 
    | And
    -- ^ code # ; 
    | Or
    -- ^ code # ; 
    | Inc -- increment a value in a reg
    -- ^ code # ; 
    | Dec -- decrement a value in a reg
    -- ^ code # ; 
    | Hlt -- halt for a X ms
    -- ^ code # ; 

    deriving (
        Show
        , Eq
    )
