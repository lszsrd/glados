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
    Instruction           (..)
) where

import Stack (Operand (..))

-- fetch decode execute

data Instruction
    = Nop
    -- ^ code #0 nop; does nothing.
    | Push Operand
    -- ^ code #1; pushes an Operand to the Stack.
    | Pop
    -- ^ code #2; pops the topmost Operand from the Stack.
    | Fetch String
    -- ^ code #3; fetches an Operand from the Environment and pushes it to the Stack.
    | JLt Int
    -- ^ code #4; jumps of a given amount of instructions if the Operand on top of the Stack is less than the second Operand on the top of the Stack.
    | JGt Int
    -- ^ code #5; jumps of a given amount of instructions if the Operand on top of the Stack is greater than the second Operand on the top of the Stack.
    | JLEq Int
    -- ^ code #6; jumps of a given amount of instructions if the Operand on top of the Stack is less than or equal to the second Operand on the top of the Stack.
    | JGEq Int
    -- ^ code #7; jumps of a given amount of instructions if the Operand on top of the Stack is greater or equal to than the second Operand on the top of the Stack.
    | JEq Int
    -- ^ code #8; jumps of a given amount of instructions if the Operand on top of the Stack is equal to the second Operand on the top of the Stack.
    | Neg Operand
    -- ^ code #9; inverses the  value on top of the Stack (ex: True => False, 42 => -42, -3.14 => 3.14). 
    | Add Operand Operand
    -- ^ code #10; additions the two top most Operand on the Stack and store it the computed result.
    | Sub Operand Operand
    -- ^ code #11; substractes the two top most Operand on the Stack and store it the computed result.
    | Mul Operand Operand
    -- ^ code #12; multiplies the two top most Operand on the Stack and store it the computed result.
    | Div Operand Operand
    -- ^ code #13; divides the two top most Operand on the Stack and store it the computed result.
    | Mod Operand Operand
    -- ^ code #14; modulos the two top most Operand on the Stack and store it the computed result.
    | And Operand Operand
    -- ^ code #15; does a logical and to the two top most Operand on the Stack and store it the computed result.
    | Or Operand Operand
    -- ^ code #16; does a logical or to the two top most Operand on the Stack and store it the computed result.
    | Inc Operand
    -- ^ code #17; adds 1 to the Operand on top of the Stack.
    | Dec Operand
    -- ^ code #18; substractes 1 to the Operand on top of the Stack.

    deriving (
        Show
        , Eq
    )
