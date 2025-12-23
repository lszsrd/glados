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
    = OPNop
    -- ^ code #0 nop; does nothing.
    | OPPush Operand
    -- ^ code #1; pushes an Operand to the Stack.
    | OPPop
    -- ^ code #2; pops the topmost Operand from the Stack.
    | OPStore String Operand
    -- ^ code #3; stores an Operand to the Environment.
    | OPFetch String
    -- ^ code #4; fetches an Operand from the Environment and pushes it to the Stack.
    | OPJLt Int
    -- ^ code #5; jumps of a given amount of instructions if the Operand on top of the Stack is less than the second Operand on the top of the Stack.
    | OPJGt Int
    -- ^ code #6; jumps of a given amount of instructions if the Operand on top of the Stack is greater than the second Operand on the top of the Stack.
    | OPJLEq Int
    -- ^ code #7; jumps of a given amount of instructions if the Operand on top of the Stack is less than or equal to the second Operand on the top of the Stack.
    | OPJGEq Int
    -- ^ code #8; jumps of a given amount of instructions if the Operand on top of the Stack is greater or equal to than the second Operand on the top of the Stack.
    | OPJEq Int
    -- ^ code #9; jumps of a given amount of instructions if the Operand on top of the Stack is equal to the second Operand on the top of the Stack.
    | OPNeg Operand
    -- ^ code #10; inverses the  value on top of the Stack (ex: True => False, 42 => -42, -3.14 => 3.14). 
    | OPAdd Operand Operand
    -- ^ code #11; additions the two top most Operand on the Stack and store it the computed result.
    | OPSub Operand Operand
    -- ^ code #12; substractes the two top most Operand on the Stack and store it the computed result.
    | OPMul Operand Operand
    -- ^ code #13; multiplies the two top most Operand on the Stack and store it the computed result.
    | OPDiv Operand Operand
    -- ^ code #14; divides the two top most Operand on the Stack and store it the computed result.
    | OPMod Operand Operand
    -- ^ code #15; modulos the two top most Operand on the Stack and store it the computed result.
    | OPAnd Operand Operand
    -- ^ code #16; does a logical and to the two top most Operand on the Stack and store it the computed result.
    | OPOr Operand Operand
    -- ^ code #17; does a logical or to the two top most Operand on the Stack and store it the computed result.
    | OPInc String
    -- ^ code #18; adds 1 to the Operand on top of the Stack.
    | OPDec String
    -- ^ code #19; substractes 1 to the Operand on top of the Stack.

    deriving (
        Show
        , Eq
    )
