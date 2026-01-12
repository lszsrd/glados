{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Function.hs
-}

module Function (
    Function
) where

import OpCodes (OpCode (..))

type Function = (String, Int, [OpCode])
