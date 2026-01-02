{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/types/Stack.hs
-}

module Stack (
    Stack
) where

import OpCodes (Operand (..))

type Stack = [Operand]
