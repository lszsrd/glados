{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/types/Stack.hs
-}

module Stack (
    Stack

    , popStackN
) where

import OpCodes (Operand (..))

type Stack = [Operand]

popStackN :: Int -> Stack -> Maybe ([Operand], Stack)
popStackN x stack = if x > y
    then Nothing
    else Just (drop (y - x) stack, take (y - x) stack)
    where y = length stack
