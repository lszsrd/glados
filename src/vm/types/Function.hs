{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/types/Function.hs
-}

module Function (
    Function

    , fetch
    , jumpTo
) where

import OpCodes (Instruction (..))

type Function = (String, [String], [Instruction])

fetch :: String -> [Function] -> Maybe Function
fetch _ [] = Nothing
fetch x (y@(y', _, _): ys) = if x == y'
    then Just y
    else fetch x ys

jumpTo :: String -> [Instruction] -> Maybe [Instruction]
jumpTo _ [] = Nothing
jumpTo x (Label y: z)
    | x == y = Just z
jumpTo x (_: ys) = jumpTo x ys
