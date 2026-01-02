{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/types/Env.hs
-}

module Env (
    Env
) where

import OpCodes (Operand (..))

type Env = [(String, Operand)]
