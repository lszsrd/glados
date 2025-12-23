{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/backend/types/Env.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Env
-- Description : Defines an environment to store foreign functions' arguments.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
-------------------------------------------------------------------------------
module Env (
    Env
) where

import Stack (Operand (..))

-- | Defines an @'Env'@, a basic data structure allowing to store functions' parameters.
type Env = [(String, Operand)]
