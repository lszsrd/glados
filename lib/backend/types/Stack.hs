{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/backend/types/Stack.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Stack
-- Description : Defines operands and stack data structure.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
-------------------------------------------------------------------------------
module Stack (
    Stack
    , Operand               (..)
) where

-- | Defines a @'Stack'@, a basic data structure allowing to manipulate instructions and values.
type Stack = [Operand]

-- | Defines an @'Operand'@ as a value that can be stored in a @'Stack'@ and manipulated via instructions.
data Operand
    = Bool Bool
    -- ^ boolean values.
    | Integer Integer
    -- ^ integer values.
    | Float Float
    -- ^ single precision floating point values.

    deriving (
        Show
        -- ^ Allows @'Operand'@ to be printed.
        , Eq
        -- ^ Allows @'Operand'@ to be compared, needed for unit tests.
    )
