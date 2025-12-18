{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/rizz/Format.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Format
-- Description : Format informations message when a warning or error is raised.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- Takes some context and builds a custom, nicely formatted informational message.
-------------------------------------------------------------------------------
module Format (
    -- * Formating functions
    fError
) where

-- | Takes a @'String'@, a (@'Int'@, @'Int'@) and a @'String'@ as parameters
-- and creates a formatted error message from them.
--
-- The first @'String'@ represents the current stream of bytes to display
-- (giving the user some context), the tuple represents the line and column
-- at which the error was detected, and the second @'String'@ is the
-- informational message to display.
fError :: String -> (Int, Int) -> String -> String
fError stream@(x: _) (line, column) message = show line ++ ":"
    ++ show column ++ ": \ESC[1;31merror\ESC[0m: " ++ message ++ "\n    "
    ++ show line ++ " | " ++ takeWhile (/= '\n') stream
