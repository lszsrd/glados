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

import Data.Char (isSpace)

-- | Takes a @'String'@, a (@'Int'@, @'Int'@) and a @'String'@ as parameters
-- and creates a formatted error message from them.
--
-- The first @'String'@ represents the current stream of bytes to display
-- (giving the user some context), the tuple represents the line and column
-- at which the error was detected, the integer is the token's size and the
-- second @'String'@ is the informational message to display.
fError :: String -> (Int, Int) -> Int -> String -> String
fError s (l, c) tokSize message
    = show l ++ ":" ++ show c ++ ": \ESC[1;31merror\ESC[0m: " ++ message
    ++ "\n    " ++ show l ++ " | "
    ++ dropWhile isSpace (lines s !! (l - 1))
    ++ "\n    " ++ replicate (length $ show l) ' ' ++ " | "
    ++ replicate (c - 1 - length (takeWhile isSpace (lines s !! (l - 1))))  ' '
    ++ "\ESC[1;32m^" ++ replicate (tokSize - 1) '~' ++ " here\ESC[0m"
