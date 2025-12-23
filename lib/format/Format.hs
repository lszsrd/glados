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
    -- * Logging messages
    Format.error
    , Format.warning

    -- * Formating functions
    , fString
    , fError
    , fWarn
) where

import Data.Char (isSpace)

error :: String
error = "\ESC[1;31merror\ESC[0m"

warning :: String
warning = "\ESC[1;35mwarning\ESC[0m"

fString :: String -> String -> (Int, Int) -> Int -> String -> String
fString level s (l, c) size message
    = show l ++ ":" ++ show c ++ ": " ++ level ++ ": " ++ message
    ++ "\n    " ++ show l ++ " | "
    ++ dropWhile isSpace (lines s !! (l - 1))
    ++ "\n    " ++ replicate (length $ show l) ' ' ++ " | "
    ++ replicate (c - 1 - length (takeWhile isSpace (lines s !! (l - 1))))  ' '
    ++ "\ESC[1;32m^" ++ replicate (size - 1) '~' ++ " here\ESC[0m"

-- | Takes a @'String'@, a (@'Int'@, @'Int'@) and a @'String'@ as parameters
-- and creates a formatted error message from them.
--
-- The first @'String'@ represents the current stream of bytes to display
-- (giving the user some context), the tuple represents the line and column
-- at which the error was detected, the integer is the token's size and the
-- second @'String'@ is the informational message to display.
fError :: String -> (Int, Int) -> Int -> String -> String
fError = fString Format.error

fWarn :: String -> (Int, Int) -> Int -> String -> String
fWarn = fString Format.warning
