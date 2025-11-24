{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Error.hs
-}

module Error (ErrorT (..), throwErr, printError) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Control.Exception (throw, toException)

data ErrorT = ErrorT {
    location :: Int,
    message :: String
} deriving(Show)

-- throw an error constructed with an ErrorT
throwErr :: ErrorT -> a
throwErr errT = throw (toException
    (userError
        (message errT ++ ", at line: " ++
            show (location errT)
        )
    ))

-- Prints error to stderr and exits with 84
printError :: String -> IO a
printError ff = hPutStrLn stderr ff
    >> exitWith (ExitFailure 84)
