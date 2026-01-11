module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

import Lexer
import Parser

main :: IO ()
main = do
    x <- head <$> getArgs
    buffer <- readFile x
    case lexer buffer of
        Left e -> hPutStrLn stderr e >> exitFailure
        Right tokens -> print tokens >> case parser tokens of
            Left e -> hPutStrLn stderr e >> exitFailure
            Right ast -> print ast
