{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Main.hs
-}

module Main where

import System.IO (stderr, hPutStrLn)
import System.Environment (getProgName, getArgs)
import System.FilePath (takeExtension)
import System.Exit (exitFailure)

import Data.List (isPrefixOf)

import Compiler (compile)

import Format (error)
import Lexer (lexer)
import Parser (parser)

parseArgs :: String -> [String] -> Either String [FilePath]
parseArgs _ [] = Left $ ": " ++ Format.error ++ ": no input files"
parseArgs _ ("-h": _) = Left "USAGE"
parseArgs extension (x: xs)
    | takeExtension x /= extension
        = Left $ ": " ++ Format.error ++ ": " ++ x ++ ": unknown file type"
    | null xs = Right [x]
    | otherwise = case parseArgs extension xs of
        Left e -> Left e
        Right files -> Right $ x: files

printUsage :: String -> String -> IO ()
printUsage ext progName = putStrLn $
    "GLaDOS project (compiler part) - Translate one or more source"
    ++ " (code) file(s) to custom bytecode file(s).\n\n"
    ++ "\ESC[1;33mUSAGE\ESC[0m: " ++ progName ++ " [source files (" ++ ext
    ++ ")]\n\nGenerated files are built from source file's path and a custom "
    ++ "\".bc\" extension is appened.\n\nByte code files are NOT object files "
    ++ "which mean that they do not link to any object linker (i.e. ld on "
    ++ "linux).\nTo use compiled files, you need to pass them to the GLaDOS"
    ++ " interpreter (glados-interpreter binary)."

main :: IO ()
main = do
    let extension = ".rz"
    let langLexer = lexer
    let langParser = parser
    progName <- getProgName
    args <- parseArgs extension <$> getArgs
    case args of
        Left e -> if "USAGE" `isPrefixOf` e then printUsage extension progName
            else hPutStrLn stderr (progName ++ e) >> exitFailure
        Right files -> compile files langLexer langParser
