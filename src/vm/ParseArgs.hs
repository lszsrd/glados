{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/ParseArgs.hs
-}

module ParseArgs (
    parseArgs
    , printUsage
) where

import System.FilePath (takeExtension)

import Format (error)

parseArgs :: [String] -> Either String [FilePath]
parseArgs (('-': _): _) = Left "USAGE"
parseArgs (x: xs)
    | takeExtension x /= ".bc" = Left
        $ ": " ++ Format.error ++ ": " ++ x ++ ": unknown file extension"
    | null xs = Right [x]
    | otherwise = case parseArgs xs of
        Left e -> Left e
        Right files -> Right $ x: files
parseArgs [] = Left $ ": " ++ Format.error ++ ": no input files"

printUsage :: String -> IO ()
printUsage x = putStrLn $
    "GLaDOS project (VM part) - Execute pre-compiled byte code.\n\n"
    ++ "\ESC[1;33mUSAGE\ESC[0m: " ++ x ++ " <compiled files (.bc)>"
