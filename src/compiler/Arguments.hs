{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Arguments.hs
-}

module Arguments (
    parseArgs
) where

import System.FilePath (takeExtension)

import Format (error)

parseArgs :: [String] -> Either String [FilePath]
parseArgs [] = Left $ ": " ++ Format.error ++ ": no input files"
parseArgs (x: xs)
    | x == "-h" = Left "USAGE"
    | takeExtension x /= ".rz"
        = Left $ ": " ++ Format.error ++ ": " ++ x ++ ": unknown file type"
    | null xs = Right [x]
    | otherwise = case parseArgs xs of
        Left e -> Left e
        Right files -> Right $ x: files
