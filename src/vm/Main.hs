{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Main.hs
-}

module Main where

import System.Environment (getProgName, getArgs)
import System.IO (stderr, hPutStrLn)
import System.FilePath (takeExtension)
import System.Exit (exitFailure, exitSuccess)

import Data.List (isPrefixOf, group, sort)

import Format (error)

import Parser (parseFunctions)
import Interpreter (call)

-- files should start with a magic byte to identify as glados compiled byte code
-- check if multiples function share the same name inside all files
--  ==> can be solved by naming function by its file name (namespace) + function name
-- fecth decode execute

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

parseArgs :: [String] -> Either String [FilePath]
parseArgs [] = Left $ ": " ++ Format.error ++ ": no input files"
parseArgs (('-': _): _) = Left "USAGE"
parseArgs (x: xs)
    | takeExtension x /= ".bc"
        = Left $ ": " ++ Format.error ++ ": " ++ x ++ ": unknown file type"
    | null xs = Right [x]
    | otherwise = case parseArgs xs of
        Left e -> Left e
        Right files -> Right $ x: files

printUsage :: String -> IO ()
printUsage x = putStrLn $
    "GLaDOS project (vm part) - Execute pre-compiled byte code.\n\n"
    ++ "\ESC[1;33mUSAGE\ESC[0m: " ++ x ++ " <compiled files (.bc)>"

interpret :: [FilePath] -> IO ()
interpret files = do
    x <- mapM readFile files 
    case parseFunctions (lines (concat x)) of
        Left e -> hPutStrLn stderr e >> exitFailure
        Right symtab -> case call "baz" [] symtab symtab [] of
            Left e -> hPutStrLn stderr e >> exitFailure
            Right y -> case y of
                Nothing -> exitSuccess
                Just z -> print $ show z

main :: IO ()
main = do
    progName <- getProgName
    arguments <- parseArgs <$> getArgs
    case arguments of
        Left e -> if "USAGE" `isPrefixOf` e
            then printUsage progName
            else hPutStrLn stderr (progName ++ e) >> exitFailure
        Right files -> interpret $ rmdups files
