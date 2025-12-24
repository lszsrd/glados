{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Compiler.hs
-}

module Compiler (
    Lexer
    , Parser

    , compile
    , eval
    , findString
    , parseFile
) where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)

import Data.List (isPrefixOf)

import Format (warning)

type Lexer a = String -> Either String [(a, (Int, Int))]
type Parser a b = [(a, (Int, Int))] -> Either String [b]

compile :: [FilePath] -> Lexer a -> Parser a b -> IO ()
compile [] _ _ = exitSuccess
compile (x: xs) lexer parser = do
    content <- readFile x
    if null content
        then eval xs lexer parser (x ++ ": " ++ warning ++ ": empty file")
        else case parseFile x content lexer parser of
            Left e -> eval xs lexer parser e
            Right (filepath, ast) -> writeFile (filepath ++ ".bc") ""
                >> putStrLn ("(OK) Compiled '" ++ filepath ++ "'")
                >> compile xs lexer parser

eval :: [FilePath] -> Lexer a -> Parser a b -> String -> IO ()
eval files lexer parser error = hPutStrLn stderr error
    >> case findString error "warning" of
        Nothing -> exitFailure
        _ -> compile files lexer parser

findString :: String -> String -> Maybe String
findString [] _ = Nothing
findString string@(_: x) needle
    | needle `isPrefixOf` string = Just string
    | otherwise = findString x needle

parseFile :: FilePath -> String -> Lexer a -> Parser a b
    -> Either String (FilePath, [b])
parseFile filepath content lexer parser = case lexer content of
    Left e -> Left $ filepath ++ ":" ++ e
    Right tokens -> case parser tokens of
        Left e -> Left $ filepath ++ ":" ++ e
        Right ast -> if null ast
            then Left $ filepath ++ ": " ++ warning ++ ": no compilation unit"
            else Right (filepath, ast)
