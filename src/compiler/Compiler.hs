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

import Arguments (Options (..))

type Lexer a = String -> Either String [(a, (Int, Int))]
type Parser a b = [(a, (Int, Int))] -> Either String [b]
type Transpiler b = [b] -> String

compile :: (Show a, Show b)
    => Options -> [FilePath] -> Lexer a -> Parser a b -> Transpiler b -> IO ()
compile _ [] _ _ _ = exitSuccess
compile opts (x: xs) lexer parser transpiler = do
    content <- readFile x
    if null content
        then eval opts xs lexer parser transpiler
            (x ++ ": " ++ warning ++ ": empty file")
        else case parseFile x content lexer parser of
            Left e -> eval opts xs lexer parser transpiler e
            Right (path, (toks, d)) -> writeFile (path ++ ".bc") (transpiler d)
                >> hCompile opts path toks d
                >> compile opts xs lexer parser transpiler

hCompile :: (Show a, Show b)
    => Options -> FilePath -> [(a, (Int, Int))] -> b -> IO ()
hCompile (Options True x) filepath tokens decl = print tokens
    >> hCompile Options {dumpToks = False, dumpAst = x} filepath tokens decl
hCompile (Options x True) filepath tokens decl = print decl
    >> hCompile Options {dumpToks = x, dumpAst = False} filepath tokens decl
hCompile _ filepath _ _ = putStrLn $ "(OK) Compiled '" ++ filepath ++ "'"

eval :: (Show a, Show b)
    => Options -> [FilePath] -> Lexer a -> Parser a b -> Transpiler b -> String
    -> IO ()
eval opts files lexer parser transpiler e = hPutStrLn stderr e
    >> case findString e "warning" of
        Nothing -> exitFailure
        _ -> compile opts files lexer parser transpiler

findString :: String -> String -> Maybe String
findString [] _ = Nothing
findString string@(_: x) needle
    | needle `isPrefixOf` string = Just string
    | otherwise = findString x needle

parseFile :: FilePath -> String -> Lexer a -> Parser a b
    -> Either String (FilePath, ([(a, (Int, Int))], [b]))
parseFile filepath content lexer parser = case lexer content of
    Left e -> Left $ filepath ++ ":" ++ e
    Right tokens -> case parser tokens of
        Left e -> Left $ filepath ++ ":" ++ e
        Right ast -> if null ast
            then Left $ filepath ++ ": " ++ warning ++ ": no compilation unit"
            else Right (filepath, (tokens, ast))
