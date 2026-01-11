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
import Text.Read (readMaybe)

import Format (warning, fError)

import Options (Options (..))

type Lexer a = String -> Either String [(a, (Int, Int))]
type Parser a b = [(a, (Int, Int))] -> Either String [b]
type Transpiler b = [b] -> String

compile :: (Show a, Show b) => Options -> [FilePath] -> Lexer a -> Parser a b
    -> Transpiler b -> IO ()
compile _ [] _ _ _ = exitSuccess
compile opts (x: xs) lexer parser transpiler = do
    content <- readFile x
    if null content
        then eval opts x xs [] lexer parser transpiler
            (x ++ ": " ++ warning ++ ": empty file")
        else case parseFile x content lexer parser of
            Left e -> eval opts x xs content lexer parser transpiler e
            Right (p, (toks, decl)) -> writeFile (p ++ ".bc") (transpiler decl)
                >> postCompile opts p toks decl
                >> compile opts xs lexer parser transpiler

postCompile :: (Show a, Show b) => Options -> FilePath -> [(a, (Int, Int))]
    -> b -> IO ()
postCompile (Options True x) filepath tokens decl
    = putStrLn ("\ESC[1;32mTokens\ESC[0m (" ++ filepath ++ ")\n" ++ show tokens)
    >> postCompile Options {dumpToks = False, dumpAst = x} filepath tokens decl
postCompile (Options x True) filepath tokens decl
    = putStrLn ("\ESC[1;33mAST\ESC[0m (" ++ filepath ++ ")\n" ++ show decl)
    >> postCompile Options {dumpToks = x, dumpAst = False} filepath tokens decl
postCompile _ filepath _ _ = putStrLn $ "(OK) Compiled '" ++ filepath ++ "'"

fixMe :: String
fixMe = " (fixez votre manière d'écrire une erreur parce que y'en a 3 "
    ++ "différentes là, rappel => LIGNE: COLONNE MESSAGE)"

formatParserError :: String -> String -> String
formatParserError content string = case words string of
    [] -> string ++ fixMe
    (x: xs) -> case break (== ':') x of
        (y, z) -> case readMaybe y :: Maybe Int of
            Just l -> case readMaybe $ drop 1 z :: Maybe Int of
                Just c -> fError content (l, c) 1 $ unwords xs
                _ -> string ++ fixMe
            _ -> string ++ fixMe

cestpascompletementoverkilljustepourafficherunmessagederreur :: String
    -> String -> String
cestpascompletementoverkilljustepourafficherunmessagederreur [] _ = []
cestpascompletementoverkilljustepourafficherunmessagederreur string content
    = case findString string "error" of
        Just _ -> string
        _ -> case findString string "warning" of
            Just _ -> string
            _ -> case break (== ':') string of
                (_, []) -> string
                (_, x) -> formatParserError content $ drop 1 x

eval :: (Show a, Show b) => Options -> FilePath -> [FilePath] -> String
    -> Lexer a -> Parser a b -> Transpiler b -> String -> IO ()
eval opts f files content lexer parser transpiler e
    = hPutStrLn stderr (f ++ ":"
    ++ cestpascompletementoverkilljustepourafficherunmessagederreur e content)
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
