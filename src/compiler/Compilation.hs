{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Compilation.hs
-}

module Compilation (
    Lexer
    , Parser

    , compileFiles
    , parseFile
    , processError
) where

import System.IO (stderr, hPutStrLn)
import System.Exit (exitSuccess, exitFailure)

import Utils (findString)

import Format (warning)

type Lexer a = String -> Either String [(a, (Int, Int))]
type Parser a b = [(a, (Int, Int))] -> Either String [b]

compileFiles :: Lexer a -> Parser a b -> [FilePath] -> IO ()
compileFiles _ _ [] = exitSuccess
compileFiles l p (x: xs) = do
    content <- readFile x
    if null content
        then processError l p (x ++ ": " ++ warning ++ ": empty file") xs
        else case parseFile l p x content of
            Left e -> processError l p e xs
            -- remplacer les double quote par la fonction de ast -> byte code
            Right (filepath, decl) -> writeFile filepath ""
                >> putStrLn ("Compiled file " ++ filepath)
                >> compileFiles l p xs

parseFile :: Lexer a -> Parser a b -> FilePath -> String
    -> Either String (FilePath, [b])
parseFile l p filepath content  = case l content of
    Left e -> Left $ filepath ++ ":" ++ e
    Right tokens -> case p tokens of
        Left e -> Left $ filepath ++ ":" ++ e
        Right decl -> if null decl
            then Left $ filepath ++ ": " ++ warning ++ ": "
                ++ "file does not contain any code"
            else Right (filepath, decl)

processError :: Lexer a -> Parser a b -> String -> [FilePath] -> IO ()
processError l p e files = hPutStrLn stderr e
    >> case findString e "warning" of
        Nothing -> exitFailure
        _ -> compileFiles l p files
