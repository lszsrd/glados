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
import System.FilePath (replaceExtension)

import Compiler (findString)
import CompilerBytecode (compileDecl)
import Ast
import Format (warning)

type Lexer a = String -> Either String [(a, (Int, Int))]
type Parser a b = [(a, (Int, Int))] -> Either String [b]

-- Bytecode generation

-- Compile a full program (list of Decl) into bytecode
-- Each declaration is compiled independently and concatenated
compileProgram :: [Decl] -> String
compileProgram = concatMap compileDecl

-- File compilation

compileFiles :: Lexer a -> Parser a Decl -> [FilePath] -> IO ()
compileFiles _ _ [] = exitSuccess
compileFiles l p (x:xs) = do
    content <- readFile x
    if null content
        then processError l p (x ++ ": " ++ warning ++ ": empty file") xs
        else case parseFile l p x content of
            Left e -> processError l p e xs
            Right (filepath, decls) ->
                let bytecode = compileProgram decls
                    outFile  = replaceExtension filepath ".bc"
                in writeFile outFile bytecode
                    >> putStrLn ("Compiled file " ++ outFile)
                    >> compileFiles l p xs

-- Parsing

parseFile:: Lexer a -> Parser a Decl -> FilePath -> String -> Either String (FilePath, [Decl])
parseFile l p filepath content =
    case l content of
        Left e -> Left (filepath ++ ":" ++ e)
        Right tokens ->
            case p tokens of
                Left e -> Left (filepath ++ ":" ++ e)
                Right decls ->
                    if null decls
                        then Left (filepath ++ ": " ++ warning ++ ": file does not contain any code")
                        else Right (filepath, decls)

-- Error handling

processError
    :: Lexer a
    -> Parser a Decl
    -> String
    -> [FilePath]
    -> IO ()
processError l p e files =
    hPutStrLn stderr e >>
    case findString e "warning" of
        Nothing -> exitFailure
        _       -> compileFiles l p files
