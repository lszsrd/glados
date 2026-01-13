{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Compiler.hs
-}

module Compiler (
    compile
    , hCompileFile
    , compileFile
    , evalError
    , wrapErrorMessage
    , formatParserError
    , findString
    , postCompile
) where

import System.IO (stderr, hPutStrLn)
import System.FilePath (takeExtension)
import System.Exit (exitSuccess, exitFailure)

import Control.Applicative ((<|>))

import Data.List (isPrefixOf)
import Text.Read (readMaybe)

import Lisp.Lexer (lexer)
import Lisp.Parser (parser)
import Lisp.Bytecode (compileDecl)

import Rizz.Lexer (lexer)
import Rizz.Parser (parser)
import Rizz.Bytecode (compileDecl)

import Format (warning, fError)

import Options (Options (..))

type Lexer a = String -> Either String [(a, (Int, Int))]
type Parser a b = [(a, (Int, Int))] -> Either String [b]
type Transpiler b = [b] -> String

compile :: Options -> [FilePath] -> IO ()
compile _ [] = exitSuccess
compile x (y: ys) = do
    z <- readFile y
    if null z
        then hPutStrLn stderr (y ++ ": " ++ warning ++ ": empty file")
            >> compile x ys
        else hCompileFile x y z >> compile x ys

hCompileFile :: Options -> FilePath -> String -> IO ()
hCompileFile x y z
    | takeExtension y == ".scm" = compileFile x y z
        Lisp.Lexer.lexer Lisp.Parser.parser Lisp.Bytecode.compileDecl
    | takeExtension y == ".rz" = compileFile x y z
        Rizz.Lexer.lexer Rizz.Parser.parser Rizz.Bytecode.compileDecl
    | otherwise = hPutStrLn stderr
        (y ++ ": " ++ Format.warning ++ ": unknown file type")

compileFile :: (Show a, Show b) => Options -> FilePath -> String -> Lexer a -> Parser a b -> Transpiler b -> IO ()
compileFile x y z gLexer gParser transpiler = case gLexer z of
    Left e -> evalError y z e
    Right tokens -> case gParser tokens of
        Left e -> evalError y z (y ++ ": " ++ e)
        Right ast -> if null ast
            then hPutStrLn stderr
                (y ++ ": " ++ warning ++ ": no compilation unit")
            else postCompile x y tokens ast
                >> writeFile (y ++ ".bc") (transpiler ast)

evalError :: FilePath -> String -> String -> IO ()
evalError x y z = hPutStrLn stderr (x ++ ":" ++ wrapErrorMessage z y)
    >> case findString z "warning" of
        Nothing -> exitFailure
        _ -> return ()

wrapErrorMessage :: String -> String -> String
wrapErrorMessage [] _ = []
wrapErrorMessage x y
    = case findString x "error" <|> findString x "warning" of
        Just _ -> x
        _ -> case break (== ':') x of
            (_, []) -> x
            (_, z) -> formatParserError y $ drop 1 z

formatParserError :: String -> String -> String
formatParserError content string = case words string of
    [] -> string ++ " (could not parse error message)"
    (x: xs) -> case break (== ':') x of
        (y, z) -> case readMaybe y :: Maybe Int of
            Just l -> case readMaybe $ drop 1 z :: Maybe Int of
                Just c -> fError content (l, c) 1 $ unwords xs
                _ -> string ++ " (could not parse error message)"
            _ -> string ++ " (could not parse error message)"

findString :: String -> String -> Maybe String
findString [] _ = Nothing
findString string@(_: x) needle
    | needle `isPrefixOf` string = Just string
    | otherwise = findString x needle

postCompile :: (Show a, Show b) => Options -> FilePath -> [(a, (Int, Int))] -> b -> IO ()
postCompile (Options True x) y z z'
    = putStrLn ("\ESC[1;32mTokens\ESC[0m (" ++ y ++ ")\n" ++ show z)
    >> postCompile Options {dumpToks = False, dumpAst = x} y z z'
postCompile (Options x True) y z z'
    = putStrLn ("\ESC[1;33mAST\ESC[0m (" ++ y ++ ")\n" ++ show z')
    >> postCompile Options {dumpToks = x, dumpAst = False} y z z'
postCompile _ x _ _ = putStrLn $ "(OK) Compiled '" ++ x ++ "'"
