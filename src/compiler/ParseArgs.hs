{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/ParseArgs.hs
-}

module ParseArgs (
    parseArgs
    , printUsage
) where

import System.FilePath (takeExtension)

import Format (error)

import Options (Options (..))

-- TODO: [(FilePath, lexer, parser, compiler)]
parseArgs :: [String] -> Either String (Options, [FilePath])
parseArgs ("--dump-ast": x) = case parseArgs x of
    Left e -> Left e
    Right (Options y _, []) -> Right (option y, [])
    Right (Options y _, z) -> Right (option y, z)
    where
        option y = (Options {dumpToks = y, dumpAst = True})
parseArgs ("--dump-tokens": x) = case parseArgs x of
    Left e -> Left e
    Right (Options _ y, []) -> Right (option y, [])
    Right (Options _ y, z) -> Right (option y, z)
    where
        option y = (Options {dumpToks = True, dumpAst = y})
parseArgs (('-': _): _) = Left "USAGE"
parseArgs (x: xs)
    | takeExtension x /= ".rz" && takeExtension x /= ".scm" = Left
        $ ": " ++ Format.error ++ ": " ++ x ++ ": unknown file type"
    | null xs = Right (Options {dumpToks = False, dumpAst = False}, [x])
    | otherwise = case parseArgs xs of
        Left e -> Left e
        Right (z, ys) -> Right (z, x: ys)
parseArgs [] = Left $ ": " ++ Format.error ++ ": no input files"

printUsage :: String -> IO ()
printUsage x = putStrLn $
    "GLaDOS project (compiler part) - Translate one or more source"
    ++ " (code) file(s) to custom bytecode file(s).\n\n"
    ++ "\ESC[1;33mUSAGE\ESC[0m: " ++ x ++ " [--dump-ast] [--dump-tokens] <sour"
    ++ "ce files (.rizz, .scm)>\n\nGenerated files are built from source file'"
    ++ "s path and a custom \".bc\" extension is appened.\n\nByte code files "
    ++ "are NOT object files which mean that they do not link to any object "
    ++ "linker (i.e. ld on linux).\nTo use compiled files, you need to pass "
    ++ "them to the GLaDOS interpreter (glados-vm binary)."
