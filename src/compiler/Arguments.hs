{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Arguments.hs
-}

module Arguments (
    Options                 (..)

    , parseArgs
    , printUsage
) where

import System.FilePath (takeExtension)

import Format (error)

data Options = Options  {
    dumpToks :: Bool
    , dumpAst :: Bool
                        }

    deriving (
        Show
        , Eq
    )

parseArgs :: String -> [String] -> Either String (Options, [FilePath])
parseArgs _ [] = Left $ ": " ++ Format.error ++ ": no input files"
parseArgs extension ("--dump-tokens": x) = case parseArgs extension x of
    Left e -> Left e
    Right (Options y _, z) -> Right (Options {dumpToks = True, dumpAst = y}, z)
parseArgs extension ("--dump-ast": x) = case parseArgs extension x of
    Left e -> Left e
    Right (Options _ y, z) -> Right (Options {dumpToks = y, dumpAst = True}, z)
parseArgs _ (('-': _): _) = Left "USAGE"
parseArgs extension (x: xs)
    | takeExtension x /= extension
        = Left $ ": " ++ Format.error ++ ": " ++ x ++ ": unknown file type"
    | null xs = Right (Options {dumpToks = False, dumpAst = False}, [x])
    | otherwise = case parseArgs extension xs of
        Left e -> Left e
        Right (opt, files) -> Right (opt, x: files)

printUsage :: String -> String -> IO ()
printUsage x y = putStrLn $
    "GLaDOS project (compiler part) - Translate one or more source"
    ++ " (code) file(s) to custom bytecode file(s).\n\n"
    ++ "\ESC[1;33mUSAGE\ESC[0m: " ++ y ++ " [--dump-ast] [--dump-tokens] <sour"
    ++ "ce files (" ++ x ++ ")>\n\nGenerated files are built from source file'"
    ++ "s path and a custom \".bc\" extension is appened.\n\nByte code files "
    ++ "are NOT object files which mean that they do not link to any object "
    ++ "linker (i.e. ld on linux).\nTo use compiled files, you need to pass "
    ++ "them to the GLaDOS interpreter (glados-interpreter binary)."
