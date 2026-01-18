{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Main.hs
-}

module Main where

import System.Environment (getProgName, getArgs)
import System.IO (stdin, stdout, stderr, hPutStrLn)
import System.Exit (exitFailure)

import Data.List (isPrefixOf, group, sort)

import ParseArgs (parseArgs, printUsage)
import Parser (parseFunctions)
import Interpreter (call)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- also check that no structs share the same name
searchFunctionMD :: [[String]] -> String -> Maybe String
searchFunctionMD [x@(_: xs), ys] y = case dropWhile (/= y) x of
    [] -> searchFunctionMD [xs, ys] y
    _ -> Just ("multiple definition of function " ++ y)
searchFunctionMD [[], y: xs] _ = searchFunctionMD [xs, xs] y
searchFunctionMD _ _ = Nothing

run :: String -> IO ()
run x = case parseFunctions $ lines x of
    Left e -> hPutStrLn stderr e >> exitFailure
    Right (y, z) -> case searchFunctionMD (replicate 2 (fst3 $ unzip3 y)) [] of
        Just e -> hPutStrLn stderr e >> exitFailure
        Nothing -> do
            a <- call "main" (y, z) [] [] [(0, stdin), (1, stdout), (2, stderr)]
            case a of
                Left e -> hPutStrLn stderr e >> exitFailure
                Right Nothing -> return ()
                Right (Just b) -> putStrLn $ "## exit code " ++ show b ++ " ##"

main :: IO ()
main = do
    progName <- getProgName
    arguments <- parseArgs <$> getArgs
    case arguments of
        Left e -> if "USAGE" `isPrefixOf` e
            then printUsage progName
            else hPutStrLn stderr (progName ++ e) >> exitFailure
        Right files -> do
            files' <- mapM (readFile . head) (group . sort $ files)
            run (concat files')
