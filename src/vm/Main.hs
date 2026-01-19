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
import Parser (parser)
import Interpreter (call)

{- fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- also check that no structs share the same name
searchMD :: [[String]] -> String -> Maybe String
searchMD [x@(_: xs), ys] y = case dropWhile (/= y) x of
    [] -> searchMD [xs, ys] y
    _ -> Just ("multiple definition of function " ++ y)
searchMD [[], y: xs] _ = searchMD [xs, xs] y
searchMD _ _ = Nothing -}

run :: String -> IO ()
run x = case parser $ lines x of
    Left e -> hPutStrLn stderr e >> exitFailure
    Right (a, b) -> do
        y <- call "@init" (a, b) [] [] [(0, stdin), (1, stdout), (2, stderr)]
        case y of
            Left e -> hPutStrLn stderr e >> exitFailure
            Right Nothing -> return ()
            Right (Just z) -> putStrLn $ "## exit code " ++ show z ++ " ##"

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
