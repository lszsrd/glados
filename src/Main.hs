module Main where

import System.Environment (getArgs)
<<<<<<< HEAD
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

import Lexer
import Parser
import Format (fError)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)

findString :: String -> String -> Maybe String
findString [] _ = Nothing
findString string@(_: x) needle
    | needle `isPrefixOf` string = Just string
    | otherwise = findString x needle

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

format :: String
    -> String -> String
format [] _ = []
format string content
    = case findString string "error" of
        Just _ -> string
        _ -> case findString string "warning" of
            Just _ -> string
            _ -> case break (== ':') string of
                (_, []) -> string
                (_, x) -> formatParserError content $ drop 1 x
=======
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))

import Lexer (lexer)
import Parser (getAST)
import Interpretor (interpret)
import AbstractTree
import Control.Exception (try, SomeException, evaluate)

-- get the buffer, if args, then try file, else read stdin
getBuffer :: [String] -> IO String
getBuffer [] = getContents
getBuffer ["-h"] = putStrLn
    ("GLaDOS: Generic Language and Data Operand Syntax.\n"
    ++ "\nusage:\n\t./glados <File> or ./glados < <File>\n"
    ++ "\nProgram takes as input a File or read content from stdin.\n"
    ++ "It then tries to Interpret it as Lisp-like language.\n\nExample:\n  "
    ++ "echo -e \"((lambda (foo bar) (+ foo bar) 2 4)\" | ./glados\n  >> 6")
    >> exitSuccess
getBuffer [x] = do
    res <- try (readFile x)
        :: IO (Either SomeException String)
    case res of
        Left e -> print e >> exitWith (ExitFailure 84)
        Right content -> return content

printResult :: Maybe Ast -> IO ()
printResult (Just (Expression a)) =
    case a of
        (Boolean True) -> putStrLn "#t"
        (Boolean False) -> putStrLn "#f"
        (Int a) -> print a
        _ -> putStrLn "#\\<procedure\\>"
printResult _ = putStrLn "Error happened: sorry not sorry"
>>>>>>> e65e5aa1daf8eb8db5e1284194cec20bc09c513b

main :: IO ()
main = do
<<<<<<< HEAD
    x <- head <$> getArgs
    buffer <- readFile x
    case lexer buffer of
        Left e -> hPutStrLn stderr (x ++ ":" ++ e) >> exitFailure
        Right tokens -> case parser tokens of
            Left e -> hPutStrLn stderr (x ++ ":" ++ format (x ++ ":" ++ e) buffer)
                >> exitFailure
            Right ast -> print ast
=======
    args <- getArgs
    buffer <- getBuffer args
    let tokenList = lexer buffer
    ast <- getAST tokenList
    res <- try (evaluate (interpret ast []))
        :: IO (Either SomeException (Maybe Ast))
    case res of
        Left e -> print e >> exitWith (ExitFailure 84)
        Right content -> printResult content >> exitSuccess
>>>>>>> e65e5aa1daf8eb8db5e1284194cec20bc09c513b
