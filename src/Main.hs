module Main where

import System.Environment (getArgs)
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

main :: IO ()
main = do
    x <- head <$> getArgs
    buffer <- readFile x
    case lexer buffer of
        Left e -> hPutStrLn stderr (x ++ ":" ++ e) >> exitFailure
        Right tokens -> case parser tokens of
            Left e -> hPutStrLn stderr (x ++ ":" ++ format (x ++ ":" ++ e) buffer)
                >> exitFailure
            Right ast -> print ast
