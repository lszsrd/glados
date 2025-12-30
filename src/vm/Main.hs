{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Main.hs
-}

module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)

import Data.List (isPrefixOf)
import Data.Char (isSpace)

-- files should start with a magic byte to identify as glados compiled byte code
-- fecth decode execute

data Instruction
    = Nop
    | Func
    | EndFunc
    | Call
    | Load
    | Store
    | PushBool
    | PushChar
    | PushInt
    | PushFloat
    | Pop
    | JumpFalse
    | JumpTrue
    | Label
    | Mul
    | Add
    | Sub
    | Div
    | Mod
    | Lt
    | Gt
    | LEq
    | GEq
    | Eq
    | NEq
    | And
    | Or
    | Ret

    deriving (
        Show
        , Eq
    )

fetch :: String -> Maybe (Instruction, String)
fetch x
    | "NOP" `isPrefixOf` x = Just (Nop, takeWhile (/= '\n') x)
    | "FUNC " `isPrefixOf` x = Just (Func, takeWhile (/= '\n') (dropWhile isSpace (drop 5 x)))
    | "ENDFUNC" `isPrefixOf` x = Just (EndFunc, takeWhile (/= '\n') x)
    | "CALL " `isPrefixOf` x = Just (Call, takeWhile (/= '\n') (dropWhile isSpace (drop 5 x)))
    | "LOAD " `isPrefixOf` x = Just (Load, takeWhile (/= '\n') (dropWhile isSpace (drop 5 x)))
    | "STORE " `isPrefixOf` x = Just (Store, takeWhile (/= '\n') (drop 6 (dropWhile isSpace x)))
    | "PUSH_BOOL " `isPrefixOf` x = Just (PushBool, takeWhile (/= '\n') (dropWhile isSpace (drop 10 x)))
    | "PUSH_CHAR " `isPrefixOf` x = Just (PushChar, takeWhile (/= '\n') (dropWhile isSpace (drop 10 x)))
    | "PUSH_INT " `isPrefixOf` x = Just (PushInt, takeWhile (/= '\n') (dropWhile isSpace (drop 9 x)))
    | "PUSH_FLOAT " `isPrefixOf` x = Just (PushFloat, takeWhile (/= '\n') (dropWhile isSpace (drop 11 x)))
    | "POP" `isPrefixOf` x = Just (Pop, takeWhile (/= '\n') x)
    | "JMP_IF_FALSE " `isPrefixOf` x = Just (JumpFalse, takeWhile (/= '\n') (dropWhile isSpace (drop 20 x)))
    | "JMP_IF_TRUE " `isPrefixOf` x = Just (JumpFalse, takeWhile (/= '\n') (dropWhile isSpace (drop 19 x)))
    | "LABEL " `isPrefixOf` x = Just (Label, takeWhile (/= '\n') (dropWhile isSpace (drop 6 x)))
    | "MUL" `isPrefixOf` x = Just (Mul, takeWhile (/= '\n') x)
    | "ADD" `isPrefixOf` x = Just (Add, takeWhile (/= '\n') x)
    | "SUB" `isPrefixOf` x = Just (Sub, takeWhile (/= '\n') x)
    | "DIV" `isPrefixOf` x = Just (Div, takeWhile (/= '\n') x)
    | "MOD" `isPrefixOf` x = Just (Mod, takeWhile (/= '\n') x)
    | "LT" `isPrefixOf` x = Just (Lt, takeWhile (/= '\n') x)
    | "GT" `isPrefixOf` x = Just (Gt, takeWhile (/= '\n') x)
    | "LT_EQ" `isPrefixOf` x = Just (LEq, takeWhile (/= '\n') x)
    | "GT_EQ" `isPrefixOf` x = Just (GEq, takeWhile (/= '\n') x)
    | "EQ" `isPrefixOf` x = Just (Eq, takeWhile (/= '\n') x)
    | "NOT_EQ" `isPrefixOf` x = Just (NEq, takeWhile (/= '\n') x)
    | "AND" `isPrefixOf` x = Just (Eq, takeWhile (/= '\n') x)
    | "OR" `isPrefixOf` x = Just (And, takeWhile (/= '\n') x)
    | "RET" `isPrefixOf` x = Just (Ret, takeWhile (/= '\n') x)
    | otherwise = Nothing

parseFile :: [String] -> Either String [(Instruction, String)]
parseFile [] = Right []
parseFile ([]: x) = parseFile x
parseFile (line@(x: xs): y)
    | isSpace x = parseFile (xs: y)
    | otherwise = case fetch line of
        Nothing -> Left ("error: " ++ line)
        Just z -> case parseFile y of
            Left e -> Left e
            Right z' -> Right $ z: z'

main :: IO ()
main = do
    path <- getArgs
    file <- readFile $ head path
    case parseFile (lines file) of
        Left e -> hPutStrLn stderr e >> exitFailure
        Right x -> print x
