{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Interpreter.hs
-}

module Interpreter (
    call
    , open
    , seek
    , stringToList
    , exec
) where

import System.IO (IOMode (..), SeekMode (..), hTell, hFileSize, hIsEOF, hIsOpen, hClose, hGetChar, hGetLine, hPutStr, hPutStrLn)

import Foreign.C (CInt (..))
import Foreign (FunPtr)

import System.Random

import OpCodes (Operand (..), Instruction (..), div, mod)
import Data (Function, Stack, Env, Fds, fetch, jumpTo, popStackN, setEnv, getEnv, purgeEnv)
import Builtins (getFd, gOpen, gSeek, gRead, gWrite)

-- TODO: struct, foreach (issues with bytecode) and global variables

foreign import ccall "stdlib.h &exit"
  p_exit :: FunPtr (CInt -> IO ())

foreign import ccall "dynamic"
  run_exit :: FunPtr (CInt -> IO ()) -> (CInt -> IO ())

call :: String -> [Function] -> Stack -> Env -> Fds -> IO (Either String (Maybe Operand))
call function x _ env fds = case fetch function x of
    Just (_, y, z) -> exec (z, z) x [] (purgeEnv env y) fds
    _ -> return (Left $ "CALL: call to undeclared function " ++ function)

open :: ([Instruction], [Instruction]) -> [Function] -> Stack -> Env -> Fds -> IOMode -> IO (Either String (Maybe Operand))
open x y stack env fds mode = do
    z <- gOpen stack mode
    case z of
        Left e -> return (Left e)
        Right z' -> exec x y (stack ++ [Integer fd]) env (fds ++ [(fd, z')])
        where fd = 1 + maximum (map fst fds)

seek :: ([Instruction], [Instruction]) -> [Function] -> Stack -> Env -> Fds -> SeekMode -> IO (Either String (Maybe Operand))
seek x y stack env fds mode = do
    z <- gSeek stack fds mode
    case z of
        Just e -> return (Left e)
        _ -> exec x y stack env fds

stringToList :: String -> Operand
stringToList [] = List []
stringToList (x: xs) = case stringToList xs of
    List y -> List $ Char x: y
    y -> y

exec :: ([Instruction], [Instruction]) -> [Function] -> Stack -> Env -> Fds -> IO (Either String (Maybe Operand))
exec (Nop: x, y) z stack env fds = exec (x, y) z stack env fds
exec (Call "exit" _: _, _) _ stack _ _ = case popStackN 1 stack of
    Just ([x], _) -> return (Right $ Just x)
    _ -> return (Left "exit: not enough operands")
exec (Call "cexit" _: _, _) _ stack _ _ = case popStackN 1 stack of
    Just ([Char x], _) -> do
        _ <- run_exit p_exit $ fromIntegral (fromEnum x)
        return $ Left []
    Just ([x], _) -> return $ Left ("cexit: expected Char, got " ++ show x)
    _ -> return (Left "cexit: not enough operands")
exec (Call "open_r" _: x, y) z stack env fds
    = open (x, y) z stack env fds ReadMode
exec (Call "open_w" _: x, y) z stack env fds
    = open (x, y) z stack env fds WriteMode
exec (Call "open_a" _: x, y) z stack env fds
    = open (x, y) z stack env fds AppendMode
exec (Call "open_rw" _: x, y) z stack env fds
    = open (x, y) z stack env fds ReadWriteMode
exec (Call "isEOF" _: x, y) z stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "isEOF: " ++ e)
        Right handle -> do
            z' <- hIsEOF handle
            exec (x, y) z (b ++ [Bool z']) env fds
    Just ([z'], _) -> return (Left $ "isEOF: expected Int, got " ++ show z')
    _ -> return (Left "isEOF: not enough operands")
exec (Call "seek_a" _: x, y) z stack env fds
    = seek (x, y) z stack env fds AbsoluteSeek
exec (Call "seek_r" _: x, y) z stack env fds
    = seek (x, y) z stack env fds RelativeSeek
exec (Call "seek_e" _: x, y) z stack env fds
    = seek (x, y) z stack env fds SeekFromEnd
exec (Call "tell" _: x, y) z stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "tell: " ++ e)
        Right handle -> do
            z' <- hTell handle
            exec (x, y) z (b ++ [Integer z']) env fds
    Just ([z'], _) -> return (Left $ "tell: expected Int, got " ++ show z')
    _ -> return (Left "tell: not enough operands")
exec (Call "getFileSize" _: x, y) z stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "getFileSize: " ++ e)
        Right handle -> do
            z' <- hFileSize handle
            exec (x, y) z (b ++ [Integer z']) env fds
    Just ([z'], _)
        -> return (Left $ "getFileSize: expected Int, got " ++ show z')
    _ -> return (Left "getFileSize: not enough operands")
exec (Call "isOpen" _: x, y) z stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "isOpen: " ++ e)
        Right handle -> do
            z' <- hIsOpen handle
            exec (x, y) z (b ++ [Bool z']) env fds
    Just ([z'], _) -> return (Left $ "isOpen: expected Int, got " ++ show z')
    _ -> return (Left "isOpen: not enough operands")
exec (Call "close" _: x, y) z stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "close: " ++ e)
        Right handle -> case hClose handle of
            _ -> case dropWhile (\(c, _) -> c /= a) fds of
                (_: zs) -> exec (x, y) z b env (z' ++ zs)
                _ -> exec (x, y) z b env fds
                where z' = takeWhile (\(c, _) -> c /= a) fds
    Just ([z'], _) -> return $ Left ("close: expected Int, got " ++ show z')
    _ -> return (Left "close: not enough operands")
exec (Call "read" _: x, y) z stack env fds = do
    g <- gRead stack fds hGetChar
    case g of
        Left e -> return (Left e)
        Right (a, stack') -> exec (x, y) z (stack' ++ [Char a]) env fds
exec (Call "readln" _: x, y) z stack env fds = do
    g <- gRead stack fds hGetLine
    case g of
        Left e -> return (Left e)
        Right (a, stack') -> exec (x, y) z (stack' ++ [stringToList a]) env fds
exec (Call "print" _: x, y) z stack env fds = do
    g <- gWrite stack fds hPutStr
    case g of
        Left e -> return (Left e)
        Right stack' -> exec (x, y) z stack' env fds
exec (Call "println" _: x, y) z stack env fds = do
    g <- gWrite stack fds hPutStrLn
    case g of
        Left e -> return (Left e)
        Right stack' -> exec (x, y) z stack' env fds
exec (Call "rand" _: x, y) z stack env fds = do
    g <- newStdGen
    case popStackN 2 stack of
        Just ([Bool a, Bool b], z')
            -> exec (x, y) z (z' ++ [Bool $ head (randomRs (a, b) g)]) env fds
        Just ([Char a, Char b], z')
            -> exec (x, y) z (z' ++ [Char $ head (randomRs (a, b) g)]) env fds
        Just ([Integer a, Integer b], z') -> exec
            (x, y) z (z' ++ [Integer $ head (randomRs (a, b) g)]) env fds
        _ -> return (Left "rand: expected Int, Int")
exec (Call "length" _: x, y) z stack env fds = case popStackN 1 stack of
    Just ([List a], b)
        -> exec (x, y) z (b ++ [Integer $ fromIntegral (length a)]) env fds
    Just ([z'], _) -> return (Left $ "length: expected List, got " ++ show z')
    _ -> return (Left "length: not enough operands")
exec (Call function argv: x, y) z stack env fds = do
    operand <- call function z stack env fds
    case operand of
        Left e -> return (Left e)
        Right (Just a) -> exec (x, y) z (stack ++ [a]) env fds
        Right Nothing -> exec (x, y) z stack env fds
exec (Load identifier: x, y) z stack env fds = case getEnv env identifier of
    Nothing -> return (Left $ "LOAD " ++ identifier ++ ": not in env")
    Just (_, a) -> exec (x, y) z (stack ++ [a]) env fds
exec (Ind: x, y) z stack env fds = case popStackN 2 stack of
    Just ([List a, Integer b], stack') -> if fromIntegral b > length a
        then return (Left $ "IND: index out of bound: " ++ show b)
        else exec (x, y) z (stack' ++ [a !! fromIntegral b]) env fds
    Just ([_, _], _) -> return (Left "IND: expected List, Int")
    _ -> return (Left "IND: empty stack")
exec (Store identifier: x, y) z stack env fds = case popStackN 1 stack of
    Just ([a], stack') -> exec (x, y) z stack' (setEnv env (identifier, a)) fds
    _ -> return (Left "STORE: empty stack")
exec (Push a: x, y) z stack env fds = exec (x, y) z (stack ++ [a]) env fds
exec (PushList a: x, y) z stack env fds = if length stack < a
    then return (Left "PUSH_LIST: not enough operands on stack")
    else exec (x, y) z (take c stack ++ [List $ drop c stack]) env fds
    where c = length stack - a
exec (Pop: x, y) z [] env fds = exec (x, y) z [] env fds
exec (Pop: x, y) z stack env fds = exec (x, y) z (init stack) env fds
exec (Jump a: _, y) z stack env fds = case jumpTo a y of
    Nothing -> return (Left $ "JMP: undeclared label: " ++ a)
    Just x' -> exec (x', y) z stack env fds
exec (JumpFalse _: _, _) _ [] _ _ = return $ Left "JMP_IF_FALSE: empty stack"
exec (JumpFalse a: x, y) z stack env fds = case last stack of
    Bool False ->  case jumpTo a y of
        Nothing -> return (Left $ "JMP_IF_FALSE: undeclared label: " ++ a)
        Just x' -> exec (x', y) z (init stack) env fds
    Bool True -> exec (x, y) z (init stack) env fds
    _ -> return (Left "JMP_IF_FALSE: non-boolean comparison")
exec (JumpTrue _: _, _) _ [] _ _ = return $ Left "JMP_IF_FALSE: empty stack"
exec (JumpTrue a: x, y) z stack env fds = case last stack of
    Bool True ->  case jumpTo a y of
        Nothing -> return (Left $ "JMP_IF_FALSE: undeclared label: " ++ a)
        Just x' -> exec (x', y) z (init stack) env fds
    Bool False -> exec (x, y) z (init stack) env fds
    _ -> return (Left "JMP_IF_FALSE: non-boolean comparison")
exec (Label _: x, y) z stack env fds = exec (x, y) z stack env fds
exec (Add: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct {}, _], _) -> return (Left "ADD: non numeric values")
    Just ([_, Struct {}], _) -> return (Left "ADD: non numeric values")
    Just ([List a, List b], c) -> exec (x, y) z (c ++ [List (a ++ b)]) env fds
    Just ([List a, b], c) -> exec (x, y) z (c ++ [List (a ++ [b])]) env fds
    Just ([a, List b], c) -> exec (x, y) z (c ++ [List (a: b)]) env fds
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [a + b]) env fds
    _ -> return (Left "ADD: not enough values on stack")
exec (Sub: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct {}, _], _) -> return (Left "SUB: non numeric values")
    Just ([List _, _], _) -> return (Left "SUB: non numeric values")
    Just ([_, Struct {}], _) -> return (Left "SUB: non numeric values")
    Just ([_, List _], _) -> return (Left "SUB: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [a - b]) env fds
    _ -> return (Left "SUB: not enough values on stack")
exec (Mul: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct {}, _], _) -> return (Left "MUL: non numeric values")
    Just ([List _, _], _) -> return (Left "MUL: non numeric values")
    Just ([_, Struct {}], _) -> return (Left "MUL: non numeric values")
    Just ([_, List _], _) -> return (Left "MUL: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [a * b]) env fds
    _ -> return (Left "MUL: not enough values on stack")
exec (Div: x, y) z stack env fds = case popStackN 2 stack of
    Just ([a, b], c) -> if b == Bool False || b == Integer 0 || b == Float 0
        then return (Left "DIV: 0 division")
        else case OpCodes.div a b of
            Left e -> return $ Left e
            Right op -> exec (x, y) z (c ++ [Float op]) env fds
    _ -> return (Left "DIV: not enough values on stack")
exec (Mod: x, y) z stack env fds = case popStackN 2 stack of
    Just ([a, b], c) -> if b == Bool False || b == Integer 0 || b == Float 0
        then return (Left "MOD: 0 modulo")
        else case OpCodes.mod a b of
            Left e -> return $ Left e
            Right op -> exec (x, y) z (c ++ [Integer op]) env fds
    _ -> return (Left "MOD: not enough values on stack")
exec (Lt: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct {}, _], _) -> return (Left "LT: non numeric values")
    Just ([List _, _], _) -> return (Left "LT: non numeric values")
    Just ([_, Struct {}], _) -> return (Left "LT: non numeric values")
    Just ([_, List _], _) -> return (Left "LT: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [Bool (a < b)]) env fds
    _ -> return (Left "LT: not enough values on stack")
exec (Gt: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct {}, _], _) -> return (Left "GT: non numeric values")
    Just ([List _, _], _) -> return (Left "GT: non numeric values")
    Just ([_, Struct {}], _) -> return (Left "GT: non numeric values")
    Just ([_, List _], _) -> return (Left "GT: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [Bool (a > b)]) env fds
    _ -> return (Left "GT: not enough values on stack")
exec (LEq: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct {}, _], _) -> return (Left "LEQ: non numeric values")
    Just ([List _, _], _) -> return (Left "LEQ: non numeric values")
    Just ([_, Struct {}], _) -> return (Left "LEQ: non numeric values")
    Just ([_, List _], _) -> return (Left "LEQ: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [Bool (a <= b)]) env fds
    _ -> return (Left "LEQ: not enough values on stack")
exec (GEq: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct {}, _], _) -> return (Left "GEQ: non numeric values")
    Just ([List _, _], _) -> return (Left "GEQ: non numeric values")
    Just ([_, Struct {}], _) -> return (Left "GEQ: non numeric values")
    Just ([_, List _], _) -> return (Left "GEQ: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [Bool (a >= b)]) env fds
    _ -> return (Left "GEQ: not enough values on stack")
exec (Eq: x, y) z stack env fds = case popStackN 2 stack of
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [Bool (a == b)]) env fds
    _ -> return (Left "EQ: not enough values on stack")
exec (NEq: x, y) z stack env fds = case popStackN 2 stack of
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [Bool (a /= b)]) env fds
    _ -> return (Left "EQ: not enough values on stack")
exec (And: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct {}, _], _) -> return (Left "AND: non numeric values")
    Just ([List _, _], _) -> return (Left "AND: non numeric values")
    Just ([_, Struct {}], _) -> return (Left "AND: non numeric values")
    Just ([_, List _], _) -> return (Left "AND: non numeric values")
    Just ([a, b], c) -> exec (x, y) z (c ++ [Bool (a /= 0 && b /= 0)]) env fds
    _ -> return $ Left "AND: non-boolean comparison"
exec (Or: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct {}, _], _) -> return (Left "OR: non numeric values")
    Just ([List _, _], _) -> return (Left "OR: non numeric values")
    Just ([_, Struct {}], _) -> return (Left "OR: non numeric values")
    Just ([_, List _], _) -> return (Left "OR: non numeric values")
    Just ([a, b], c) -> exec (x, y) z (c ++ [Bool (a /= 0 || b /= 0)]) env fds
    _ -> return $ Left "OR: non-boolean comparison"
exec (Ret: _, _) _ [] _ _ = return (Right Nothing)
exec (Ret: _, _) _ stack _ _ = return (Right $ Just (last stack))
exec ([], _) _ _ _ _ = return (Right Nothing)
