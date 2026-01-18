{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Interpreter.hs
-}

module Interpreter (
    call
    , open
    , builtin
    , exec
) where

import System.IO    (
    IOMode (ReadMode, WriteMode, AppendMode, ReadWriteMode),
    SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd),
    hTell,
    hFileSize,
    hIsEOF,
    hIsOpen,
    hClose,

    hPutStr,
    hPrint,
    hGetChar,
    hGetLine
                    )

import Foreign (FunPtr)
import Foreign.C (CInt (..))

import System.Random

import OpCodes (Operand (..), Instruction (..), div, mod, showList')
import Data (Function, Stack, Env, Fds, fetch, jumpTo, popStackN, setEnv, getEnv, purgeEnv)
import Builtins (getFd, gOpen, gSeek)

foreign import ccall "stdlib.h &exit"
  p_exit:: FunPtr (CInt -> IO ())

foreign import ccall "dynamic"
  run_exit:: FunPtr (CInt -> IO ()) -> (CInt -> IO ())



{- stringToList :: String -> Operand
stringToList [] = List []
stringToList (x: xs) = case stringToList xs of
    List y -> List $ Char x: y
    y -> y

 -}


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

builtin :: String -> ([Instruction], [Instruction]) -> [Function] -> Stack -> Env -> Fds -> IO (Either String (Maybe Operand))
builtin "exit" _ _ stack _ _ = case popStackN 1 stack of
    Just ([x], _) -> return (Right $ Just x)
    _ -> return (Left "exit: not enough operands")
builtin "cexit" _ _ stack _ _ = case popStackN 1 stack of
    Just ([Char x], _) -> do
        _ <- run_exit p_exit $ fromIntegral (fromEnum x)
        return $ Left []
    Just ([x], _) -> return $ Left ("cexit: expected Char, got " ++ show x)
    _ -> return (Left "cexit: not enough operands")
builtin "open_r" x y stack env fds = open x y stack env fds ReadMode
builtin "open_w" x y stack env fds = open x y stack env fds WriteMode
builtin "open_a" x y stack env fds = open x y stack env fds AppendMode
builtin "open_rw" x y stack env fds = open x y stack env fds ReadWriteMode
builtin "isEOF" x y stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "isEOF: " ++ e)
        Right handle -> do
            z <- hIsEOF handle
            exec x y (b ++ [Bool z] ) env fds
    Just ([z], _) -> return (Left $ "isEOF: expected Int, got " ++ show z)
    _ -> return (Left "isEOF: not enough operands")
builtin "seek_a" x y stack env fds = seek x y  stack env fds AbsoluteSeek
builtin "seek_r" x y stack env fds = seek x y stack env fds RelativeSeek
builtin "seek_e" x y stack env fds = seek x y stack env fds SeekFromEnd
builtin "tell" x y stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "tell: " ++ e)
        Right handle -> do
            z <- hTell handle
            exec x y (b ++ [Integer z] ) env fds
    Just ([z], _) -> return (Left $ "tell: expected Int, got " ++ show z)
    _ -> return (Left "tell: not enough operands")
builtin "getFileSize" x y stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "getFileSize: " ++ e)
        Right handle -> do
            z <- hFileSize handle
            exec x y (b ++ [Integer z] ) env fds
    Just ([z], _) -> return (Left $ "getFileSize: expected Int, got " ++ show z)
    _ -> return (Left "getFileSize: not enough operands")
builtin "isOpen" x y stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "isOpen: " ++ e)
        Right handle -> do
            z <- hIsOpen handle
            exec x y (b ++ [Bool z] ) env fds
    Just ([z], _) -> return (Left $ "isOpen: expected Int, got " ++ show z)
    _ -> return (Left "isOpen: not enough operands")
builtin "close" x y stack env fds = case popStackN 1 stack of
    Just ([Integer a], b) -> case getFd a fds of
        Left e -> return (Left $ "close: " ++ e)
        Right handle -> case hClose handle of
            _ -> case dropWhile (\(c, _) -> c /= a) fds of
                (_: zs) -> exec x y b env (z ++ zs)
                _ -> exec x y b env fds
                where z = takeWhile (\(c, _) -> c /= a) fds
    Just ([z], _) -> return $ Left ("close: expected Int, got " ++ show z)
    _ -> return (Left "close: not enough operands")

builtin "rand" x y stack env fds = do
    g <- newStdGen
    case popStackN 2 stack of
        Just ([Bool a, Bool b], z)
            -> exec x y (z ++ [Bool $ head (randomRs (a, b) g)]) env fds
        Just ([Char a, Char b], z)
            -> exec x y (z ++ [Char$ head (randomRs (a, b) g)]) env fds
        Just ([Integer a, Integer b], z)
            -> exec x y (z ++ [Integer $ head (randomRs (a, b) g)]) env fds
        _ -> return (Left "rand: expected Int, Int")
builtin x _ _ _ _ _ = return (Left $ "CALL: call to undeclared function " ++ x)

exec :: ([Instruction], [Instruction]) -> [Function] -> Stack -> Env -> Fds -> IO (Either String (Maybe Operand))
exec (Nop: x, y) z stack env fds = exec (x, y) z stack env fds
exec (Call function argc: x, y) z stack env fds = do
    operand <- call function z stack env fds
    case operand of
        Left _ -> builtin function (x, y) z stack env fds
        Right Nothing -> exec (x, y) z stack' env fds
        Right (Just a) -> exec (x, y) z (stack' ++ [a]) env fds
        where stack' = take (length stack - argc) stack
exec (Load identifier: x, y) z stack env fds = case getEnv env identifier of
    Nothing -> return (Left $ "LOAD " ++ identifier ++ ": not in env")
    Just (_, a) -> exec (x, y) z (stack ++ [a]) env fds
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
    Just ([Struct _, _], _) -> return (Left "ADD: non numeric values")
    Just ([_, Struct _], _) -> return (Left "ADD: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [a + b]) env fds
    _ -> return (Left "ADD: not enough values on stack")
exec (Sub: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct _, _], _) -> return (Left "SUB: non numeric values")
    Just ([List _, _], _) -> return (Left "SUB: non numeric values")
    Just ([_, Struct _], _) -> return (Left "SUB: non numeric values")
    Just ([_, List _], _) -> return (Left "SUB: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [a - b]) env fds
    _ -> return (Left "SUB: not enough values on stack")
exec (Mul: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct _, _], _) -> return (Left "MUL: non numeric values")
    Just ([List _, _], _) -> return (Left "MUL: non numeric values")
    Just ([_, Struct _], _) -> return (Left "MUL: non numeric values")
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
    Just ([Struct _, _], _) -> return (Left "LT: non numeric values")
    Just ([List _, _], _) -> return (Left "LT: non numeric values")
    Just ([_, Struct _], _) -> return (Left "LT: non numeric values")
    Just ([_, List _], _) -> return (Left "LT: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [Bool (a < b)]) env fds
    _ -> return (Left "LT: not enough values on stack")
exec (Gt: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct _, _], _) -> return (Left "GT: non numeric values")
    Just ([List _, _], _) -> return (Left "GT: non numeric values")
    Just ([_, Struct _], _) -> return (Left "GT: non numeric values")
    Just ([_, List _], _) -> return (Left "GT: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [Bool (a > b)]) env fds
    _ -> return (Left "GT: not enough values on stack")
exec (LEq: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct _, _], _) -> return (Left "LEQ: non numeric values")
    Just ([List _, _], _) -> return (Left "LEQ: non numeric values")
    Just ([_, Struct _], _) -> return (Left "LEQ: non numeric values")
    Just ([_, List _], _) -> return (Left "LEQ: non numeric values")
    Just ([a, b], stack') -> exec (x, y) z (stack' ++ [Bool (a <= b)]) env fds
    _ -> return (Left "LEQ: not enough values on stack")
exec (GEq: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct _, _], _) -> return (Left "GEQ: non numeric values")
    Just ([List _, _], _) -> return (Left "GEQ: non numeric values")
    Just ([_, Struct _], _) -> return (Left "GEQ: non numeric values")
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
    Just ([Struct _, _], _) -> return (Left "AND: non numeric values")
    Just ([List _, _], _) -> return (Left "AND: non numeric values")
    Just ([_, Struct _], _) -> return (Left "AND: non numeric values")
    Just ([_, List _], _) -> return (Left "AND: non numeric values")
    Just ([a, b], c) -> exec (x, y) z (c ++ [Bool (a /= 0 && b /= 0)]) env fds
    _ -> return $ Left "AND: non-boolean comparison"
exec (Or: x, y) z stack env fds = case popStackN 2 stack of
    Just ([Struct _, _], _) -> return (Left "OR: non numeric values")
    Just ([List _, _], _) -> return (Left "OR: non numeric values")
    Just ([_, Struct _], _) -> return (Left "OR: non numeric values")
    Just ([_, List _], _) -> return (Left "OR: non numeric values")
    Just ([a, b], c) -> exec (x, y) z (c ++ [Bool (a /= 0 || b /= 0)]) env fds
    _ -> return $ Left "OR: non-boolean comparison"
exec (Ret: _, _) _ [] _ _ = return (Right Nothing)
exec (Ret: _, _) _ stack _ _ = return (Right $ Just (last stack))
exec ([], _) _ _ _ _ = return (Right Nothing)

{- 
exec symtab bOps (Call "print" _: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("print: " ++ e)
    Right (Integer x, y, z) -> case getFd x fds of
        Left e -> return $ Left ("print: " ++ e)
        Right handle -> hPutStr handle (show y) >> hFlush handle
            >> exec symtab bOps ops z env fds
    Right (x, _, _) -> return $ Left ("print: expected fd, got " ++ show x)
exec symtab bOps (Call "println" _: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("println: " ++ e)
    Right (Integer x, y, z) -> case getFd x fds of
        Left e -> return $ Left ("println: " ++ e)
        Right handle -> hPrint handle y >> hFlush handle
            >> exec symtab bOps ops z env fds
    Right (x, _, _) -> return $ Left ("println: expected fd, got " ++ show x)
exec symtab bOps (Call "read" _: ops) stack env fds = case unsnoc stack of
    Nothing -> return $ Left "read: not enough operands"
    Just (x, Integer y) -> case getFd y fds of
        Left e -> return $ Left ("read: " ++ e)
        Right handle -> do
            z <- hGetChar handle
            exec symtab bOps ops (x ++ [Char z] ) env fds
    Just (_, x) -> return $ Left ("read: expected fd, got " ++ show x)
exec symtab bOps (Call "readln" _: ops) stack env fds = case unsnoc stack of
    Nothing -> return $ Left "readln: not enough operands"
    Just (x, Integer y) -> case getFd y fds of
        Left e -> return $ Left ("readln: " ++ e)
        Right handle -> do
            z <- hGetLine handle
            exec symtab bOps ops (x ++ [stringToList z]) env fds
    Just (_, x) -> return $ Left ("readln: expected fd, got " ++ show x)
-}
