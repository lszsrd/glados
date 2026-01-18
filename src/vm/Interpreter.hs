{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Interpreter.hs
-}

module Interpreter (
    call
    , exec
) where

{- import System.IO (openFile, IOMode (..), hSetBinaryMode, hFileSize, hIsOpen, hIsEOF, hClose, hPutStr, hPrint, hFlush, hGetChar, hGetLine, hTell, hSeek, SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd))
import System.Random

import Data.Char (digitToInt)

import Foreign (fromBool, FunPtr)
import Foreign.C (CInt (..)) -}

import OpCodes (Operand (..), Instruction (..), div, mod, showList')
import Function (Function, fetch, jumpTo)
import Stack (Stack, popStackN)
import Env (Env, setEnv, getEnv, purgeEnv)
import Fds (Fds)
import Debug.Trace (trace)
--import Utils
-- import Debug.Trace (trace)

{- foreign import ccall "stdlib.h &exit"
  p_exit:: FunPtr (CInt -> IO ())

foreign import ccall "dynamic"
  mkFun :: FunPtr (CInt -> IO ()) -> (CInt -> IO ()) -}

call :: String -> [Function] -> Env -> Fds -> IO (Either String (Maybe Operand))
call function x env fds = case fetch function x of
    Just (_, y, z) -> exec (z, z) x [] (purgeEnv env y) fds
    _ -> return (Left $ "CALL: call to undeclared function " ++ function) -- handle builtin here

exec :: ([Instruction], [Instruction]) -> [Function] -> Stack -> Env -> Fds -> IO (Either String (Maybe Operand))
exec (Nop: x, y) z stack env fds = exec (x, y) z stack env fds
exec (Call function argc: x, y) z stack env fds = do
    operand <- call function z env fds
    case operand of
        Left e -> return (Left e)
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

{- open :: [Function] -> [OpCode] -> [OpCode] -> Stack -> Env -> Fds -> IOMode -> IO (Either String (Maybe Operand))
open symtab bOps ops stack env fds mode = case unsnoc stack of
    Nothing -> return $ Left "open: not enough operands"
    Just (x, List y) -> do
        z <- openFile (showList' y) mode
        _ <- hSetBinaryMode z True
        exec symtab bOps ops (x ++ [Integer fd]) env (fds ++ [(fd, z)])
            where fd = 1 + maximum (map fst fds)
    Just (_, x) -> return $ Left ("open: expected [Char], got " ++ show x)

seek :: [Function] -> [OpCode] -> [OpCode] -> Stack -> Env -> Fds -> SeekMode -> IO (Either String (Maybe Operand))
seek symtab bOps ops stack env fds mode = case pop2 stack of
    Left e -> return $ Left ("seek_a: " ++ e)
    Right (Integer x, Integer y, z) -> case getFd x fds of
        Left e -> return $ Left ("tell: " ++ e)
        Right handle -> do
            _ <- hSeek handle mode y
            exec symtab bOps ops z env fds
    Right (_, _, _) -> return $ Left "seek: expected Fd, Int"

-- run a function's body (all its instructions)
exec :: [Function] -> [OpCode] -> [OpCode] -> Stack -> Env -> Fds -> IO (Either String (Maybe Operand))
exec symtab bOps (Nop: ops) stack env fds = exec symtab bOps ops stack env fds
exec _ _ (Call "exit" _: _) stack _ _ = case unsnoc stack of
    Nothing -> return $ Left "exit: not enough operands"
    Just (_, y) -> return $ Right (Just y)
exec _ _ (Call "cexit" _: _) stack _ _ = case unsnoc stack of
    Nothing -> return $ Left "cexit: not enough operands"
    Just (_, Integer y) -> do
        _ <- mkFun p_exit $ fromIntegral y
        return $ Left []
    Just (_, x) -> return $ Left ("cexit: expected Int, got " ++ show x)
exec x y (Call "open_r" _: ys) z z' fds = open x y ys z z' fds ReadMode
exec x y (Call "open_w" _: ys) z z' fds = open x y ys z z' fds WriteMode
exec x y (Call "open_a" _: ys) z z' fds = open x y ys z z' fds AppendMode
exec x y (Call "open_rw" _: ys) z z' fds = open x y ys z z' fds ReadWriteMode
exec symtab bOps (Call "getFileSize" _: ops) stack env fds = case unsnoc stack of
    Nothing -> return $ Left "isOpen: not enough operands"
    Just (x, Integer y) -> case getFd y fds of
        Left _ -> exec symtab bOps ops (x ++ [Integer (-1)] ) env fds
        Right handle -> do
            z <- hFileSize handle
            exec symtab bOps ops (x ++ [Integer z] ) env fds
    Just (_, x) -> return $ Left ("read: expected fd, got " ++ show x)
exec symtab bOps (Call "isOpen" _: ops) stack env fds = case unsnoc stack of
    Nothing -> return $ Left "isOpen: not enough operands"
    Just (x, Integer y) -> case getFd y fds of
        Left _ -> exec symtab bOps ops (x ++ [Bool False] ) env fds
        Right handle -> do
            z <- hIsOpen handle
            exec symtab bOps ops (x ++ [Bool z] ) env fds
    Just (_, x) -> return $ Left ("read: expected fd, got " ++ show x)
exec symtab bOps (Call "close" _: ops) stack env fds = case unsnoc stack of
    Nothing -> return $ Left "close: not enough operands"
    Just (x, Integer y) -> case getFd y fds of
        Left e -> return $ Left ("close: " ++ e)
        Right handle -> case hClose handle of
            _ -> case dropWhile (\(a, _) -> a /= y) fds of
                (_: zs) -> exec symtab bOps ops x env (z ++ zs)
                _ -> exec symtab bOps ops x env z
                where z = takeWhile (\(a, _) -> a /= y) fds
    Just (_, x) -> return $ Left ("close: expected fd, got " ++ show x)
exec symtab bOps (Call "isEOF" _: ops) stack env fds = case unsnoc stack of
    Nothing -> return $ Left "isEOF: not enough operands"
    Just (x, Integer y) -> case getFd y fds of
        Left e -> return $ Left ("isEOF: " ++ e)
        Right handle -> do
            z <- hIsEOF handle
            exec symtab bOps ops (x ++ [Bool z] ) env fds
    Just (_, x) -> return $ Left ("read: expected fd, got " ++ show x)
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
exec symtab bOps (Call "tell" _: ops) stack env fds = case unsnoc stack of
    Nothing -> return $ Left "tell: not enough operands"
    Just (x, Integer y) -> case getFd y fds of
        Left e -> return $ Left ("tell: " ++ e)
        Right handle -> do
            z <- hTell handle
            exec symtab bOps ops (x ++ [Integer z]) env fds
    Just (_, x) -> return $ Left ("tell: expected fd, got " ++ show x)
exec x y (Call "seek_a" _: ys) z z' fds = seek x y ys z z' fds AbsoluteSeek
exec x y (Call "seek_r" _: ys) z z' fds = seek x y ys z z' fds RelativeSeek
exec x y (Call "seek_e" _: ys) z z' fds = seek x y ys z z' fds SeekFromEnd
exec s b (Call "rand" _: ops) stack env fds = do
    g <- newStdGen
    case pop2 stack of
        Right (Bool x, Bool y, z)
            -> exec s b ops (z ++ [Bool $ head (randomRs (x, y) g)]) env fds
        Right (Char x, Char y, z)
            -> exec s b ops (z ++ [Char $ head (randomRs (x, y) g)]) env fds
        Right (Integer x, Integer y, z)
            -> exec s b ops (z ++ [Integer $ head (randomRs (x, y) g)]) env fds
        _ -> return $ Left "rand: expected Int, Int"
exec symtab bOps (Label _: ops) stack env fds
    = exec symtab bOps ops stack env fds -}
