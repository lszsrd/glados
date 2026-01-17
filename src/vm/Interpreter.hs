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

import System.IO (openFile, IOMode (..), hSetBinaryMode, hFileSize, hIsOpen, hIsEOF, hClose, hPutStr, hPrint, hFlush, hGetChar, hGetLine, hTell, hSeek, SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd))
import System.Random

import Data.Char (digitToInt)

import Foreign (fromBool, FunPtr)
import Foreign.C (CInt (..))

import OpCodes (Operand (..), OpCode (..), showList')
import Function (Function)
import Utils
import Debug.Trace (trace)

-- TODO: urge to refactor arith operations AND handle list arith operations

foreign import ccall "stdlib.h &exit"
  p_exit:: FunPtr (CInt -> IO ())

foreign import ccall "dynamic"
  mkFun :: FunPtr (CInt -> IO ()) -> (CInt -> IO ())

-- invoke a new function and runs it
call :: String -> [Function] -> [Function] -> Env -> Fds -> IO (Either String (Maybe Operand))
call fnName [] _ _ _ = return $ Left ("call to undeclared function " ++ fnName)
call fnName ((function, argc, opcodes): xs) symtab env fds
    | fnName == function = if length env < argc
        then return $
            Left ("not enough arguments for function call: " ++ fnName)
        else exec symtab opcodes opcodes [] env fds -- TODO: flush env
    | otherwise = call fnName xs symtab env fds

open :: [Function] -> [OpCode] -> [OpCode] -> Stack -> Env -> Fds -> IOMode -> IO (Either String (Maybe Operand))
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
exec symtab bOps (Call x _: ops) stack env fds = do
    y <- call x symtab symtab env fds
    case y of
        Left e -> return $ Left e
        Right z -> case z of
            Nothing -> exec symtab bOps ops stack env fds
            Just operand -> exec symtab bOps ops (stack ++ [operand]) env fds
exec symtab bOps (Load x: ops) stack env fds = case getEnv x env of
    Nothing -> return $ Left ("LOAD: not in env: " ++ x)
    Just y -> exec symtab bOps ops (stack ++ [y]) env fds
exec symtab bOps (Store x: ops) stack env fds = case unsnoc stack of
    Nothing -> return $ Left "STORE: empty stack"
    Just (y, z) -> exec symtab bOps ops y (pushEnv env (x, z)) fds
exec symtab bOps (PushBool x: ops) stack env fds = 
    exec symtab bOps ops (stack ++ [x]) env fds
exec symtab bOps (PushChar x: ops) stack env fds =
    exec symtab bOps ops (stack ++ [x]) env fds
exec symtab bOps (PushInt x: ops) stack env fds =
    exec symtab bOps ops (stack ++ [x]) env fds
exec symtab bOps (PushFloat x: ops) stack env fds =
    exec symtab bOps ops (stack ++ [x]) env fds
exec symtab bOps (PushList x: ops) stack env fds = if length stack < x
        then return $ Left "PUSH_LIST: not enough operands on stack"
        else exec symtab bOps ops
            (take (length stack - x) stack
            ++ [List $ drop (length stack - x) stack])
            env fds
exec symtab bOps (Pop: ops) [] env fds = exec symtab bOps ops [] env fds
exec symtab bOps (Pop: ops) stack env fds
    = exec symtab bOps ops (init stack) env fds
exec symtab bOps (Jump x: _) stack env fds = case jumpTo x bOps of
    Nothing -> return $ Left ("JMP: jump to undeclared label: " ++ x)
    Just opcodes -> exec symtab bOps opcodes stack env fds
exec _ _ (JumpFalse _: _) [] _ _ = return $ Left "JMP_IF_FALSE: empty stack"
exec symtab bOps (JumpFalse x: ops) stack env fds = case last stack of
    Bool y -> if not y
        then case jumpTo x bOps of
            Nothing -> return
                $ Left ("JMP_IF_FALSE: jump to undeclared label: " ++ x)
            Just opcodes -> exec symtab bOps opcodes (init stack) env fds
        else exec symtab bOps ops (init stack) env fds
    _ -> return $ Left "JMP_IF_FALSE: non-boolean comparison"
exec _ _ (JumpTrue _: _) [] _ _ = return $ Left "JMP_IF_TRUE: empty stack"
exec symtab bOps (JumpTrue x: ops) stack env fds = case last stack of
    Bool y -> if y
        then case jumpTo x bOps of
            Nothing -> return
                $ Left ("JMP_IF_TRUE: jump to undeclared label: " ++ x)
            Just opcodes -> exec symtab bOps opcodes (init stack) env fds
        else exec symtab bOps ops (init stack) env fds
    _ -> return $ Left "JMP_IF_TRUE: non-boolean comparison"
exec symtab bOps (Label _: ops) stack env fds
    = exec symtab bOps ops stack env fds
-- TODO: improve aritmethic operations
exec symtab bOps (Mul: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("MUL: " ++ e)
    Right (Integer x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Integer (x * y)]) env fds
    Right (x, y, rest) -> case mulOperand x y of
        Left e -> return $ Left e
        Right z -> exec symtab bOps ops (rest ++ [Float z]) env fds
exec symtab bOps (Add: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("ADD: " ++ e)
    Right (Integer x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Integer (x + y)]) env fds
    Right (x, y, rest) -> case addList x y of
        Nothing -> case addOperand x y of
            Left e -> return $ Left e
            Right z -> exec symtab bOps ops (rest ++ [Float z]) env fds
        Just z -> exec symtab bOps ops (rest ++ [z]) env fds
exec symtab bOps (Sub: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("SUB: " ++ e)
    Right (Integer x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Integer (x - y)]) env fds
    Right (x, y, rest) -> case subOperand x y of
        Left e -> return $ Left e
        Right z -> exec symtab bOps ops (rest ++ [Float z]) env fds
exec symtab bOps (Div: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("DIV: " ++ e)
    Right (_, Integer 0, _) -> return $ Left "DIV: 0 division"
    Right (_, Float 0, _) -> return $ Left "DIV: 0 division"
    Right (x, y, rest) -> case divOperand x y of
        Left e -> return $ Left e
        Right z -> exec symtab bOps ops (rest ++ [Float z]) env fds
exec symtab bOps (Mod: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("MOD: " ++ e)
    Right (_, Integer 0, _) -> return $ Left "MOD: 0 modulo"
    Right (_, Float 0, _) -> return $ Left "MOD: 0 modulo"
    Right (x, y, rest) -> case modOperand x y of
        Left e -> return $ Left e
        Right z -> exec symtab bOps ops (rest ++ [Float z]) env fds
exec symtab bOps (Lt: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("LT: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (ltOperand x y)]) env fds
exec symtab bOps (Gt: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("GT: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (gtOperand x y)]) env fds
exec symtab bOps (LEq: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("LT_EQ: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (leOperand x y)]) env fds
exec symtab bOps (GEq: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("GT_EQ: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (geOperand x y)]) env fds
exec symtab bOps (Eq: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("EQ: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (eqOperand x y)]) env fds
exec symtab bOps (NEq: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("NOT_EQ: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (neqOperand x y)]) env fds
exec symtab bOps (And: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("AND: " ++ e)
    Right (Bool x, Bool y, z)
        -> exec symtab bOps ops (z ++ [Bool (x && y)]) env fds
    Right _ -> return $ Left "AND: non-boolean comparison"
exec symtab bOps (Or: ops) stack env fds = case pop2 stack of
    Left e -> return $ Left ("OR: " ++ e)
    Right (Bool x, Bool y, z)
        -> exec symtab bOps ops (z ++ [Bool (x || y)]) env fds
    Right _ -> return $ Left "OR: non-boolean comparison"
exec _ _ (Ret: _) [] _ _ = return $ Right Nothing
exec _ _ (Ret: _) stack _ _ = return $ Right $ Just (last stack)
exec _ _ x _ _ _
    | null x = return $ Right Nothing
    | otherwise = return $ Left ("unknwon instruction: " ++ show (head x))

addList :: Operand -> Operand -> Maybe Operand
addList (List x) y = Just $ List (x ++ [y])
addList x (List y) = Just $ List (x: y)
addList _ _ = Nothing


addOperand :: Operand -> Operand -> Either String Float
addOperand (Integer x)  (Integer y)   = Right (fromIntegral x + fromIntegral y)
addOperand (Integer x)  (Float y)   = Right (fromIntegral x + y)
addOperand (Float x)    (Integer y) = Right (x + fromIntegral y)
addOperand (Float x)    (Float y)   = Right (x + y)
addOperand (Char x) y = addOperand (Integer $ toInteger (digitToInt x)) y
addOperand x (Char y) = addOperand x (Integer $ toInteger (digitToInt y))
addOperand x y                      = trace (show x ++ "," ++ show y)
     Left "ADD: non-numeric operands"

subOperand :: Operand -> Operand -> Either String Float
subOperand (Integer x)  (Float y)   = Right (fromIntegral x - y)
subOperand (Float x)    (Integer y) = Right (x - fromIntegral y)
subOperand (Float x)    (Float y)   = Right (x - y)
subOperand _ _                      =
     Left "SUB: non-numeric operands"

mulOperand :: Operand -> Operand -> Either String Float
mulOperand (Integer x)  (Float y)   = Right (fromIntegral x * y)
mulOperand (Float x)    (Integer y) = Right (x * fromIntegral y)
mulOperand (Float x)    (Float y)   = Right (x * y)
mulOperand _ _                      =
     Left "Div: non-numeric operands"

divOperand :: Operand -> Operand -> Either String Float
divOperand (Integer x)  (Integer y) = Right (fromIntegral x / fromIntegral y)
divOperand (Integer x)  (Float y)   = Right (fromIntegral x / y)
divOperand (Float x)    (Integer y) = Right (x / fromIntegral y)
divOperand (Float x)    (Float y)   = Right (x / y)
divOperand _ _                      =
     Left "Div: non-numeric operands"

modOperand :: Operand -> Operand -> Either String Float
modOperand (Integer x)  (Integer y) = Right (fromIntegral x / fromIntegral y)
modOperand (Integer x)  (Float y)   = Right (fromIntegral x / y)
modOperand (Float x)    (Integer y) = Right (x / fromIntegral y)
modOperand (Float x)    (Float y)   = Right (x / y)
modOperand _ _                      =
     Left "MOD: non-numeric operands or stack error"

ltOperand :: Operand -> Operand -> Bool
ltOperand (Integer x)  (Float y)   = fromIntegral x < y
ltOperand (Float x)    (Integer y) = x < fromIntegral y
ltOperand (Bool x)     (Integer y) = fromBool x < y
ltOperand (Integer x)  (Bool y)    = x < fromBool y
ltOperand (Bool x)     (Float y)   = fromBool x < y
ltOperand (Float x)    (Bool y)    = x < fromBool y
ltOperand x y                      = x < y

gtOperand :: Operand -> Operand -> Bool
gtOperand (Integer x)  (Float y)   = fromIntegral x > y
gtOperand (Float x)    (Integer y) = x > fromIntegral y
gtOperand (Bool x)     (Integer y) = fromBool x > y
gtOperand (Integer x)  (Bool y)    = x > fromBool y
gtOperand (Bool x)     (Float y)   = fromBool x > y
gtOperand (Float x)    (Bool y)    = x > fromBool y
gtOperand x y                      = x > y

leOperand :: Operand -> Operand -> Bool
leOperand (Integer x)  (Float y)   = fromIntegral x <= y
leOperand (Float x)    (Integer y) = x <= fromIntegral y
leOperand (Bool x)     (Integer y) = fromBool x <= y
leOperand (Integer x)  (Bool y)    = x <= fromBool y
leOperand (Bool x)     (Float y)   = fromBool x <= y
leOperand (Float x)    (Bool y)    = x <= fromBool y
leOperand x y                      = x <= y

geOperand :: Operand -> Operand -> Bool
geOperand (Integer x)  (Float y)   = fromIntegral x >= y
geOperand (Float x)    (Integer y) = x >= fromIntegral y
geOperand (Bool x)     (Integer y) = fromBool x >= y
geOperand (Integer x)  (Bool y)    = x >= fromBool y
geOperand (Bool x)     (Float y)   = fromBool x >= y
geOperand (Float x)    (Bool y)    = x >= fromBool y
geOperand x y                      = x >= y

eqOperand :: Operand -> Operand -> Bool
eqOperand (Integer x)  (Float y)   = fromIntegral x == y
eqOperand (Float x)    (Integer y) = x == fromIntegral y
eqOperand (Bool x)     (Integer y) = fromBool x == y
eqOperand (Integer x)  (Bool y)    = x == fromBool y
eqOperand (Bool x)     (Float y)   = fromBool x == y
eqOperand (Float x)    (Bool y)    = x == fromBool y
eqOperand x y                      = x == y

neqOperand :: Operand -> Operand -> Bool
neqOperand (Integer x)  (Float y)   = fromIntegral x /= y
neqOperand (Float x)    (Integer y) = x /= fromIntegral y
neqOperand (Bool x)     (Integer y) = fromBool x /= y
neqOperand (Integer x)  (Bool y)    = x /= fromBool y
neqOperand (Bool x)     (Float y)   = fromBool x /= y
neqOperand (Float x)    (Bool y)    = x /= fromBool y
neqOperand x y                      = x /= y
