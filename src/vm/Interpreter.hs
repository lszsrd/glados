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

import OpCodes (Operand (..), OpCode (..))
import Function (Function)
import Stack (Stack)
import Env (Env)

call :: String -> [Operand] -> [Function] -> [Function] -> Either String (IO (Maybe Operand))
call fnName _ [] _ = Left ("call to undeclared function " ++ fnName)
call fnName argv ((function, argc, opcodes): xs) symtab
    | fnName == function = if True == False -- length argv /= argc
        then Left ("invalid arguments to function call: " ++ fnName)
        else case exec symtab opcodes opcodes [] [] of
            Left e -> Left e
            Right x -> Right x
    | otherwise = call fnName argv xs symtab

jumpTo :: String -> [OpCode] -> Maybe [OpCode]
jumpTo _ [] = Nothing
jumpTo x (Label y: z)
    | x == y = Just z
jumpTo x (_: ys) = jumpTo x ys

getEnv :: String -> Env -> Maybe Operand
getEnv _ [] = Nothing
getEnv x ((y, ys): z)
    | x == y = Just ys
    | otherwise = getEnv x z

-- Big L that we cannot use base-4.19 or newer
-- https://github.com/haskell/core-libraries-committee/issues/165 
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

exec :: [Function] -> [OpCode] -> [OpCode] -> Stack -> Env -> Either String (IO (Maybe Operand))
exec _ _ [] _ _ = Right $ return Nothing

exec symtab bOps (Nop: ops) stack env = exec symtab bOps ops stack env

exec symtab bOps (Call x y: ops) stack env = call x [] symtab symtab -- push ret'd value to the stack

exec symtab bOps (Load x: ops) stack env = case getEnv x env of
    Nothing -> Left ("LOAD: not in env: " ++ x)
    Just y -> exec symtab bOps ops (stack ++ [y]) env

exec symtab bOps (Store x: ops) stack env = case unsnoc stack of
    Nothing -> Left "STORE: empty stack"
    Just (y, z) -> exec symtab bOps ops y (env ++ [(x, z)])

exec symtab bOps (PushBool x: ops) stack env = exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushChar x: ops) stack env = exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushInt x: ops) stack env = exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushFloat x: ops) stack env = exec symtab bOps ops (stack ++ [x]) env

exec symtab bOps (Pop: ops) [] env = exec symtab bOps ops [] env
exec symtab bOps (Pop: ops) stack env = exec symtab bOps ops (init stack) env

exec symtab _ (JumpFalse x: ops) [] env = Left "JUMP_FALSE: empty stack"
exec symtab bOps (JumpFalse x: ops) stack env = case last stack of
    Bool y -> if y == False
        then case jumpTo x bOps of
            Nothing -> Left ("JUMP_FALSE: jump to undeclared label: " ++ x)
            Just opcodes -> exec symtab bOps opcodes (init stack) env
        else exec symtab bOps ops (init stack) env
    _ -> Left "JUMP_FALSE: non-boolean comparison"

exec _ _ x _ _ = Left ("illegal instruction: " ++ show (head x))
