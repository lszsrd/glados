{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Interpreter.hs
-}

module Interpreter (
    call
    , exec
    , unsnoc
    , getEnv
    , jumpTo
    , pop2
) where

import Foreign (fromBool)

import OpCodes (Operand (..), OpCode (..))
import Function (Function)
import Stack (Stack)
import Env (Env)

-- invoke a new function and runs it
call :: String -> [Operand] -> [Function] -> [Function] -> Env -> Either String (Maybe Operand)
call fnName _ [] _ _ = Left ("call to undeclared function " ++ fnName)
call fnName argv ((function, argc, opcodes): xs) symtab env
    | fnName == function = if False -- length argv /= argc
        then Left ("invalid arguments to function call: " ++ fnName)
        else case exec symtab opcodes opcodes [] env of
            Left e -> Left e
            Right x -> Right x
    | otherwise = call fnName argv xs symtab env

-- run a function's body (all its instructions)
exec :: [Function] -> [OpCode] -> [OpCode] -> Stack -> Env -> Either String (Maybe Operand)
exec symtab bOps (Nop: ops) stack env = exec symtab bOps ops stack env
exec symtab bOps (Call x y: ops) stack env = case call x [] symtab symtab env of
    Left e -> Left e
    Right z -> case z of
        Nothing -> exec symtab bOps ops stack env
        Just operand -> exec symtab bOps ops (stack ++ [operand]) env
exec symtab bOps (Load x: ops) stack env = case getEnv x env of
    Nothing -> Left ("LOAD: not in env: " ++ x)
    Just y -> exec symtab bOps ops (stack ++ [y]) env
exec symtab bOps (Store x: ops) stack env = case unsnoc stack of
    Nothing -> Left "STORE: empty stack"
    Just (y, z) -> exec symtab bOps ops y $ pushEnv env (x, z)
exec symtab bOps (PushBool x: ops) stack env = 
    exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushChar x: ops) stack env =
    exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushInt x: ops) stack env =
    exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushFloat x: ops) stack env =
    exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (Pop: ops) [] env = exec symtab bOps ops [] env
exec symtab bOps (Pop: ops) stack env = exec symtab bOps ops (init stack) env
exec symtab bOps (Jump x: _) stack env = case jumpTo x bOps of
    Nothing -> Left ("JMP: jump to undeclared label: " ++ x)
    Just opcodes -> exec symtab bOps opcodes stack env
exec _ _ (JumpFalse _: _) [] _ = Left "JMP_IF_FALSE: empty stack"
exec symtab bOps (JumpFalse x: ops) stack env = case last stack of
    Bool y -> if not y
        then case jumpTo x bOps of
            Nothing -> Left ("JMP_IF_FALSE: jump to undeclared label: " ++ x)
            Just opcodes -> exec symtab bOps opcodes (init stack) env
        else exec symtab bOps ops (init stack) env
    _ -> Left "JMP_IF_FALSE: non-boolean comparison"
exec _ _ (JumpTrue _: _) [] _ = Left "JMP_IF_TRUE: empty stack"
exec symtab bOps (JumpTrue x: ops) stack env = case last stack of
    Bool y -> if y
        then case jumpTo x bOps of
            Nothing -> Left ("JMP_IF_TRUE: jump to undeclared label: " ++ x)
            Just opcodes -> exec symtab bOps opcodes (init stack) env
        else exec symtab bOps ops (init stack) env
    _ -> Left "JMP_IF_TRUE: non-boolean comparison"
exec symtab bOps (Label _: ops) stack env = exec symtab bOps ops stack env
-- TODO: improve aritmethic operations
exec symtab bOps (Mul: ops) stack env = case pop2 stack of
    Left e -> Left ("MUL: " ++ e)
    Right (Integer x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Integer (x * y)]) env
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Float (fromIntegral x * y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Float (x * fromIntegral y)]) env
    Right (Float x, Float y, z) ->
        exec symtab bOps ops (z ++ [Float (x * y)]) env
    Right _ -> Left "MUL: non-numeric operands"
exec symtab bOps (Add: ops) stack env = case pop2 stack of
    Left e -> Left ("ADD: " ++ e)
    Right (Integer x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Integer (x + y)]) env
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Float (fromIntegral x + y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Float (x + fromIntegral y)]) env
    Right (Float x, Float y, z) ->
        exec symtab bOps ops (z ++ [Float (x + y)]) env
    Right _ -> Left "ADD: non-numeric operands"
exec symtab bOps (Sub: ops) stack env = case pop2 stack of
    Left e -> Left ("SUB: " ++ e)
    Right (Integer x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Integer (x - y)]) env
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Float (fromIntegral x - y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Float (x - fromIntegral y)]) env
    Right (Float x, Float y, z) ->
        exec symtab bOps ops (z ++ [Float (x - y)]) env
    Right _ -> Left "SUB: non-numeric operands"
exec symtab bOps (Div: ops) stack env = case pop2 stack of
    Left e -> Left ("DIV: " ++ e)
    Right (Integer x, Integer y, z) -> exec symtab bOps ops
        (z ++ [Float (fromIntegral x / fromIntegral y)]) env
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Float (fromIntegral x / y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Float (x / fromIntegral y)]) env
    Right (Float x, Float y, z) ->
        exec symtab bOps ops (z ++ [Float (x / y)]) env
    Right (_, Integer 0, _) -> Left "DIV: 0 division"
    Right (_, Float 0, _) -> Left "DIV: 0 division"
    Right _ -> Left "DIV : non-numeric operands"
exec symtab bOps (Mod: ops) stack env = case pop2 stack of
    Left e -> Left ("MOD: " ++ e)
    Right (Integer x, Integer y, z) ->exec symtab bOps ops
        (z ++ [Float (fromIntegral x / fromIntegral y)]) env
    Right (Integer x, Float y, z) -> exec symtab bOps ops
        (z ++ [Float (fromIntegral x / y)]) env
    Right (Float x, Integer y, z) -> exec symtab bOps ops
        (z ++ [Float (x / fromIntegral y)]) env
    Right (Float x, Float y, z) -> exec symtab bOps ops
        (z ++ [Float (x / y)]) env
    Right (_, Integer 0, _) -> Left "MOD: 0 modulo"
    Right (_, Float 0, _) -> Left "MOD: 0 modulo"
    Right _ -> Left "MOD: non-numeric operands"
exec symtab bOps (Lt: ops) stack env = case pop2 stack of
    Left e -> Left ("LT: " ++ e)
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromIntegral x < y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (x < fromIntegral y)]) env
    Right (Bool x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x < y)]) env
    Right (Integer x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x < fromBool y)]) env
    Right (Bool x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x < y)]) env
    Right (Float x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x < fromBool y)]) env
    Right (x, y, z) -> exec symtab bOps ops (z ++ [Bool ( x < y)]) env
exec symtab bOps (Gt: ops) stack env = case pop2 stack of
    Left e -> Left ("GT: " ++ e)
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromIntegral x > y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (x > fromIntegral y)]) env
    Right (Bool x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x > y)]) env
    Right (Integer x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x > fromBool y)]) env
    Right (Bool x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x > y)]) env
    Right (Float x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x > fromBool y)]) env
    Right (x, y, z) -> exec symtab bOps ops (z ++ [Bool ( x > y)]) env
exec symtab bOps (LEq: ops) stack env = case pop2 stack of
    Left e -> Left ("LT_EQ: " ++ e)
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromIntegral x <= y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (x <= fromIntegral y)]) env
    Right (Bool x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x <= y)]) env
    Right (Integer x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x <= fromBool y)]) env
    Right (Bool x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x <= y)]) env
    Right (Float x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x <= fromBool y)]) env
    Right (x, y, z) -> exec symtab bOps ops (z ++ [Bool ( x <= y)]) env
exec symtab bOps (GEq: ops) stack env = case pop2 stack of
    Left e -> Left ("GT_EQ: " ++ e)
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromIntegral x >= y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (x >= fromIntegral y)]) env
    Right (Bool x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x >= y)]) env
    Right (Integer x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x >= fromBool y)]) env
    Right (Bool x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x >= y)]) env
    Right (Float x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x >= fromBool y)]) env
    Right (x, y, z) -> exec symtab bOps ops (z ++ [Bool ( x >= y)]) env
exec symtab bOps (Eq: ops) stack env = case pop2 stack of
    Left e -> Left ("EQ: " ++ e)
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromIntegral x == y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (x == fromIntegral y)]) env
    Right (Bool x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x == y)]) env
    Right (Integer x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x == fromBool y)]) env
    Right (Bool x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x == y)]) env
    Right (Float x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x == fromBool y)]) env
    Right (x, y, z) -> exec symtab bOps ops (z ++ [Bool ( x == y)]) env
exec symtab bOps (NEq: ops) stack env = case pop2 stack of
    Left e -> Left ("NOT_EQ: " ++ e)
    Right (Integer x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromIntegral x /= y)]) env
    Right (Float x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (x /= fromIntegral y)]) env
    Right (Bool x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x /= y)]) env
    Right (Integer x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x /= fromBool y)]) env
    Right (Bool x, Float y, z) ->
        exec symtab bOps ops (z ++ [Bool (fromBool x /= y)]) env
    Right (Float x, Bool y, z) ->
        exec symtab bOps ops (z ++ [Bool (x /= fromBool y)]) env
    Right (x, y, z) -> exec symtab bOps ops (z ++ [Bool ( x /= y)]) env
exec symtab bOps (And: ops) stack env = case pop2 stack of
    Left e -> Left ("AND: " ++ e)
    Right (Bool x, Bool y, z)
        -> exec symtab bOps ops (z ++ [Bool (x && y)]) env
    Right _ -> Left "AND: non-boolean comparison"
exec symtab bOps (Or: ops) stack env = case pop2 stack of
    Left e -> Left ("OR: " ++ e)
    Right (Bool x, Bool y, z)
        -> exec symtab bOps ops (z ++ [Bool (x || y)]) env
    Right _ -> Left "OR: non-boolean comparison"
exec _ _ (Ret: _) [] _ = Right Nothing
exec _ _ (Ret: _) stack _ = Right $ Just (last stack)
exec _ _ x _ _
    | null x = Right Nothing
    | otherwise = Left ("unknwon instruction: " ++ show (head x))

-- Big L that we cannot use base-4.19 or newer
-- https://github.com/haskell/core-libraries-committee/issues/165 
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

getEnv :: String -> Env -> Maybe Operand -- should remove from env?
getEnv _ [] = Nothing
getEnv x ((y, ys): z)
    | x == y = Just ys
    | otherwise = getEnv x z

pushEnv :: Env -> (String, Operand) -> Env
pushEnv [] x = [x]
pushEnv env (x, y)
    | x `notElem` map fst env = env ++ [(x, y)]
    | otherwise = map (\(x', y') -> if x' == x then (x', y) else (x', y')) env

jumpTo :: String -> [OpCode] -> Maybe [OpCode]
jumpTo _ [] = Nothing
jumpTo x (Label y: z)
    | x == y = Just z
jumpTo x (_: ys) = jumpTo x ys

pop2 :: Stack -> Either String (Operand, Operand, Stack)
pop2 x = case unsnoc x of
    Nothing -> Left "not enough operands"
    Just (y, op1) -> case unsnoc y of
        Nothing -> Left "missing one operand"
        Just (z, op2) -> Right (op2, op1, z)
