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
call :: String -> [Function] -> [Function] -> Env -> IO (Either String (Maybe Operand))
call fnName [] _ _ = return $ Left ("call to undeclared function: " ++ fnName)
call fnName ((function, argc, opcodes): xs) symtab env
    | fnName == function = if length env < argc
        then return $
            Left ("not enough arguments for function call: " ++ fnName)
        else exec symtab opcodes opcodes [] env -- TODO: flush previous env
    | otherwise = call fnName xs symtab env

-- run a function's body (all its instructions)
exec :: [Function] -> [OpCode] -> [OpCode] -> Stack -> Env -> IO (Either String (Maybe Operand))
exec symtab bOps (Nop: ops) stack env = exec symtab bOps ops stack env
exec symtab bOps (Call "print" _: ops) stack env = case unsnoc stack of
    Nothing -> return $ Left "not enough operands"
    Just (stack', x) -> putStr (show x) >> exec symtab bOps ops stack' env
exec symtab bOps (Call "println" _: ops) stack env = case unsnoc stack of
    Nothing -> return $ Left "not enough operands"
    Just (stack', x) -> print x >> exec symtab bOps ops stack' env
exec symtab bOps (Call x _: ops) stack env = do
    y <- call x symtab symtab env
    case y of
        Left e -> return $ Left e
        Right z -> case z of
            Nothing -> exec symtab bOps ops stack env
            Just operand -> exec symtab bOps ops (stack ++ [operand]) env
exec symtab bOps (Load x: ops) stack env = case getEnv x env of
    Nothing -> return $ Left ("LOAD: not in env: " ++ x)
    Just y -> exec symtab bOps ops (stack ++ [y]) env
exec symtab bOps (Store x: ops) stack env = case unsnoc stack of
    Nothing -> return $ Left "STORE: empty stack"
    Just (y, z) -> exec symtab bOps ops y $ pushEnv env (x, z)
exec symtab bOps (PushBool x: ops) stack env = 
    exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushChar x: ops) stack env =
    exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushInt x: ops) stack env =
    exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushFloat x: ops) stack env =
    exec symtab bOps ops (stack ++ [x]) env
exec symtab bOps (PushList x: ops) stack env = if length stack < x
        then return $ Left "PUSH_LIST: not enough operands on stack"
        else exec symtab bOps ops stack' env
        where stack' = stack ++ [List $ reverse(take x $ reverse stack)]
exec symtab bOps (Pop: ops) [] env = exec symtab bOps ops [] env
exec symtab bOps (Pop: ops) stack env = exec symtab bOps ops (init stack) env
exec symtab bOps (Jump x: _) stack env = case jumpTo x bOps of
    Nothing -> return $ Left ("JMP: jump to undeclared label: " ++ x)
    Just opcodes -> exec symtab bOps opcodes stack env
exec _ _ (JumpFalse _: _) [] _ = return $ Left "JMP_IF_FALSE: empty stack"
exec symtab bOps (JumpFalse x: ops) stack env = case last stack of
    Bool y -> if not y
        then case jumpTo x bOps of
            Nothing -> return
                $ Left ("JMP_IF_FALSE: jump to undeclared label: " ++ x)
            Just opcodes -> exec symtab bOps opcodes (init stack) env
        else exec symtab bOps ops (init stack) env
    _ -> return $ Left "JMP_IF_FALSE: non-boolean comparison"
exec _ _ (JumpTrue _: _) [] _ = return $ Left "JMP_IF_TRUE: empty stack"
exec symtab bOps (JumpTrue x: ops) stack env = case last stack of
    Bool y -> if y
        then case jumpTo x bOps of
            Nothing -> return
                $ Left ("JMP_IF_TRUE: jump to undeclared label: " ++ x)
            Just opcodes -> exec symtab bOps opcodes (init stack) env
        else exec symtab bOps ops (init stack) env
    _ -> return $ Left "JMP_IF_TRUE: non-boolean comparison"
exec symtab bOps (Label _: ops) stack env = exec symtab bOps ops stack env
-- TODO: improve aritmethic operations
exec symtab bOps (Mul: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("MUL: " ++ e)
    Right (Integer x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Integer (x * y)]) env
    Right (x, y, rest) -> case mulOperand x y of
        Left e -> return $ Left e
        Right z -> exec symtab bOps ops (rest ++ [Float z]) env
exec symtab bOps (Add: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("ADD: " ++ e)
    Right (Integer x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Integer (x + y)]) env
    Right (x, y, rest) -> case addOperand x y of
        Left e -> return $ Left e
        Right z -> exec symtab bOps ops (rest ++ [Float z]) env
exec symtab bOps (Sub: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("SUB: " ++ e)
    Right (Integer x, Integer y, z) ->
        exec symtab bOps ops (z ++ [Integer (x - y)]) env
    Right (x, y, rest) -> case subOperand x y of
        Left e -> return $ Left e
        Right z -> exec symtab bOps ops (rest ++ [Float z]) env
exec symtab bOps (Div: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("DIV: " ++ e)
    Right (_, Integer 0, _) -> return $ Left "DIV: 0 division"
    Right (_, Float 0, _) -> return $ Left "DIV: 0 division"
    Right (x, y, rest) -> case divOperand x y of
        Left e -> return $ Left e
        Right z -> exec symtab bOps ops (rest ++ [Float z]) env
exec symtab bOps (Mod: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("MOD: " ++ e)
    Right (_, Integer 0, _) -> return $ Left "MOD: 0 modulo"
    Right (_, Float 0, _) -> return $ Left "MOD: 0 modulo"
    Right (x, y, rest) -> case modOperand x y of
        Left e -> return $ Left e
        Right z -> exec symtab bOps ops (rest ++ [Float z]) env
exec symtab bOps (Lt: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("LT: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (ltOperand x y)]) env 
exec symtab bOps (Gt: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("GT: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (gtOperand x y)]) env 
exec symtab bOps (LEq: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("LT_EQ: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (leOperand x y)]) env 
exec symtab bOps (GEq: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("GT_EQ: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (geOperand x y)]) env 
exec symtab bOps (Eq: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("EQ: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (eqOperand x y)]) env 
exec symtab bOps (NEq: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("NOT_EQ: " ++ e)
    Right (x, y, rest) ->
        exec symtab bOps ops (rest ++ [Bool (neqOperand x y)]) env 
exec symtab bOps (And: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("AND: " ++ e)
    Right (Bool x, Bool y, z)
        -> exec symtab bOps ops (z ++ [Bool (x && y)]) env
    Right _ -> return $ Left "AND: non-boolean comparison"
exec symtab bOps (Or: ops) stack env = case pop2 stack of
    Left e -> return $ Left ("OR: " ++ e)
    Right (Bool x, Bool y, z)
        -> exec symtab bOps ops (z ++ [Bool (x || y)]) env
    Right _ -> return $ Left "OR: non-boolean comparison"
exec _ _ (Ret: _) [] _ = return $ Right Nothing
exec _ _ (Ret: _) stack _ = return $ Right $ Just (last stack)
exec _ _ x _ _
    | null x = return $ Right Nothing
    | otherwise = return $ Left ("unknwon instruction: " ++ show (head x))

addOperand :: Operand -> Operand -> Either String Float
addOperand (Integer x)  (Float y)   = Right (fromIntegral x + y)
addOperand (Float x)    (Integer y) = Right (x + fromIntegral y)
addOperand (Float x)    (Float y)   = Right (x + y)
addOperand _ _                      =
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
