{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/types/Data.hs
-}

module Data (
    Function
    , Stack
    , Env
    , Fds

    , fetch
    , jumpTo
    , popStackN
    , getEnv
    , setEnv
    , purgeEnv
) where

import System.IO (Handle)

import Data.List (find)

import OpCodes (Operand (..), Instruction (..))

type Function = (String, [String], [Instruction])
type Stack = [Operand]
type Env = [(String, Operand)]
type Fds = [(Integer, Handle)]

fetch :: String -> [Function] -> Maybe Function
fetch _ [] = Nothing
fetch x (y@(y', _, _): ys) = if x == y'
    then Just y
    else fetch x ys

jumpTo :: String -> [Instruction] -> Maybe [Instruction]
jumpTo _ [] = Nothing
jumpTo x (Label y: z)
    | x == y = Just z
jumpTo x (_: ys) = jumpTo x ys

popStackN :: Int -> Stack -> Maybe ([Operand], Stack)
popStackN x stack = if x > y
    then Nothing
    else Just (drop (y - x) stack, take (y - x) stack)
    where y = length stack

getEnv :: Env -> String -> Maybe (String, Operand)
getEnv x y = find (\(a, _) -> a == y) x

setEnv :: Env -> (String, Operand) -> Env
setEnv x (y, z) = case getEnv x y of
    Nothing -> x ++ [(y, z)]
    _ -> map (\(a, b) -> if a == y then (y, z) else (a, b)) x

purgeEnv :: Env -> [String] -> Env
purgeEnv [] _ = []
purgeEnv ((x, y): xs) z = if x `elem` z
    then (x, y): purgeEnv xs z
    else purgeEnv xs z
