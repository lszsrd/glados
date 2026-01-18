{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/types/Env.hs
-}

module Env (
    Env

    , getEnv
    , setEnv
    , purgeEnv
) where

import Data.List (find)

import OpCodes (Operand (..))

type Env = [(String, Operand)]

getEnv :: Env -> String -> Maybe (String, Operand)
getEnv x y = find (\(a, _) -> a == y) x

setEnv :: Env -> (String, Operand) -> Env
setEnv x (y, z) = case span (\(a, _) -> a == y) x of
    ([], _) -> x ++ [(y, z)]
    (_, x') -> x' ++ [(y, z)]

purgeEnv :: Env -> [String] -> Env
purgeEnv [] _ = []
purgeEnv ((x, y): xs) z = if x `elem` z
    then (x, y): purgeEnv xs z
    else purgeEnv xs z
