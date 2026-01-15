{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/instructions/Utils.hs
-}

module Utils (
    OpCode
    , Stack
    , Env
    , Fds

    , unsnoc
    , pop2
    , stringToList
    , getEnv
    , pushEnv
    , jumpTo
    , getFd
) where

import System.IO (Handle)

import OpCodes (Operand (..), OpCode (..))
import Stack (Stack)
import Env (Env)
import Fds (Fds)

-- https://github.com/haskell/core-libraries-committee/issues/165 
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

pop2 :: Stack -> Either String (Operand, Operand, Stack)
pop2 x = case unsnoc x of
    Nothing -> Left "not enough operands"
    Just (y, op1) -> case unsnoc y of
        Nothing -> Left "missing one operand"
        Just (z, op2) -> Right (op2, op1, z)

stringToList :: String -> Operand
stringToList [] = List []
stringToList (x: xs) = case stringToList xs of
    List y -> List $ Char x: y
    y -> y

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

getFd :: Integer -> Fds -> Either String Handle
getFd x [] = Left $ "invalid fd " ++ show x
getFd x ((y, y'): ys)
    | x == y = Right y'
    | otherwise = getFd x ys

