{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/types/Data.hs
-}

module Data (
    Function
    , Struct
    , Stack
    , Env
    , Fds

    , fetch
    , getStructBP
    , jumpTo
    , popStackN
    , getEnv
    , setEnv
    , purgeEnv
) where

import System.IO (Handle)

import OpCodes (Operand (..), Instruction (..))

type Function = (String, [String], [Instruction])
type Struct = (String, [String])
type Stack = [Operand]
type Env = [(String, Operand, Bool)]
type Fds = [(Integer, Handle)]

fetch :: String -> [Function] -> Maybe Function
fetch _ [] = Nothing
fetch x (y@(y', _, _): ys) = if x == y'
    then Just y
    else fetch x ys

getStructBP :: [Struct] -> String -> Maybe Struct
getStructBP x y = case filter (\(a, _) -> a == y) x of
    [(a, b)] -> Just (a, b)
    _ -> Nothing

{- resolveField :: [String] -> String -> Maybe Operand
resolveField x y = case break (== '@') y of     -- y => x@y
    (a, _: b) -> case break (== '@') b of       -- a => x, b => y
        (c, []) -> case getEnv env c of
            Just (_, Struct _ d e) -> case filter (\(f, _) -> f == b) (zip d e) of
                [] -> return (Left $ "LOAD " ++ c ++ ": no field " ++ b)
                (_, operand): _ -> exec (x, y) z (stack ++ [operand]) env fds
        _ -> return (Left $ "LOAD " ++ ident ++ ": not in env")
    _ -> Nothing -}

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
getEnv x y = case filter (\(a, _, _) -> a == y) x of
    [(a, b, _)] -> Just (a, b)
    _ -> Nothing

setEnv :: Env -> (String, Operand) -> Env
setEnv x (y, z) = case getEnv x y of
    Nothing -> x ++ [(y, z, False)]
    _ -> map (\(a, b, _) -> if a == y then (y, z, False) else (a, b, False)) x

purgeEnv :: Env -> [String] -> Env
purgeEnv [] _ = []
purgeEnv ((x, y, True): xs) z = (x, y, True): purgeEnv xs z
purgeEnv ((x, y, _): xs) z = if x `elem` z
    then (x, y, False): purgeEnv xs z
    else purgeEnv xs z
