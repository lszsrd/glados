{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Builtins.hs
-}

module Builtins (
    getFd
    , gOpen
    , gSeek
) where

import System.IO    (
    Handle,
    IOMode,
    SeekMode,
    openFile,
    hSetBinaryMode,
    hFlush,
    hSeek,
                    )

import OpCodes (Operand (..), showList')
import Data (Stack, Fds, popStackN)

getFd :: Integer -> Fds -> Either String Handle
getFd x [] = Left $ "invalid file descriptor " ++ show x
getFd x ((y, y'): ys)
    | x == y = Right y'
    | otherwise = getFd x ys

gOpen :: Stack -> IOMode -> IO (Either String Handle)
gOpen stack mode = case popStackN 1 stack of
    Just ([List y], _) -> do
        x <- openFile (showList' y) mode
        _ <- hSetBinaryMode x True
        return (Right x)
    Just (x, _) -> return $ Left ("open: expected [Char], got " ++ show x)
    _ -> return $ Left "open: not enough operands"

gSeek :: Stack -> Fds -> SeekMode -> IO (Maybe String)
gSeek stack fds mode = case popStackN 2 stack of
    Just ([Integer x, Integer y], z) -> case getFd x fds of
        Left e -> return (Just $ "seek: " ++ e)
        Right handle -> hSeek handle mode y >> return (Nothing)
    Just (x, _) -> return (Just $ "seek: expected Int, got " ++ show x)
    _ -> return (Just "seek: not enough operands")
