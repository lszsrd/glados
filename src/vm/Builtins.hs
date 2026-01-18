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
    , gRead
    , gWrite
) where

import System.IO (Handle, IOMode, SeekMode, openFile, hSetBinaryMode, hFlush, hSeek)

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
    Just ([Integer x, Integer y], _) -> case getFd x fds of
        Left e -> return (Just $ "seek: " ++ e)
        Right handle -> hSeek handle mode y >> return (Nothing)
    Just (x, _) -> return (Just $ "seek: expected Int, got " ++ show x)
    _ -> return (Just "seek: not enough operands")

gRead :: Stack -> Fds -> (Handle -> IO (a)) -> IO (Either String (a, Stack))
gRead stack fds callback = case popStackN 1 stack of
    Just ([Integer x], stack') -> case getFd x fds of
        Left e -> return (Left $ "read: " ++ e)
        Right handle -> do
            z <- callback handle
            return (Right (z, stack'))
    Just ([x], _) -> return (Left $ "read: expected Int, got " ++ show x)
    _ -> return (Left "read: not enough operands")

gWrite :: Stack -> Fds -> (Handle -> String -> IO ()) -> IO (Either String Stack)
gWrite stack fds callback = case popStackN 2 stack of
    Just ([Integer x, y], stack') -> case getFd x fds of
        Left e -> return (Left $ "write: " ++ e)
        Right handle -> do
            _ <- callback handle (show y)
            _ <- hFlush handle
            return (Right stack')
    Just ([x], _) -> return (Left $ "write: expected Int, got " ++ show x)
    _ -> return (Left "write: not enough operands")
