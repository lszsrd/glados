{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/Utils.hs
-}

module Utils (
    findString
) where

import Data.List (isPrefixOf)

findString :: String -> String -> Maybe String
findString [] _ = Nothing
findString hay@(_: x) needle
    | needle `isPrefixOf` hay = Just hay
    | otherwise = findString x needle
