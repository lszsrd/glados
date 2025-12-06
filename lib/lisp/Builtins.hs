{-
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Builtins.hs
-}

module Builtins (
      applyBuiltin
    , builtinToken
    , builtinComparisonToken
) where

import AbstractTree
import Error

-- BUILTINS
applyBuiltin :: Identifier -> [Integer] -> Maybe Expr
applyBuiltin "+" n                      =  Just $ Int $ sum n
applyBuiltin "-" n                      =  case n of
    [] -> throwErr "Bro thought \"- nothing\" is gonna work smh."
    [x] -> Just $ Int (-x)
    (x:xs) -> Just $ Int (x - sum xs)
applyBuiltin "*" n                      =  Just $ Int $ product n
applyBuiltin "div" n                    =  case n of
    [a,b] -> if b == 0
        then throwErr "Blud trying to divide by 0."
        else Just $ Int (a `div` b)
    e -> throwErr (show e ++ ": What im i supposed to divide here?")
applyBuiltin "mod" n                    =  case n of
    [a,b] -> if b == 0
        then throwErr "Blud trying modulo 0."
        else Just $ Int (a `mod` b)
    e -> throwErr (show e ++ ": What im i supposed to modulo here?")
applyBuiltin "<" n                      =  case n of
    [a,b] -> Just $ Boolean (a < b)
    e -> throwErr (show e ++ ": Kid tryna do something idk what tho.")
applyBuiltin "eq?" n                    =  case n of
    [a,b] -> Just $ Boolean (a == b)
    e -> throwErr (show e ++ ": eq? this? no.")
applyBuiltin _ _                        = Nothing


builtinToken :: [Identifier]
builtinToken                            = [ "+", "-", "*", "div", "mod" ]

builtinComparisonToken :: [Identifier]
builtinComparisonToken                  = [ "<", "eq?" ]
