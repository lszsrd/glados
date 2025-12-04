{- EPITECH PROJECT, 2025
   GENERIC LANGUAGE AND DATA OPERAND SYNTAX
   File description:
   src/Interpretor.hs
-}

module Builtins (
      applyBuiltin
    , builtinToken
    , builtinComparisonToken
) where

import AbstractTree

-- BUILTINS
applyBuiltin :: Identifier -> [Integer] -> Maybe Expr
applyBuiltin "+" n                      =  Just $ Int $ sum n
applyBuiltin "-" n                      =  case n of
    [] -> Nothing
    [x] -> Just $ Int (-x)
    (x:xs) -> Just $ Int (x - sum xs)
applyBuiltin "*" n                      =  Just $ Int $ product n
applyBuiltin "div" n                    =  case n of
    [a,b] -> if b == 0
        then Nothing
        else Just $ Int (a `div` b)
    _ -> Nothing
applyBuiltin "mod" n                    =  case n of
    [a,b] -> if b == 0
        then Nothing
        else Just $ Int (a `mod` b)
    _ -> Nothing
applyBuiltin "<" n                      =  case n of
    [a,b] -> Just $ Boolean (a < b)
    _ -> Nothing
applyBuiltin ">" n                      =  case n of
    [a,b] -> Just $ Boolean (a > b)
    _ -> Nothing
applyBuiltin "eq?" n                    =  case n of
    [a,b] -> Just $ Boolean (a == b)
    _ -> Nothing
applyBuiltin _ _                        = Nothing


builtinToken :: [Identifier]
builtinToken                            = [ "+", "-", "*", "div", "mod" ]

builtinComparisonToken :: [Identifier]
builtinComparisonToken                  = [ "<", "eq?" ]
