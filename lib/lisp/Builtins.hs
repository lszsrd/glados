{-
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Builtins.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Builtins
-- Description : Primitive builtin operations (arithmetic & comparisons)
--
-- Provides a set of host-level primitive functions used by the interpreter:
-- arithmetic (+, -, *, div, mod) and basic comparisons (<, eq?)
-- Each builtin receives a list of already-evaluated integers and returns
-- either an expression literal (Int / Boolean) wrapped in @Just@ or triggers
-- an evaluation error via @throwErr@ on invalid usage (arity / division by zero)
--
-- Exports:
--   * applyBuiltin :: Identifier -> [Integer] -> Maybe Expr
--   * builtinToken :: [Identifier]
--   * builtinComparisonToken :: [Identifier]
--
-- License     : MIT
-- Maintainers : maxence.pierre@epitech.eu, nathan.flachat@epitech.eu
-------------------------------------------------------------------------------
module Builtins (
      applyBuiltin
    , builtinToken
    , builtinComparisonToken
) where

import AbstractTree
import Error

-- | applyBuiltin
-- Apply a builtin identified by the given Identifier to a list of Integer
-- arguments. The function expects arguments already reduced to Integer values
--
-- Returns:
--   * @Just (Int n)@ when the builtin produces an integer result
--   * @Just (Boolean b)@ when the builtin produces a boolean result
--   * Calls @throwErr@ with a descriptive message on invalid usage
--   * @Nothing@ for unknown builtin identifiers
--
-- Notes:
--   * Division and modulo explicitly check division-by-zero and raise an error.
--   * Arity is validated per-operation and triggers an error message when wrong
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
-- unknown builtin identifier: return Nothing so caller can try other resolution paths

-- | builtinToken
-- List of arithmetic builtins recognized by the evaluator
builtinToken :: [Identifier]
builtinToken                            = [ "+", "-", "*", "div", "mod" ]


-- | builtinComparisonToken
-- List of comparison builtins recognized by the evaluator
builtinComparisonToken :: [Identifier]
builtinComparisonToken                  = [ "<", "eq?" ]
