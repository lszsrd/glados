{-
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/EnvStoreRetrieve.hs
-}




-------------------------------------------------------------------------------
-- |
-- Module      : EnvStoreRetrieve
-- Description : Manages Environment storing defined expressions.
--
-- License     : MIT
-- Maintainers  : maxence.pierre@epitech.eu, nathan.flachat@epitech.eu
--
-------------------------------------------------------------------------------
module EnvStoreRetrieve (
    -- * Env type export
      Env(..)
    -- * Retrieving in Env
    , getKey
    , lookupDefined
    , checkList
    , checkCallToken

    -- * Convert
    , expToInt
) where

import AbstractTree
import Builtins
import Debug.Trace

data Env =  DefinedExpr     Identifier Expr
    |       Variable        Identifier (Either Integer Bool)
    deriving Show

-- | Takes an @'Expr'@ as parameter,
-- returns an @'Identifier'@.
--
-- This function takes an expression as a parameter and retrieve the
-- Identifier.
-- e.g.: Var "foo" -> "foo"
--
-- It is used when resolving variables bindings.
getKey :: Expr -> Identifier
getKey var@(Var key)                    = key
getKey c@(Call (Var key) _)             = key
getKey _                                = ""

-- | Takes a List of @'Env'@ and an @'Expr'@ as parameters,
-- returns a __Maybe__ @'Expr'@.
--
-- this functions tries to find the given expression in the user's defined
-- expressions, whether a defined function or a bound variable.
-- Returns the Expression bound to the variable.
lookupDefined :: [Env] -> Expr -> Maybe Expr
lookupDefined (Variable name value:xs) f
    | name == getKey f                  = case value of
        Left i                          -> Just $ Int i
        Right a                         -> Just $ Boolean a
    | otherwise                         = lookupDefined xs f
lookupDefined (DefinedExpr name body : xs) f
    | name == getKey f                  = Just body
    | otherwise                         = lookupDefined xs f
lookupDefined _ _                       = Nothing

-- | Takes an @'Expr'@ and a List of @'Identifier'@ as parameters,
-- returns a __Maybe__ @'Identifier'@.
--
-- This function takes a list of keywords as parameters and tries to
-- find a correspondance with the given expression.
--
-- This function is used to know whether a call is on a builtin or not.
checkList :: Expr -> [Identifier] -> Maybe Identifier
checkList fc@(Call (Var id) _) (x:xs)
    | id == x   = Just x
    | otherwise = checkList fc xs
checkList _ _   = Nothing

-- | Takes a List of @'Expr'@ as parameter,
-- returns a __Maybe__ List of @'Integer'@.
--
-- This function takes an expression list as parameter, and tries to
-- convert it in an Integer list.
--
-- This is used to convert and verify that given params to a builtin is
-- a list of Integers values.
expToInt :: [Expr] -> Maybe [Integer]
expToInt (Int i: xs)                    = do
    rest <- expToInt xs                 
    Just $ i : rest
expToInt []                             = Just []
expToInt _                              = Nothing

-- | Takes a List of @'Env'@ an @'Expr'@, and a List of @'Expr'@ as parameters
-- returns a __Maybe__ @'Expr'@
--
-- This function checks if the passed expression is a call to an existing function,
-- Either a Builtin or an User defined function.
-- In the case of a builtin, verify param and execute right after.
-- Else returning the body of the defined call.
--
-- This is used to reduce a Call or to retrieve a defined function body,
-- to evaluate later (See Interpretor.tryEvalLambda,
-- called by reduceExpr, after verifying function call).
checkCallToken :: [Env] -> Expr -> [Expr] -> Maybe Expr
checkCallToken e fc@(Call f _) args     = case checkList fc builtinToken of
    Nothing -> case checkList fc builtinComparisonToken of
        Nothing -> lookupDefined e fc
        (Just id) -> do
            argList <- expToInt args
            applyBuiltin id argList
    (Just id) -> do
            argList <- expToInt args
            applyBuiltin id argList
checkCallToken e v@(Var _) args         = lookupDefined e v
checkCallToken _ _ _                    = Nothing
