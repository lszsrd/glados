{- EPITECH PROJECT, 2025
   GENERIC LANGUAGE AND DATA OPERAND SYNTAX
   File description:
   src/EnvStoreRetrieve.hs
-}

module EnvStoreRetrieve (
      Env(..)
    , getKey
    , lookupDefined
    , checkList
    , expToInt
    , checkCallToken
) where

import AbstractTree
import Builtins
import UserDefined

data Env =  DefinedExpr     Identifier Expr
    |       Variable        Identifier (Either Integer Bool)
    deriving Show

getKey :: Expr -> Identifier
getKey var@(Var key)                    = key
getKey c@(Call (Var key) _)             = key
getKey _                                = ""

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

checkList :: Expr -> [Identifier] -> Maybe Identifier
checkList fc@(Call (Var id) _) (x:xs)
    | id == x   = Just x
    | otherwise = checkList fc xs
checkList _ _   = Nothing

expToInt :: [Expr] -> Maybe [Integer]
expToInt (Int i: xs)                    = do
    rest <- expToInt xs                 
    Just $ i : rest
expToInt []                             = Just []
expToInt _                              = Nothing

checkCallToken :: [Env] -> Expr -> [Expr] -> Maybe Expr
checkCallToken e fc@(Call f _) args     = case checkList fc builtinToken of
    Nothing -> case checkList fc builtinComparisonToken of
        Nothing -> applyUser (lookupDefined e fc) args
        (Just id) -> do
            argList <- expToInt args
            applyBuiltin id argList
    (Just id) -> do
            argList <- expToInt args
            applyBuiltin id argList
checkCallToken e v@(Var _) args         = applyUser (lookupDefined e v) args
checkCallToken _ _ _                    = Nothing
