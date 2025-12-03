{- EPITECH PROJECT, 2025
   GENERIC LANGUAGE AND DATA OPERAND SYNTAX
   File description:
   src/Interpretor.hs
-}

module Interpretor () where

-- import Parser (Ast(..))
-- import Control.Exception (try, SomeException, evaluate)
import Debug.Trace

type Identifier     = String

data Expr = Lambda          [Identifier] Expr
  |         If              Expr Expr Expr
  |         Call            Expr [Expr]
  |         Var             Identifier
  |         Boolean         Bool
  |         Int             Integer
  deriving Show

data Ast =  Define          Identifier Expr
  |         Expression      Expr
  deriving Show

type List = [Ast]

data Env =  DefinedExpr     Identifier Expr
    |       Variable        Identifier (Either Integer Bool)
    deriving Show

-- BUILTINS
applyBuiltin :: Identifier -> [Integer] -> Maybe Expr
applyBuiltin "+" n = Just $ Int $ sum n
applyBuiltin "-" n = case n of
    [] -> Nothing
    [x] -> Just $ Int (-x)
    (x:xs) -> Just $ Int (x - sum xs)
applyBuiltin "*" n = Just $ Int $ product n
applyBuiltin "div" n = case n of
    [a,b] -> if b == 0 then Nothing else Just $ Int (a `div` b)
    _ -> Nothing
applyBuiltin "mod" n = case n of
    [a,b] -> if b == 0 then Nothing else Just $ Int (a `mod` b)
    _ -> Nothing
applyBuiltin "<" n = case n of
    [a,b] -> Just $ Boolean (a < b)
    _ -> Nothing
applyBuiltin ">" n = case n of
    [a,b] -> Just $ Boolean (a > b)
    _ -> Nothing
applyBuiltin "eq?" n = case n of
    [a,b] -> Just $ Boolean (a == b)
    _ -> Nothing
applyBuiltin _ _ = Nothing


builtinToken :: [Identifier]
builtinToken = [ "+", "-", "*", "div", "mod" ]

builtinComparisonToken :: [Identifier]
builtinComparisonToken = [ "<", "eq?" ]

lookupDefined :: [Env] -> Expr -> Maybe Expr
lookupDefined (Variable name value:xs) var@(Var key)
    | name == key   = case value of
        Left i -> Just $ Int i
        Right a -> Just $ Boolean a
    | otherwise     = lookupDefined xs var
lookupDefined (DefinedExpr name body : xs) c@(Call (Var key) _)
    | name == key   = Just body
    | otherwise     = lookupDefined xs c
lookupDefined _ _ = Nothing

checkList :: Expr -> [Identifier] -> Maybe Identifier
checkList fc@(Call (Var id) _) (x:xs)
    | id == x = Just x
    | otherwise = checkList fc xs
checkList _ _ = Nothing

expToInt :: [Expr] -> Maybe [Integer]
expToInt (Int i: xs) = do
    rest <- expToInt xs
    Just $ i : rest
expToInt [] = Just []
expToInt _ = Nothing

checkCallToken :: [Env] -> Expr -> [Expr] -> Maybe Expr
checkCallToken e fc@(Call f _) args = case checkList fc builtinToken of
    Nothing -> case checkList fc builtinComparisonToken of
        Nothing -> lookupDefined e fc
        (Just id) -> do
            argList <- expToInt args
            applyBuiltin id argList
    (Just id) -> do
            argList <- expToInt args
            applyBuiltin id argList
checkCallToken e v@(Var _) _ = lookupDefined e v
checkCallToken _ _ _ = Nothing

reduceExpr :: [Env] -> Expr -> Maybe Expr
reduceExpr _ e@(Int _) = Just e
reduceExpr _ e@(Boolean _) = Just e
reduceExpr env v@(Var e) = checkCallToken env v []
reduceExpr env c@(Call f ag) = do
    agList <- mapM (reduceExpr env) ag
    checkCallToken env c agList

interpret :: [Ast] -> [Env] -> Maybe Ast
interpret [] _ = Nothing
interpret (Define ex body: ast) env = case body of
    (Int i) -> interpret ast (Variable ex (Left i): env)
    (Boolean i) -> interpret ast (Variable ex (Right i): env)
    _ -> interpret ast (DefinedExpr ex body: env)
interpret ((Expression f1@(Call _ _)): ast) env = do
    res <- reduceExpr env f1
    return (Expression res)
interpret ((Expression expr): ast) env = do
    res <- reduceExpr env expr 
    return (Expression res)
