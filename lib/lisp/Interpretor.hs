{- EPITECH PROJECT, 2025
   GENERIC LANGUAGE AND DATA OPERAND SYNTAX
   File description:
   src/Interpretor.hs
-}

module Interpretor (
      interpret
    , reduceExpr
    , tryEvalLambda
    , createLocalEnv

) where

import AbstractTree
import EnvStoreRetrieve 

createLocalEnv :: [Identifier] -> [Expr] -> Maybe [Env]
createLocalEnv [] [] = Just []
createLocalEnv (x:xs) (y:ys) = do
    rest <- createLocalEnv xs ys
    case y of
        (Int i)     -> Just (Variable x (Left i):  rest)
        (Boolean i) -> Just (Variable x (Right i): rest)
createLocalEnv _ _ = Nothing

tryEvalLambda :: [Env] -> Expr -> [Expr] -> Maybe Expr
tryEvalLambda env (Lambda args bdy) arglist = do
    locEnv <- createLocalEnv args arglist
    reduceExpr (env ++ locEnv) bdy
tryEvalLambda _ _ _ = Nothing

reduceExpr :: [Env] -> Expr -> Maybe Expr
reduceExpr _ e@(Int _)                  = Just e
reduceExpr _ e@(Boolean _)              = Just e
reduceExpr env v@(Var e)                = do
    expr <- checkCallToken env v []
    reduceExpr env expr
reduceExpr env c@(Call f ag)            = do
    agList <- mapM (reduceExpr env) ag
    expr <- checkCallToken env c agList
    case reduceExpr env expr of
        Nothing -> tryEvalLambda env expr agList
        Just a -> Just a
reduceExpr _ _                          = Nothing 

interpret :: [Ast] -> [Env] -> Maybe Ast
interpret [] _                          = Nothing
interpret (Define ex body: ast) env     = case body of
    (Int i)     -> interpret ast (Variable ex (Left i): env)
    (Boolean i) -> interpret ast (Variable ex (Right i): env)
    _           -> interpret ast (DefinedExpr ex body: env)
interpret ((Expression expr): ast) env  = do
    res <- reduceExpr env expr 
    return (Expression res)
