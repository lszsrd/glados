{- EPITECH PROJECT, 2025
   GENERIC LANGUAGE AND DATA OPERAND SYNTAX
   File description:
   src/Interpretor.hs
-}

module Interpretor () where

import AbstractTree
import EnvStoreRetrieve 
-- import Control.Exception (try, SomeException, evaluate)
import Debug.Trace


reduceExpr :: [Env] -> Expr -> Maybe Expr
reduceExpr _ e@(Int _)                  = Just e
reduceExpr _ e@(Boolean _)              = Just e
reduceExpr env v@(Var e)                = checkCallToken env v []
reduceExpr env c@(Call f ag)            = do
    agList <- mapM (reduceExpr env) ag
    checkCallToken env c agList
reduceExpr _ e                          = trace (show e) Nothing 

interpret :: [Ast] -> [Env] -> Maybe Ast
interpret [] _                          = Nothing
interpret (Define ex body: ast) env     = case body of
    (Int i)     -> interpret ast (Variable ex (Left i): env)
    (Boolean i) -> interpret ast (Variable ex (Right i): env)
    _           -> interpret ast (DefinedExpr ex body: env)
interpret ((Expression expr): ast) env  = do
    res <- reduceExpr env expr 
    return (Expression res)
