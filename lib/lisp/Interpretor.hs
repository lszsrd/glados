{- EPITECH PROJECT, 2025
   GENERIC LANGUAGE AND DATA OPERAND SYNTAX
   File description:
   src/Interpretor.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Interpretor
-- Description : Ram through a given Abstract syntactic tree
-- and evaluate expressions.
--
-- License     : MIT
-- Maintainers  : maxence.pierre@epitech.eu, nathan.flachat@epitech.eu
--
-- Takes a List of __@'AST'@__ and tries to evaluate expressions.
--
-- * __@'Ast'@__ is an abstract way to represent the whole code-base
-- effectively making it easier to interpret and compute
--
-------------------------------------------------------------------------------
module Interpretor (
      -- * evaluation
      interpret
    , reduceExpr
    , tryEvalLambda
    , createLocalEnv
) where

import AbstractTree
import EnvStoreRetrieve 
import Debug.Trace

-- | Takes a list of @'Identifier'@ and an another list of @'Expr'@,
-- returns a __Maybe__ @'[Env]'@
--
-- This function tries to mimic variable definition in a case of a
-- function declaration, binding given identifiers to their values.
--
-- This function is used when detecting a call to a function with parameters
createLocalEnv :: [Identifier] -> [Expr] -> Maybe [Env]
createLocalEnv [] [] = Just []
createLocalEnv (x:xs) (y:ys) = do
    rest <- createLocalEnv xs ys
    case y of
        (Int i)     -> Just (Variable x (Left i):  rest)
        (Boolean i) -> Just (Variable x (Right i): rest)
createLocalEnv _ _ = Nothing

-- | Takes a list of @'Env'@, an @'Expr'@ and a list of @'Expr'@,
-- returns a Maybe @'Expr'@.
--
-- This function tries to parse and evaluate a lambda.
-- /!\ (May be reused when implementing @'named functions'@)
-- binds lambda's parameters to given values,
-- if correct, reduce body of the lambda
tryEvalLambda :: [Env] -> Expr -> [Expr] -> Maybe Expr
tryEvalLambda env (Lambda args bdy) arglist = do
    locEnv <- createLocalEnv args arglist
    reduceExpr (env ++ locEnv) bdy
tryEvalLambda _ _ _ = Nothing

-- | Takes a List of @'Env'@ and an Expr
-- returns an @'Expr'@
--
-- This function is the Core of the interpretor, it is used recursively
-- to effectively reduce the given expression to its minimum,
-- Either an @'Integer'@, a @'Boolean'@, Or @'Nothing'@.
reduceExpr :: [Env] -> Expr -> Maybe Expr
reduceExpr _ e@(Int _)                  = Just e
reduceExpr _ e@(Boolean _)              = Just e
reduceExpr env v@(Var e)                = do
    expr <- checkCallToken env v []
    reduceExpr env expr
reduceExpr env c@(Call f ag)            = do
    agList <- mapM (reduceExpr env) ag
    case checkCallToken env c agList of
        Nothing -> tryEvalLambda env f agList
        Just expr -> case reduceExpr env expr of
            Nothing -> tryEvalLambda env expr agList
            Just a -> Just a
reduceExpr env i@(If cond th el)        = do
    case reduceExpr env cond of
        Just (Boolean True) -> reduceExpr env th
        Just (Boolean False) -> reduceExpr env el
        _ -> Nothing
reduceExpr _ _                          = Nothing 

-- | Takes a List of @'Ast'@, a List of @'Env'@,
-- returns a __Maybe__ @'Ast'@
--
-- This is the main Function of the Interpretor,
-- where we can find function definition, as well as function call,
-- or just expression we need to evaluate.
--
-- Returns the last evaluation.
--
-- Note that an empty @'Ast'@ returns Nothing,
-- for more explanation on this phenomenon :
-- try compiling an empty file with gcc (dumass)
interpret :: [Ast] -> [Env] -> Maybe Ast
interpret [] _                          = Nothing
interpret (Define ex body: ast) env     = case body of
    (Int i)     -> interpret ast (Variable ex (Left i): env)
    (Boolean i) -> interpret ast (Variable ex (Right i): env)
    _           -> interpret ast (DefinedExpr ex body: env)
interpret ((Expression expr): ast) env  = do
    res <- reduceExpr env expr 
    case ast of
        [] -> Just $ Expression res
        _ -> interpret ast env
