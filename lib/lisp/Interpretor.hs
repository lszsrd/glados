{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Parser.hs
-}

module Interpretor (interpret) where

-- import Parser (Ast(..))
-- import Control.Exception (try, SomeException, evaluate)

type Identifier     = String
type Args           = [Identifier]
data FunCall        = FunCall Identifier Args Ast deriving Show

data Ast = Define   Identifier Args Ast
    |      If       FunCall Ast Ast
    |      Var      Identifier
    |      Bool     Bool
    |      Integer  Integer
    |      Call     Identifier Args 
    deriving Show

type Value = String

data Env = Variable String Value
    |      Function FunCall
    deriving Show

searchInEnv :: [Env] -> Ast -> Bool
searchInEnv [] _ = False
searchInEnv (Function(FunCall i1 args1 ast1):xs) fCall@(Call i2 args2) =
    (i1 == i2) || searchInEnv xs fCall
    -- TODO Need to verify types of args
searchInEnv (_:xs) f = searchInEnv xs f

eval :: Ast -> Maybe Ast
eval _ = Just $ Bool True

evalFunc :: FunCall -> Maybe Bool
evalFunc _ = Just False

-- Ram through the AST and interpret it  
interpret :: [Ast] -> [Env] -> Maybe Ast
interpret [] _ = Nothing

interpret (Define i ag bd: ast) env =
    interpret ast (env ++ [Function (FunCall i ag bd)])

interpret (If cond th el: ast) env = do
    condEval <- evalFunc cond
    if condEval then eval th else eval el

interpret (call@(Call f args): ast) env = if searchInEnv env call
    then Just $ Var ("Call to " ++ show f ++ " Sucess")
    else Nothing

-- interpretResult :: [Ast] -> IO Ast
-- interpretResult ast = do
--     evalParser <- try (evaluate (interpret ast []))
--         :: IO (Either SomeException Ast)
--     case evalParser of
--         Left err -> printError (show err)
--         Right str -> return str
-- [(Define "foo" ["a", "b"] (Call "+" ["a", "b"]))]
