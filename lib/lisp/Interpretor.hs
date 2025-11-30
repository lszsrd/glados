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

data Env = Variable String
    | Function FunCall
    deriving Show

searchInEnv :: [Env] -> Ast -> Bool
searchInEnv [] _ = False
searchInEnv (Function(FunCall i1 args1 ast1):xs) fCall@(Call i2 args2) =
    (i1 == i2) || searchInEnv xs fCall
    -- TODO Need to verify types of args
searchInEnv (_:xs) f = searchInEnv xs f


-- Ram through the AST and interpret it  
interpret :: [Ast] -> [Env] -> Maybe Ast
interpret [] _ = Nothing
interpret (Define i ag bd: ast) env =
    interpret ast (env ++ [Function (FunCall i ag bd)])
interpret (call@(Call f args): ast) env = if searchInEnv env call
    then Just $ Var ("Call to " ++ show f ++ " Sucess")
    else Just $ Var ("Call to " ++ show f ++ " Failed")

-- interpretResult :: [Ast] -> IO Ast
-- interpretResult ast = do
--     evalParser <- try (evaluate (interpret ast []))
--         :: IO (Either SomeException Ast)
--     case evalParser of
--         Left err -> printError (show err)
--         Right str -> return str
-- [(Define "foo" ["a", "b"] (Call "+" ["a", "b"]))]



-- BUILTINS

applyBuiltin :: String -> [Integer] -> (Either Integer Bool)

applyBuiltin _ _ = Nothing

applyBuiltin "+" n = Just $ sum n

applyBuiltin "-" n = case n of
    []  -> Nothing
    [x] -> Just (-x)
    (x:xs) -> Just $ x - sum xs

applyBuiltin "*" n = Just $ product n

applyBuiltin "div" n = case n of
    [a,b] -> if b == 0 then Nothing else Just (a `div` b)
    _ -> Nothing


applyBuiltin "mod" n = case n of
    [a,b] -> if b == 0 then Nothing else Just $ Left (a `mod` b)
    _ -> Nothing

applyBuiltin "<" n = case n of
    [a,b] -> Just $ Right (a < b)
    _ -> Nothing

applyBuiltin ">" n = case n of
    [a,b] -> Just $ Right (a > b)
    _ -> Nothing

applyBuiltin "eq?" n = case n of
    [a,b] -> Just $ Right (a == b)
    _ -> Nothing