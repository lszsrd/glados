{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Parser.hs
-}

module Interpretor (interpret) where

-- import Parser (Ast(..))
-- import Control.Exception (try, SomeException, evaluate)
import Debug.Trace

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

searchInEnv :: [Env] -> Ast -> Maybe FunCall
searchInEnv [] _ = Nothing
searchInEnv (Function f1@(FunCall i1 _ _):xs) fCall@(Call i2 _) =
    if i1 == i2
        then Just f1
        else searchInEnv xs fCall
    -- TODO Need to verify types of args
searchInEnv (_:xs) f = searchInEnv xs f

checkArgs :: Args -> Args -> Args -> Maybe Args
checkArgs [] (x:xs) _ = Nothing
checkArgs _ _ [] = Nothing
checkArgs ag1@(x:xs) ag2@(y:ys) ag3@(z:zs) = if x == y
    then Just [z]
    else do 
        found <- checkArgs xs ag2 zs
        rest <- checkArgs ag1 ys ag3
        Just (found ++ rest)
checkArgs _ _ _ = Just []

checkBuiltin :: Ast -> Maybe Identifier
checkBuiltin (Call "+" _) = Just "+"
checkBuiltin (Call "-" _) = Just "-"
checkBuiltin _ = Nothing

-- BUILTINS
applyBuiltin :: String -> [Integer] -> Maybe Ast
applyBuiltin "+" n = Just $ Integer $ sum n
applyBuiltin "-" n = case n of
    []  -> Nothing
    [x] -> Just $ Integer (-x)
    (x:xs) -> Just $ Integer $ x - sum xs
applyBuiltin "*" n = Just $ Integer $ product n
applyBuiltin "div" n = case n of
    [a,b] -> if b == 0 then Nothing else Just $ Integer (a `div` b)
    _ -> Nothing
applyBuiltin "mod" n = case n of
    [a,b] -> if b == 0 then Nothing else Just $ Integer (a `mod` b)
    _ -> Nothing
applyBuiltin "<" n = case n of
    [a,b] -> Just $ Bool (a < b)
    _ -> Nothing
applyBuiltin ">" n = case n of
    [a,b] -> Just $ Bool (a > b)
    _ -> Nothing
applyBuiltin "eq?" n = case n of
    [a,b] -> Just $ Bool (a == b)
    _ -> Nothing

-- Define func (a b ) (+ a b)
eval :: Args -> Ast -> Args -> [Env] -> Maybe Ast
eval expecAg call@(Call id ag) givenAg globEnv = 
    case searchInEnv globEnv call of
        Nothing -> do
            builtin <- checkBuiltin call
            agList <- checkArgs expecAg ag givenAg
            applyBuiltin builtin (map (read::String->Integer) agList)
        Just f -> do
            agList <- checkArgs expecAg ag givenAg
            Nothing
eval _ _ _ _ = Nothing

-- Ram through the AST and interpret it  
interpret :: [Ast] -> [Env] -> Maybe Ast

interpret (Define i ag bd: ast) env =
    interpret ast (env ++ [Function (FunCall i ag bd)])

interpret (call@(Call f args): ast) env = case searchInEnv env call of
    Just (FunCall _ expectedArgs body) -> eval expectedArgs body args env
    Nothing -> Nothing

interpret _ _ = Nothing

-- interpretResult :: [Ast] -> IO Ast
-- interpretResult ast = do
--     evalParser <- try (evaluate (interpret ast []))
--         :: IO (Either SomeException Ast)
--     case evalParser of
--         Left err -> printError (show err)
--         Right str -> return str
-- [(Define "foo" ["a", "b"] (Call "+" ["a", "b"]))]
