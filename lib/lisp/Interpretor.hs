{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Parser.hs
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
    deriving Show

-- BUILTINS
-- applyBuiltin :: String -> [Integer] -> Maybe Expr
-- applyBuiltin "+" n = Just $ Int $ sum n
-- applyBuiltin "-" n = case n of
--     []  -> Nothing
--     [x] -> Just $ Int (-x)
--     (x:xs) -> Just $ Int $ x - sum xs
-- applyBuiltin "*" n = Just $ Int $ product n
-- applyBuiltin "div" n = case n of
--     [a,b] -> if b == 0 then Nothing else Just $ Int (a `div` b)
--     _ -> Nothing
-- applyBuiltin "mod" n = case n of
--     [a,b] -> if b == 0 then Nothing else Just $ Int (a `mod` b)
--     _ -> Nothing
-- applyBuiltin "<" n = case n of
--     [a,b] -> Just $ Boolean (a < b)
--     _ -> Nothing
-- applyBuiltin ">" n = case n of
--     [a,b] -> Just $ Boolean (a > b)
--     _ -> Nothing
-- applyBuiltin "eq?" n = case n of
--     [a,b] -> Just $ Boolean (a == b)
--     _ -> Nothing

builtinToken :: [Identifier]
builtinToken = [ "+", "-", "*", "div", "mod"]

builtinComparisonToken :: [Identifier]
builtinComparisonToken = [ ">", "<", "eq?"]

checkCallToken :: Expr -> [Identifier] -> Maybe Identifier
checkCallToken fcall@(Call (Var id) _) (x:xs)
    | id == x = Just id
    | otherwise = checkCallToken fcall xs
checkCallToken _ [] = Nothing

searchInEnv :: [Env] -> Expr -> Maybe Expr
searchInEnv [] _ = Nothing
searchInEnv ((DefinedExpr i1 _):xs) fCall@(Call (Var i2) _) =
    if i1 == i2
        then Just fCall
        else searchInEnv xs fCall
    -- TODO Need to verify types of args
searchInEnv (_:xs) f = searchInEnv xs f

eval :: Expr -> [Env] -> Maybe Expr
eval call@(Call (Lambda id bdy) ag) globEnv = Nothing
eval call@(Call (Var id) ag) globEnv = 
    case searchInEnv globEnv call of
        Nothing -> do
            builtin <- checkCallToken call builtinToken
            -- applyBuiltin builtin ag
            Nothing
        Just f -> Nothing
eval _ _ = Nothing

-- Ram through the AST and interpret it  
interpret :: [Ast] -> [Env] -> Maybe Ast

interpret (Define ex body: ast) env = interpret ast (DefinedExpr ex body: env)
interpret ((Expression f1@(Call id args)): ast) env = do
    expr <- eval f1 env
    interpret ast env
interpret (Expression (Lambda args bdy): ast) env = Nothing -- Scan env to find FunctionCall and evalualte it with given args
interpret (Expression (If cond th el): ast) env = Nothing -- Eval cond, if true, exec th else el
interpret _ _ = Nothing


-- checkArgs :: Args -> Args -> Args -> Maybe Args
-- checkArgs [] (x:xs) _ = Nothing
-- checkArgs _ _ [] = Nothing
-- checkArgs ag1@(x:xs) ag2@(y:ys) ag3@(z:zs) = if x == y
--     then Just [z]
--     else do 
--         found <- checkArgs xs ag2 zs
--         rest <- checkArgs ag1 ys ag3
--         Just (found ++ rest)
-- checkArgs _ _ _ = Just []
--
--

-- interpretResult :: [Ast] -> IO Ast
-- interpretResult ast = do
--     evalParser <- try (evaluate (interpret ast []))
--         :: IO (Either SomeException Ast)
--     case evalParser of
--         Left err -> printError (show err)
--         Right str -> return str
-- [(Define "foo" ["a", "b"] (Call "+" ["a", "b"]))]
