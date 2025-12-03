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
builtinToken = [ "+", "-", "*", "div", "mod"]

builtinComparisonToken :: [Identifier]
builtinComparisonToken = [ ">", "<", "eq?"]

checkCallToken :: Expr -> [Identifier] -> Maybe Identifier
checkCallToken (Call (Var id) _) (x:xs)
    | id == x = Just id
    | otherwise = checkCallToken (Call (Var id) []) xs
checkCallToken _ [] = Nothing
checkCallToken _ _  = Nothing

lookupDefined :: [Env] -> Identifier -> Maybe Expr
lookupDefined [] _ = Nothing
lookupDefined (DefinedExpr name body : xs) key
    | name == key = Just body
    | otherwise   = lookupDefined xs key

evalArgToInt :: Expr -> [Env] -> Maybe Integer
evalArgToInt (Int n) _ = Just n
evalArgToInt (Var name) env = case lookupDefined env name of
    Just (Int n) -> Just n
    _ -> Nothing
evalArgToInt call@(Call (Var fname) args) env = do
    b <- checkCallToken call (builtinToken ++ builtinComparisonToken)
    ints <- mapM (`evalArgToInt` env) args
    resExpr <- applyBuiltin b ints
    case resExpr of
        Int n -> Just n
        _     -> Nothing
evalArgToInt _ _ = Nothing


evalArgs :: [Expr] -> [Env] -> Maybe [Expr]
evalArgs [] _ = Just []
evalArgs (a:as) env = do
    av <- eval a env
    rest <- evalArgs as env
    return (av:rest)

eval :: Expr -> [Env] -> Maybe Expr

eval e@(Int _) _ = Just e
eval e@(Boolean _) _ = Just e
eval l@(Lambda _ _) _ = Just l

-- eval (If cond th el) env = do
--     c <- eval cond env
--     case c of
--         Boolean True  -> eval th env
--         Boolean False -> eval el env
--         _             -> Nothing


interpret :: [Ast] -> [Env] -> Maybe Ast
interpret [] _ = Nothing
interpret (Define ex body: ast) env = interpret ast (DefinedExpr ex body: env)
interpret ((Expression f1@(Call _ _)): ast) env = do
    res <- eval f1 env
    return (Expression res)
interpret ((Expression expr): ast) env = do
    res <- eval expr env
    return (Expression res)






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


-- searchInEnv :: [Env] -> Expr -> Maybe Expr
-- searchInEnv [] _ = Nothing
-- searchInEnv ((DefinedExpr i1 _):xs) fCall@(Call (Var i2) _) =
--     if i1 == i2
--         then Just fCall
--         else searchInEnv xs fCall
-- searchInEnv (_:xs) f = searchInEnv xs f

-- TESTS