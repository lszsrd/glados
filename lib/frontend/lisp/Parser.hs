{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/lisp/Parser.hs
-}

module Parser (
    parser
) where

import Tokens
import Ast

-- TODO: check for unbound variables, codingding style

parser :: Tokens -> Either String [Expr]
parser [] = Right []
parser x = case parseDecl x of
    Left [] -> case parseExpr x of
        Left e -> Left e
        Right (y, ys) -> case parser ys of
            Left e -> Left e
            Right z -> Right $ y: z
    Left e -> Left e
    Right (y, ys) -> case parser ys of
        Left e -> Left e
        Right z -> Right $ y: z

expect :: (Int, Int) -> String -> String -> String
expect (x, x') y z = show x ++ ":" ++ show x' ++ " expected " ++ y
    ++ ", got " ++ z

-- fetch all identifiers for a named define (define (foo [<Identifier>]))
hParseDecl :: (Int, Int) -> Tokens -> Either String ([Identifier], Tokens)
hParseDecl x [] = Left $ expect x "')'" "<EOF>"
hParseDecl x ((Atom (Tokens.Identifier y), _): ys) = case hParseDecl x ys of
    Left e -> Left e
    Right (z, zs) -> Right (y: z, zs)
hParseDecl _ ((RBracket Close, _): xs) = Right ([], xs)
hParseDecl _ ((x, y): _) = Left $ expect y "')'" $ show x

parseDecl :: Tokens -> Either String (Expr, Tokens)
-- (define <Identifier> (lambda ...)) which is a special case
parseDecl ((RBracket Open, _): (Atom (Operator Tokens.Define), _):
  (Atom (Tokens.Identifier x), _): xs@((RBracket Open, _):
  (Atom (Operator Tokens.Lambda), _): xs'))
    = case parseLambda xs of
        Left e -> Left e
        Right (y, ys) -> case ys of
            ((RBracket Close, _): zs) -> Right (Defun $ Ast.Define x y, zs)
            [] -> Left $ expect (snd $ last xs') "')'" "<EOF>"
            ((z, zs): _) -> Left $ expect zs "')'" $ show z
-- (define <Identifier> ([<Identifier>]) Expr)
parseDecl ((RBracket Open, _): (Atom (Operator Tokens.Define), _):
  (Atom (Tokens.Identifier a), _): (RBracket Open, x): xs)
    = case hParseDecl x xs of
        Left e -> Left e
        Right ([], _) -> Left $ expect (snd $ head xs) "<argument(s)>" "<none>"
        Right (y: y', ys) -> case parseExpr ys of
            Left e -> Left e
            Right (z, zs) -> case zs of
                ((RBracket Close, _): zs')
                    -> Right (Defun $ Ast.Define a (Defun $ Func y y' z), zs')
                [] -> Left $ expect (snd $ head zs) "')'" "<EOF>"
                ((z', zs'): _) -> Left $ expect zs' "')'" $ show z'
-- (define <Identifier> <Atom>)
parseDecl ((RBracket Open, _): (Atom (Operator Tokens.Define), _):
  (Atom (Tokens.Identifier x), _): xs)
    = case parseExpr xs of
        Left e -> Left e
        Right (y, ys) -> case ys of
            ((RBracket Close, _): zs) -> Right (Defun $ Ast.Define x y, zs)
            [] -> Left $ expect (snd $ head ys) "')" "<EOF>"
            ((z, zs): _) -> Left $ expect zs "')'" $ show z
parseDecl _ = Left []

parseExpr :: Tokens -> Either String (Expr, Tokens)
parseExpr tokens = case parseConstExpr tokens of
    Nothing -> case parseLambda tokens of
        Left [] -> case parseIfExpr tokens of
            Left [] -> case parseBinaryExpr tokens of
                Left [] -> parseCallExpr tokens
                x -> x
            x -> x
        x -> x
    Just x -> Right x

parseConstExpr :: Tokens -> Maybe (Expr, Tokens)
parseConstExpr ((Atom (Bool x), _): xs) = Just (Const (Boolean x), xs)
parseConstExpr ((Atom (Integer x), _): xs) = Just (Const (Int x), xs)
parseConstExpr ((Atom (Float x), _): xs) = Just (Const (SPrecision x), xs)
parseConstExpr ((Atom (Tokens.Identifier x), _): xs) =
    Just (Const (Ast.Identifier x), xs)
parseConstExpr _ = Nothing

parseIfExpr :: Tokens -> Either String (Expr, Tokens)
parseIfExpr ((RBracket Open, _): (Atom (Operator Tokens.If), _):
  (Atom (Bool x), _): xs) = do
    (y, ys) <- parseExpr xs
    case parseExpr ys of
        Left _ -> Left $ expect (snd $ head ys) "<else>" $ show (fst $ head ys)
        Right (z, zs) -> case zs of
            ((RBracket Close, _): zs') -> Right (Ast.If (OpBool x) y z, zs')
            [] -> Left $ expect (snd $ head zs) "')'" "<EOF>"
            ((z', zs'): _) -> Left $ expect zs' "')'" $ show z'
parseIfExpr ((RBracket Open, _): (Atom (Operator Tokens.If), _): xs)
    = case parseBinaryExpr xs of
        Left e -> Left e
        Right (BinaryOp y ifExpr elseExpr, ys) -> case y of
            CondExpr condition -> Right (Ast.If condition ifExpr elseExpr, ys)
            _ -> Left $ expect (snd $ head xs) "<condition>" $ show y
        _ -> Left $ expect (snd $ head xs) "<condition>" $ show (fst $ head xs)
parseIfExpr _ = Left []

hParseCallExpr :: Tokens -> Either String ([Expr], Tokens)
hParseCallExpr xs = case parseExpr xs of
    Left e -> Left e
    Right (y, ys) -> case hParseCallExpr ys of
        Left _ -> Right ([y], ys)
        Right (z, zs) -> Right (y: z, zs)

parseCallExpr :: Tokens -> Either String (Expr, Tokens)
parseCallExpr ((RBracket Open, _): (Atom (Tokens.Identifier x), _): xs)
    = case hParseCallExpr xs of
        Left e -> Left e
        Right (_, []) -> Left $ expect (snd $ last xs) "')'" "<EOF>"
        Right (y, ys) -> case ys of
            ((RBracket Close, _): zs) -> Right (Call x y, zs)
            ((z, zs): _) -> Left $ expect zs "')'" $ show z
parseCallExpr _ = Left []

hParseBinaryExpr :: Operator -> Maybe BinaryOperator
hParseBinaryExpr Add = Just (ArithExpr OpAdd)
hParseBinaryExpr Sub = Just (ArithExpr OpSub)
hParseBinaryExpr Mul = Just (ArithExpr OpMul)
hParseBinaryExpr Div = Just (ArithExpr OpDiv)
hParseBinaryExpr Mod = Just (ArithExpr OpMod)
hParseBinaryExpr Lt = Just (CondExpr OpLt)
hParseBinaryExpr Eq = Just (CondExpr OpEq)
hParseBinaryExpr _ = Nothing

parseBinaryExpr :: Tokens -> Either String (Expr, Tokens)
parseBinaryExpr ((RBracket Open, _): (Atom (Operator x), y): xs) = do
    (a, as) <- parseExpr xs
    (b, bs) <- parseExpr as
    case bs of
        ((RBracket Close, _): zs) -> case hParseBinaryExpr x of
            Nothing -> Left $ expect y "<operator>" $ show x
            Just z -> Right (BinaryOp z a b, zs)
        [] -> Left $ expect (snd $ head bs) "')'" "<EOF>"
        ((z, zs): _) -> Left $ expect zs "')'" $ show z
parseBinaryExpr _ = Left []

-- (lambda ([<Identifier>]) Expr)
parseLambda :: Tokens -> Either String (Expr, Tokens)
parseLambda ((RBracket Open, _): (Atom (Operator Tokens.Lambda), _):
  (RBracket Open, x): xs)
    = case hParseDecl x xs of
        Left e -> Left e
        Right (y, ys) -> case parseExpr ys of
            Left e -> Left e
            Right (z, (RBracket Close, _): zs)
                -> Right (Defun $ Ast.Lambda y z, zs)
            _ -> Left $ expect (snd $ head ys) "')'" $ show (fst $ head ys)
parseLambda _ = Left []
