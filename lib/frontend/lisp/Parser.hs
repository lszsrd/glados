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

-- TODO: lambdas, check for unbound variables, pretty format & codingding style

parser :: Tokens -> Either String [Expr]
parser [] = Right []
parser tokens = case parseDecl tokens of
    Left [] -> case parseExpr tokens of
        Left [] -> Left $ errorAt (snd $ head tokens) "unknown expression"
        Left e -> Left e
        Right (expr, toks) -> case parser toks of
            Left e -> Left e
            Right exprs -> Right $ expr: exprs
    Left e -> Left e
    Right (expr, toks) -> case parser toks of
        Left e -> Left e
        Right exprs -> Right $ expr: exprs

errorAt :: (Int, Int) -> String -> String
errorAt (l, c) message = show l ++ ":" ++ show c ++ " " ++ message

-- fetch all identifiers for a named define (define (foo [<Identifier>]))
hParseDecl :: (Int, Int) -> Tokens -> Either String ([Identifier], Tokens)
hParseDecl p [] = Left $ errorAt p "expected ')' #EOF"
hParseDecl p ((Atom (Tokens.Identifier x), _): xs) = case hParseDecl p xs of
    Left e -> Left e
    Right (identifier, xs') -> Right (x: identifier, xs')
hParseDecl _ ((RBracket Close, _): xs) = Right ([], xs)
hParseDecl _ ((x, y): _) = Left $ errorAt y "expected ')', got " ++ show x

parseDecl :: Tokens -> Either String (Expr, Tokens)
-- (define <Identifier> ([<Identifier>]) Expr)
parseDecl ((RBracket Open, _): (Atom (Operator Tokens.Define), _): (Atom (
 Tokens.Identifier y), _): (RBracket Open, p'): xs) = case hParseDecl p' xs of
        Left e -> Left e
        Right ([], _) -> Left $ errorAt (snd $ head xs) "expected arguments"
        Right (y': y'', xs') -> case parseExpr xs' of
            Left e -> Left e
            Right (z, tokens) -> case tokens of
                ((RBracket Close, _): toks) -> Right
                    (Defun $ Ast.Define y (Defun $ Func y' y'' z), toks)
                [] -> Left $ errorAt (snd $ head tokens) "expected ')' #EOF"
                ((x, p): _) -> Left $ errorAt p "expected ')', got " ++ show x
-- (define <Identifier> <Atom>)
parseDecl ((RBracket Open, _): (Atom (Operator Tokens.Define), _):
    (Atom (Tokens.Identifier x), _): xs)
    = case parseExpr xs of
        Left e -> Left e
        Right (expr, toks) -> case toks of
            ((RBracket Close, _): ys) -> Right (Defun $ Ast.Define x expr, ys)
            [] -> Left $ errorAt (snd $ head toks) "expected ')' #EOF"
            ((tok, y): _) -> Left $ errorAt y "expected ')', got " ++ show tok
parseDecl _ = Left []

parseExpr :: Tokens -> Either String (Expr, Tokens)
parseExpr tokens = case parseConstExpr tokens of
    Nothing -> case parseIfExpr tokens of
        Left [] -> case parseBinaryExpr tokens of
            Left [] -> parseCallExpr tokens
            x -> x
        x -> x
    Just x -> Right x

parseConstExpr :: Tokens -> Maybe (Expr, Tokens)
parseConstExpr ((Atom (Bool x), _): xs) = Just (Const (Boolean x), xs)
parseConstExpr ((Atom (Integer x), _): xs) = Just (Const (Int x), xs)
parseConstExpr ((Atom (Float x), _): xs) = Just (Const (SPrecision x), xs)
parseConstExpr ((Atom (Tokens.Identifier x), _): xs)
    = Just (Const (Ast.Identifier x), xs)
parseConstExpr _ = Nothing

parseIfExpr :: Tokens -> Either String (Expr, Tokens)
parseIfExpr ((RBracket Open, _): (Atom (Operator Tokens.If), _):
    (Atom (Bool x), _): xs) = do
    (y, ys) <- parseExpr xs
    case parseExpr ys of
        Left _ -> Left $ errorAt (snd $ head ys) "expected #\\<else\\>, got "
            ++ show (fst $ head ys)
        Right (z, zs) -> case zs of
            ((RBracket Close, _): zs') -> Right (Ast.If (OpBool x) y z, zs')
            [] -> Left $ errorAt (snd $ head zs) "expected ')' #EOF"
            ((tok, p): _) -> Left $ errorAt p "expected ')', got " ++ show tok
parseIfExpr ((RBracket Open, _): (Atom (Operator Tokens.If), _): xs) =
    case parseBinaryExpr xs of
        Left e -> Left e
        Right (BinaryOp x ifExpr elseExpr, xs') -> case x of
            CondExpr condition -> Right (Ast.If condition ifExpr elseExpr, xs')
            _ -> Left $ errorAt (snd $ head xs)
                "expected #\\<condition\\>, got " ++ show x
        _ -> Left $ errorAt (snd $ head xs) "expected #\\<condition\\>, got "
            ++ show (fst $ head xs) ++ "'"
parseIfExpr _ = Left []

hParseCallExpr :: Tokens -> Either String ([Expr], Tokens)
hParseCallExpr tokens = case parseExpr tokens of
    Left e -> Left e
    Right (x, xs) -> case hParseCallExpr xs of
        Left _ -> Right ([x], xs)
        x' -> x'

parseCallExpr :: Tokens -> Either String (Expr, Tokens)
parseCallExpr ((RBracket Open, _): (Atom (Tokens.Identifier x), _): xs)
    = case hParseCallExpr xs of
        Left e -> Left e
        Right (exprs, []) -> Left
            $ errorAt (snd $ xs !! length exprs) "expected ')' #EOF"
        Right (exprs, toks) -> case toks of
            ((RBracket Close, _): xs') -> Right (Call x exprs, xs')
            ((tok, p): _) -> Left $ errorAt p "expected ')', got " ++ show tok
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
parseBinaryExpr ((RBracket Open, _): (Atom (Operator x), p): xs) = do
    (x', xs') <- parseExpr xs
    (x'', xs'') <- parseExpr xs'
    case xs'' of
        ((RBracket Close, _): tokens) -> case hParseBinaryExpr x of
            Nothing -> Left
                        $ errorAt p "expected #\\<operator\\>, got " ++ show x
            Just op -> Right (BinaryOp op x' x'', tokens)
        [] -> Left $ errorAt (snd $ head xs'') "expected ')' #EOF"
        ((tok, p'): _) -> Left $ errorAt p' "expected ')', got " ++ show tok
parseBinaryExpr _ = Left []
