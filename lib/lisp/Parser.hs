{-
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Parser.hs
-}

module Parser (getAST, Ast(..), SExpr(..)) where

import Lexer (Token(..))
import Control.Exception (try, SomeException, evaluate)
import Error

type Identifier = String

data SExpr
  = SExprInt Integer
  | SExprSymbol String
  | SExprList [SExpr]
  deriving (Show, Eq)

data Ast = ExprDefine  Identifier Ast
    |      ExprLambda  Identifier [Ast]
    |      ExprIf      Ast Ast Ast
    |      ExprVar     Identifier
    |      ExprBool    Bool
    |      ExprInteger Integer
    |      ExprApp     Identifier [Ast]
    deriving Show

parseOne :: [Lexer.Token] -> Maybe (SExpr, [Lexer.Token])
parseOne [] = Nothing
parseOne (Constant n : rest) = Just (SExprInt n, rest)
parseOne (Boolean True : rest) = Just (SExprSymbol "#t", rest)
parseOne (Boolean False : rest) = Just (SExprSymbol "#f", rest)
parseOne (Operator op : rest) = Just (SExprSymbol op, rest)
parseOne (Identifier id : rest) = Just (SExprSymbol id, rest)
parseOne (Keyword kw : rest) = Just (SExprSymbol kw, rest)
parseOne (Delimiter "(" : rest) = parseList rest []
parseOne _ = Nothing

parseList :: [Lexer.Token] -> [SExpr] -> Maybe (SExpr, [Lexer.Token])
parseList (Delimiter ")" : rest) elements =
    Just (SExprList elements, rest)
parseList [] elements =
    Nothing
parseList tokens elements =
    case parseOne tokens of
        Nothing -> Nothing
        Just (sexpr, rest) ->
            parseList rest (elements ++ [sexpr])

tokensToSExpr :: [Lexer.Token] -> Maybe SExpr
tokensToSExpr tokens = case parseOne tokens of
    Just (sexpr, []) -> Just sexpr
    _ -> Nothing

convertAllArguments :: [SExpr] -> Maybe [Ast]
convertAllArguments [] = Just []
convertAllArguments (firstArg : restArgs) =
    case sexprToAST firstArg of
        Nothing -> Nothing
        Just firstAst -> case convertAllArguments restArgs of
            Nothing -> Nothing
            Just restAsts -> Just (firstAst : restAsts)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SExprInt n) = Just (ExprInteger n)
sexprToAST (SExprSymbol "#t") = Just (ExprBool True)
sexprToAST (SExprSymbol "#f") = Just (ExprBool False)
sexprToAST (SExprSymbol s) = Just (ExprVar s)
sexprToAST (SExprList []) = Nothing
sexprToAST (SExprList (SExprSymbol funcName : argsSExpr)) =
    case convertAllArguments argsSExpr of
        Nothing -> Nothing
        Just argsAst -> Just (ExprApp funcName argsAst)
sexprToAST _ = Nothing

parsor :: [Lexer.Token] -> Ast
parsor [] = throwErr $ Error.ErrorT { location = 0, message = "No input" }
parsor tokens = case tokensToSExpr tokens of
    Nothing -> throwErr $ Error.ErrorT { location = 0, message = "Pars error" }
    Just sexpr -> case sexprToAST sexpr of
        Nothing -> throwErr $ Error.ErrorT { location = 0, message = "ASTerr" }
        Just ast -> ast

getAST :: [Lexer.Token] -> IO Ast
getAST tkList = do
    evalParser <- try (evaluate (parsor tkList))
        :: IO (Either SomeException Ast)
    case evalParser of
        Left err -> printError (show err)
        Right ast -> return ast
