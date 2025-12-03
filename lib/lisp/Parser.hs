{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Parser.hs
-}

module Parser (getAST, Ast(..)) where

import Lexer (Token(..))
import Control.Exception (try, SomeException, evaluate)
import Error

type Identifier = String
type List       = [Ast]
type Parser a   = [Token] -> Either ErrorT (a, [Token])

data Expr = Lambda [Identifier] Expr
  | If             Expr Expr Expr
  | Call           Expr [Expr]
  | Var            Identifier
  | Boolean        Bool
  | Int            Integer
  deriving Show

data Ast = Define Identifier Expr
  | Expression    Expr
  | LexerToken    Token
  deriving Show

defineHandler :: Parser Ast
defineHandler (Lexer.Identifier name : xs) = do
    (astExpr, rest) <- parseExpression xs
    case (astExpr, rest) of
        (Expression expr, Lexer.Delimiter ")" : remaining) -> Right (Define name expr, remaining)
        _ -> Left $ ErrorT {location = 0, message = "Missing ')'"}
defineHandler _ = Left $ ErrorT {location = 0, message = "Invalid define"}

lambdaHandler :: Parser Ast


lambdaHandler _ = Left $ ErrorT {location = 0, message = "Invalid lambda"}

isIdentifierOrConstant :: Token -> Maybe Token
isIdentifierOrConstant (Lexer.Identifier x) = Just(Lexer.Identifier x)
isIdentifierOrConstant (Lexer.Constant x)   = Just(Lexer.Constant x)
isIdentifierOrConstant _                    = Nothing

ifHandler :: Parser Ast
ifHandler _ = Left $ ErrorT {location = 0, message = "Invalid if Operator"}

callHandler :: Parser Ast
callHandler tokens = Left $ ErrorT {location = 0, message = "Invalid function call"}

parseKeywordExpression :: Parser Ast
parseKeywordExpression (Lexer.Keyword "define" : xs) = defineHandler xs
parseKeywordExpression (Lexer.Keyword "lambda" : xs)                = lambdaHandler xs
parseKeywordExpression (Lexer.Keyword "if" : xs)                    = ifHandler xs
parseKeywordExpression expression                                   = callHandler expression

parseExpression :: Parser Ast
parseExpression (Lexer.Constant c : xs)    = Right (Expression (Int c), xs)
parseExpression (Lexer.Boolean b : xs)     = Right (Expression (Parser.Boolean b), xs)
parseExpression (Lexer.Identifier i : xs)  = Right (Expression (Var i), xs)
parseExpression (Lexer.Operator o : xs)    = Right (Expression (Var o), xs)
parseExpression (Lexer.Delimiter "(" : xs) = parseKeywordExpression xs
parseExpression (Lexer.Delimiter ")" : xs) = Right (LexerToken (Lexer.Delimiter ")"), xs)


parseList :: Parser List
parseList [] = Right ([], [])
parseList tokens = do
    (expression, xs)       <- parseExpression tokens
    (expressions, xsfinal) <- parseList       xs
    Right (expression : expressions, xsfinal)

-- Parse and create the AST with the given TokenList
parsor :: [Token] -> Either ErrorT List
parsor tokens =
    case parseList tokens of
        Left err         -> Left err
        Right (list, []) -> Right list
        Right (_, _)     -> Left $ ErrorT {location = 0, message = "Can't parse all tokens"}

getAST :: [Token] -> IO List
getAST tkList =
    case parsor tkList of
        Left err -> printError (show err)
        Right ast -> return ast
