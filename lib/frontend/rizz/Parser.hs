{-
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/Parser.hs
-}

-- Builds AST from tokens
module Parser (
    parser,
    parseDecl,
    parseTopLevel,
    parseFunctionDecl,
    parseVarDecl
) where

import qualified Ast as A
import qualified Tokens as T
import qualified ParserHelper as H
import qualified ParserStmt as PS

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

parseReturnType :: Parser (Maybe A.BuiltinType)
parseReturnType tokens@((T.Punctuator T.Arrow, _) : rest1) = do
    (returntype, rest2) <- H.parseBuiltinType rest1
    Right(Just returntype, rest2)
parseReturnType toks = Right (Nothing, toks)

parseVarDecl :: Parser A.Decl
parseVarDecl tokens = do
    (vardecl, rest1) <- H.parseVarDeclStmt tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest1
    Right (A.VarDecl vardecl, rest2)

parseFunctionDecl :: Parser A.Decl
parseFunctionDecl tokens = do
    (_, rest0)          <- H.expectToken (T.Keyword T.Fn) "Expected 'Fn'" tokens
    (name, rest1)       <- H.parseIdentifier rest0
    (_, rest2)          <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket)) "expected '('" rest1
    (pvdelist, rest3)   <- H.parsePVDEList rest2
    (_, rest4)          <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest3
    (returntype, rest5) <- parseReturnType rest4
    (compStmt, rest6)   <- PS.parseCompoundStmt rest5
    Right (A.FunctionDecl name pvdelist compStmt returntype, rest6)

parseTopLevel :: Parser A.Decl
parseTopLevel tokens@((T.Keyword T.Fn, _) : _) = parseFunctionDecl tokens
parseTopLevel tokens =
    case H.parseBuiltinType tokens of
        Right _ -> parseVarDecl tokens
        Left error -> Left error

parseDecl :: Parser [A.Decl]
parseDecl [] = Right ([], [])
parseDecl tokens = do
    (decl, xs)       <- parseTopLevel tokens
    (decls, xsfinal) <- parseDecl xs
    Right (decl : decls, xsfinal)

parser :: [SingleToken] -> Either String [A.Decl]
parser tokens =
    case parseDecl tokens of
        Left error -> Left error
        Right (decls, []) -> Right decls
        Right (_, rest) -> Left "0: 0 Parser Exception."
