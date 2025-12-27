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

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

-- Not working just place holder
parseCompoundStmt :: Parser A.CompoundStmt
parseCompoundStmt tokens = do
    (_, rest1) <- H.expectToken (T.Punctuator (T.CBracket T.OpenCBracket)) "expected '{'" tokens
    (_, rest2) <- H.expectToken (T.Punctuator (T.CBracket T.CloseCBracket)) "expected '}'" tokens
    Right (A.CompoundStmt [], rest2)

parseReturnType :: Parser (Maybe A.BuiltinType)
parseReturnType tokens@((T.Punctuator T.Arrow, _) : rest1) = do
    (returntype, rest2) <- H.parseBuiltinType rest1
    Right(Just returntype, rest2)
parseReturnType toks = Right (Nothing, toks)

parseVarDecl :: Parser A.Decl
parseVarDecl tokens = do
    (typ, rest1) <- H.parseBuiltinType tokens
    (name, rest2) <- H.parseIdentifier rest1
    (op, rest3) <- H.parseAssignOp rest2
    (value, rest4) <- H.parseParmCallDecl rest3
    (_, rest5) <- H.expectToken (T.Punctuator T.Semicolon) "expected ';'" rest4
    Right (A.VarDecl (A.VarDeclStmt typ name op value), rest5)

parseFunctionDecl :: Parser A.Decl
parseFunctionDecl tokens = do
    (_, rest0)          <- H.expectToken (T.Keyword T.Fn) "Expected 'Fn'" tokens
    (name, rest1)       <- H.parseIdentifier rest0
    (_, rest2)          <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket)) "expected '('" rest1
    (pvdelist, rest3)   <- H.parsePVDEList rest2
    (_, rest4)          <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest3
    (returntype, rest5) <- parseReturnType rest4
    (compStmt, rest6)   <- parseCompoundStmt rest5
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
        Right (_, rest) -> Left "Parser Exception."
