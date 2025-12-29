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

parseCompoundStmt :: Parser A.CompoundStmt
parseCompoundStmt tokens = do
    (_, rest1) <- H.expectToken (T.Punctuator (T.CBracket T.OpenCBracket)) "expected '{'" tokens
    (stmts, rest2) <- parseStmtList rest1
    (_, rest3) <- H.expectToken (T.Punctuator (T.CBracket T.CloseCBracket)) "expected '}'" rest2
    Right (A.CompoundStmt stmts, rest3)

parseStmtList :: Parser [A.Stmt]
parseStmtList tokens@((T.Punctuator (T.CBracket T.CloseCBracket), _) : _) = Right ([], tokens)
parseStmtList tokens = do
    (stmt, rest1) <- parseStmt tokens
    (stmts, rest2) <- parseStmtList rest1
    Right (stmt : stmts, rest2)

parseStmt :: Parser A.Stmt
parseStmt tokens@((tok, pos) : _) = case tok of
    T.Identifier _ ->
        case tokens of
            ((T.Identifier _, _) : (T.Punctuator (T.RBracket T.OpenRBracket), _) : _) ->
                parseCallExpr tokens
            _ -> parseDeclStmtExpr tokens
    T.Keyword T.Foreach -> parseForeach tokens
    T.Keyword T.While   -> parseWhile tokens
    -- TODO T.Keyword ret/if/while/for etc
    T.Keyword T.Bool    -> parseDeclVarExpr tokens
    T.Keyword T.Char    -> parseDeclVarExpr tokens
    T.Keyword T.Int     -> parseDeclVarExpr tokens
    T.Keyword T.Float   -> parseDeclVarExpr tokens
    _ -> Left $ "Unexpected token in stmt at " ++ show pos
parseStmt [] = Left "Unexpected end of input in statement"

parseCallExpr :: Parser A.Stmt
parseCallExpr tokens = do
    (call, rest1) <- H.parseCallExprDecl tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest1
    Right (A.CallExpr call, rest2)

parseDeclStmtExpr :: Parser A.Stmt
parseDeclStmtExpr tokens = do
    (declstmt, rest1) <- H.parseDeclStmt tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest1
    Right (A.DeclStmt declstmt, rest2)

parseDeclVarExpr :: Parser A.Stmt
parseDeclVarExpr tokens = do
    (vardecl, rest1) <- H.parseVarDeclStmt tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest1
    Right (A.DeclVarExpr vardecl, rest2)

parseForeach :: Parser A.Stmt
parseForeach tokens = do
    (_, rest1) <- H.expectToken (T.Keyword T.Foreach) "Expected 'foreach'" tokens
    (_, rest2) <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket)) "expected '('" rest1
    (container, rest3) <- H.parseIdentifier rest2
    (_, rest4) <- H.expectToken (T.Punctuator T.Colon) "Expected ':'" rest3
    (iterator, rest5) <- H.parseIdentifier rest4
    (_, rest6) <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest5
    (body, rest7) <- parseCompoundStmt rest6
    Right (A.ForeachStmt container iterator body, rest7)

--TODO: add BinaryOPexpr after
parseWhile :: Parser A.Stmt
parseWhile tokens = do
    (_, rest1) <- H.expectToken (T.Keyword T.While) "Expected 'while'" tokens
    (_, rest2) <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket)) "Expected '('" rest1
    (cond, rest3) <- H.parseParmCallDecl rest2
    (_, rest4) <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest3
    (body, rest5) <- parseCompoundStmt rest4
    Right (A.WhileStmt (A.BinaryOpConst cond) body, rest5)

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
