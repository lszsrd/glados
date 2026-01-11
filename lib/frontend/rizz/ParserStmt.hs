{-
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/ParserStmt.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : ParserStmt
-- Description : Parses statements and compound statements into Abstract Syntax Tree nodes.
-- License     : MIT
-- Maintainer  : maxence.pierre@epitech.eu, hugo.duda@epitech.eu, florian.grave@epitech.eu
--
-- Handles the parsing of statements including if/else, while loops, for loops, return statements, and compound statements.
--
-- If an unexpected token is found or if the syntax is invalid, the parsing functions return an pretty formatted error message.
-------------------------------------------------------------------------------
module ParserStmt (
    parseCompoundStmt
) where

import qualified Ast as A
import qualified Tokens as T
import qualified ParserHelper as H
import ParserBinaryExpr (parseBinaryOpExpr)

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

parseTernaryOperation :: A.Decl -> Parser A.Stmt
parseTernaryOperation f ((T.Punctuator (T.RBracket T.OpenRBracket), _) : r)
    = do
    (binaryOpExpr, rest2) <- parseBinaryOpExpr f r
    (_, rest3) <- H.expectToken
        (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest2
    (_, rest4) <- H.expectToken (T.Punctuator T.QMark) "Expected '?'" rest3
    (compoundStmt1, rest5) <- parseCompoundStmt f rest4
    (_, rest6) <- H.expectToken (T.Punctuator T.Colon) "Expected ':'" rest5
    (compoundStmt2, rest7) <- parseCompoundStmt f rest6
    Right (A.IfStmt binaryOpExpr compoundStmt1 (Just compoundStmt2), rest7)
parseTernaryOperation _ e = H.errorAt (1, 1)
    ("Expected Ternary, got " ++ show (head e))

parseStmtList :: A.Decl -> Parser [A.Stmt]
parseStmtList _ tokens@((T.Punctuator (T.CBracket T.CloseCBracket), _) : _)
    = Right ([], tokens)
parseStmtList f@(A.FunctionDecl n pvdelist bdy ret) tokens = do
    (stmt, rest1) <- parseStmt f tokens
    newParms <- H.addIfVarExpr (H.getPos 0 tokens) stmt pvdelist
    (stmts, rest2) <- parseStmtList (A.FunctionDecl n newParms bdy ret) rest1
    Right (stmt : stmts, rest2)
parseStmtList _ e = H.errorAt (1, 1)
    ("Expected Ternary, got " ++ show (head e))

parseCompoundStmt :: A.Decl -> Parser A.CompoundStmt
parseCompoundStmt f tokens = do
    (_, rest1) <- H.expectToken (T.Punctuator (T.CBracket T.OpenCBracket))
        "expected '{'" tokens
    (stmts, rest2) <- parseStmtList f rest1
    (_, rest3) <- H.expectToken (T.Punctuator (T.CBracket T.CloseCBracket))
        "expected '}'" rest2
    Right (A.CompoundStmt stmts, rest3)


parseBuiltinTypes :: A.Decl -> Parser A.Stmt
parseBuiltinTypes f tokens@((tok, pos) : _) = case tok of
    T.Keyword T.Bool    -> parseDeclVarExpr f tokens
    T.Keyword T.Char    -> parseDeclVarExpr f tokens
    T.Keyword T.Int     -> parseDeclVarExpr f tokens
    T.Keyword T.Float   -> parseDeclVarExpr f tokens
    _ -> H.errorAt pos "Unexpected token in stmt"
parseBuiltinTypes _ [] =
    H.errorAt (1, 1) "Unexpected end of input in statement"

parseKeywords :: A.Decl -> Parser A.Stmt
parseKeywords f tokens@((tok, _) : _) = case tok of
    T.Keyword T.Foreach                      -> parseForeach f tokens
    T.Keyword T.While                        -> parseWhile f tokens
    T.Keyword T.Ret                          -> parseRet f tokens
    T.Keyword T.If                           -> parseIf f tokens
    T.Keyword T.For                          -> parseFor f tokens
    T.Punctuator (T.SBracket T.OpenSBracket) -> parseDeclVarExpr f tokens
    T.Punctuator (T.RBracket T.OpenRBracket) -> parseTernaryOperation f tokens
    _ -> parseBuiltinTypes f tokens
parseKeywords _ [] = H.errorAt (1, 1) "Unexpected end of input in statement"

parseStmt :: A.Decl -> Parser A.Stmt
parseStmt f tokens@((tok, _) : _) = case tok of
    T.Identifier _ ->
        case tokens of
            ((T.Identifier _, _) :
                (T.Punctuator (T.RBracket T.OpenRBracket), _) : _) ->
                parseCallExpr f tokens
            _ -> parseDeclStmtExpr f tokens
    _ -> parseKeywords f tokens
parseStmt _ [] = H.errorAt (1, 1) "Unexpected end of input in statement"

parseCallExpr :: A.Decl -> Parser A.Stmt
parseCallExpr f tokens = do
    (call, rest) <- H.parseCallExprDecl f tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    Right (A.CallExpr call, rest2)

parseDeclStmtExpr :: A.Decl -> Parser A.Stmt
parseDeclStmtExpr f tokens = do
    (declstmt, rest) <- H.parseDeclStmt f tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    Right (A.DeclStmt declstmt, rest2)

parseDeclVarExpr :: A.Decl -> Parser A.Stmt
parseDeclVarExpr f tokens = do
    (vardecl, rest) <- H.parseVarDeclStmt f tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    Right (A.DeclVarExpr vardecl, rest2)

parseRet :: A.Decl -> Parser A.Stmt
parseRet f@(A.FunctionDecl _ _ _ (Just _)) tokens = do
    (_, rest)      <- H.expectToken (T.Keyword T.Ret) "Expected 'ret'" tokens
    (expr, rest1) <- parseBinaryOpExpr f rest
    (_, r2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest1
    Right (A.RetStmt expr, r2)
parseRet (A.FunctionDecl _ _ _ Nothing) ((_, pos): _) =
    H.errorAt pos "Function return type not specified"
parseRet _ e = H.errorAt (1,1) ("Expected Ret stmt, got " ++ show e)

parseIfH :: A.Decl -> Parser A.BinaryOpExpr
parseIfH f tokens = do
    (_, brkNRest) <- H.expectToken (T.Keyword T.If) "Expected 'if'" tokens
    (_, rest) <- H.expectToken
        (T.Punctuator (T.RBracket T.OpenRBracket)) "Expected '('" brkNRest
    (cond, rest1)    <- parseBinaryOpExpr f rest
    (_, rest2) <- H.expectToken
        (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest1
    Right (cond, rest2)

parseIf :: A.Decl -> Parser A.Stmt
parseIf f tokens = do
    (cond, rest)   <- parseIfH f tokens 
    (bdy, rest2)    <- parseCompoundStmt f rest
    case rest2 of
        (T.Keyword T.Else,_) : elseBdyNrest -> do
            (elseBdy, rest4) <- parseCompoundStmt f elseBdyNrest
            Right (A.IfStmt cond bdy (Just elseBdy), rest4)
        _ -> Right (A.IfStmt cond bdy Nothing, rest2)

parseForBody :: Parser a -> Parser (Maybe a)
parseForBody p tokens = do
    (op, rest) <- H.parseMaybe p tokens
    (_, rest1) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    Right (op, rest1)

parseTemp :: A.Decl -> Parser (Maybe A.VarDeclStmt, A.Decl)
parseTemp f@(A.FunctionDecl n parms bdy ret) tokens = do
    (decl, rest) <- H.parseMaybe (H.parseVarDeclStmt f) tokens
    (_, rest1) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    case decl of
        Nothing -> Right ((Nothing, f), rest1)
        Just st -> do
            newP <- H.addIfVarExpr (H.getPos 0 tokens) (A.DeclVarExpr st) parms
            Right ((decl, A.FunctionDecl n newP bdy ret), rest1)
parseTemp _ e = H.errorAt (1, 1) ("Expected Expression, got " ++ show (head e))

parseForH :: A.Decl -> Parser (Maybe A.VarDeclStmt, A.Decl)
parseForH f tokens = do
    (_, rest)      <- H.expectToken (T.Keyword T.For) "Expected 'for'" tokens
    (_, vDeclNRest) <- H.expectToken
        (T.Punctuator (T.RBracket T.OpenRBracket)) "Expected '('" rest
    parseTemp f vDeclNRest

parseFor :: A.Decl -> Parser A.Stmt
parseFor f tokens = do
    ((vDecl, newF) , rest) <- parseForH f tokens
    (binOp, rest1) <- parseForBody (parseBinaryOpExpr newF) rest
    (decl, rest2)   <- H.parseMaybe (H.parseDeclStmt newF) rest1
    (_, bdyNRest) <- H.expectToken
        (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest2
    (bdy, rest3)    <- parseCompoundStmt newF bdyNRest
    Right (A.ForStmt vDecl binOp decl bdy, rest3)

parseForeachH :: Parser T.Identifier
parseForeachH t = do
    (_, rest1) <- H.expectToken (T.Keyword T.Foreach) "Expected 'foreach'" t
    (_, rest2) <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket))
        "Expected '('" rest1
    (container, rest3) <- H.parseIdentifier rest2
    (_, rest4) <- H.expectToken (T.Punctuator T.Colon) "Expected ':'" rest3
    Right (container, rest4)

parseForeach :: A.Decl -> Parser A.Stmt
parseForeach f tokens = do
    (container, rest1) <- parseForeachH tokens
    (iterator, rest2) <- H.parseIdentifier rest1
    (_, rest3) <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket))
        "Expected ')'" rest2
    (body, rest4) <- parseCompoundStmt f rest3
    Right (A.ForeachStmt container iterator body, rest4)

parseWhile :: A.Decl -> Parser A.Stmt
parseWhile f tokens = do
    (_, rest1) <- H.expectToken (T.Keyword T.While) "Expected 'while'" tokens
    (_, binOpNrest) <- H.expectToken
        (T.Punctuator (T.RBracket T.OpenRBracket)) "Expected '('" rest1
    (cond, rest2) <- parseBinaryOpExpr f binOpNrest
    (_, bdyNrest) <- H.expectToken
        (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest2
    (body, rest3) <- parseCompoundStmt f bdyNrest
    Right (A.WhileStmt cond body, rest3)
