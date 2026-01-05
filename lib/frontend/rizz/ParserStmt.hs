{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/ParserStmt.hs
-}

module ParserStmt (
    parseCompoundStmt
) where

import qualified Ast as A
import qualified Tokens as T
import qualified ParserHelper as H

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

parseBinaryOpParm :: Parser A.BinaryOpParm
parseBinaryOpParm tokens = case tokens of
    ((T.Punctuator (T.RBracket T.OpenRBracket), _) : rest) -> do
        (e, rest1) <- parseBinaryOpExpr rest
        (_, rest2) <- H.expectToken
            (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest1
        Right (A.BinaryOpParmBOp e, rest2)
    _ -> do
        (p, rest) <- H.parseParmCallDecl tokens
        Right (A.BinaryOpParm p, rest)

parseBinaryOpExpr :: Parser A.BinaryOpExpr
parseBinaryOpExpr tokens@(_ : (T.Punctuator (T.BinaryOp op), _) : rest) = do
    (parm1, _) <- parseBinaryOpParm tokens
    (parm2, rest3) <- parseBinaryOpParm rest
    Right (A.BinaryOpExpr parm1 op parm2, rest3)
parseBinaryOpExpr tokens = do
    (parm1, rest) <- parseBinaryOpParm tokens
    case H.parseBinaryOp rest of
        Left err -> do
            (const, _) <- H.parseParmCallDecl tokens
            Right (A.BinaryOpConst const, rest)
        Right (binop, rest2) -> do
            (parm2, rest3) <- parseBinaryOpParm rest2
            Right (A.BinaryOpExpr parm1 binop parm2, rest3)

parseStmtList :: Parser [A.Stmt]
parseStmtList tokens@((T.Punctuator (T.CBracket T.CloseCBracket), _) : _)
    = Right ([], tokens)
parseStmtList tokens = do
    (stmt, rest1) <- parseStmt tokens
    (stmts, rest2) <- parseStmtList rest1
    Right (stmt : stmts, rest2)

parseCompoundStmt :: Parser A.CompoundStmt
parseCompoundStmt tokens = do
    (_, rest1) <- H.expectToken (T.Punctuator (T.CBracket T.OpenCBracket))
        "expected '{'" tokens
    (stmts, rest2) <- parseStmtList rest1
    (_, rest3) <- H.expectToken (T.Punctuator (T.CBracket T.CloseCBracket))
        "expected '}'" rest2
    Right (A.CompoundStmt stmts, rest3)

parseStmt :: Parser A.Stmt
parseStmt tokens@((tok, pos) : _) = case tok of
    T.Identifier _ ->
        case tokens of
            ((T.Identifier _, _) : (T.Punctuator (T.RBracket T.OpenRBracket), _) : _) ->
                parseCallExpr tokens
            _ -> parseDeclStmtExpr tokens
    T.Keyword T.Foreach -> parseForeach tokens
    T.Keyword T.While   -> parseWhile tokens
    T.Keyword T.Ret     -> parseRet tokens
    T.Keyword T.If      -> parseIf tokens
    T.Keyword T.For     -> parseFor tokens
    T.Keyword T.Bool    -> parseDeclVarExpr tokens
    T.Keyword T.Char    -> parseDeclVarExpr tokens
    T.Keyword T.Int     -> parseDeclVarExpr tokens
    T.Keyword T.Float   -> parseDeclVarExpr tokens
    _ -> H.errorAt pos "Unexpected token in stmt at "
parseStmt [] = H.errorAt (0, 0) "Unexpected end of input in statement"

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

parseRet :: Parser A.Stmt
parseRet tokens = do
    (_, rest)      <- H.expectToken (T.Keyword T.Ret) "Expected 'Ret'" tokens
    (expr, rest1) <- parseBinaryOpExpr rest
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "expected ';'" rest1
    Right (A.RetStmt expr, rest2)

parseIf :: Parser A.Stmt
parseIf tokens = do
    (_, brkNRest)      <- H.expectToken (T.Keyword T.If) "Expected 'if'" tokens
    (_, rest) <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket)) "Expected '('" brkNRest
    (cond, rest1)    <- parseBinaryOpExpr rest
    (_, rest2) <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest1
    (bdy, rest3)    <- parseCompoundStmt rest2
    case rest3 of
        (T.Keyword T.Else,_) : elseBdyNrest -> do
            (elseBdy, rest4) <- parseCompoundStmt elseBdyNrest
            Right (A.IfStmt cond bdy (Just elseBdy), rest4)
        _ -> Right (A.IfStmt cond bdy Nothing, rest3)

parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe f tokens = case f tokens of 
    Right (e, rest) -> Right (Just e, rest)
    Left e -> Right (Nothing, tokens)

parseFor :: Parser A.Stmt
parseFor tokens = do
    (_, rest)      <- H.expectToken (T.Keyword T.For) "Expected 'for'" tokens
    (_, vDeclNRest) <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket)) "expected '('" rest
    (vDecl, rest1)  <- parseMaybe H.parseVarDeclStmt vDeclNRest
    (_, binOpNRest) <- H.expectToken (T.Punctuator T.Semicolon) "expected ';'" rest1
    (binOp, rest2)  <- parseMaybe parseBinaryOpExpr binOpNRest
    (_, declNRest)  <- H.expectToken (T.Punctuator T.Semicolon) "expected ';'" rest2
    (decl, rest3)   <- parseMaybe H.parseDeclStmt declNRest
    (_, bdyNRest) <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest3
    (bdy, rest4)    <- parseCompoundStmt bdyNRest
    Right (A.ForStmt vDecl binOp decl bdy, rest4)

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

parseWhile :: Parser A.Stmt
parseWhile tokens = do
    (_, rest1) <- H.expectToken (T.Keyword T.While) "Expected 'while'" tokens
    (_, binOpNrest) <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket)) "expected '('" rest1
    (cond, rest2) <- parseBinaryOpExpr binOpNrest
    (_, bdyNrest) <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest2
    (body, rest3) <- parseCompoundStmt bdyNrest
    Right (A.WhileStmt cond body, rest3)
