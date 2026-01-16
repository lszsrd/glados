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

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.CompoundStmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a compound statement, (e.g.: a function's body, a for's body).
parseCompoundStmt :: Bool -> ([A.Decl], A.Decl) -> Parser A.CompoundStmt
parseCompoundStmt b f tokens = do
    (_, rest1) <- H.expectToken (T.Punctuator (T.CBracket T.OpenCBracket))
        "expected '{'" tokens
    (stmts, rest2) <- parseStmtList b f rest1
    (_, rest3) <- H.expectToken (T.Punctuator (T.CBracket T.CloseCBracket))
        "expected '}'" rest2
    Right (A.CompoundStmt stmts, rest3)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a Ternary statement.
parseTernaryOperation :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseTernaryOperation f ((T.Punctuator (T.RBracket T.OpenRBracket), _) : r)
    = do
    (binaryOpExpr, rest2) <- parseBinaryOpExpr f r
    (_, rest3) <- H.expectToken
        (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest2
    (_, rest4) <- H.expectToken (T.Punctuator T.QMark) "Expected '?'" rest3
    (compoundStmt1, rest5) <- parseCompoundStmt False f rest4
    (_, rest6) <- H.expectToken (T.Punctuator T.Colon) "Expected ':'" rest5
    (compoundStmt2, rest7) <- parseCompoundStmt False f rest6
    Right (A.IfStmt binaryOpExpr compoundStmt1 (Just compoundStmt2), rest7)
parseTernaryOperation _ [] = H.errorAt (1, 1)
    "Expected TernaryExpr, got End Of Stream"
parseTernaryOperation _ ((a, pos):_) = H.errorAt pos
    ("Expected Ternary, got " ++ show a)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'[A.Stmt]'@.
--
-- On success, this function returns a @'[A.Stmt]'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a statement list.
parseStmtList :: Bool -> ([A.Decl], A.Decl) -> Parser [A.Stmt]
parseStmtList _ _ tokens@((T.Punctuator (T.CBracket T.CloseCBracket), _) : _)
    = Right ([], tokens)
parseStmtList b f@(fl, A.FunctionDecl n pvdelist bdy ret) tokens = do
    (stmt, r1) <- parseStmt b f tokens
    newParms <- H.addIfVarExpr (H.getPos 0 tokens) stmt pvdelist
    (stmts, rest2) <- parseStmtList b (fl, A.FunctionDecl n newParms bdy ret) r1
    Right (stmt : stmts, rest2)
parseStmtList _ _ [] = H.errorAt (1, 1)
    "Expected '{' or '}' after Stmt List, got End Of Stream"
parseStmtList _ _ ((a, pos):_) = H.errorAt pos
    ("Expected '{' or '}', got " ++ show a)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse all builtins type statements.
parseBuiltinTypes :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseBuiltinTypes f tokens@((tok, pos) : _) = case tok of
    T.Keyword T.Bool    -> parseDeclVarExpr f tokens
    T.Keyword T.Char    -> parseDeclVarExpr f tokens
    T.Keyword T.Int     -> parseDeclVarExpr f tokens
    T.Keyword T.Float   -> parseDeclVarExpr f tokens
    T.Punctuator (T.SBracket T.OpenSBracket) -> parseDeclVarExpr f tokens
    T.Punctuator (T.RBracket T.OpenRBracket) -> parseTernaryOperation f tokens
    _ -> H.errorAt pos "Unexpected token in stmt"
parseBuiltinTypes _ [] =
    H.errorAt (1, 1) "Unexpected end of input in statement"

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse all keywords statements.
parseKeywords :: Bool -> ([A.Decl], A.Decl) -> Parser A.Stmt
parseKeywords b f tokens@((tok, _) : _) = case tok of
    T.Keyword T.Foreach                      -> parseForeach f tokens
    T.Keyword T.While                        -> parseWhile f tokens
    T.Keyword T.Ret                          -> parseRet f tokens
    T.Keyword T.If                           -> parseIf f tokens
    T.Keyword T.For                          -> parseFor f tokens
    T.Keyword T.Continue                     -> parseLoopCtrlStmt b tokens
    T.Keyword T.Break                        -> parseLoopCtrlStmt b tokens
    _ -> parseBuiltinTypes f tokens
parseKeywords _ _  [] = H.errorAt (1, 1) "Unexpected end of input in statement"

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a statement.
parseStmt :: Bool -> ([A.Decl], A.Decl) -> Parser A.Stmt
parseStmt b f tokens@((tok, _) : xs) = case tok of
    T.Identifier id1 ->
        case xs of
            ((T.Punctuator (T.RBracket T.OpenRBracket), _) : _) ->
                parseCallExpr f tokens
            ((T.Punctuator T.Arrow, _) : (T.Identifier id2, _): (_, pos) : rest3) ->
                parseDeclVarExpr f ((T.Identifier (H.craftIdentifierWithStructVarDecl id1 id2), pos) : rest3)
            ((T.Identifier _, _) : (T.Punctuator (T.AssignOp _), _) : _) ->
                parseDeclVarExpr f tokens
            _ -> parseDeclStmtExpr f tokens
    _ -> parseKeywords b f tokens
parseStmt _ _ [] = H.errorAt (1, 1) "Unexpected end of input in statement"

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a funcion call statement.
parseCallExpr :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseCallExpr f tokens = do
    (call, rest) <- H.parseCallExprDecl f tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    Right (A.CallExpr call, rest2)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an operation on a variable statement.
parseDeclStmtExpr :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseDeclStmtExpr f tokens = do
    (declstmt, rest) <- H.parseDeclStmt f tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    Right (A.DeclStmt declstmt, rest2)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a variable declaration statement.
parseDeclVarExpr :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseDeclVarExpr f tokens = do
    (vardecl, rest) <- H.parseVarDeclStmt f tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    Right (A.DeclVarExpr vardecl, rest2)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a Ret statement.
parseRet :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseRet f@(_, A.FunctionDecl _ _ _ (Just _)) tokens = do
    (_, rest)      <- H.expectToken (T.Keyword T.Ret) "Expected 'ret'" tokens
    (expr, rest1) <- parseBinaryOpExpr f rest
    (_, r2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest1
    Right (A.RetStmt expr, r2)
parseRet (_, A.FunctionDecl _ _ _ Nothing) ((_, pos): _) =
    H.errorAt pos "Function return type not specified"
parseRet _ e = H.errorAt (1,1) ("Expected Ret stmt, got " ++ show e)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse the declaration of the If statement.
parseIfH :: ([A.Decl], A.Decl) -> Parser A.BinaryOpExpr
parseIfH f tokens = do
    (_, brkNRest) <- H.expectToken (T.Keyword T.If) "Expected 'if'" tokens
    (_, rest) <- H.expectToken
        (T.Punctuator (T.RBracket T.OpenRBracket)) "Expected '('" brkNRest
    (cond, rest1)    <- parseBinaryOpExpr f rest
    (_, rest2) <- H.expectToken
        (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest1
    Right (cond, rest2)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an If statement.
parseIf :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseIf f tokens = do
    (cond, rest)   <- parseIfH f tokens 
    (bdy, rest2)    <- parseCompoundStmt False f rest
    case rest2 of
        (T.Keyword T.Else,_) : elseBdyNrest -> do
            (elseBdy, rest4) <- parseCompoundStmt False f elseBdyNrest
            Right (A.IfStmt cond bdy (Just elseBdy), rest4)
        _ -> Right (A.IfStmt cond bdy Nothing, rest2)

-- | Takes a @'Parser'@ @'a'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ ( __Maybe__ @'a'@).
--
-- On success, this function returns a __Maybe__ @'a'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to __Maybe__ parse an 'a' statement.
parseForBody :: Parser a -> Parser (Maybe a)
parseForBody p tokens = do
    (op, rest) <- H.parseMaybe p tokens
    (_, rest1) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    Right (op, rest1)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ ( __Maybe__ @'A.VarDeclStmt'@, @'A.Decl'@).
--
-- On success, this function returns a ( __Maybe__ @'A.VarDeclStmt'@, @'A.Decl'@).
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is an helper to parse the rest of the for statement.
parseTemp :: ([A.Decl], A.Decl) -> Parser (Maybe A.VarDeclStmt, A.Decl)
parseTemp f@(_, fun@(A.FunctionDecl n parms bdy ret)) tokens = do
    (decl, rest) <- H.parseMaybe (H.parseVarDeclStmt f) tokens
    (_, rest1) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest
    case decl of
        Nothing -> Right ((Nothing, fun), rest1)
        Just st -> do
            newP <- H.addIfVarExpr (H.getPos 0 tokens) (A.DeclVarExpr st) parms
            Right ((decl, A.FunctionDecl n newP bdy ret), rest1)
parseTemp _ e = H.errorAt (1, 1) ("Expected Expression, got " ++ show (head e))

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ ( __Maybe__ @'A.VarDeclStmt'@, @'A.Decl'@).
--
-- On success, this function returns a ( __Maybe__ @'A.VarDeclStmt'@, @'A.Decl'@).
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is an helper to parse the for statement.
parseForH :: ([A.Decl], A.Decl) -> Parser (Maybe A.VarDeclStmt, A.Decl)
parseForH f tokens = do
    (_, rest)      <- H.expectToken (T.Keyword T.For) "Expected 'for'" tokens
    (_, vDeclNRest) <- H.expectToken
        (T.Punctuator (T.RBracket T.OpenRBracket)) "Expected '('" rest
    parseTemp f vDeclNRest


parseLoopCtrlStmt :: Bool -> Parser A.Stmt
parseLoopCtrlStmt False ((_, pos):_) = H.errorAt pos
    "break or continue outside loop scope."
parseLoopCtrlStmt True ((T.Keyword T.Continue, _):
    (T.Punctuator T.Semicolon,_): rest) =
    Right (A.LoopControlStmt T.Continue, rest)
parseLoopCtrlStmt True ((T.Keyword T.Break, _):
    (T.Punctuator T.Semicolon,_): rest) =
    Right (A.LoopControlStmt T.Break, rest)
parseLoopCtrlStmt _ e =
    H.errorAt (1, 1) ("Expected Expression, got " ++ show e)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a For statement.
parseFor :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseFor f@(fl, _) tokens = do
    ((vDecl, newF) , rest) <- parseForH f tokens
    (binOp, rest1) <- parseForBody (parseBinaryOpExpr (fl, newF)) rest
    (decl, rest2)   <- H.parseMaybe (H.parseDeclStmt (fl, newF)) rest1
    (_, bdyNRest) <- H.expectToken
        (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest2
    (bdy, rest3)    <- parseCompoundStmt True (fl, newF) bdyNRest
    Right (A.ForStmt vDecl binOp decl bdy, rest3)

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is an helper to parse a ForEach statement.
parseForeachH :: Parser T.Identifier
parseForeachH t = do
    (_, rest1) <- H.expectToken (T.Keyword T.Foreach) "Expected 'foreach'" t
    (_, rest2) <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket))
        "Expected '('" rest1
    (container, rest3) <- H.parseIdentifier rest2
    (_, rest4) <- H.expectToken (T.Punctuator T.Colon) "Expected ':'" rest3
    Right (container, rest4)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a Foreach statement.
parseForeach :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseForeach f tokens = do
    (container, rest1) <- parseForeachH tokens
    (iterator, rest2) <- H.parseIdentifier rest1
    (_, rest3) <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket))
        "Expected ')'" rest2
    (body, rest4) <- parseCompoundStmt False f rest3
    Right (A.ForeachStmt container iterator body, rest4)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.Stmt'@.
--
-- On success, this function returns a @'A.Stmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a While statement.
parseWhile :: ([A.Decl], A.Decl) -> Parser A.Stmt
parseWhile f tokens = do
    (_, rest1) <- H.expectToken (T.Keyword T.While) "Expected 'while'" tokens
    (_, binOpNrest) <- H.expectToken
        (T.Punctuator (T.RBracket T.OpenRBracket)) "Expected '('" rest1
    (cond, rest2) <- parseBinaryOpExpr f binOpNrest
    (_, bdyNrest) <- H.expectToken
        (T.Punctuator (T.RBracket T.CloseRBracket)) "Expected ')'" rest2
    (body, rest3) <- parseCompoundStmt True f bdyNrest
    Right (A.WhileStmt cond body, rest3)
