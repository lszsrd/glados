{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/Parser.hs
-}

-- Builds AST from tokens
module ParserHelper (
    parseBuiltinType,
    parseIdentifier,
    parsePVDE,
    parsePVDEList,
    parseAssignOp,
    parseParmCallDecl,
    parseCallExprDecl,
    parseVarDeclStmt,
    parseDeclStmt,
    errorAt,
    expectToken
) where

import qualified Ast as A
import qualified Tokens as T

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

-- Error message builder
errorAt :: (Int, Int) -> String -> Either String a
errorAt (ligne, colonne) message =
    Left (show ligne ++ ":" ++ show colonne ++ " " ++ message)

-- Return a errorAt if the token is not expected otherwise right
expectToken :: T.Token -> String -> Parser()
expectToken _ message [] = errorAt (0,0) message
expectToken expected message ((token, position) : xs)
    | token == expected = Right((), xs)
    | otherwise         = errorAt position (message ++ ", got " ++ show token)

-- Helper for parse AssignOp
parseAssignOp :: Parser T.AssignOp
parseAssignOp ((T.Punctuator (T.AssignOp op), _) : rest) = Right (op, rest)
parseAssignOp ((token, position) : _) = 
    errorAt position ("Expected AssignOp, but got: " ++ show token)
parseAssignOp [] = errorAt (0,0) "Expected AssignOp"

-- Helper for parse Literal
parseLiteral :: Parser T.Literal
parseLiteral ((T.Literal lit, _) : rest) = Right (lit, rest)
parseLiteral ((token, position) : _) =
    errorAt position ("Expected Litteral, but got: " ++ show token)
parseLiteral [] = errorAt (0,0) "Expected Literal"

-- Helper for parse BuiltinType
parseBuiltinType :: Parser A.BuiltinType
parseBuiltinType ((T.Keyword T.Bool, _) : rest) = Right (A.Boolean, rest)
parseBuiltinType ((T.Keyword T.Char, _) : rest) = Right (A.Character, rest)
parseBuiltinType ((T.Keyword T.Int, _) : rest) = Right (A.Integer, rest)
parseBuiltinType ((T.Keyword T.Float, _) : rest) = Right (A.SinglePrecision, rest)
parseBuiltinType ((token, position) : _) =
    errorAt position ("Expected builtintype, but got: " ++ show token)

-- Helper for parse Identifier token
parseIdentifier :: Parser T.Identifier
parseIdentifier [] = errorAt (0, 0) "Unexpected err"
parseIdentifier ((T.Identifier id, _) : rest) = Right (id, rest)
parseIdentifier ((token, position) : _) =
    errorAt position ("Expected identifier, but got: " ++ show token)

-- Helper for parse ParmVarDeclExpr (BuiltinType Identifier)
parsePVDE :: Parser A.ParmVarDeclExpr
parsePVDE token = do
    (btype, rest1) <- parseBuiltinType token
    (_, rest2)          <-
        expectToken (T.Punctuator T.Colon) "Expected ':' in parameter" rest1
    (identifier, rest3) <- parseIdentifier rest2
    Right(A.ParmVarDeclExpr btype identifier, rest3)

-- Helper for parse list of ParmVarDeclExpr [BuiltinType Identifier]
parsePVDEList :: Parser [A.ParmVarDeclExpr]
parsePVDEList tokens@((T.Punctuator (T.RBracket T.CloseRBracket), _) : _)
    = Right ([], tokens)
parsePVDEList tokens = do
    (param, rest1) <- parsePVDE tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (params, rest3) <- parsePVDEList rest2
            Right(param : params, rest3)
        _ -> Right ([param], rest1)

-- Helper for parse ParmCallDecl
parseParmCallDecl :: Parser A.ParmCallDecl
parseParmCallDecl tokens@((T.Identifier _, _) :
    (T.Punctuator (T.RBracket T.OpenRBracket), _) : _) = do
    (call, rest) <- parseCallExprDecl tokens
    Right (A.ParmCallDeclExpr call, rest)
parseParmCallDecl tokens@((T.Identifier id, _) : rest) =
    Right (A.ParmCallDeclIdent id, rest)
parseParmCallDecl tokens@((T.Literal li, _) : rest) =
    Right (A.ParmCallDeclLiteral li, rest)
parseParmCallDecl ((token, position) : _) =
    errorAt position ("Expected literal/ident/call-expr, got: " ++ show token)
parseParmCallDecl [] = errorAt (0, 0) "Expected ParmCallDecl"

-- Helper for parse a list of ParmCallDecl
parseParmCallDeclList :: Parser [A.ParmCallDecl]
parseParmCallDeclList toks@((T.Punctuator (T.RBracket T.CloseRBracket), _) : _)
    = Right ([], toks)
parseParmCallDeclList tokens = do
    (arg, rest1) <- parseParmCallDecl tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (args, rest3) <- parseParmCallDeclList rest2
            Right (arg : args, rest3)
        _ -> Right ([arg], rest1)

-- Helper for parse CallExprDecl
parseCallExprDecl :: Parser A.CallExprDecl
parseCallExprDecl tokens = do
    (fname, rest1) <- parseIdentifier tokens
    (_, rest2) <- expectToken (T.Punctuator
        (T.RBracket T.OpenRBracket)) "Expected '(' after function name" rest1
    (parmcldllist, rest3) <- parseParmCallDeclList rest2
    (_, rest4) <- expectToken (T.Punctuator
        (T.RBracket T.CloseRBracket)) "Expected ')'" rest3
    Right (A.CallExprDecl fname parmcldllist, rest4)

-- Helper for parse DeclAssignStmtLiteral
parseDeclAssignStmtLiteral :: Parser A.DeclStmt
parseDeclAssignStmtLiteral tokens = do
    (id, rest1) <- parseIdentifier tokens
    (ap, rest2) <- parseAssignOp rest1
    (parmcldl, rest3) <- parseParmCallDecl rest2
    Right (A.DeclAssignStmtLiteral id ap parmcldl, rest3)

-- Helper for parse VarDeclStmt (Type name = value)
parseVarDeclStmt :: Parser A.VarDeclStmt
parseVarDeclStmt tokens = do
    (typ, rest1) <- parseBuiltinType tokens
    (_, rest2) <- expectToken (T.Punctuator T.Colon)
        "Expected ':' after type" rest1
    (name, rest3) <- parseIdentifier rest2
    (op, rest4) <- parseAssignOp rest3
    (value, rest5) <- parseParmCallDecl rest4
    Right (A.VarDeclStmt typ name op value, rest5)

parseDeclStmt :: Parser A.DeclStmt
parseDeclStmt tokens@((T.Identifier i, _) : rest1) =
    case rest1 of
        ((T.Punctuator (T.UnaryOp u), _) : rest2) -> 
            Right (A.DeclAssignStmtUnary (A.UnaryOperatorExpr i u), rest2)
        _ -> parseDeclAssignStmtLiteral tokens
parseDeclStmt ((token, position) : _) =
    errorAt position ("Expected identifier, got: " ++ show token)
