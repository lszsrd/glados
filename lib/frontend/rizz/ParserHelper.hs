{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/Parser.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : ParserHelper
-- Description : Helper functions for parsing tokens
-- License     : MIT
-- Maintainer  : maxence.pierre@epitech.eu, florian.grave@epitech.eu, hugo.duda@epitech.eu
--
-- Provides helper functions for parsing tokens
--
-- If an unexpected token is found or if the syntax is invalid, the parsing functions return an pretty formatted error message.
-------------------------------------------------------------------------------
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
    parseBinaryOp,
    parseListLiteral,
    parseListElements,
    errorAt,
    expectToken,
    parseMaybe,
    getPos,
    addIfVarExpr,
    findString
) where

import qualified Ast as A
import qualified Tokens as T
import Data.List

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

-- | Takes a (@'Data.Int'@, @'Data.Int'@) position and a @'String'@ message as parameters and returns a __Either__ @'String'@ a.
--
-- On failure, this function returns a pretty formatted error message with line & col position.
errorAt :: (Int, Int) -> String -> Either String a
errorAt (ligne, colonne) message =
    Left (show ligne ++ ":" ++ show colonne ++ " " ++ message)

-- | Takes a @'T.Token'@, a @'String'@ message and a @'Parser'@ @'SingleToken'@ list as parameters and returns a __Either__ @'String'@ ((), [@'SingleToken'@]).
--
-- On success, this function returns a tuple...
--
-- On failure, this function returns a pretty formatted error message with line & col position.
expectToken :: T.Token -> String -> Parser()
expectToken _ message [] = errorAt (1, 1) (message ++ ", ")
expectToken expected message ((token, position) : xs)
    | token == expected = Right((), xs)
    | otherwise         = errorAt position (message ++ ", got " ++ show token)

parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe f tokens = case f tokens of 
    Right (e, rest) -> Right (Just e, rest)
    Left e -> case findString "Undefined" e of
        Just _ -> Left e
        Nothing -> Right (Nothing, tokens)

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 tokens = case p1 tokens of
    Right a -> Right a
    Left e -> case findString "Undefined" e of
        Just _ -> Left e
        Nothing -> p2 tokens

doesVarExists :: (Int, Int) -> A.Decl -> T.Identifier -> Either String String
doesVarExists _ _ "True" = Right "OK"
doesVarExists _ _ "False" = Right "OK"
doesVarExists pos (A.FunctionDecl _ [] _ _) iden =
    errorAt pos ("Undefined reference to " ++ show iden)
doesVarExists pos (A.FunctionDecl n (A.ParmVarDeclExpr _ id1:xs) bdy ret) id2 =
    if id1 == id2 then Right "OK"
        else doesVarExists pos (A.FunctionDecl n xs bdy ret) id2
doesVarExists _ _ _ = Left "1:1 Error"

addIfVarExpr :: (Int, Int) -> A.Stmt -> [A.ParmVarDeclExpr]
    -> Either String [A.ParmVarDeclExpr]
addIfVarExpr _ (A.DeclVarExpr (A.VarDeclStmt t iden _ _)) [] =
    Right [A.ParmVarDeclExpr t iden]
addIfVarExpr p var@(A.DeclVarExpr (A.VarDeclStmt _ iden _ _))
    (x@(A.ParmVarDeclExpr _ id2): xs) =
    if iden == id2 then errorAt p ("Multiple definition of " ++ show iden)
        else do
            rest <- addIfVarExpr p var xs
            Right (x:rest)
addIfVarExpr _ _ parms = Right parms

-- Helper for parse AssignOp
parseAssignOp :: Parser T.AssignOp
parseAssignOp ((T.Punctuator (T.AssignOp op), _) : rest) = Right (op, rest)
parseAssignOp ((token, position) : _) = 
    errorAt position ("Expected AssignOp, got " ++ show token)
parseAssignOp [] = errorAt (1, 1) "Expected AssignOp, got "

parseBinaryOp :: Parser T.BinaryOp
parseBinaryOp ((T.Punctuator (T.BinaryOp op), _) : rest) = Right (op, rest)
parseBinaryOp ((token, position) : _) = 
    errorAt position ("Expected BinaryOp, got " ++ show token)
parseBinaryOp [] = errorAt (1, 1) "Expected BinaryOp, got "

-- Helper for parse Literal
parseLiteral :: Parser T.Literal
parseLiteral ((T.Literal lit, _) : rest) = Right (lit, rest)
parseLiteral ((token, position) : _) =
    errorAt position ("Expected Litteral, got " ++ show token)
parseLiteral [] = errorAt (1, 1) "Expected Literal, got "

-- Helper for parse list of Literal elements (similar to parsePVDEList)
parseListElements :: Parser [T.Literal]
parseListElements tokens@((T.Punctuator (T.SBracket T.CloseSBracket), _) : _)
    = Right ([], tokens)
parseListElements tokens@((T.Punctuator (T.SBracket T.OpenSBracket), _) : _) = do
    (elt, rest1) <- parseListLiteral tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (elems, rest3) <- parseListElements rest2
            Right (elt : elems, rest3)
        _ -> Right ([elt], rest1)
parseListElements tokens = do
    (elt, rest1) <- parseLiteral tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (elems, rest3) <- parseListElements rest2
            Right (elt : elems, rest3)
        _ -> Right ([elt], rest1)

-- Helper for parse list literal (similar to parseCallExprDecl)
parseListLiteral :: Parser T.Literal
parseListLiteral ((T.Punctuator (T.SBracket T.OpenSBracket), _) : rest1) = do
    (elements, rest2) <- parseListElements rest1
    (_, rest3) <- expectToken (T.Punctuator (T.SBracket T.CloseSBracket))
        "Expected ']'" rest2
    Right (T.ListLiteral elements, rest3)
parseListLiteral ((token, position) : _) =
    errorAt position ("Expected '[' for list literal, got " ++ show token)
parseListLiteral [] = errorAt (1,1) "Expected list literal"

-- Helper for parse BuiltinType
parseBuiltinType :: Parser A.BuiltinType
parseBuiltinType ((T.Punctuator (T.SBracket T.OpenSBracket), _) : rest1) = do
    (innerType, rest2) <- parseBuiltinType rest1
    (_, rest3) <- expectToken (T.Punctuator (T.SBracket T.CloseSBracket))
        "Expected ']' for list type" rest2
    Right (A.ListType innerType, rest3)
parseBuiltinType ((T.Keyword T.Bool, _) : rest) = Right (A.Boolean, rest)
parseBuiltinType ((T.Keyword T.Char, _) : rest) = Right (A.Character, rest)
parseBuiltinType ((T.Keyword T.Int, _) : rest) = Right (A.Integer, rest)
parseBuiltinType ((T.Keyword T.Float, _) : r) = Right (A.SinglePrecision, r)
parseBuiltinType ((token, position) : _) =
    errorAt position ("Expected builtintype, got " ++ show token)
parseBuiltinType [] = errorAt (1,1) "Expected builtintype, got Nothing"

-- Helper for parse Identifier token
parseIdentifier :: Parser T.Identifier
parseIdentifier [] = errorAt (1, 1) "Unexpected err"
parseIdentifier ((T.Identifier id1, _) : rest) = Right (id1, rest)
parseIdentifier ((token, position) : _) =
    errorAt position ("Expected identifier, got " ++ show token)

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
parseParmCallDecl :: A.Decl -> Parser A.ParmCallDecl
parseParmCallDecl _ t@((T.Punctuator (T.SBracket T.OpenSBracket), _) : _) = do
    (listLit, rest) <- parseListLiteral t
    Right (A.ParmCallDeclLiteral listLit, rest)
parseParmCallDecl f tokens@((T.Identifier _, _) :
    (T.Punctuator (T.RBracket T.OpenRBracket), _) : _) = do
    (call, rest) <- parseCallExprDecl f tokens
    Right (A.ParmCallDeclExpr call, rest)
parseParmCallDecl f ((T.Identifier id1, pos) : rest) = do
    _ <- doesVarExists pos f id1
    Right (A.ParmCallDeclIdent id1, rest)
parseParmCallDecl _ ((T.Literal li, _) : rest) =
    Right (A.ParmCallDeclLiteral li, rest)
parseParmCallDecl _ ((token, position) : _) =
    errorAt position ("Expected literal/ident/call-expr, got " ++ show token)
parseParmCallDecl _ [] = errorAt (1, 1) "Expected ParmCallDecl, got " 

parseSingle :: A.Decl -> Parser A.ParmCallDecl
parseSingle f tokens = case tokens of
    ((T.Punctuator (T.RBracket T.OpenRBracket), _) : rest) -> do
        (p1, rest1) <- parseParmCallDeclBExpr f rest
        (_, rest2) <- expectToken
            (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest1
        Right (p1, rest2)
    _ -> parseParmCallDecl f tokens

parseParmCallDeclBExpr :: A.Decl -> Parser A.ParmCallDecl
parseParmCallDeclBExpr f tokens = do
    (p1, rest) <- parseSingle f tokens
    case parseBinaryOp rest of
        Right (op, rest1) -> do
            (p2, r) <- parseSingle f rest1
            Right (A.ParmCallBExpr (A.BinaryOpParm p1)op(A.BinaryOpParm p2),r)
        Left _ -> Right (p1, rest)

-- Helper for parse a list of ParmCallDecl
parseParmCallDeclList :: A.Decl -> Parser [A.ParmCallDecl]
parseParmCallDeclList _ t@((T.Punctuator (T.RBracket T.CloseRBracket), _) : _)
    = Right ([], t)
parseParmCallDeclList f tokens = do
    (arg, rest1) <- parseOr (parseParmCallDeclBExpr f)
        (parseParmCallDecl f) tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (args, rest3) <- parseParmCallDeclList f rest2
            Right (arg : args, rest3)
        _ -> Right ([arg], rest1)

-- Helper for parse CallExprDecl
parseCallExprDecl :: A.Decl -> Parser A.CallExprDecl
parseCallExprDecl f tokens = do
    (fname, rest1) <- parseIdentifier tokens
    (_, rest2) <- expectToken (T.Punctuator
        (T.RBracket T.OpenRBracket)) "Expected '(' after function name" rest1
    (parmcldllist, rest3) <- parseParmCallDeclList f rest2
    (_, rest4) <- expectToken (T.Punctuator
        (T.RBracket T.CloseRBracket)) "Expected ')'" rest3
    Right (A.CallExprDecl fname parmcldllist, rest4)

-- Helper for parse DeclAssignStmtLiteral
parseDeclAssignStmtLiteral :: A.Decl -> Parser A.DeclStmt
parseDeclAssignStmtLiteral f tokens = do
    (id1, rest1) <- parseIdentifier tokens
    _ <- doesVarExists (getPos 0 tokens) f id1
    (ap, rest2) <- parseAssignOp rest1
    (parmcldl, rest3) <- parseParmCallDecl f rest2
    Right (A.DeclAssignStmtLiteral id1 ap parmcldl, rest3)

-- Helper for parse VarDeclStmt (Type name = value)
parseVarDeclStmt :: A.Decl -> Parser A.VarDeclStmt
parseVarDeclStmt f tokens = do
    (typ, rest1) <- parseBuiltinType tokens
    (name, rest2) <- parseIdentifier rest1
    (op, rest3) <- parseAssignOp rest2
    (value, rest4) <- parseParmCallDecl f rest3
    Right (A.VarDeclStmt typ name op value, rest4)

parseDeclStmt :: A.Decl -> Parser A.DeclStmt
parseDeclStmt f tokens@((T.Identifier i, pos) : rest1) = do
    _ <- doesVarExists pos f i
    case rest1 of
        ((T.Punctuator (T.UnaryOp u), _) : rest2) -> 
            Right (A.DeclAssignStmtUnary (A.UnaryOperatorExpr i u), rest2)
        _ -> parseDeclAssignStmtLiteral f tokens
parseDeclStmt _ ((token, position) : _) =
    errorAt position ("Expected identifier, got " ++ show token)
parseDeclStmt _ _ = errorAt (1, 1) "Expected identifier, got Nothing"

-- | Takes an @'Integer'@ and a stream of @'SingleToken'@ as parameters,
--  and returns a tuple of @'Integer'@
--
-- this function serves as a helper function to get the position of the first or last token in the stream.
getPos :: Int -> [SingleToken] -> (Int, Int)
getPos 0 ((_, (l, c)): _) = (l, c - 1)
getPos 1 [(_, (l, c))] = (l, c + 1)
getPos 1 (_ : xs) = getPos 1 xs
getPos _ _ = (1, 1)
