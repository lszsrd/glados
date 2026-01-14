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
-- This function returns a pretty formatted error message with line & col position.
--
-- NOTE: This function is used as an helper to format error messages.
errorAt :: (Int, Int) -> String -> Either String a
errorAt (ligne, colonne) message =
    Left (show ligne ++ ":" ++ show colonne ++ " " ++ message)

-- | Takes a @'T.Token'@, a @'String'@ message and a @'Parser'@ @'SingleToken'@ list as parameters and returns a __Either__ @'String'@ ((), [@'SingleToken'@]).
--
-- On success, this function returns a tuple containing the rest of the stream
--
-- On failure, this function returns a pretty formatted error message with line & col position.
expectToken :: T.Token -> String -> Parser()
expectToken _ message [] = errorAt (1, 1) (message ++ ", ")
expectToken expected message ((token, position) : xs)
    | token == expected = Right((), xs)
    | otherwise         = errorAt position (message ++ ", got " ++ show token)

-- | Takes a @'Parser'@ @'a'@ and a list as parameters and
-- returns a __Either__ @'String'@ (__Maybe__ @'a'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple containing __Just__ @'a'@ and the rest of the stream.
--
-- On failure, this function returns a tuple containing __Nothing__ and the rest of the stream.
--
-- This function is used to parse a expression that can be empty.
-- (e.g.: a forExpr can either be "for(Int a = 0; a < 0; a++)" and "for(;;)" )
parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe f tokens = case f tokens of 
    Right (e, rest) -> Right (Just e, rest)
    Left e -> case findString "Undefined" e of
        Just _ -> Left e
        Nothing -> Right (Nothing, tokens)

-- | Takes two @'Parser'@ @'a'@ and a list as parameters and
-- returns a __Either__ @'String'@ (@'a'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple containing @'a'@ and the rest of the stream.
--
-- On failure, this function returns a the result of the second parser passed as parameter.
--
-- This function is used to parse an expression that can be two different things.
parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 tokens = case p1 tokens of
    Right a -> Right a
    Left e -> case findString "Undefined" e of
        Just _ -> Left e
        Nothing -> p2 tokens

-- | Takes a (@'Data.Int'@, @'Data.Int'@), an @'A.Decl'@ and an @'T.Identifier'@ as parameters and
-- returns a __Either__ @'String'@ @'String'@.
--
-- On success, this function returns a String, to signify that the var exists.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to verify that a variable used is defined before.
doesVarExists :: (Int, Int) -> A.Decl -> T.Identifier -> Either String String
doesVarExists _ _ "True" = Right "OK"
doesVarExists _ _ "False" = Right "OK"
doesVarExists pos (A.FunctionDecl _ [] _ _) iden =
    errorAt pos ("Undefined reference to " ++ show iden)
doesVarExists pos (A.FunctionDecl n (A.ParmVarDeclExpr _ id1:xs) bdy ret) id2 =
    if id1 == id2 then Right "OK"
        else doesVarExists pos (A.FunctionDecl n xs bdy ret) id2
doesVarExists _ _ _ = Left "1:1 Error"

-- | Takes a (@'Data.Int'@, @'Data.Int'@), an @'A.Stmt@ and a list of @'A.ParmVarDeclExpr'@ as parameters and
-- returns a __Either__ @'String'@ @'[A.ParmVarDeclExpr]'@.
--
-- On success, this function returns a @'[A.ParmVarDeclExpr]'@, with the newly added variable in the list.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to add a variable to the defined variables.
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

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'[T.AssignOp]'@.
--
-- On success, this function returns a @'T.AssignOp'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an assignment operator.
parseAssignOp :: Parser T.AssignOp
parseAssignOp ((T.Punctuator (T.AssignOp op), _) : rest) = Right (op, rest)
parseAssignOp ((token, position) : _) = 
    errorAt position ("Expected AssignOp, got " ++ show token)
parseAssignOp [] = errorAt (1, 1) "Expected AssignOp, got "

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'[T.BinaryOp]'@.
--
-- On success, this function returns a @'T.BinaryOp'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a binary operator.
parseBinaryOp :: Parser T.BinaryOp
parseBinaryOp ((T.Punctuator (T.BinaryOp op), _) : rest) = Right (op, rest)
parseBinaryOp ((token, position) : _) = 
    errorAt position ("Expected BinaryOp, got " ++ show token)
parseBinaryOp [] = errorAt (1, 1) "Expected BinaryOp, got "

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'T.Literal'@.
--
-- On success, this function returns a @'T.Literal'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a literal value.
parseLiteral :: Parser T.Literal
parseLiteral ((T.Literal lit, _) : rest) = Right (lit, rest)
parseLiteral ((token, position) : _) =
    errorAt position ("Expected Litteral, got " ++ show token)
parseLiteral [] = errorAt (1, 1) "Expected Literal, got "

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'[T.Literal]'@.
--
-- On success, this function returns a @'[T.Litteral]'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a list of literal values.
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

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'T.Literal'@.
--
-- On success, this function returns a @'T.Litteral'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a list of literal values.
parseListLiteral :: Parser T.Literal
parseListLiteral ((T.Punctuator (T.SBracket T.OpenSBracket), _) : rest1) = do
    (elements, rest2) <- parseListElements rest1
    (_, rest3) <- expectToken (T.Punctuator (T.SBracket T.CloseSBracket))
        "Expected ']'" rest2
    Right (T.ListLiteral elements, rest3)
parseListLiteral ((token, position) : _) =
    errorAt position ("Expected '[' for list literal, got " ++ show token)
parseListLiteral [] = errorAt (1,1) "Expected list literal"

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.BuiltinType'@.
--
-- On success, this function returns a @'A.BuiltinType'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a Rizz Builtin type (Int, Bool, Char, ...).
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

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'T.Identifier'@.
--
-- On success, this function returns a @'T.Identifier'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an Identifier (e.g.: foo, bar, a, ...).
parseIdentifier :: Parser T.Identifier
parseIdentifier [] = errorAt (1, 1) "Unexpected err"
parseIdentifier ((T.Identifier id1, _) : rest) = Right (id1, rest)
parseIdentifier ((token, position) : _) =
    errorAt position ("Expected identifier, got " ++ show token)

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmVarDeclExpr'@.
--
-- On success, this function returns a @'A.ParmVarDeclExpr'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a ParmVarDeclExpr (composed of a BuiltinType and an Identifier).
parsePVDE :: Parser A.ParmVarDeclExpr
parsePVDE token = do
    (btype, rest1) <- parseBuiltinType token
    (_, rest2)          <-
        expectToken (T.Punctuator T.Colon) "Expected ':' in parameter" rest1
    (identifier, rest3) <- parseIdentifier rest2
    Right(A.ParmVarDeclExpr btype identifier, rest3)

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'[A.ParmVarDeclExpr]'@.
--
-- On success, this function returns a @'[A.ParmVarDeclExpr]'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a list of ParmVarDeclExpr, typically function parameters.
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

-- | Takes an @'A.Decl'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmCallDecl'@.
--
-- On success, this function returns a @'A.ParmCallDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse the a single parameter of a function call (e.g.: 4, 'c', foo(), ...).
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

-- | Takes an @'A.Decl'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmCallDecl'@.
--
-- On success, this function returns a @'A.ParmCallDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a single parameter of a function call Either a BinaryOpExpr or a simple parameter.
parseSingle :: A.Decl -> Parser A.ParmCallDecl
parseSingle f tokens = case tokens of
    ((T.Punctuator (T.RBracket T.OpenRBracket), _) : rest) -> do
        (p1, rest1) <- parseParmCallDeclBExpr f rest
        (_, rest2) <- expectToken
            (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest1
        Right (p1, rest2)
    _ -> parseParmCallDecl f tokens

-- | Takes an @'A.Decl'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmCallDecl'@.
--
-- On success, this function returns a @'A.ParmCallDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a BinaryOpExpr in a function (e.g.: 6 - 4, foo(4) * 14 - 67).
parseParmCallDeclBExpr :: A.Decl -> Parser A.ParmCallDecl
parseParmCallDeclBExpr f tokens = do
    (p1, rest) <- parseSingle f tokens
    case parseBinaryOp rest of
        Right (op, rest1) -> do
            (p2, r) <- parseSingle f rest1
            Right (A.ParmCallBExpr (A.BinaryOpParm p1)op(A.BinaryOpParm p2),r)
        Left _ -> Right (p1, rest)

-- | Takes an @'A.Decl'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmCallDecl'@.
--
-- On success, this function returns a @'A.ParmCallDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse all the parameters of a function call.
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

-- | Takes an @'A.Decl'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.CallExprDecl'@.
--
-- On success, this function returns a @'A.CallExprDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a function call.
parseCallExprDecl :: A.Decl -> Parser A.CallExprDecl
parseCallExprDecl f tokens = do
    (fname, rest1) <- parseIdentifier tokens
    (_, rest2) <- expectToken (T.Punctuator
        (T.RBracket T.OpenRBracket)) "Expected '(' after function name" rest1
    (parmcldllist, rest3) <- parseParmCallDeclList f rest2
    (_, rest4) <- expectToken (T.Punctuator
        (T.RBracket T.CloseRBracket)) "Expected ')'" rest3
    Right (A.CallExprDecl fname parmcldllist, rest4)

-- | Takes an @'A.Decl'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.DeclStmt'@.
--
-- On success, this function returns a @'A.DeclStmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an assignment statement.
parseDeclAssignStmtLiteral :: A.Decl -> Parser A.DeclStmt
parseDeclAssignStmtLiteral f tokens = do
    (id1, rest1) <- parseIdentifier tokens
    _ <- doesVarExists (getPos 0 tokens) f id1
    (ap, rest2) <- parseAssignOp rest1
    (parmcldl, rest3)
        <- parseOr (parseParmCallDeclBExpr f) (parseParmCallDecl f) rest2
    Right (A.DeclAssignStmtLiteral id1 ap parmcldl, rest3)

-- | Takes an @'A.Decl'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.VarDeclStmt'@.
--
-- On success, this function returns a @'A.VarDeclStmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a variable declaration statement.
parseVarDeclStmt :: A.Decl -> Parser A.VarDeclStmt
parseVarDeclStmt f tokens = do
    (typ, rest1) <- parseBuiltinType tokens
    (name, rest2) <- parseIdentifier rest1
    (op, rest3) <- parseAssignOp rest2
    (value, rest4) <- parseOr (parseParmCallDeclBExpr f) (parseParmCallDecl f) rest3
    Right (A.VarDeclStmt typ name op value, rest4)

-- | Takes an @'A.Decl'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.DeclStmt'@.
--
-- On success, this function returns a @'A.DeclStmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an operation on an existing variable.
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
