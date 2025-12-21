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
    errorAt,
    expectToken
) where

import qualified Ast as A
import qualified Tokens as T

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

-- Error message builder
errorAt :: (Int, Int) -> String -> Either String a
errorAt (ligne, colonne) message = Left (show ligne ++ ":" ++ show colonne ++ ": " ++ message)

-- Return a errorAt if the token is not expected otherwise right
expectToken :: T.Token -> String -> Parser()
expectToken _ message [] = Left(message)
expectToken expected message ((token, position) : xs)
    | token == expected = Right((), xs)
    | otherwise         = errorAt position (message ++ " (got " ++ show token ++ ")")

-- Helper for parse BuiltinType
parseBuiltinType :: Parser A.BuiltinType
parseBuiltinType ((T.Keyword T.Bool, _) : rest) = Right (A.Boolean, rest)
parseBuiltinType ((T.Keyword T.Char, _) : rest) = Right (A.Character, rest)
parseBuiltinType ((T.Keyword T.Int, _) : rest) = Right (A.Integer, rest)
parseBuiltinType ((T.Keyword T.Float, _) : rest) = Right (A.SinglePrecision, rest)
parseBuiltinType ((token, position) : _) = errorAt position ("Expected builtinetype got: " ++ show token)
parseBuiltinType [] = Left "Unexpected err"

-- Helper for parse Identifier token
parseIdentifier :: Parser T.Identifier
parseIdentifier [] = Left "Unexpected err"
parseIdentifier ((T.Identifier id, _) : rest) = Right (id, rest)
parseIdentifier ((token, position) : _) = errorAt position ("Expected identifier got: " ++ show token)

-- Helper for parse ParmVarDeclExpr (BuiltinType Identifier)
parsePVDE :: Parser A.ParmVarDeclExpr
parsePVDE token = do
    (btype, rest1) <- parseBuiltinType token
    (_, r2)          <- expectToken (T.Punctuator T.Colon) "Expected ':' in parameter" rest1
    (identifier, r3) <- parseIdentifier r2
    Right(A.ParmVarDeclExpr btype identifier, r3)

-- Helper for parse list of ParmVarDeclExpr [BuiltinType Identifier]
parsePVDEList :: Parser [A.ParmVarDeclExpr]
parsePVDEList tokens@((T.Punctuator (T.RBracket T.CloseRBracket), _) : _) = Right ([], tokens)
parsePVDEList tokens = do
    (param, rest1) <- parsePVDE tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (params, rest3) <- parsePVDEList rest2
            Right(param : params, rest3)
        _ -> Right ([param], rest1)

-- TODO DeclAssignStmtLiteral and DeclVarStmt
parseDeclStmt :: Parser A.DeclStmt
parseDeclStmt tokens@(((T.Identifier i), _) : rest1) =
    case rest1 of
        ((T.Punctuator (T.UnaryOp u), _) : rest2) -> Right ((A.DeclAssignStmtUnary (A.UnaryOperatorExpr i u)), rest2)
        ((token, position) : _) -> errorAt position ("Expected got: " ++ show token)
parseDeclStmt ((token, position) : _) = errorAt position ("Expected got: " ++ show token)
 