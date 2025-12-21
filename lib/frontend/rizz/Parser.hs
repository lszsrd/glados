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
import qualified Token as T

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
helperparseBuiltinType :: Parser A.BuiltinType
helperparseBuiltinType ((T.Keyword T.Bool, _) : rest) = Right (A.Boolean, rest)
helperparseBuiltinType ((T.Keyword T.Char, _) : rest) = Right (A.Character, rest)
helperparseBuiltinType ((T.Keyword T.Int, _) : rest) = Right (A.Integer, rest)
helperparseBuiltinType ((T.Keyword T.Float, _) : rest) = Right (A.SinglePrecision, rest)
helperparseBuiltinType ((token, position) : _) = errorAt position ("Expected builtinetype got: " ++ show token)
helperparseBuiltinType [] = Left "Unexpected err"

-- Helper for parse Identifier token
helperParseIdentifier :: Parser T.Identifier
helperParseIdentifier [] = Left "Unexpected err"
helperParseIdentifier ((T.Identifier id, _) : rest) = Right (id, rest)
helperParseIdentifier ((token, position) : _) = errorAt position ("Expected identifier got: " ++ show token)

-- Helper for parse ParmVarDeclExpr (BuiltinType Identifier)
helperParsePVDE :: Parser A.ParmVarDeclExpr
helperParsePVDE token = do
    (btype, rest1) <- helperparseBuiltinType token
    (_, r2)          <- expectToken (T.Punctuator T.Colon) "Expected ':' in parameter" rest1
    (identifier, r3) <- helperParseIdentifier r2
    Right(A.ParmVarDeclExpr btype identifier, r3)

-- Helper for parse list of ParmVarDeclExpr [BuiltinType Identifier]
helperParsePVDEList :: Parser [A.ParmVarDeclExpr]
helperParsePVDEList tokens@((T.Punctuator (T.RBracket T.CloseRBracket), _) : _) = Right ([], tokens)
helperParsePVDEList tokens = do
    (param, rest1) <- helperParsePVDE tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (params, rest3) <- helperParsePVDEList rest2
            Right(param : params, rest3)
        _ -> Right ([param], rest1)

-- Not working just place holder
helperParseCompoundStmt :: Parser A.CompoundStmt
helperParseCompoundStmt tokens = do
    (_, rest1) <- expectToken (T.Punctuator (T.CBracket T.OpenCBracket)) "expected '{'" tokens
    (_, rest2) <- expectToken (T.Punctuator (T.CBracket T.CloseCBracket)) "expected '}'" tokens
    Right (A.CompoundStmt [], rest2)

parseReturnType :: Parser (Maybe A.BuiltinType)
parseReturnType tokens@((T.Punctuator T.Arrow, _) : rest1) = do
    (returntype, rest2) <- helperparseBuiltinType rest1
    Right(Just returntype, rest2)
parseReturnType toks = Right (Nothing, toks)

parseVarDecl :: Parser A.Decl
parseVarDecl tokens = do
    (typ, rest1) <- helperparseBuiltinType tokens
    (name, rest2) <- helperParseIdentifier rest1
    case rest2 of
        (T.Punctuator (T.AssignOp T.Equal), _) : rest3 ->
            case rest3 of
                (T.Literal x, _) : (T.Punctuator T.Semicolon, _) : rest4 ->
                    Right (A.VarDecl (A.VarDeclStmt typ name T.Equal (A.ParmCallDeclLiteral x)), rest4)
                (T.Identifier id, _) : (T.Punctuator T.Semicolon, _) : rest4 ->
                    Right (A.VarDecl (A.VarDeclStmt typ name T.Equal (A.ParmCallDeclIdent id)), rest4)
                _ -> Left "Expected expression after semicollon"
        _ -> Left "Expected '=' after variable declaration"

parseFunctionDecl :: Parser A.Decl
parseFunctionDecl tokens = do
    (_, rest0)          <- expectToken (T.Keyword T.Fn) "Expected 'Fn'" tokens
    (name, rest1)       <- helperParseIdentifier rest0
    (_, rest2)          <- expectToken (T.Punctuator (T.RBracket T.OpenRBracket)) "expected '('" rest1
    (pvdelist, rest3)   <- helperParsePVDEList rest2
    (_, rest4)          <- expectToken (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest3
    (returntype, rest5) <- parseReturnType rest4
    (compStmt, rest6)   <- helperParseCompoundStmt rest5
    Right (A.FunctionDecl name pvdelist compStmt returntype, rest6)

parseTopLevel :: Parser A.Decl
parseTopLevel tokens@((T.Keyword T.Fn, _) : _) = parseFunctionDecl tokens
parseTopLevel tokens =
    case helperparseBuiltinType tokens of
        Right _ -> parseVarDecl tokens
        Left error  -> Left error

parseDecl :: Parser [A.Decl]
parseDecl [] = Right ([], [])
parseDecl tokens = do
    (decl, xs)       <- parseTopLevel tokens
    (decls, xsfinal) <- parseDecl xs
    Right (decl : decls, xsfinal)

parser :: [(T.Token, (Int, Int))] -> Either String [A.Decl]
parser tokens =
    case parseDecl tokens of
        Left error -> Left error
        Right (decls, []) -> Right decls
        Right (_, rest) -> Left "Parser Exception."
