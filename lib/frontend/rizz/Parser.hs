{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/rizz/Parser.hs
-}

-- Builds AST from tokens
module Parser (
    -- parser,
) where

import Lexer
import Ast
import Token


type Parser a = [(Token, (Int, Int))] -> Either String (a, [(Token, (Int, Int))])

-- parseFunctionDecl :: [(Token, (Int, Int))] -> Either String Decl
-- parseFunctionDecl tokens = 
--
-- parseDecl :: [(Token, (Int, Int))] -> Either String [Decl]
-- parseDecl (Lexer.BuiltinType:Lexer.Identifier:, lines) =

{- parseVarDecl :: Parser Decl
parseVarDecl tokens = do
    (typ, rest1) <- parseBuiltinType tokens
    (name, rest2) <- Parser.parseIdentifier rest1
    case rest2 of
        (Punctuator Equal, _) : rest3 ->
            case rest3 of
                (Literal x, _) : (Punctuator Semicolon, _) : rest4 ->
                    Right (VarDecl (VarDeclStmt typ name (ParmCallDeclLiteral x)), rest4)
                (Identifier id, _) : (Punctuator Semicolon, _) : rest4 ->
                    Right (VarDecl (VarDeclStmt typ name (ParmCallDeclIdent id)), rest4)
                _ -> Left "Expected expression after semicollon"
        _ -> Left "Expected '=' after variable declaration"

-- helper fn
parseBuiltinType :: Parser BuiltinType
parseBuiltinType ((Keyword Bool, _) : rest) = Right (Boolean, rest)
parseBuiltinType ((Keyword Char, _) : rest) = Right (Character, rest)
parseBuiltinType ((Keyword Int, _) : rest) = Right (Integer, rest)
parseBuiltinType ((Keyword Float, _) : rest) = Right (SinglePrecision, rest)
parseBuiltinType [] = Left "Unexpected err"

parseIdentifier :: Parser Identifier
parseIdentifier ((Identifier id, _) : rest) = Right (id, rest)
parseIdentifier [] = Left "Unexpected err"


parser :: [(Token, (Int, Int))] -> Either String [Decl]
parser tokens =
    case parseDecl tokens of
        Left error -> Left error
        Right (decls, []) -> Right decls
        Right (_, rest) -> Left "err" -}
