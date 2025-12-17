{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/rizz/Parser.hs
-}

-- Builds AST from tokens
module Parser (
    parser
) where

import Lexer
import Ast

parseFunctionDecl :: [(Token, (Int, Int))] -> Either String Decl
parseFunctionDecl tokens = 

parseDecl :: [(Token, (Int, Int))] -> Either String [Decl]
parseDecl (Lexer.BuiltinType:Lexer.Identifier:, lines) =

parser :: [(Token, (Int, Int))] -> Either String [Decl]
    parser tokens = 
        case parseDecl tokens of
            Left error -> Left error
            Right tokens -> Right tokens
