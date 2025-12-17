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

parser :: [(Token, (Int, Int))] -> [Stmt]
    parser tokens = 
