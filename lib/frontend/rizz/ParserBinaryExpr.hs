{-
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/ParserBinaryExpr.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : ParserBinaryExpr
-- Description : Parses binary operation expressions with operator precedence.
-- License     : MIT
-- Maintainer  : maxence.pierre@epitech.eu hugo.duda@epitech.eu
--
-- Parsing of binary operation expressions
--
-- If an unexpected token is found or the syntax is invalid, the parsing functions return an error message.
-------------------------------------------------------------------------------
module ParserBinaryExpr (
    parseBinaryOpExpr
) where

import qualified Ast as A
import qualified Tokens as T
import qualified ParserHelper as H
import qualified ParserBinOp2 as POP

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'(A.BinaryOpParm, [SingleToken])'@
--
-- On success, this function returns a parsed BinaryOpParm.
-- representing a single parameter in a BinaryOpExpr
-- 
-- On failure, this function returns a pretty formatted error message.
parseBinaryOpParm :: ([A.Decl], A.Decl) -> Parser A.BinaryOpParm
parseBinaryOpParm f tokens = case tokens of
    ((T.Punctuator (T.RBracket T.OpenRBracket), _) : rest) -> do
        (e, rest1) <- parseFormBinaryOpExpr f rest
        (_, rest2) <- H.expectToken
            (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest1
        Right (A.BinaryOpParmBOp e, rest2)
    _ -> do
        (p, rest) <- H.parseParmCallDecl f tokens
        Right (A.BinaryOpParm p, rest)

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'(A.BinaryOpExpr, [SingleToken])'@
--
-- On success, this function returns a parsed BinaryOpExpr.
-- 
-- On failure, this function returns a pretty formatted error message.
parseFormBinaryOpExpr :: ([A.Decl], A.Decl) -> Parser A.BinaryOpExpr
parseFormBinaryOpExpr f tokens@(_ : (T.Punctuator (T.BinaryOp op), _) : rest) = do
    (parm1, _) <- parseBinaryOpParm f tokens
    (parm2, rest3) <- parseBinaryOpParm f rest
    Right (A.BinaryOpExpr parm1 op parm2, rest3)
parseFormBinaryOpExpr f tokens = do
    (parm1, rest) <- parseBinaryOpParm f tokens
    case POP.parseBinaryOp rest of
        Left _ -> do
            (cons, _) <- H.parseParmCallDecl f tokens
            Right (A.BinaryOpConst cons, rest)
        Right (binop, rest2) -> do
            (parm2, rest3) <- parseBinaryOpParm f rest2
            Right (A.BinaryOpExpr parm1 binop parm2, rest3)

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'(A.BinaryOpExpr, [SingleToken])'@
--
-- On success, this function returns a parsed Binary Operation Expression.
-- in the case of a complex expr (e.g: '3 * 4 - 1 / foo(4 - 1)'),
--      - automatically format the expression.
--      - organise the expression and taking in account operator priorities.
--
-- On failure, this function returns a pretty formatted error message.
parseBinaryOpExpr :: ([A.Decl], A.Decl) -> Parser A.BinaryOpExpr
parseBinaryOpExpr f tokens = do
    (binOpPacked, rest) <- POP.packBinOpExpr tokens
    let binOpFormatted = POP.formatBinOpExpr binOpPacked
    parseFormBinaryOpExpr f (binOpFormatted ++ rest)
