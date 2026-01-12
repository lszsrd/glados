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

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'(A.BinaryOpParm, [SingleToken])'@
--
-- On success, this function returns a parsed BinaryOpParm.
-- representing a single parameter in a BinaryOpExpr
-- 
-- On failure, this function returns a pretty formatted error message.
parseBinaryOpParm :: A.Decl -> Parser A.BinaryOpParm
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
parseFormBinaryOpExpr :: A.Decl -> Parser A.BinaryOpExpr
parseFormBinaryOpExpr f tokens@(_ : (T.Punctuator (T.BinaryOp op), _) : rest) = do
    (parm1, _) <- parseBinaryOpParm f tokens
    (parm2, rest3) <- parseBinaryOpParm f rest
    Right (A.BinaryOpExpr parm1 op parm2, rest3)
parseFormBinaryOpExpr f tokens = do
    (parm1, rest) <- parseBinaryOpParm f tokens
    case H.parseBinaryOp rest of
        Left _ -> do
            (cons, _) <- H.parseParmCallDecl f tokens
            Right (A.BinaryOpConst cons, rest)
        Right (binop, rest2) -> do
            (parm2, rest3) <- parseBinaryOpParm f rest2
            Right (A.BinaryOpExpr parm1 binop parm2, rest3)

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'([SingleToken], [SingleToken])'@
--
-- On success, this function returns a stream of token.
-- representing inside the bracket of an expr.
-- (e.g: "foo(bar)", "(4 - 5)" )
-- 
-- On failure, this function returns a pretty formatted error message.
packBracket :: Parser [SingleToken]
packBracket (op@(T.Punctuator (T.RBracket T.CloseRBracket), _): rest)
    = Right ([op], rest)
packBracket (op@(T.Punctuator (T.RBracket T.OpenRBracket), _): rest) = do
    (blk, rest1) <- packBracket rest -- first (blk)
    (blk2, rest2) <- packBracket rest1 -- rest after )
    Right (op: blk ++ blk2, rest2)
packBracket (op: rest) = do
    (blk, rest1) <- packBracket rest
    Right (op : blk, rest1)
packBracket [] = Right ([], [])

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'([SingleToken], [SingleToken])'@
--
-- On success, this function returns a stream of token.
-- representing a single parameter in the BinaryOpExpr.
-- (e.g: "foo(bar)", "9", "(4 - 5)" )
-- 
-- On failure, this function returns a pretty formatted error message.
getFirstParam :: Parser [SingleToken]
getFirstParam [] = Right ([], [])
getFirstParam (punct : rest) = case punct of
    op@(T.Punctuator (T.BinaryOp _), _) -> Right ([], op: rest)
    op@(T.Punctuator T.Semicolon, _) -> Right ([], op: rest)
    op@(T.Punctuator (T.RBracket T.OpenRBracket), _) -> do 
        (param, rest1) <- packBracket rest
        Right (op: param, rest1)
    op@(T.Punctuator (T.RBracket T.CloseRBracket), _) -> Right ([], op: rest)
    _ -> do
        (param, rest1) <- getFirstParam rest
        Right (punct: param, rest1)

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'[SingleToken]'@
--
-- On success, this function returns a packed Binary Operation Expression.
-- That means, grouping the first param of the bigger fish.
-- 
-- On failure, this function returns a pretty formatted error message.
packFirstParamH :: [SingleToken] -> Parser [SingleToken]
packFirstParamH first rest = case first of
    (a@(T.Identifier _, _) :
        x@(T.Punctuator (T.RBracket T.OpenRBracket),_): xs) -> do
        (parm, rest1) <- packBinOpExpr xs
        Right (a: x : formatBinOpExpr parm ++ rest1, rest)
    _ -> Right (first, rest)

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'[SingleToken]'@
--
-- On success, this function returns a packed Binary Operation Expression.
-- That means, grouping the first param of the bigger fish.
-- 
-- On failure, this function returns a pretty formatted error message.
packFirstParam :: Parser [SingleToken]
packFirstParam t = do
    (first, rest) <- getFirstParam t
    case first of
        (x@(T.Punctuator (T.RBracket T.OpenRBracket),_): xs) -> do
            (parm, rest1) <- packBinOpExpr xs
            Right (x : formatBinOpExpr parm ++ rest1,rest)
        _ -> packFirstParamH first rest

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'([([SingleToken], Maybe T.BinaryOp)], [SingleToken])'@
--
-- On success, this function returns a packed Binary Operation Expression.
-- That means, grouping one parameter with its following Operator Token, until the end of BinaryOpExpr.
-- 
-- On failure, this function returns a pretty formatted error message.
packBinOpExpr :: Parser [([SingleToken], Maybe T.BinaryOp)]
packBinOpExpr tokens = do
    (first, rest) <- packFirstParam tokens
    case H.parseBinaryOp rest of
        Left _ -> Right ([(first, Nothing)], rest)
        Right (op, rest1) -> do
            (restParams, restBuffer) <- packBinOpExpr rest1
            Right ((first, Just op) : restParams, restBuffer)

-- | Takes two @'([SingleToken], Maybe T.BinaryOp)'@ as parameters
--  and returns an @'Integer'@
--
-- this function serves as a helper function to get the priority of operator.
-- (e.g:
--      -> "4 / 2 + ..." -> priority is 1st operator, returns 0
--      -> "4 - 2 * ..." -> priority is 2nd operator, returns 1
-- )
getPrioOp :: ([SingleToken], Maybe T.BinaryOp)
    -> ([SingleToken], Maybe T.BinaryOp) -> Int
getPrioOp (_, Just T.Mul) _ = 0
getPrioOp (_, Just T.Div) _ = 0
getPrioOp _ (_, Just T.Mul) = 1
getPrioOp _ (_, Just T.Div) = 1
getPrioOp (_, Just T.Mod) _ = 0
getPrioOp _ (_, Just T.Mod) = 1
getPrioOp _ _ = 0

-- | Takes a stream of @'SingleToken'@ as parameter,
--  and returns a stream of @'SingleToken'@
--
-- this function serves as a helper function to regroup params inbetween parenthesis.
-- (e.g: "4 - 2 -> (4 - 2)" )
createBlockParam :: [SingleToken] -> [SingleToken]
createBlockParam p =
    [(T.Punctuator (T.RBracket T.OpenRBracket), H.getPos 0 p)] ++ p ++
    [(T.Punctuator (T.RBracket T.CloseRBracket), H.getPos 1 p)]

-- | Takes a stream of @'([SingleToken], Maybe T.BinaryOp]'@ as parameter,
--  representing the unformatted binaryOpExpr, composed of a parameter with a Maybe binary Operator
-- (e.g: "foo(y) * 2 -> [ [ (foo, (, y, )], Just Mul), ([2], Nothing)])" )
--  and returns a stream of @'SingleToken'@
--
-- this function returns a formatted stream of token, representing the formatted BinaryOpExpr.
formatBinOpExpr :: [([SingleToken], Maybe T.BinaryOp)] -> [SingleToken]
formatBinOpExpr (first@(p1, Just op1) : second@(p2, Just op2): rest) = 
    if getPrioOp first second == 0
        then
            createBlockParam (p1 ++ [(T.Punctuator (T.BinaryOp op1),
            H.getPos 0 p2)] ++ p2) ++
            [(T.Punctuator (T.BinaryOp op2), H.getPos 1 p2)] ++
            createBlockParam (formatBinOpExpr rest)
        else
            p1 ++ [(T.Punctuator (T.BinaryOp op1), H.getPos 1 p1)] ++
            createBlockParam (formatBinOpExpr (second : rest))
formatBinOpExpr [(t, Nothing)] = t
formatBinOpExpr [(t, Just op), (t2, Nothing)] =
    t ++ [(T.Punctuator (T.BinaryOp op), H.getPos 0 t2)] ++ t2
formatBinOpExpr _ = []

-- | Takes a stream of @'SingleToken'@ as parameter
--  and returns a __Either __ @'String'@ @'(A.BinaryOpExpr, [SingleToken])'@
--
-- On success, this function returns a parsed Binary Operation Expression.
-- in the case of a complex expr (e.g: '3 * 4 - 1 / foo(4 - 1)'),
--      - automatically format the expression.
--      - organise the expression and taking in account operator priorities.
--
-- On failure, this function returns a pretty formatted error message.
parseBinaryOpExpr :: A.Decl -> Parser A.BinaryOpExpr
parseBinaryOpExpr f tokens = do
    (binOpPacked, rest) <- packBinOpExpr tokens
    let binOpFormatted = formatBinOpExpr binOpPacked
    parseFormBinaryOpExpr f (binOpFormatted ++ rest)
