{-
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/ParserBinaryExpr.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : ParserBinOp2
-- Description : Parses binary operation expressions with operator precedence.
-- License     : MIT
-- Maintainer  : maxence.pierre@epitech.eu hugo.duda@epitech.eu
--
-- Parsing of binary operation expressions
--
-- If an unexpected token is found or the syntax is invalid, the parsing functions return an error message.
-------------------------------------------------------------------------------
module ParserBinOp2 (
    packBinOpExpr
    , parseBinaryOp
    , formatBinOpExpr
) where

import qualified Tokens as T

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

-- | Takes an @'Integer'@ and a stream of @'SingleToken'@ as parameters,
--  and returns a tuple of @'Integer'@
--
-- this function serves as a helper function to get the position of the first or last token in the stream.
getPos :: Int -> [SingleToken] -> (Int, Int)
getPos 0 ((_, (l, c)): _) = (l, c - 1)
getPos 1 [(_, (l, c))] = (l, c + 1)
getPos 1 (_ : xs) = getPos 1 xs
getPos _ _ = (1, 1)

-- | Takes a (@'Data.Int'@, @'Data.Int'@) position and a @'String'@ message as parameters and returns a __Either__ @'String'@ a.
--
-- This function returns a pretty formatted error message with line & col position.
--
-- NOTE: This function is used as an helper to format error messages.
errorAt :: (Int, Int) -> String -> Either String a
errorAt (ligne, colonne) message =
    Left (show ligne ++ ":" ++ show colonne ++ " " ++ message)

-- | Takes a @'[SingleToken]'@ as parameter and returns a __Either__ @'String'@ @'[T.BinaryOp]'@.
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
    case parseBinaryOp rest of
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
    [(T.Punctuator (T.RBracket T.OpenRBracket), getPos 0 p)] ++ p ++
    [(T.Punctuator (T.RBracket T.CloseRBracket), getPos 1 p)]

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
            getPos 0 p2)] ++ p2) ++
            [(T.Punctuator (T.BinaryOp op2), getPos 1 p2)] ++
            createBlockParam (formatBinOpExpr rest)
        else
            p1 ++ [(T.Punctuator (T.BinaryOp op1), getPos 1 p1)] ++
            createBlockParam (formatBinOpExpr (second : rest))
formatBinOpExpr [(t, Nothing)] = t
formatBinOpExpr [(t, Just op), (t2, Nothing)] =
    t ++ [(T.Punctuator (T.BinaryOp op), getPos 0 t2)] ++ t2
formatBinOpExpr _ = []
