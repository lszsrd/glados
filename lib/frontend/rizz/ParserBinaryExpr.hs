{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/ParserBinaryExpr.hs
-}

module ParserBinaryExpr (
    parseBinaryOpExpr
) where

import qualified Ast as A
import qualified Tokens as T
import qualified ParserHelper as H
import Debug.Trace (trace)

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

parseBinaryOpParm :: Parser A.BinaryOpParm
parseBinaryOpParm tokens = case tokens of
    ((T.Punctuator (T.RBracket T.OpenRBracket), _) : rest) -> do
        (e, rest1) <- parseFormBinaryOpExpr rest
        (_, rest2) <- H.expectToken
            (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest1
        Right (A.BinaryOpParmBOp e, rest2)
    _ -> do
        (p, rest) <- H.parseParmCallDecl tokens
        Right (A.BinaryOpParm p, rest)

parseFormBinaryOpExpr :: Parser A.BinaryOpExpr
parseFormBinaryOpExpr tokens@(_ : (T.Punctuator (T.BinaryOp op), _) : rest) = do
    (parm1, _) <- parseBinaryOpParm tokens
    (parm2, rest3) <- parseBinaryOpParm rest
    Right (A.BinaryOpExpr parm1 op parm2, rest3)
parseFormBinaryOpExpr tokens = do
    (parm1, rest) <- parseBinaryOpParm tokens
    case H.parseBinaryOp rest of
        Left err -> do
            (const, _) <- H.parseParmCallDecl tokens
            Right (A.BinaryOpConst const, rest)
        Right (binop, rest2) -> do
            (parm2, rest3) <- parseBinaryOpParm rest2
            Right (A.BinaryOpExpr parm1 binop parm2, rest3)

packBracket :: Parser [SingleToken]
packBracket (op@(T.Punctuator (T.RBracket T.OpenRBracket), _): rest) = Right ([op], rest)
packBracket (op: rest) = do
    (blk, rest1) <- packBracket rest
    Right (op : blk, rest)

getFirstParam :: Parser [SingleToken]
getFirstParam [] = Right ([], [])
getFirstParam (punct : rest) =
    case punct of
        op@(T.Punctuator (T.BinaryOp _), _) -> Right ([], op: rest)
        op@(T.Punctuator T.Semicolon, _) -> Right ([], op: rest)
        op@(T.Punctuator (T.RBracket T.CloseRBracket), _) -> Right ([], op: rest)
        op@(T.Punctuator (T.RBracket T.OpenRBracket), _) -> packBracket (op:rest)
        _ -> do
            (param, rest1) <- getFirstParam rest
            Right (punct: param, rest1)

packBinOpExpr :: Parser [([SingleToken], Maybe T.BinaryOp)]
packBinOpExpr tokens = do
    (first, rest) <- getFirstParam tokens
    case H.parseBinaryOp rest of
        Left _ -> Right ([(first, Nothing)], rest)
        Right (op, rest1) -> do
            (restParams, restBuffer) <- packBinOpExpr rest1
            Right ((first, Just op) : restParams, restBuffer)

getPrioOp :: ([SingleToken], Maybe T.BinaryOp)
    -> ([SingleToken], Maybe T.BinaryOp) -> Int
getPrioOp (_, Just T.Mul) _ = 0
getPrioOp (_, Just T.Div) _ = 0
getPrioOp _ (_, Just T.Mul) = 1
getPrioOp _ (_, Just T.Div) = 1
getPrioOp (_, Just T.Mod) _ = 0
getPrioOp _ (_, Just T.Mod) = 1
getPrioOp _ _ = 0

formatBinOpExpr :: [([SingleToken], Maybe T.BinaryOp)] -> [SingleToken]
formatBinOpExpr (first@(p1, Just op1) : second@(p2, Just op2): rest) = 
    case getPrioOp first second of
        0 -> [(T.Punctuator (T.RBracket T.OpenRBracket), (0,0))] ++ p1 ++
            [(T.Punctuator (T.BinaryOp op1), (0,0))] ++ p2 ++
            [(T.Punctuator (T.RBracket T.CloseRBracket), (0,0))] ++
            [(T.Punctuator (T.BinaryOp op2), (0,0))] ++
            [(T.Punctuator (T.RBracket T.OpenRBracket), (0,0))] ++
            formatBinOpExpr rest ++
            [(T.Punctuator (T.RBracket T.CloseRBracket), (0,0))]
        1 -> p1 ++ [(T.Punctuator (T.BinaryOp op1), (0,0))] ++
            [(T.Punctuator (T.RBracket T.OpenRBracket), (0,0))] ++
            formatBinOpExpr (second : rest) ++
            [(T.Punctuator (T.RBracket T.CloseRBracket), (0,0))]
formatBinOpExpr [(t, Nothing)] = t
formatBinOpExpr [(t, Just op), (t2, Nothing)] =
    t ++ [(T.Punctuator (T.BinaryOp op), (0,0))] ++ t2

parseBinaryOpExpr :: Parser A.BinaryOpExpr
parseBinaryOpExpr tokens = do
    (binOpPacked, rest) <- packBinOpExpr tokens
    let binOpFormatted = formatBinOpExpr binOpPacked
    parseFormBinaryOpExpr (binOpFormatted ++ rest)
