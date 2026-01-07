{-
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/Parser.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Parser
-- Description : Performs syntactic analysis of a list of tokens and builds an Abstract Syntax Tree.
-- License     : MIT
-- Maintainer  : maxence.pierre@epitech.eu, florian.grave@epitech.eu, hugo.duda@protonmail.com
--
-- Takes a list of __@'T.Token'@__ and do an syntaxic analysis, based on a BNF grammar, to build an __@'A.Decl'@__ list representing the program Abstract Syntax Tree.
--
-- If an unexpected token is found or the syntax is invalid, the @'parser'@ function returns an error message.
-------------------------------------------------------------------------------
module Parser (
    parser
    , parseDecl
    , parseTopLevel
    , parseFunctionDecl
    , parseVarDecl
) where

import qualified Ast as A
import qualified Tokens as T
import qualified ParserHelper as H
import qualified ParserStmt as PS

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'Maybe'@ @'A.BuiltinType'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed return type wrapped in @'Maybe'@ and the remaining tokens.
--
-- If an arrow punctuator is found, it expects a builtin type after it. Otherwise, it returns @'Nothing'@ for the return type.
parseReturnType :: Parser (Maybe A.BuiltinType)
parseReturnType tokens@((T.Punctuator T.Arrow, _) : rest1) = do
    (returntype, rest2) <- H.parseBuiltinType rest1
    Right(Just returntype, rest2)
parseReturnType toks = Right (Nothing, toks)

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'A.Decl'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed variable declaration as @'A.VarDecl'@ and the remaining tokens.
--
-- On failure, this function returns a pretty formatted error message if invalid syntax or a semicolon is missing.
parseVarDecl :: Parser A.Decl
parseVarDecl tokens = do
    (vardecl, rest1) <- H.parseVarDeclStmt tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest1
    Right (A.VarDecl vardecl, rest2)



-- | Takes a @'Parser'@
--
-- On success,
--
-- On failure, this function returns a pretty formatted error message if invalid syntax or a semicolon is missing.

parseFunctionDecl :: Parser A.Decl
parseFunctionDecl tokens = do
    (_, rest0)          <- H.expectToken (T.Keyword T.Fn) "Expected 'Fn'" tokens
    (name, rest1)       <- H.parseIdentifier rest0
    (_, rest2)          <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket)) "expected '('" rest1
    (pvdelist, rest3)   <- H.parsePVDEList rest2
    (_, rest4)          <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest3
    (returntype, rest5) <- parseReturnType rest4
    (compStmt, rest6)   <- PS.parseCompoundStmt rest5
    Right (A.FunctionDecl name pvdelist compStmt returntype, rest6)


-- | Takes a @'Parser'@
--
-- On success,
--
-- On failure,
parseTopLevel :: Parser A.Decl
parseTopLevel tokens@((T.Keyword T.Fn, _) : _) = parseFunctionDecl tokens
parseTopLevel tokens =
    case H.parseBuiltinType tokens of
        Right _ -> parseVarDecl tokens
        Left error -> Left error


-- | Takes a
--
-- On success,
--
-- On failure,
parseDecl :: Parser [A.Decl]
parseDecl [] = Right ([], [])
parseDecl tokens = do
    (decl, xs)       <- parseTopLevel tokens
    (decls, xsfinal) <- parseDecl xs
    Right (decl : decls, xsfinal)

-- | Takes a
--
-- On success,
--
-- On failure,
parser :: [SingleToken] -> Either String [A.Decl]
parser tokens =
    case parseDecl tokens of
        Left error -> Left error
        Right (decls, []) -> Right decls
        Right (_, rest) -> Left "0: 0 Parser Exception."
