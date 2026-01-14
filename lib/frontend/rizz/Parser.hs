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
-- Maintainer  : maxence.pierre@epitech.eu, florian.grave@epitech.eu, hugo.duda@epitech.eu
--
-- Takes a list of __@'T.Token'@__ and do an syntaxic analysis, based on a BNF grammar, to build an __@'A.Decl'@__ list representing the program Abstract Syntax Tree.
--
-- If an unexpected token is found or the syntax is invalid, the @'parser'@ function returns an pretty :) error message.
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
-- On faillure, if an arrow punctuator is found, it expects a builtin type after it. Otherwise, it returns @'Nothing'@ for the return type.
parseReturnType :: Parser (Maybe A.BuiltinType)
parseReturnType ((T.Punctuator T.Arrow, _) : rest1) = do
    (returntype, rest2) <- H.parseBuiltinType rest1
    Right(Just returntype, rest2)
parseReturnType toks = Right (Nothing, toks)

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'A.Decl'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed return type and the remaining tokens.
--
-- On failure, this function return a error message in a @'String'@, this message contain what expected.
parseRecordDeclExpr :: Parser A.Decl
parseRecordDeclExpr tokens = do
    (_, rest) <- H.expectToken (T.Keyword T.Struct)
        "expected struct" tokens
    (identifier, rest2) <- H.parseIdentifier rest
    (_, rest3) <- H.expectToken (T.Punctuator (T.CBracket T.OpenCBracket))
        "expected '{'" rest2
    (pvdel, rest4) <- H.parsePVDEList rest3
    (_, rest5) <- H.expectToken (T.Punctuator (T.CBracket T.CloseCBracket))
        "expected '}'" rest4
    Right (A.RecordDecl (A.RecordDeclExpr identifier pvdel), rest5)

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'A.Decl'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed variable declaration as @'A.VarDecl'@ and the remaining tokens.
--
-- On failure, this function returns a pretty formatted error message if invalid syntax or a semicolon is missing.
parseVarDecl :: ([A.Decl], A.Decl) -> Parser A.Decl
parseVarDecl f tokens = do
    (vardecl, rest1) <- H.parseVarDeclStmt f tokens
    (_, rest2) <- H.expectToken (T.Punctuator T.Semicolon) "Expected ';'" rest1
    Right (A.VarDecl vardecl, rest2)

parseFunctionDeclH :: Parser T.Identifier
parseFunctionDeclH tokens = do
    (_ , rest) <- H.expectToken (T.Keyword T.Fn) "Expected 'Fn'" tokens
    (name, r1)       <- H.parseIdentifier rest
    (_, rest2) <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket))
        "expected '('" r1
    Right (name, rest2)
-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'A.Decl'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed function decl.
--
-- On failure, this function returns a pretty formatted error message.
parseFunctionDecl :: Parser A.Decl
parseFunctionDecl tokens = do
    (n, rest)        <- parseFunctionDeclH tokens 
    (pvdelist, rest3)   <- H.parsePVDEList rest
    (_, rest4) <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket))
        "expected ')'" rest3
    (returntype, rest5) <- parseReturnType rest4
    (compStmt, rest6)   <- PS.parseCompoundStmt ([], A.FunctionDecl n pvdelist
        (A.CompoundStmt []) returntype) rest5
    Right (A.FunctionDecl n pvdelist compStmt returntype, rest6)

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'A.Decl'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed top-level declaration (either a function or variable declaration) and the remaining tokens.
--
-- On failure, this function returns a pretty formatted error message.
parseTopLevel :: Parser A.Decl
parseTopLevel tokens@((T.Keyword T.Fn, _) : _) = parseFunctionDecl tokens
parseTopLevel tokens@((T.Keyword T.Struct, _) : _) = parseRecordDeclExpr tokens
parseTopLevel tokens =
    case H.parseBuiltinType tokens of
        Right _ -> parseVarDecl
            ([], A.FunctionDecl "" [] (A.CompoundStmt []) Nothing) tokens
        Left err -> Left err

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ ([@'A.Decl'@], [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed declaration list and the remaining tokens.
--
-- On failure, this function returns a pretty formatted error message.
parseDecl :: Parser [A.Decl]
parseDecl [] = Right ([], [])
parseDecl tokens = do
    (decl, xs)       <- parseTopLevel tokens
    (decls, xsfinal) <- parseDecl xs
    Right (decl : decls, xsfinal)

-- | Takes a [@'SingleToken'@] as parameter and returns a __Either__ @'String'@ [@'A.Decl'@].
--
-- On success, this function returns a list of @'A.Decl'@ representing the complete AST of the program.
--
-- On failure, this function returns a pretty formatted error message.
parser :: [SingleToken] -> Either String [A.Decl]
parser tokens =
    case parseDecl tokens of
        Left err -> Left err
        Right (decls, []) -> Right decls
        Right (_, _) -> H.errorAt (1, 1) "Parser Exception."
