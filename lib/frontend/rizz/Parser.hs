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
import Data.Maybe (isNothing)

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

verifDefinition :: (Int, Int) -> [A.Decl] -> T.Identifier
    -> Either String String
verifDefinition pos a name = 
    case H.doesVarExists pos
        (a, A.FunctionDecl "" [] (A.CompoundStmt []) Nothing) name of
        Right _ -> H.errorAt pos (show name ++ ": already defined")
        Left _ -> Right "Ok"

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'Maybe'@ @'A.BuiltinType'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed return type wrapped in @'Maybe'@ and the remaining tokens.
--
-- On faillure, if an arrow punctuator is found, it expects a builtin type after it. Otherwise, it returns @'Nothing'@ for the return type.
parseReturnType :: [A.Decl] -> Parser (Maybe A.BuiltinType)
parseReturnType a ((T.Punctuator T.Arrow, _) : rest1) = do
    (returntype, rest2) <- H.parseBuiltinType a rest1
    Right(Just returntype, rest2)
parseReturnType _ toks = Right (Nothing, toks)

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'A.Decl'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed return type and the remaining tokens.
--
-- On failure, this function return a error message in a @'String'@, this message contain what expected.
parseRecordDeclExpr :: [A.Decl] -> Parser A.Decl
parseRecordDeclExpr a tokens = do
    (_, rest) <- H.expectToken (T.Keyword T.Struct) "Expected keyword" tokens
    (identifier, rest2) <- H.parseIdentifier rest
    (_, rest3) <- H.expectToken (T.Punctuator (T.CBracket T.OpenCBracket))
        "expected '{'" rest2
    (pvdel, rest4) <- H.parsePVDEList a rest3
    (_, rest5) <- H.expectToken (T.Punctuator (T.CBracket T.CloseCBracket))
        "expected '}'" rest4
    _ <- verifDefinition (H.getPos 0 rest2) a identifier
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

parseFunctionDeclH :: [A.Decl] -> Parser T.Identifier
parseFunctionDeclH a tokens = do
    (_ , rest) <- H.expectToken (T.Keyword T.Fn) "Expected 'Fn'" tokens
    (name, r1)       <- H.parseIdentifier rest
    (_, rest2) <- H.expectToken (T.Punctuator (T.RBracket T.OpenRBracket))
        "expected '('" r1
    _ <- verifDefinition (H.getPos 0 rest2) a name
    Right (name, rest2)

verifRet :: (Int, Int) -> A.CompoundStmt -> Maybe A.BuiltinType
    -> Either String String
verifRet p x ret = case x of
    (A.CompoundStmt []) -> if isNothing ret then Right "Ok" else H.errorAt p
        ("Missing return In function returning " ++ show ret)
    (A.CompoundStmt (A.RetStmt tp: xs)) -> if isNothing tp && isNothing ret then
       verifRet p (A.CompoundStmt xs) ret else Right "Ok" 
    (A.CompoundStmt (_: xs)) -> verifRet p (A.CompoundStmt xs) ret

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'A.Decl'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed function decl.
--
-- On failure, this function returns a pretty formatted error message.
parseFunctionDecl :: [A.Decl] -> Parser A.Decl
parseFunctionDecl a tokens = do
    (n, rest)        <- parseFunctionDeclH a tokens 
    (pvdelist, rest3)   <- H.parsePVDEList a rest
    (_, rest4) <- H.expectToken (T.Punctuator (T.RBracket T.CloseRBracket))
        "expected ')'" rest3
    (returntype, rest5) <- parseReturnType a rest4
    (comp, rest6)   <- PS.parseCompoundStmt False (a, A.FunctionDecl n pvdelist
        (A.CompoundStmt []) returntype) rest5
    _ <- verifRet (H.getPos 0 rest4) comp returntype
    Right (A.FunctionDecl n pvdelist comp returntype, rest6)

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ (@'A.Decl'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed top-level declaration (either a function or variable declaration) and the remaining tokens.
--
-- On failure, this function returns a pretty formatted error message.
parseTopLevel :: [A.Decl] -> Parser A.Decl
parseTopLevel a tokens@((T.Keyword T.Fn, _) : _) = parseFunctionDecl a tokens
parseTopLevel a t@((T.Keyword T.Struct, _) : _) = parseRecordDeclExpr a t
parseTopLevel a tokens =
    case H.parseBuiltinType a tokens of
        Right _ -> parseVarDecl
            (a, A.FunctionDecl "" [] (A.CompoundStmt []) Nothing) tokens
        Left err -> Left err

-- | Takes a @'Parser'@ @'SingleToken'@ list as parameter and returns a __Either__ @'String'@ ([@'A.Decl'@], [@'SingleToken'@]).
--
-- On success, this function returns a tuple made of the parsed declaration list and the remaining tokens.
--
-- On failure, this function returns a pretty formatted error message.
parseDecl :: [A.Decl] -> Parser [A.Decl]
parseDecl _ [] = Right ([], [])
parseDecl a tokens = do
    (decl, xs)       <- parseTopLevel a tokens
    (decls, xsfinal) <- parseDecl (decl : a) xs
    Right (decl : decls, xsfinal)

-- | Takes a [@'SingleToken'@] as parameter and returns a __Either__ @'String'@ [@'A.Decl'@].
--
-- On success, this function returns a list of @'A.Decl'@ representing the complete AST of the program.
--
-- On failure, this function returns a pretty formatted error message.
parser :: [SingleToken] -> Either String [A.Decl]
parser tokens =
    case parseDecl [] tokens of
        Left err -> Left err
        Right (decls, []) -> Right decls
        Right (_, _) -> H.errorAt (1, 1) "Parser Exception."
