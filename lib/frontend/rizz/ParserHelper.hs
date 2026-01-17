{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/Parser.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : ParserHelper
-- Description : Helper functions for parsing tokens
-- License     : MIT
-- Maintainer  : maxence.pierre@epitech.eu, florian.grave@epitech.eu, hugo.duda@epitech.eu
--
-- Provides helper functions for parsing tokens
--
-- If an unexpected token is found or if the syntax is invalid, the parsing functions return an pretty formatted error message.
-------------------------------------------------------------------------------
module ParserHelper (
    parseBuiltinType,
    parseIdentifier,
    parsePVDE,
    parsePVDEList,
    parseAssignOp,
    parseParmCallDecl,
    parseCallExprDecl,
    parseVarDeclStmt,
    parseDeclStmt,
    parseBinaryOp,
    parseListLiteral,
    parseListElements,
    errorAt,
    expectToken,
    parseMaybe,
    getPos,
    doesVarExists,
    addIfVarExpr,
    findString,
    getTypeDecl
) where

import qualified Ast as A
import qualified Tokens as T
import Data.List

type SingleToken = (T.Token, (Int, Int))
type Parser a = [SingleToken] -> Either String (a, [SingleToken])

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

-- | Takes a (@'Data.Int'@, @'Data.Int'@) position and a @'String'@ message as parameters and returns a __Either__ @'String'@ a.
--
-- This function returns a pretty formatted error message with line & col position.
--
-- NOTE: This function is used as an helper to format error messages.
errorAt :: (Int, Int) -> String -> Either String a
errorAt (ligne, colonne) message =
    Left (show ligne ++ ":" ++ show colonne ++ " " ++ message)

-- | Takes a @'T.Token'@, a @'String'@ message and a @'Parser'@ @'SingleToken'@ list as parameters and returns a __Either__ @'String'@ ((), [@'SingleToken'@]).
--
-- On success, this function returns a tuple containing the rest of the stream
--
-- On failure, this function returns a pretty formatted error message with line & col position.
expectToken :: T.Token -> String -> Parser()
expectToken _ message [] = errorAt (1, 1) (message ++ ", ")
expectToken expected message ((token, position) : xs)
    | token == expected = Right((), xs)
    | otherwise         = errorAt position (message ++ ", got " ++ show token)

-- | Takes a @'Parser'@ @'a'@ and a list as parameters and
-- returns a __Either__ @'String'@ (__Maybe__ @'a'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple containing __Just__ @'a'@ and the rest of the stream.
--
-- On failure, this function returns a tuple containing __Nothing__ and the rest of the stream.
--
-- This function is used to parse a expression that can be empty.
-- (e.g.: a forExpr can either be "for(Int a = 0; a < 0; a++)" and "for(;;)" )
parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe f tokens = case f tokens of 
    Right (e, rest) -> Right (Just e, rest)
    Left e -> case findString "Undefined" e of
        Just _ -> Left e
        Nothing -> Right (Nothing, tokens)

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 tokens = case p1 tokens of
    Right a -> Right a
    Left e -> case findString "Undefined" e of
        Just _ -> Left e
        Nothing -> p2 tokens

getIden :: A.Decl -> T.Identifier
getIden (A.FunctionDecl n _ _ _) = n
getIden (A.ParmVarDecl n) = case n of
    (A.ParmVarDeclExpr _ name) -> name
    (A.ParmVarRecord (A.RecordDeclExpr name _)) -> name
getIden (A.VarDecl (A.VarDeclStmt _ n _ _)) = n
getIden (A.RecordDecl (A.RecordDeclExpr n _)) = n


getTypeDecl :: A.Decl -> Maybe A.BuiltinType
getTypeDecl (A.FunctionDecl _ _ _ ret) = ret
getTypeDecl (A.ParmVarDecl n) = case n of
    (A.ParmVarDeclExpr tp _) -> Just tp
    (A.ParmVarRecord (A.RecordDeclExpr na _)) -> Just (A.Struct na)
getTypeDecl (A.VarDecl (A.VarDeclStmt n _ _ _)) = Just n
getTypeDecl (A.RecordDecl (A.RecordDeclExpr n _)) = Just (A.Struct n)

getType :: [A.Decl] -> A.ParmCallDecl -> Maybe A.BuiltinType
getType _ (A.ParmCallDeclLiteral (T.BoolLiteral _)) = Just A.Boolean
getType _ (A.ParmCallDeclLiteral (T.CharLiteral _)) = Just A.Character
getType _ (A.ParmCallDeclLiteral (T.IntLiteral _)) = Just A.Integer
getType _ (A.ParmCallDeclLiteral (T.FloatLiteral _)) = Just A.SinglePrecision
getType fl (A.ParmCallDeclLiteral (T.ListLiteral (a :_))) =
    getType fl (A.ParmCallDeclLiteral a)
getType fl (A.ParmCallDeclIdent id1) =
    case doesVarExists (1,1) (fl, A.FunctionDecl "n" []
        (A.CompoundStmt []) Nothing) id1 of
        Left _ -> Nothing
        Right var -> getTypeDecl var
getType fl (A.ParmCallDeclExpr (A.CallExprDecl f _)) =
    case doesVarExists (1,1) (fl, A.FunctionDecl "n" []
        (A.CompoundStmt []) Nothing) f of
        Left _ -> Nothing
        Right var -> getTypeDecl var
getType _ (A.ParmCallDeclList _) = Just (A.Struct "Va te faire foutre aussi")
getType _ (A.ParmCallBExpr _ _ a) = Nothing
getType _ (A.ParmCallDeclIdx _ a) = Nothing
getType _ (A.ParmCallDeclLiteral (T.ListLiteral [])) = Nothing

-- | Takes two @'Parser'@ @'a'@ and a list as parameters and
-- returns a __Either__ @'String'@ (@'a'@, [@'SingleToken'@]).
--
-- On success, this function returns a tuple containing @'a'@ and the rest of the stream.
--
-- On failure, this function returns a the result of the second parser passed as parameter.
--
-- This function is used to parse an expression that can be two different things.
searchInFile :: [A.Decl] -> T.Identifier -> Either String A.Decl 
searchInFile [] n = Left ("Undefined reference to " ++ show n)
searchInFile (decl:xs) vName =
    if getIden decl == vName
        then Right decl
        else searchInFile xs vName

-- | Takes a (@'Data.Int'@, @'Data.Int'@), an @'([A.Decl], A.Decl)'@ and an @'T.Identifier'@ as parameters and
-- returns a __Either__ @'String'@ @'String'@.
--
-- On success, this function returns a String, to signify that the var exists.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to verify that a variable used is defined before.
doesVarExists :: (Int, Int) -> ([A.Decl], A.Decl) -> T.Identifier
    -> Either String A.Decl 
doesVarExists _ _ "True" = Right (A.ParmVarDecl
    (A.ParmVarDeclExpr A.Boolean "True"))
doesVarExists _ _ "False" = Right (A.ParmVarDecl
    (A.ParmVarDeclExpr A.Boolean "False"))
doesVarExists pos (file, A.FunctionDecl _ [] _ _) iden =
    case searchInFile file iden of
        Left e -> errorAt pos e
        Right a -> Right a
doesVarExists pos (fl, A.FunctionDecl n
    (p1@(A.ParmVarDeclExpr _ id1):xs) bdy ret) id2 =
    if id1 == id2 then Right (A.ParmVarDecl p1)
        else doesVarExists pos (fl, A.FunctionDecl n xs bdy ret) id2
doesVarExists p _ _ = errorAt p "var detection Error"

depthStructListCreate :: [A.Decl] -> A.ParmCallDecl -> T.Identifier
    -> A.BuiltinType -> Either String [A.ParmVarDeclExpr]
depthStructListCreate fl x t (A.Struct n) = do
    var <- doesVarExists (1,1)
        (fl, A.FunctionDecl " " [] (A.CompoundStmt []) Nothing) n
    case var of
        (A.RecordDecl (A.RecordDeclExpr _ parm)) -> createList parm x t fl
        e -> errorAt (1,1) ("? " ++ show e)
depthStructListCreate _ _ _ _ = errorAt (1,1) "This shi is useless"

createList :: [A.ParmVarDeclExpr] -> A.ParmCallDecl -> T.Identifier
    -> [A.Decl] -> Either String [A.ParmVarDeclExpr]
createList [] (A.ParmCallDeclList []) _ _ = Right []
createList [] (A.ParmCallDeclList (_:_)) _ _ = Left "    Wrong number of Param"
createList (_:_) (A.ParmCallDeclList []) _ _ = Left "    Wrong number of Param"
createList ((A.ParmVarRecord (A.RecordDeclExpr _ ls)):as)
    (A.ParmCallDeclList (x:xs)) t fl = do
    first <- createList ls x t fl
    rest <- createList as (A.ParmCallDeclList xs) t fl
    Right (first ++ rest)
createList (A.ParmVarDeclExpr tp n:as) (A.ParmCallDeclList (x:xs)) t fl = do
    rest <- createList as (A.ParmCallDeclList xs) t fl
    case getType fl x of
        Just (A.Struct _) -> do
            stparam <- depthStructListCreate fl x (t ++"@" ++ n) tp
            Right (A.ParmVarDeclExpr tp (t ++ "@" ++ n) : stparam ++ rest)
        Just typ -> if typ == tp
            then Right (A.ParmVarDeclExpr tp (t ++ "@" ++ n) : rest)
            else Left ("    Wrong variable type :" ++ show x)
        _ -> Left ("    Wrong variable type :" ++ show x)
createList a _ _ _ = Right a

addIfList :: A.BuiltinType -> T.Identifier -> A.ParmCallDecl -> [A.Decl]
    -> Either String [A.ParmVarDeclExpr]
addIfList _ _ (A.ParmCallDeclList []) _ = Right []
addIfList (A.Struct tp) t l@(A.ParmCallDeclList _) fl = do
    var <- doesVarExists (1, 1) (fl, A.FunctionDecl "n" []
        (A.CompoundStmt []) Nothing) tp
    case var of
        A.RecordDecl (A.RecordDeclExpr _ list) -> createList list l t fl 
        _ -> Right []
addIfList _ _ _ _ = Right []

-- | Takes a (@'Data.Int'@, @'Data.Int'@), an @'A.Stmt@ and a list of @'A.ParmVarDeclExpr'@ as parameters and
-- returns a __Either__ @'String'@ @'[A.ParmVarDeclExpr]'@.
--
-- On success, this function returns a @'[A.ParmVarDeclExpr]'@, with the newly added variable in the list.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to add a variable to the defined variables.
addIfVarExpr :: (Int, Int) -> A.Stmt -> [A.Decl] -> [A.ParmVarDeclExpr]
    -> Either String [A.ParmVarDeclExpr]
addIfVarExpr pos (A.DeclVarExpr (A.VarDeclStmt t iden _ pcallDecl)) fl [] =
    case addIfList t iden pcallDecl fl of
        Left e -> errorAt pos (drop 4 e)
        Right parms -> Right (A.ParmVarDeclExpr t iden : parms)
addIfVarExpr p var@(A.DeclVarExpr (A.VarDeclStmt _ iden _ _)) fl
    (x@(A.ParmVarDeclExpr _ id2): xs) =
    if iden == id2 then errorAt p ("Multiple definition of " ++ show iden)
        else do
            rest <- addIfVarExpr p var fl xs
            Right (x:rest)
addIfVarExpr _ _ _ parms = Right parms


canAssign :: (Int, Int) -> A.Decl -> A.BuiltinType -> Either String String
canAssign p (A.FunctionDecl {}) _ = errorAt p
    "Cannot assign value to function definition"
canAssign p (A.RecordDecl _) _ = errorAt p
    "Cannot assign value to struct definition"
canAssign p (A.ParmVarDecl (A.ParmVarRecord _)) _ = errorAt p
    "Cannot assign value to struct"
canAssign p (A.ParmVarDecl (A.ParmVarDeclExpr tp _)) typ =
    if typ == tp then Right "OK" else errorAt p (
        "Trying to assign from " ++ show typ ++ " to " ++ show tp)
canAssign p (A.VarDecl (A.VarDeclStmt tp _ _ _)) typ =
    if typ == tp then Right "OK" else errorAt p (
        "Trying to assign from " ++ show typ ++ " to " ++ show tp)

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'[T.AssignOp]'@.
--
-- On success, this function returns a @'T.AssignOp'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an assignment operator.
parseAssignOp :: Parser T.AssignOp
parseAssignOp ((T.Punctuator (T.AssignOp op), _) : rest) = Right (op, rest)
parseAssignOp ((token, position) : _) = 
    errorAt position ("Expected AssignOp, got " ++ show token)
parseAssignOp [] = errorAt (1, 1) "Expected AssignOp, got "

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

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'T.Literal'@.
--
-- On success, this function returns a @'T.Literal'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a literal value.
parseLiteral :: Parser T.Literal
parseLiteral ((T.Literal lit, _) : rest) = Right (lit, rest)
parseLiteral ((token, position) : _) =
    errorAt position ("Expected Litteral, got " ++ show token)
parseLiteral [] = errorAt (1, 1) "Expected Literal, got "

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'[T.Literal]'@.
--
-- On success, this function returns a @'[T.Litteral]'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a list of literal values.
parseListElements :: Parser [T.Literal]
parseListElements tokens@((T.Punctuator (T.SBracket T.CloseSBracket), _) : _)
    = Right ([], tokens)
parseListElements tokens@((T.Punctuator (T.SBracket T.OpenSBracket), _) : _) = do
    (elt, rest1) <- parseListLiteral tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (elems, rest3) <- parseListElements rest2
            Right (elt : elems, rest3)
        _ -> Right ([elt], rest1)
parseListElements tokens = do
    (elt, rest1) <- parseLiteral tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (elems, rest3) <- parseListElements rest2
            Right (elt : elems, rest3)
        _ -> Right ([elt], rest1)

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'T.Literal'@.
--
-- On success, this function returns a @'T.Litteral'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a list of literal values.
parseListLiteral :: Parser T.Literal
parseListLiteral ((T.Punctuator (T.SBracket T.OpenSBracket), _) : rest1) = do
    (elements, rest2) <- parseListElements rest1
    (_, rest3) <- expectToken (T.Punctuator (T.SBracket T.CloseSBracket))
        "Expected ']'" rest2
    Right (T.ListLiteral elements, rest3)
parseListLiteral ((token, position) : _) =
    errorAt position ("Expected '[' for list literal, got " ++ show token)
parseListLiteral [] = errorAt (1,1) "Expected list literal"

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.BuiltinType'@.
--
-- On success, this function returns a @'A.BuiltinType'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a Rizz Builtin type (Int, Bool, Char, ...).
parseBuiltinType :: [A.Decl] -> Parser A.BuiltinType
parseBuiltinType a ((T.Punctuator (T.SBracket T.OpenSBracket), _) : rest1) = do
    (innerType, rest2) <- parseBuiltinType a rest1
    (_, rest3) <- expectToken (T.Punctuator (T.SBracket T.CloseSBracket))
        "Expected ']' for list type" rest2
    Right (A.ListType innerType, rest3)
parseBuiltinType _ ((T.Keyword T.Bool, _) : rest) = Right (A.Boolean, rest)
parseBuiltinType _ ((T.Keyword T.Char, _) : rest) = Right (A.Character, rest)
parseBuiltinType _ ((T.Keyword T.Int, _) : rest) = Right (A.Integer, rest)
parseBuiltinType _ ((T.Keyword T.Float, _) : r) = Right (A.SinglePrecision, r)
parseBuiltinType a ((T.Identifier i, p) : rest) = do
    _ <-doesVarExists p (a, A.FunctionDecl "" [] (A.CompoundStmt []) Nothing) i
    Right (A.Struct i, rest)
parseBuiltinType _ ((token, position) : _) =
    errorAt position ("Expected builtintype, got " ++ show token)
parseBuiltinType _ [] = errorAt (1,1) "Expected builtintype, got Nothing"

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'T.Identifier'@.
--
-- On success, this function returns a @'T.Identifier'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an Identifier (e.g.: foo, bar, a, ...).
parseIdentifier :: Parser T.Identifier
parseIdentifier [] = errorAt (1, 1) "Unexpected err"
parseIdentifier ((T.Identifier id1, _): (T.Punctuator T.Arrow, _) :
    p2@(T.Identifier id2, _): rest) =
    case parseIdentifier (p2: rest) of    
        Right (id3, rest2) -> Right (id1 ++ "@" ++ id3, rest2)
        Left _ -> Right (id1 ++ "@" ++ id2, rest)
parseIdentifier ((T.Identifier id1, _) : rest) = Right (id1, rest)
parseIdentifier ((token, position) : _) =
    errorAt position ("Expected identifier, got " ++ show token)

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmVarDeclExpr'@.
--
-- On success, this function returns a @'A.ParmVarDeclExpr'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a ParmVarDeclExpr (composed of a BuiltinType and an Identifier).
parsePVDE :: [A.Decl] -> Parser A.ParmVarDeclExpr
parsePVDE a token = do
    (btype, rest1) <- parseBuiltinType a token
    (_, rest2)          <-
        expectToken (T.Punctuator T.Colon) "Expected ':' in parameter" rest1
    (identifier, rest3) <- parseIdentifier rest2
    Right(A.ParmVarDeclExpr btype identifier, rest3)

-- | Takes a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'[A.ParmVarDeclExpr]'@.
--
-- On success, this function returns a @'[A.ParmVarDeclExpr]'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a list of ParmVarDeclExpr, typically function parameters.
parsePVDEList :: [A.Decl] -> Parser [A.ParmVarDeclExpr]
parsePVDEList _ tokens@((T.Punctuator (T.RBracket T.CloseRBracket), _) : _)
    = Right ([], tokens)
parsePVDEList a tokens = do
    (param, rest1) <- parsePVDE a tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (params, rest3) <- parsePVDEList a rest2
            Right(param : params, rest3)
        _ -> Right ([param], rest1)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmCallDecl'@.
--
-- On success, this function returns a @'A.ParmCallDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse the a single parameter of a function call (e.g.: 4, 'c', foo(), ...).
parseParmCallDecl :: ([A.Decl], A.Decl) -> Parser A.ParmCallDecl
parseParmCallDecl _ t@((T.Punctuator (T.SBracket T.OpenSBracket), _) : _) = do
    (listLit, rest) <- parseListLiteral t
    Right (A.ParmCallDeclLiteral listLit, rest)
parseParmCallDecl f tokens@((T.Identifier _, _) :
    (T.Punctuator (T.RBracket T.OpenRBracket), _) : _) = do
    (call, rest) <- parseCallExprDecl f tokens
    Right (A.ParmCallDeclExpr call, rest)
parseParmCallDecl f ((T.Identifier id1, pos) :
    (T.Punctuator (T.SBracket T.OpenSBracket), _) : rest1) = do
    _ <- doesVarExists pos f id1
    (idxExpr, rest2) <- parseOr
        (parseParmCallDeclBExpr f) (parseParmCallDecl f) rest1
    (_, rest3) <- expectToken (T.Punctuator (T.SBracket T.CloseSBracket))
        "Expected ']' after index" rest2
    parseIdxChain f (A.ParmCallDeclIdx (A.ParmCallDeclIdent id1) idxExpr) rest3
parseParmCallDecl f t@((T.Identifier _, pos) : _) = do
    (id1, rest) <- parseIdentifier t
    _ <- doesVarExists pos f id1
    Right (A.ParmCallDeclIdent id1, rest)
parseParmCallDecl _ ((T.Literal li, _) : rest) =
    Right (A.ParmCallDeclLiteral li, rest)
parseParmCallDecl f ((T.Punctuator (T.CBracket T.OpenCBracket), _): rest) = do
    (value, rest1) <- parseParmCallDeclList f rest
    (_, rest2) <- expectToken (T.Punctuator (T.CBracket T.CloseCBracket))
                    "expected '}'" rest1
    Right (A.ParmCallDeclList value, rest2)
parseParmCallDecl _ ((token, position) : _) =
    errorAt position ("Wrong variable assignation, got " ++ show token)
parseParmCallDecl _ [] = errorAt (1, 1) "Expected ParmCallDecl, got "

-- | Parses double index access (e.g.: @arr[i][j]@).
parseIdxChain :: ([A.Decl], A.Decl) -> A.ParmCallDecl -> Parser A.ParmCallDecl
parseIdxChain f base ((T.Punctuator (T.SBracket T.OpenSBracket), _) : rest1) = do
    (idxExpr, rest2) <- parseOr
        (parseParmCallDeclBExpr f) (parseParmCallDecl f) rest1
    (_, rest3) <- expectToken (T.Punctuator (T.SBracket T.CloseSBracket))
        "Expected ']' after index" rest2
    parseIdxChain f (A.ParmCallDeclIdx base idxExpr) rest3
parseIdxChain _ base rest = Right (base, rest) 

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmCallDecl'@.
--
-- On success, this function returns a @'A.ParmCallDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a single parameter of a function call Either a BinaryOpExpr or a simple parameter.
parseSingle :: ([A.Decl], A.Decl) -> Parser A.ParmCallDecl
parseSingle f tokens = case tokens of
    ((T.Punctuator (T.RBracket T.OpenRBracket), _) : rest) -> do
        (p1, rest1) <- parseParmCallDeclBExpr f rest
        (_, rest2) <- expectToken
            (T.Punctuator (T.RBracket T.CloseRBracket)) "expected ')'" rest1
        Right (p1, rest2)
    _ -> parseParmCallDecl f tokens

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmCallDecl'@.
--
-- On success, this function returns a @'A.ParmCallDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a BinaryOpExpr in a function (e.g.: 6 - 4, foo(4) * 14 - 67).
parseParmCallDeclBExpr :: ([A.Decl], A.Decl) -> Parser A.ParmCallDecl
parseParmCallDeclBExpr f tokens = do
    (p1, rest) <- parseSingle f tokens
    case parseBinaryOp rest of
        Right (op, rest1) -> do
            (p2, r) <- parseSingle f rest1
            Right (A.ParmCallBExpr (A.BinaryOpParm p1)op(A.BinaryOpParm p2),r)
        Left _ -> Right (p1, rest)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.ParmCallDecl'@.
--
-- On success, this function returns a @'A.ParmCallDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse all the parameters of a function call.
parseParmCallDeclList :: ([A.Decl], A.Decl) -> Parser [A.ParmCallDecl]
parseParmCallDeclList _ t@((T.Punctuator (T.RBracket T.CloseRBracket), _) : _)
    = Right ([], t)
parseParmCallDeclList f tokens = do
    (arg, rest1) <- parseOr (parseParmCallDeclBExpr f)
        (parseParmCallDecl f) tokens
    case rest1 of
        ((T.Punctuator T.Comma, _) : rest2) -> do
            (args, rest3) <- parseParmCallDeclList f rest2
            Right (arg : args, rest3)
        _ -> Right ([arg], rest1)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.CallExprDecl'@.
--
-- On success, this function returns a @'A.CallExprDecl'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a function call.
parseCallExprDecl :: ([A.Decl], A.Decl) -> Parser A.CallExprDecl
parseCallExprDecl f tokens = do
    (fname, rest1) <- parseIdentifier tokens
    (_, rest2) <- expectToken (T.Punctuator
        (T.RBracket T.OpenRBracket)) "Expected '(' after function name" rest1
    (parmcldllist, rest3) <- parseParmCallDeclList f rest2
    (_, rest4) <- expectToken (T.Punctuator
        (T.RBracket T.CloseRBracket)) "Expected ')'" rest3
    Right (A.CallExprDecl fname parmcldllist, rest4)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.DeclStmt'@.
--
-- On success, this function returns a @'A.DeclStmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an assignment statement (@`foo = bar`@ or @`foo++`@).
parseDeclAssignStmtLiteral :: ([A.Decl], A.Decl) -> Parser A.DeclStmt
parseDeclAssignStmtLiteral f@(fl, _) tokens = do
    (id1, rest1) <- parseIdentifier tokens
    var <- doesVarExists (getPos 0 tokens) f id1
    (ap, r2) <- parseAssignOp rest1
    (p, rest3) <- parseOr (parseParmCallDeclBExpr f) (parseParmCallDecl f) r2
    case getType fl p of
        Nothing -> Right (A.DeclAssignStmtLiteral id1 ap p, rest3)
        Just tp -> do
            _ <- canAssign (getPos 0 tokens) var tp
            Right (A.DeclAssignStmtLiteral id1 ap p, rest3)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.VarDeclStmt'@.
--
-- On success, this function returns a @'A.VarDeclStmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse a variable declaration statement (@`Int foo = 1`@).
parseVarDeclStmt :: ([A.Decl], A.Decl) -> Parser A.VarDeclStmt
parseVarDeclStmt f@(fl, _) tokens = do
    (typ, rest1) <- parseBuiltinType fl tokens
    (name, rest2) <- parseIdentifier rest1
    (op, rest3) <- parseAssignOp rest2
    (value, rest4) <-
        parseOr (parseParmCallDeclBExpr f) (parseParmCallDecl f) rest3
    case doesVarExists (1, 1) f name of 
        Right _ -> errorAt (getPos 0 rest1)
            ("variable already exists " ++ show name)
        Left _ -> Right (A.VarDeclStmt typ name op value, rest4)

canUnaryAssign :: (Int, Int) -> A.Decl -> Either String String
canUnaryAssign p var =
    case getTypeDecl var of
        Nothing -> errorAt p "Trying to increment void"
        Just tp -> canAssign p var tp

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.DeclStmt'@.
--
-- On success, this function returns a @'A.DeclStmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an assignment statement (@`foo = bar`@ or @`foo++`@).
parseDeclAssignStmtIDX :: ([A.Decl], A.Decl) -> T.Identifier -> Parser A.DeclStmt
parseDeclAssignStmtIDX f var tokens = do
    (ap, rest2) <- parseAssignOp tokens
    (parmcldl, rest3)
        <- parseOr (parseParmCallDeclBExpr f) (parseParmCallDecl f) rest2
    Right (A.DeclAssignStmtLiteral var ap parmcldl, rest3)

-- | Takes an @'([A.Decl], A.Decl)'@ and a @'[SingleToken]'@ as parameter and
-- returns a __Either__ @'String'@ @'A.DeclStmt'@.
--
-- On success, this function returns a @'A.DeclStmt'@.
--
-- On failure, this function returns a pretty formatted message error.
--
-- This function is used to parse an operation on an existing variable.
parseDeclStmt :: ([A.Decl], A.Decl) -> Parser A.DeclStmt
parseDeclStmt f@(fl, _) tokens@((T.Identifier _, pos) :
    (T.Punctuator (T.SBracket T.OpenSBracket),_) : _) =
    case parseParmCallDecl f tokens of
        Right (var@(A.ParmCallDeclIdx _ ex), rest) ->
            case getType fl ex of
                Just _ -> parseDeclAssignStmtIDX f (show var) rest
                _ -> errorAt pos ("could not get type: " ++ show ex)
        _ -> errorAt pos "unexpected result"
parseDeclStmt f tokens@((T.Identifier _, pos) : _) = do
    (i, rest1) <- parseIdentifier tokens
    var <- doesVarExists pos f i
    case rest1 of
        ((T.Punctuator (T.UnaryOp u), _) : rest2) -> do
            _ <- canUnaryAssign pos var
            Right (A.DeclAssignStmtUnary (A.UnaryOperatorExpr i u), rest2)
        _ -> parseDeclAssignStmtLiteral f tokens
parseDeclStmt _ ((token, position) : _) =
    errorAt position ("Expected identifier, got " ++ show token)
parseDeclStmt _ _ = errorAt (1, 1) "Expected identifier, got Nothing"

-- | Takes an @'Integer'@ and a stream of @'SingleToken'@ as parameters,
--  and returns a tuple of @'Integer'@
--
-- this function serves as a helper function to get the position of the first or last token in the stream.
getPos :: Int -> [SingleToken] -> (Int, Int)
getPos 0 ((_, (l, c)): _) = (l, c - 1)
getPos 1 [(_, (l, c))] = (l, c + 1)
getPos 1 (_ : xs) = getPos 1 xs
getPos _ _ = (1, 1)
