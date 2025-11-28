{- 
-- EPITECH PROJECT, 2025
-- GENERIC LANGUAGE AND DATA OPERAND SYNTAX
-- File description:
-- src/Parser.hs
-}

module Interpretor (interpretResult) where

-- import Parser (Ast(..))
import Control.Exception (try, SomeException, evaluate)
import Error

type Identifier     = String
type Operator       = Identifier
type Args           = [Identifier]
data FunCall        = FunCall Operator Args Ast deriving Show

data Ast = Define   Identifier Ast
    |      Lambda   Identifier Args Ast
    |      If       FunCall Ast Ast
    |      Var      Identifier
    |      Bool     Bool
    |      Integer  Integer
    |      Call     FunCall
    deriving Show

-- Ram through the AST and interpret it  
interpret :: Ast -> Ast
{- interpret (TODO _) = throwErr $ Error.ErrorT
    { location = 0, message = "No input" }  -}
interpret (Var x) = Var x
interpret _ = Var "Nothing"

interpretResult :: Ast -> IO Ast
interpretResult ast = do
    evalParser <- try (evaluate (interpret ast))
        :: IO (Either SomeException Ast)
    case evalParser of
        Left err -> printError (show err)
        Right str -> return str
