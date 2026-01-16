{--
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- Bytecode compiler for the Lisp backend
-- Translates AST expressions into a simple stack-based bytecode
--}

-- | Bytecode compilation module
--   This module is responsible for translating high-level AST
--   expressions and declarations into bytecode instructions
module Bytecode (
    compileDecl
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (ord)

import Ast

-- | Function environment
--   Maps a function name to its list of parameter identifiers
type FuncEnv = Map Identifier [Identifier]

-- | Build a function environment from a list of expressions
--   It scans the AST and collects all top-level function definitions
buildFunc :: [Expr] -> FuncEnv
buildFunc = foldr collect Map.empty
  where
    collect (Defun (Func name params _)) acc =
        Map.insert name params acc
    collect _ acc = acc

-- | Compile a list of top-level expressions into bytecode
--   This function first builds the function environment then compiles each expression sequentially
compileDecl :: [Expr] -> String
compileDecl exprs =
    let env = buildFunc exprs
    in concatMap (compileTop env) exprs

-- | Compile a top-level expression
--   Function definitions generate bytecode functions, calls are compiled directly, and other expressions are evaluated and popped from the stack
compileTop :: FuncEnv -> Expr -> String
compileTop env (Defun d)     = compileDeclFromDecl env d
compileTop env (Call f args) = compileCallWithNamedParams env f args
compileTop env e             = compileExpr env e ++ "POP\n"

-- | Compile a declaration into bytecode
--   This includes named functions, lambdas, and global definitions
compileDeclFromDecl :: FuncEnv -> Decl -> String
compileDeclFromDecl env (Func name params body) =
    let header   = "FUNC " ++ name ++ " " ++ show (length params) ++ "\n"
        bodyCode = compileExpr env body
        footer   = "RET\nENDFUNC\n"
    in header ++ bodyCode ++ footer

compileDeclFromDecl env (Lambda params body) =
    let name     = lambdaLabel params body
        header   = "FUNC " ++ name ++ " " ++ show (length params) ++ "\n"
        bodyCode = compileExpr env body
        footer   = "RET\nENDFUNC\n"
    in header ++ bodyCode ++ footer

compileDeclFromDecl env (Define name expr) =
    compileExpr env expr ++ "STORE " ++ name ++ "\n"

-- | Compile an expression into bytecode
--   Expressions push their result onto the stack
compileExpr :: FuncEnv -> Expr -> String
compileExpr _ (Const n) =
    compileNumber n

compileExpr env (If cond th el) =
    let elseLabel = label "else" cond
        endLabel  = label "end" cond
    in compileCond env cond
    ++ "JMP_IF_FALSE " ++ elseLabel ++ "\n"
    ++ compileExpr env th
    ++ "JMP " ++ endLabel ++ "\n"
    ++ "LABEL " ++ elseLabel ++ "\n"
    ++ compileExpr env el
    ++ "LABEL " ++ endLabel ++ "\n"

compileExpr env (Call name args) =
    compileCallWithNamedParams env name args

compileExpr env (BinaryOp (ArithExpr op) l r) =
    compileExpr env l
 ++ compileExpr env r
 ++ arithOpToInstr op ++ "\n"

compileExpr env (BinaryOp (CondExpr op) l r) =
    compileExpr env l
 ++ compileExpr env r
 ++ condOpToInstr op ++ "\n"

compileExpr env (Defun d) =
    compileDeclFromDecl env d

-- | Compile a numeric or identifier constant into bytecode
compileNumber :: Number -> String
compileNumber (Boolean b) =
    "PUSH_BOOL " ++ if b then "true\n" else "false\n"
compileNumber (Int i) =
    "PUSH_INT " ++ show i ++ "\n"
compileNumber (SPrecision f) =
    "PUSH_FLOAT " ++ show f ++ "\n"
compileNumber (Identifier i) =
    "LOAD " ++ i ++ "\n"

-- | Compile a conditional operator or operand
--   Used primarily in 'if' expressions
compileCond :: FuncEnv -> CondOperator -> String
compileCond _ (OpBool b) =
    "PUSH_BOOL " ++ if b then "true\n" else "false\n"
compileCond _ (OpIdentifier i) =
    "LOAD " ++ i ++ "\n"
compileCond _ OpLt = "LT\n"
compileCond _ OpEq = "EQ\n"

-- | Compile a function call
--   If the function exists in the environment, its parameters are stored by name before calling it
compileCallWithNamedParams :: FuncEnv -> Identifier -> [Expr] -> String
compileCallWithNamedParams env fname args =
    let argsCode = concatMap (compileExpr env) args
    in case Map.lookup fname env of
        Nothing ->
            argsCode ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"
        Just params ->
            let storeCode =
                    concatMap (\p -> "STORE " ++ p ++ "\n") (reverse params)
            in argsCode
            ++ storeCode
            ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"

-- | Translate an arithmetic operator into its bytecode instruction
arithOpToInstr :: ArithOperator -> String
arithOpToInstr OpAdd = "ADD"
arithOpToInstr OpSub = "SUB"
arithOpToInstr OpMul = "MUL"
arithOpToInstr OpDiv = "DIV"
arithOpToInstr OpMod = "MOD"

-- | Translate a conditional operator into its bytecode instruction
condOpToInstr :: CondOperator -> String
condOpToInstr OpLt = "LT"
condOpToInstr OpEq = "EQ"
condOpToInstr _    = "NOP"

-- | Generate a deterministic label based on a value
--   Used to avoid label collisions in control-flow constructs
label :: Show a => String -> a -> String
label kind a =
    let s = show a
        h = abs (sum (map ord s)) `mod` 1000000
    in kind ++ "_" ++ show h

-- | Generate a unique label for a lambda function
--   based on its parameters and body
lambdaLabel :: Show a => [Identifier] -> a -> String
lambdaLabel params body =
    let s = show params ++ show body
        h = abs (sum (map ord s)) `mod` 1000000
    in "lambda_" ++ show h
