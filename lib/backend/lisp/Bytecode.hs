{-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/backend/lisp/Bytecode.hs
-}

module Bytecode (
    compileDecl
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (ord)

import Ast

type FuncEnv = Map Identifier [Identifier]

buildFunc :: [Expr] -> FuncEnv
buildFunc = foldr collect Map.empty
  where
    collect :: Expr -> FuncEnv -> FuncEnv
    collect (Defun (Func name params _)) acc = Map.insert name params acc
    collect _ acc = acc

compileDecl :: [Expr] -> String
compileDecl exprs =
    let env = buildFunc exprs
    in concatMap (compileTop env) exprs

compileTop :: FuncEnv -> Expr -> String
compileTop env (Defun d)        = compileDeclFromDecl env d
compileTop env (Call f args)    = compileCallWithNamedParams env f args
compileTop env e                = compileExpr env e ++ "POP\n"

compileDeclFromDecl :: FuncEnv -> Decl -> String
compileDeclFromDecl env (Func name params body) =
    let header   = "FUNC " ++ name ++ " " ++ show (length params) ++ "\n"
        bodyCode = compileExpr env body
        footer   = "RET\nENDFUNC\n"
    in header ++ bodyCode ++ footer
compileDeclFromDecl env (Define name expr) =
    compileExpr env expr ++ "STORE " ++ name ++ "\n"
compileDeclFromDecl _ (Lambda _ _) = "" 
compileExpr :: FuncEnv -> Expr -> String

compileExpr _ (Const n) = compileNumber n

compileExpr env (If cond th el) =
    let elseLabel = label "else" cond
        endLabel  = label "end" cond
        condCode  = compileCond env cond
        thenCode  = compileExpr env th
        elseCode  = compileExpr env el
    in condCode
       ++ "JMP_IF_FALSE " ++ elseLabel ++ "\n"
       ++ thenCode
       ++ "JMP " ++ endLabel ++ "\n"
       ++ "LABEL " ++ elseLabel ++ "\n"
       ++ elseCode
       ++ "LABEL " ++ endLabel ++ "\n"

compileExpr env (Call name args) = compileCallWithNamedParams env name args

compileExpr env (BinaryOp (ArithExpr op) l r) =
    compileExpr env l ++ compileExpr env r ++ arithOpToInstr op ++ "\n"
compileExpr env (BinaryOp (CondExpr condOp) l r) =
    compileExpr env l ++ compileExpr env r ++ condOpToInstr condOp ++ "\n"

compileExpr env (Defun d) = compileDeclFromDecl env d

compileNumber :: Number -> String
compileNumber (Boolean b)    = "PUSH_BOOL " ++ (if b then "true\n" else "false\n")
compileNumber (Int i)        = "PUSH_INT " ++ show i ++ "\n"
compileNumber (SPrecision f) = "PUSH_FLOAT " ++ show f ++ "\n"
compileNumber (Identifier i) = "LOAD " ++ i ++ "\n"

compileCond :: FuncEnv -> CondOperator -> String
compileCond _ (OpBool b)        = "PUSH_BOOL " ++ (if b then "true\n" else "false\n")
compileCond _ (OpIdentifier i)  = "LOAD " ++ i ++ "\n"
compileCond _ OpLt              = "PUSH_BOOL false\n"
compileCond _ OpEq              = "PUSH_BOOL false\n"

compileCallWithNamedParams :: FuncEnv -> Identifier -> [Expr] -> String
compileCallWithNamedParams env fname args =
    let argsCode = concatMap (compileExpr env) args
    in case Map.lookup fname env of
        Nothing -> argsCode ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"
        Just params ->
            let
                storeCode = concatMap (\p -> "STORE " ++ p ++ "\n") (reverse params)
            in argsCode ++ storeCode ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"

arithOpToInstr :: ArithOperator -> String
arithOpToInstr OpAdd = "ADD"
arithOpToInstr OpSub = "SUB"
arithOpToInstr OpMul = "MUL"
arithOpToInstr OpDiv = "DIV"
arithOpToInstr OpMod = "MOD"

condOpToInstr :: CondOperator -> String
condOpToInstr OpLt = "LT"
condOpToInstr OpEq = "EQ"
condOpToInstr _    = "NOP"

label :: Show a => String -> a -> String
label kind a =
    let s = show a
        h = abs (sum (map ord s)) `mod` 1000000
    in kind ++ "_" ++ show h
