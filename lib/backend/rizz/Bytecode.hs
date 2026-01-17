{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/backend/rizz/Bytecode.hs
-}

{-|
Module      : Bytecode
Description : AST to Bytecode
Copyright   : EPITECH, 2025
License     : All rights reserved

This module is responsible for compiling an AST into a textual bytecode representation for a virtual machine

It handles:
  * Function declarations
  * Function calls with named parameters
  * Binary expressions
  * Control structures (if, while, for)
  * Literals and arithmetic/logical operations
  * Loop controls (break / continue)
-}

module Bytecode (
    compileDecl,
    compileFunctionDecl
) where

import Ast
import Tokens
import Data.Map (Map)
import qualified Data.Map as Map

-- | Function environment
-- Maps a function identifier to its list of parameters
type FuncEnv = Map Identifier [Identifier]

-- | Context used for loop compilation
-- Allows break and continue to know where to jump
data LoopCtx = LoopCtx {
    continueLabel :: String,
    breakLabel    :: String
}

-- | Builds the function environment from a list of declarations
--
-- Only function declarations are collected
buildFuncEnv :: [Decl] -> FuncEnv
buildFuncEnv = foldr collect Map.empty
  where
    collect (FunctionDecl name params _ _) acc =
        Map.insert name (map paramName params) acc
    collect _ acc = acc

    -- | Extracts the name of a parameter
    paramName :: ParmVarDeclExpr -> Identifier
    paramName (ParmVarDeclExpr _ ident) = ident

-- | Compiles a list of declarations into bytecode
--
-- This function first builds the function environment then compiles each declaration
compileDecl :: [Decl] -> String
compileDecl decls =
    let env = buildFuncEnv decls
    in concatMap (compileDeclWithEnv env) decls

-- | Compiles a single declaration using a pre-built function environment
compileDeclWithEnv :: FuncEnv -> Decl -> String
compileDeclWithEnv env (FunctionDecl name params body _ret) =
    compileFunctionWithEnv env name params body
compileDeclWithEnv _ _ = ""

-- | Compiles a complete function (start and end)
compileFunctionWithEnv :: FuncEnv -> Identifier -> [ParmVarDeclExpr] -> CompoundStmt -> String
compileFunctionWithEnv env name params (CompoundStmt stmts) =
    let header = "FUNC " ++ name ++ " " ++ show (length params) ++ "\n"
        body = concatMap (compileStmt env Nothing) stmts
        footer = "ENDFUNC\n"
    in header ++ body ++ footer

-- | Compiles a standalone function declaration
--
-- A minimal environment is created containing only the current function
compileFunctionDecl :: Identifier -> [ParmVarDeclExpr] -> CompoundStmt -> String
compileFunctionDecl name params =
    compileFunctionWithEnv
        (Map.singleton name
            (map (\(ParmVarDeclExpr _ ident) -> ident) params))
        name
        params

-- | Compiles a statement into bytecode
compileStmt :: FuncEnv -> Maybe LoopCtx -> Stmt -> String

-- Variable declaration with initialization
compileStmt env _ (DeclVarExpr (VarDeclStmt _ ident _ rhs)) =
    compileParmCall env rhs ++
    "STORE " ++ ident ++ "\n"

-- Simple assignment
compileStmt env _ (DeclStmt (DeclAssignStmtLiteral ident _ rhs)) =
    compileParmCall env rhs ++
    "STORE " ++ ident ++ "\n"

-- Increment / decrement unary operation
compileStmt _ _ (DeclStmt (DeclAssignStmtUnary (UnaryOperatorExpr ident op))) =
    case op of
        IdentIncrement ->
            "LOAD " ++ ident ++ "\n" ++
            "PUSH_INT 1\n" ++
            "ADD\n" ++
            "STORE " ++ ident ++ "\n"
        IdentDecrement ->
            "LOAD " ++ ident ++ "\n" ++
            "PUSH_INT 1\n" ++
            "SUB\n" ++
            "STORE " ++ ident ++ "\n"

-- Function call
compileStmt env _ (CallExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args

-- Binary expression used as a statement
compileStmt env _ (BinaryOperator expr) =
    compileBinaryOpExpr env expr ++
    "POP\n"

-- Return statement
compileStmt env _ (RetStmt (Just expr)) =
    compileBinaryOpExpr env expr ++
    "RET\n"

-- If / else statement
compileStmt env ctx (IfStmt cond (CompoundStmt body) mElse) =
    let condCode = compileBinaryOpExpr env cond
        thenCode = concatMap (compileStmt env ctx) body
        elseCode = maybe ""
            (\(CompoundStmt b) -> concatMap (compileStmt env ctx) b) mElse
    in condCode ++
       "JMP_IF_FALSE endif\n" ++
       thenCode ++
       "LABEL endif\n" ++
       elseCode

-- While loop with break/continue support
compileStmt env _ (WhileStmt cond (CompoundStmt body)) =
    let ctx = LoopCtx {
            continueLabel = "while_start",
            breakLabel = "while_end"
        }
    in "LABEL while_start\n" ++
       compileBinaryOpExpr env cond ++
       "JMP_IF_FALSE while_end\n" ++
       concatMap (compileStmt env (Just ctx)) body ++
       "JMP while_start\n" ++
       "LABEL while_end\n"

-- For loop with break/continue support
compileStmt env _ (ForStmt mInit mCond mStep (CompoundStmt body)) =
    let ctx = LoopCtx {
            continueLabel = "for_continue",
            breakLabel = "for_end"
        }
    in compileForInit env mInit ++
       "LABEL for_start\n" ++
       compileForCond env mCond ++
       concatMap (compileStmt env (Just ctx)) body ++
       "LABEL for_continue\n" ++
       compileForStep env mStep ++
       "JMP for_start\n" ++
       "LABEL for_end\n"

-- Loop control statements (break / continue)
compileStmt _ Nothing (LoopControlStmt _) =
    "NOP\n"

compileStmt _ (Just ctx) (LoopControlStmt kw) =
    case kw of
        Continue -> "JMP " ++ continueLabel ctx ++ "\n"
        Break    -> "JMP " ++ breakLabel ctx ++ "\n"
        _        -> "NOP\n"

-- Unrecognized statement
compileStmt _ _ _ = "NOP\n"

-- | Compiles a function call with named parameters
compileCallWithNamedParams :: FuncEnv -> Identifier -> [ParmCallDecl] -> String
compileCallWithNamedParams env fname args =
    let argsCode = concatMap (compileParmCall env) args
    in case Map.lookup fname env of
        Nothing ->
            argsCode ++
            "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"
        Just params ->
            let storeCode =
                    concatMap (\p -> "STORE " ++ p ++ "\n")
                        (reverse params)
            in argsCode ++
               storeCode ++
               "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"

-- | Compiles a binary expression into bytecode
compileBinaryOpExpr :: FuncEnv -> BinaryOpExpr -> String
compileBinaryOpExpr env (BinaryOpConst p) =
    compileParmCall env p
compileBinaryOpExpr env (BinaryOpExpr l op r) =
    compileBinaryOpParm env l ++
    compileBinaryOpParm env r ++
    opToInstr op ++ "\n"

-- | Compiles a binary operator operand
compileBinaryOpParm :: FuncEnv -> BinaryOpParm -> String
compileBinaryOpParm env (BinaryOpParm p) =
    compileParmCall env p
compileBinaryOpParm env (BinaryOpParmBOp expr) =
    compileBinaryOpExpr env expr

-- | Compiles a parameter (literal, identifier, or function call)
compileParmCall :: FuncEnv -> ParmCallDecl -> String
compileParmCall _   (ParmCallDeclLiteral lit) = compileLiteral lit
compileParmCall _   (ParmCallDeclIdent ident) = "LOAD " ++ ident ++ "\n"
compileParmCall env (ParmCallDeclExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args

compileParmCall env (ParmCallDeclList _) = "\n"
compileParmCall env (ParmCallBExpr l op r) =
    compileBinaryOpParm env l ++
    compileBinaryOpParm env r ++
    opToInstr op ++ "\n"

-- | Compiles the initialization part of a for loop
compileForInit :: FuncEnv -> Maybe VarDeclStmt -> String
compileForInit _   Nothing = ""
compileForInit env (Just vds) = compileStmt env Nothing (DeclVarExpr vds)

-- | Compiles the condition part of a for loop
compileForCond :: FuncEnv -> Maybe BinaryOpExpr -> String
compileForCond _   Nothing = ""
compileForCond env (Just cond) =
    compileBinaryOpExpr env cond ++
    "JMP_IF_FALSE for_end\n"

-- | Compiles the step part of a for loop
compileForStep :: FuncEnv -> Maybe DeclStmt -> String
compileForStep _   Nothing = ""
compileForStep env (Just ds) = compileStmt env Nothing (DeclStmt ds)

-- | Compiles a literal into bytecode
compileLiteral :: Literal -> String
compileLiteral (CharLiteral i)   = "PUSH_CHAR " ++ show i ++ "\n"
compileLiteral (IntLiteral i)   = "PUSH_INT " ++ show i ++ "\n"
compileLiteral (BoolLiteral b)  = "PUSH_BOOL " ++
    (if b then "True\n" else "False\n")
compileLiteral (FloatLiteral f) = "PUSH_FLOAT " ++ show f ++ "\n"
compileLiteral (ListLiteral elems) =
    concatMap compileLiteral elems ++
    "PUSH_LIST " ++ show (length elems) ++ "\n"

-- | Converts a binary operator into a bytecode instruction
opToInstr :: BinaryOp -> String
opToInstr Add = "ADD"
opToInstr Sub = "SUB"
opToInstr Mul = "MUL"
opToInstr Div = "DIV"
opToInstr Mod = "MOD"
opToInstr Eq  = "EQ"
opToInstr Lt  = "LT"
opToInstr Gt  = "GT"
opToInstr LEq = "LE"
opToInstr GEq = "GE"
opToInstr NEq = "NEQ"
opToInstr And = "AND"
opToInstr Or  = "OR"
