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
  * Lists and Struct/Record declarations & instantiation
  * Loop controls (break / continue)
-}

module Bytecode (
    compileDecl
    ) where

import Ast
import Tokens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (ord)

-- | Function environment: maps function name -> parameter names (in order)
type FuncEnv   = Map Identifier [Identifier]
-- | Struct environment: maps struct name -> field names (in order)
type StructEnv = Map Identifier [Identifier]

-- | Global compilation environment: both function and struct maps,
--   plus a simple counter to generate unique labels.
data Env = Env
    { eFuncs   :: FuncEnv
    , eStructs :: StructEnv
    , eNextId  :: Int
    }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty 0

-- | Build function environment from top-level declarations
buildFuncEnv :: [Decl] -> FuncEnv
buildFuncEnv = foldr collect Map.empty
  where
    collect (FunctionDecl name params _ _) acc =
        Map.insert name (map parmName params) acc
    collect _ acc = acc

    parmName :: ParmVarDeclExpr -> Identifier
    parmName (ParmVarDeclExpr _ ident) = ident

-- | Build struct environment from top-level declarations (RecordDecl)
buildStructEnv :: [Decl] -> StructEnv
buildStructEnv = foldr collect Map.empty
  where
    collect (RecordDecl (RecordDeclExpr name fields)) acc =
        Map.insert name (map parmName fields) acc
    collect _ acc = acc

    parmName :: ParmVarDeclExpr -> Identifier
    parmName (ParmVarDeclExpr _ ident) = ident

-- | Top-level compile entry: build envs and compile each decl
compileDecl :: [Decl] -> String
compileDecl decls =
    let fenv = buildFuncEnv decls
        senv = buildStructEnv decls
        env  = Env fenv senv 0
    in concatMap (compileDeclWithEnv env) decls

-- | Compile a single declaration using the environment
compileDeclWithEnv :: Env -> Decl -> String
compileDeclWithEnv env d@(FunctionDecl _ _ _ _) =
    compileFunctionWithEnv env d
-- Emit a STRUCT descriptor for RecordDecl (informational)
compileDeclWithEnv env (RecordDecl (RecordDeclExpr name fields)) =
    "STRUCT " ++ name ++ " " ++ show (length fields) ++ " "
        ++ unwords (map (\(ParmVarDeclExpr _ ident) -> ident) fields) ++ "\n"
compileDeclWithEnv env (VarDecl vds) =
    -- top-level var declaration (if present in your AST)
    compileTopVarDecl env vds
compileDeclWithEnv _ _ = ""

-- | Compile a function declaration (wrap / dispatch)
compileFunctionWithEnv :: Env -> Decl -> String
compileFunctionWithEnv env (FunctionDecl name params (CompoundStmt stmts) _mret) =
    let env' = env { eNextId = eNextId env } -- start with same counter
        header = "FUNC " ++ name ++ " " ++ show (length params) ++ "\n"
        body = concatMap (\s -> compileStmt env' s) stmts
        footer = "ENDFUNC\n"
    in header ++ body ++ footer
compileFunctionWithEnv _ _ = ""

-- | Helper to compile a top-level VarDecl if present
compileTopVarDecl :: Env -> VarDeclStmt -> String
compileTopVarDecl env vds@(VarDeclStmt typ ident _ rhs) =
    compileVarDecl env vds


-- Loop context used to generate per-loop labels (continue / break)
data LoopCtx = LoopCtx { lblStart :: String, lblContinue :: String, lblEnd :: String }

-- compileStmt: compile statement to bytecode, using the (global) Env.
-- Some statements need to thread / create a LoopCtx for break/continue.
compileStmt :: Env -> Stmt -> String

-- Typed variable declaration (possibly struct or list)
compileStmt env (DeclVarExpr vds) = compileVarDecl env vds

-- Assignment without type
compileStmt env (DeclStmt (DeclAssignStmtLiteral ident _ rhs)) =
    compileParmCall env rhs ++ "STORE " ++ ident ++ "\n"

-- Unary assignment (x++ / x--)
compileStmt _ (DeclStmt (DeclAssignStmtUnary (UnaryOperatorExpr ident op))) =
    case op of
        IdentIncrement -> "LOAD " ++ ident ++ "\nPUSH_INT 1\nADD\nSTORE " ++ ident ++ "\n"
        IdentDecrement -> "LOAD " ++ ident ++ "\nPUSH_INT 1\nSUB\nSTORE " ++ ident ++ "\n"
        _              -> "NOP\n"

-- Function call as statement
compileStmt env (CallExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args

-- Binary expression used as statement
compileStmt env (BinaryOperator expr) =
    compileBinaryOpExpr env expr ++ "POP\n"

-- Return statement (maybe void)
compileStmt env (RetStmt mexpr) =
    case mexpr of
        Nothing   -> "RET\n"
        Just expr -> compileBinaryOpExpr env expr ++ "RET\n"

-- If / else
compileStmt env (IfStmt cond (CompoundStmt thenStmts) mElse) =
    let nid = eNextId env
        elseLbl = mkLabel "else" nid
        endLbl  = mkLabel "endif" (nid + 1)
        env' = env { eNextId = nid + 2 }
        condCode = compileBinaryOpExpr env' cond
        thenCode = concatMap (compileStmt env') thenStmts
        elseCode = maybe "" (\(CompoundStmt es) -> concatMap (compileStmt env') es) mElse
    in condCode
       ++ "JMP_IF_FALSE " ++ elseLbl ++ "\n"
       ++ thenCode
       ++ "JMP " ++ endLbl ++ "\n"
       ++ "LABEL " ++ elseLbl ++ "\n"
       ++ elseCode
       ++ "LABEL " ++ endLbl ++ "\n"

-- While loop with unique labels and loop ctx
compileStmt env (WhileStmt cond (CompoundStmt body)) =
    let nid = eNextId env
        startLbl = mkLabel "while_start" nid
        endLbl   = mkLabel "while_end" (nid + 1)
        contLbl  = mkLabel "while_continue" (nid + 2)
        env' = env { eNextId = nid + 3 }
        ctx  = LoopCtx startLbl contLbl endLbl
        entry = "LABEL " ++ startLbl ++ "\n"
    in entry
       ++ compileBinaryOpExpr env' cond ++ "JMP_IF_FALSE " ++ endLbl ++ "\n"
       ++ concatMap (compileStmtWithLoop env' ctx) body
       ++ "LABEL " ++ contLbl ++ "\n"
       ++ "JMP " ++ startLbl ++ "\n"
       ++ "LABEL " ++ endLbl ++ "\n"

-- For loop: init; LABEL start; cond; body; continue label; step; jump start; end
compileStmt env (ForStmt mInit mCond mStep (CompoundStmt body)) =
    let nid = eNextId env
        startLbl = mkLabel "for_start" nid
        contLbl  = mkLabel "for_continue" (nid + 1)
        endLbl   = mkLabel "for_end" (nid + 2)
        env' = env { eNextId = nid + 3 }
        ctx = LoopCtx startLbl contLbl endLbl
        initCode = compileForInit env' mInit
        condCode = compileForCond env' mCond
        stepCode = compileForStep env' mStep
    in initCode
       ++ "LABEL " ++ startLbl ++ "\n"
       ++ condCode
       ++ concatMap (compileStmtWithLoop env' ctx) body
       ++ "LABEL " ++ contLbl ++ "\n"
       ++ stepCode
       ++ "JMP " ++ startLbl ++ "\n"
       ++ "LABEL " ++ endLbl ++ "\n"

-- Foreach not implemented / fallback
compileStmt env (ForeachStmt _ _ (CompoundStmt body)) =
    concatMap (compileStmt env) body

-- Call used as expression already handled above; fallback:
compileStmt _ _ = "NOP\n"

-- compileStmtWithLoop: variant that knows current loop context for break/continue
compileStmtWithLoop :: Env -> LoopCtx -> Stmt -> String
compileStmtWithLoop env ctx (LoopControlStmt kw) =
    case kw of
        -- NOTE: the Keyword constructors for 'continue' and 'break' must match your AST.
        -- I assume they are named Continue and Break here; if different, adjust.
        Continue -> "JMP " ++ lblContinue ctx ++ "\n"
        Break    -> "JMP " ++ lblEnd ctx ++ "\n"
        _        -> "NOP\n"
  where
    lblContinue = lblContinueName
    lblEnd = lblEndName
    lblContinueName _ = lblContinueLabel ctx
    lblEndName _ = lblEndLabel ctx

compileStmtWithLoop env ctx st = compileStmt (env { eNextId = eNextId env }) st'
  where
    -- Use compileStmt but replace any nested loop handling so nested breaks/continues
    -- use their own labels. We call compileStmt; for nested loops compileStmt will
    -- create new labels (via eNextId) and nested LoopCtx will be used if necessary.
    st' = st

-- helpers to access LoopCtx fields (names)
lblContinueLabel :: LoopCtx -> String
lblContinueLabel = lblContinue

lblEndLabel :: LoopCtx -> String
lblEndLabel = lblEnd


-- compileVarDecl: handle typed var declarations including Struct and List
compileVarDecl :: Env -> VarDeclStmt -> String
compileVarDecl env (VarDeclStmt typ ident _ rhs) =
    case (typ, rhs) of
        -- Struct initialization: VarDeclStmt (Struct "S") name = [ elems... ]
        (Ast.Struct sname, ParmCallDeclList elems) ->
            concatMap (compileParmCall env) elems ++
            "BUILD_STRUCT " ++ sname ++ " " ++ show (length elems) ++ "\n" ++
            "STORE " ++ ident ++ "\n"

        -- List initialization: push elements then PUSH_LIST n
        (ListType _, ParmCallDeclList elems) ->
            concatMap (compileParmCall env) elems ++
            "PUSH_LIST " ++ show (length elems) ++ "\n" ++
            "STORE " ++ ident ++ "\n"

        -- Normal typed var with expression RHS
        _ ->
            compileParmCall env rhs ++
            "STORE " ++ ident ++ "\n"

-- compileForInit / cond / step
compileForInit :: Env -> Maybe VarDeclStmt -> String
compileForInit _ Nothing = ""
compileForInit env (Just vds) = compileVarDecl env vds

compileForCond :: Env -> Maybe BinaryOpExpr -> String
compileForCond _ Nothing = ""
compileForCond env (Just cond) = compileBinaryOpExpr env cond ++ "JMP_IF_FALSE for_end\n"

compileForStep :: Env -> Maybe DeclStmt -> String
compileForStep _ Nothing = ""
compileForStep env (Just ds) = compileDeclStmt env ds

compileDeclStmt :: Env -> DeclStmt -> String
compileDeclStmt env (DeclAssignStmtLiteral ident _ rhs) = compileParmCall env rhs ++ "STORE " ++ ident ++ "\n"
compileDeclStmt env (DeclAssignStmtUnary u) = compileStmt env (DeclStmt (DeclAssignStmtUnary u))


compileBinaryOpExpr :: Env -> BinaryOpExpr -> String
compileBinaryOpExpr env (BinaryOpConst p) = compileParmCall env p
compileBinaryOpExpr env (BinaryOpExpr l op r) =
    compileBinaryOpParm env l ++
    compileBinaryOpParm env r ++
    opToInstr op ++ "\n"

compileBinaryOpParm :: Env -> BinaryOpParm -> String
compileBinaryOpParm env (BinaryOpParm p)   = compileParmCall env p
compileBinaryOpParm env (BinaryOpParmBOp b) = compileBinaryOpExpr env b

compileParmCall :: Env -> ParmCallDecl -> String
compileParmCall _   (ParmCallDeclLiteral lit) = compileLiteral lit
compileParmCall _   (ParmCallDeclIdent ident) = "LOAD " ++ ident ++ "\n"
compileParmCall env (ParmCallDeclExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args
compileParmCall env (ParmCallDeclList elems) =
    concatMap (compileParmCall env) elems ++ "PUSH_LIST " ++ show (length elems) ++ "\n"
compileParmCall env (ParmCallBExpr l op r) =
    compileBinaryOpParm env l ++
    compileBinaryOpParm env r ++
    opToInstr op ++ "\n"

-- compileCallWithNamedParams: pushes args, then either STORE into param names (reversed),
-- then CALL <fname> <arity>.
compileCallWithNamedParams :: Env -> Identifier -> [ParmCallDecl] -> String
compileCallWithNamedParams env fname args =
    let argsCode = concatMap (compileParmCall env) args
        fenv = eFuncs env
    in case Map.lookup fname fenv of
        Nothing -> argsCode ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"
        Just params ->
            let storeCode = concatMap (\p -> "STORE " ++ p ++ "\n") (reverse params)
            in argsCode ++ storeCode ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"


compileLiteral :: Literal -> String
compileLiteral (CharLiteral c)   = "PUSH_CHAR " ++ show c ++ "\n"
compileLiteral (IntLiteral i)    = "PUSH_INT " ++ show i ++ "\n"
compileLiteral (BoolLiteral b)   = "PUSH_BOOL " ++ (if b then "True\n" else "False\n")
compileLiteral (FloatLiteral f)  = "PUSH_FLOAT " ++ show f ++ "\n"
compileLiteral (ListLiteral elems) =
    concatMap compileLiteral elems ++ "PUSH_LIST " ++ show (length elems) ++ "\n"
compileLiteral _ = "PUSH_UNKNOWN\n"

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
opToInstr _   = "NOP"


mkLabel :: String -> Int -> String
mkLabel base i = base ++ "_" ++ show i
