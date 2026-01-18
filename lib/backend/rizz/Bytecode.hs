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
-}

module Bytecode (
    compileDecl
) where

import Ast
import Tokens
import Data.Map (Map)
import qualified Data.Map as Map

type FuncEnv   = Map Identifier [Identifier]
type StructEnv = Map Identifier [Identifier]

data Env = Env
    { eFuncs   :: FuncEnv
    , eStructs :: StructEnv
    , eNextId  :: Int
    }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty 0

buildFuncEnv :: [Decl] -> FuncEnv
buildFuncEnv = foldr collect Map.empty
  where
    collect (FunctionDecl name params _ _) acc =
        Map.insert name (map parmName params) acc
    collect _ acc = acc

    parmName (ParmVarDeclExpr _ ident) = ident

buildStructEnv :: [Decl] -> StructEnv
buildStructEnv = foldr collect Map.empty
  where
    collect (RecordDecl (RecordDeclExpr name fields)) acc =
        Map.insert name (map parmName fields) acc
    collect _ acc = acc

    parmName (ParmVarDeclExpr _ ident) = ident

compileDecl :: [Decl] -> String
compileDecl decls =
    let fenv = buildFuncEnv decls
        senv = buildStructEnv decls
        env  = Env fenv senv 0
    in concatMap (compileDeclWithEnv env) decls

compileDeclWithEnv :: Env -> Decl -> String
compileDeclWithEnv env d@(FunctionDecl _ _ _ _) =
    compileFunctionWithEnv env d

compileDeclWithEnv _ (RecordDecl (RecordDeclExpr name fields)) =
    "STRUCT " ++ name ++ " " ++ show (length fields) ++ " "
        ++ unwords (map (\(ParmVarDeclExpr _ ident) -> ident) fields) ++ "\n"

compileDeclWithEnv env (VarDecl vds) =
    compileTopVarDecl env vds

compileDeclWithEnv _ _ = ""

compileFunctionWithEnv :: Env -> Decl -> String
compileFunctionWithEnv env (FunctionDecl name params (CompoundStmt stmts) _) =
    let header = "FUNC " ++ name ++ " " ++ show (length params) ++ "\n"
        body = concatMap (compileStmt env) stmts
    in header ++ body ++ "ENDFUNC\n"

compileFunctionWithEnv _ _ = ""

compileTopVarDecl :: Env -> VarDeclStmt -> String
compileTopVarDecl env vds = compileVarDecl env vds

data LoopCtx = LoopCtx
    { lblStart    :: String
    , lblContinue :: String
    , lblEnd      :: String
    }

compileStmt :: Env -> Stmt -> String

compileStmt env (DeclVarExpr vds) =
    compileVarDecl env vds

compileStmt env (DeclStmt (DeclAssignStmtLiteral ident _ rhs)) =
    compileParmCall env rhs ++ "STORE " ++ ident ++ "\n"

compileStmt _ (DeclStmt (DeclAssignStmtUnary (UnaryOperatorExpr ident op))) =
    case op of
        IdentIncrement ->
            "LOAD " ++ ident ++ "\nPUSH_INT 1\nADD\nSTORE " ++ ident ++ "\n"
        IdentDecrement ->
            "LOAD " ++ ident ++ "\nPUSH_INT 1\nSUB\nSTORE " ++ ident ++ "\n"
        _ -> "NOP\n"

compileStmt env (CallExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args

compileStmt env (BinaryOperator expr) =
    compileBinaryOpExpr env expr ++ "POP\n"

compileStmt env (RetStmt Nothing) = "RET\n"
compileStmt env (RetStmt (Just expr)) =
    compileBinaryOpExpr env expr ++ "RET\n"

compileStmt env (IfStmt cond (CompoundStmt thenStmts) mElse) =
    let nid = eNextId env
        elseLbl = mkLabel "else" nid
        endLbl  = mkLabel "endif" (nid + 1)
        env' = env { eNextId = nid + 2 }
        condCode = compileBinaryOpExpr env' cond
        thenCode = concatMap (compileStmt env') thenStmts
        elseCode = maybe "" (\(CompoundStmt es) ->
            concatMap (compileStmt env') es) mElse
    in condCode ++
       "JMP_IF_FALSE " ++ elseLbl ++ "\n" ++
       thenCode ++
       "JMP " ++ endLbl ++ "\n" ++
       "LABEL " ++ elseLbl ++ "\n" ++
       elseCode ++
       "LABEL " ++ endLbl ++ "\n"

compileStmt env (WhileStmt cond (CompoundStmt body)) =
    let nid = eNextId env
        startLbl = mkLabel "while_start" nid
        endLbl   = mkLabel "while_end" (nid + 1)
        contLbl  = mkLabel "while_continue" (nid + 2)
        env' = env { eNextId = nid + 3 }
        ctx  = LoopCtx startLbl contLbl endLbl
    in "LABEL " ++ startLbl ++ "\n" ++
       compileBinaryOpExpr env' cond ++
       "JMP_IF_FALSE " ++ endLbl ++ "\n" ++
       concatMap (compileStmtWithLoop env' ctx) body ++
       "LABEL " ++ contLbl ++ "\n" ++
       "JMP " ++ startLbl ++ "\n" ++
       "LABEL " ++ endLbl ++ "\n"

compileStmt env (ForStmt mInit mCond mStep (CompoundStmt body)) =
    let nid = eNextId env
        startLbl = mkLabel "for_start" nid
        contLbl  = mkLabel "for_continue" (nid + 1)
        endLbl   = mkLabel "for_end" (nid + 2)
        env' = env { eNextId = nid + 3 }
        ctx = LoopCtx startLbl contLbl endLbl
    in compileForInit env' mInit ++
       "LABEL " ++ startLbl ++ "\n" ++
       compileForCond env' mCond ++
       concatMap (compileStmtWithLoop env' ctx) body ++
       "LABEL " ++ contLbl ++ "\n" ++
       compileForStep env' mStep ++
       "JMP " ++ startLbl ++ "\n" ++
       "LABEL " ++ endLbl ++ "\n"

compileStmt env (ForeachStmt _ _ (CompoundStmt body)) =
    concatMap (compileStmt env) body

compileStmt _ _ = "NOP\n"

compileStmtWithLoop :: Env -> LoopCtx -> Stmt -> String
compileStmtWithLoop _ ctx (LoopControlStmt Continue) =
    "JMP " ++ lblContinue ctx ++ "\n"

compileStmtWithLoop _ ctx (LoopControlStmt Break) =
    "JMP " ++ lblEnd ctx ++ "\n"

compileStmtWithLoop env _ stmt =
    compileStmt env stmt

compileVarDecl :: Env -> VarDeclStmt -> String
compileVarDecl env (VarDeclStmt typ ident _ rhs) =
    case (typ, rhs) of
        (Ast.Struct sname, ParmCallDeclList elems) ->
            concatMap (compileParmCall env) elems ++
            "BUILD_STRUCT " ++ sname ++ " " ++ show (length elems) ++ "\n" ++
            "STORE " ++ ident ++ "\n"

        (ListType _, ParmCallDeclList elems) ->
            concatMap (compileParmCall env) elems ++
            "PUSH_LIST " ++ show (length elems) ++ "\n" ++
            "STORE " ++ ident ++ "\n"

        _ ->
            compileParmCall env rhs ++
            "STORE " ++ ident ++ "\n"

compileForInit :: Env -> Maybe VarDeclStmt -> String
compileForInit _ Nothing = ""
compileForInit env (Just vds) = compileVarDecl env vds

compileForCond :: Env -> Maybe BinaryOpExpr -> String
compileForCond _ Nothing = ""
compileForCond env (Just cond) =
    compileBinaryOpExpr env cond ++ "JMP_IF_FALSE for_end\n"

compileForStep :: Env -> Maybe DeclStmt -> String
compileForStep _ Nothing = ""
compileForStep env (Just ds) = compileDeclStmt env ds

compileDeclStmt :: Env -> DeclStmt -> String
compileDeclStmt env (DeclAssignStmtLiteral ident _ rhs) =
    compileParmCall env rhs ++ "STORE " ++ ident ++ "\n"

compileDeclStmt env (DeclAssignStmtUnary u) =
    compileStmt env (DeclStmt (DeclAssignStmtUnary u))

compileBinaryOpExpr :: Env -> BinaryOpExpr -> String
compileBinaryOpExpr env (BinaryOpConst p) = compileParmCall env p
compileBinaryOpExpr env (BinaryOpExpr l op r) =
    compileBinaryOpParm env l ++
    compileBinaryOpParm env r ++
    opToInstr op ++ "\n"

compileBinaryOpParm :: Env -> BinaryOpParm -> String
compileBinaryOpParm env (BinaryOpParm p) = compileParmCall env p
compileBinaryOpParm env (BinaryOpParmBOp b) = compileBinaryOpExpr env b

compileParmCall :: Env -> ParmCallDecl -> String

compileParmCall _ (ParmCallDeclLiteral lit) =
    compileLiteral lit

compileParmCall _ (ParmCallDeclIdent ident) =
    "LOAD " ++ ident ++ "\n"

compileParmCall env (ParmCallDeclExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args

compileParmCall env (ParmCallDeclList elems) =
    concatMap (compileParmCall env) elems ++
    "PUSH_LIST " ++ show (length elems) ++ "\n"

compileParmCall env (ParmCallBExpr l op r) =
    compileBinaryOpParm env l ++
    compileBinaryOpParm env r ++
    opToInstr op ++ "\n"

compileParmCall env (ParmCallDeclIdx base idx) =
    case base of
        ParmCallDeclIdent name ->
            case idx of
                ParmCallDeclLiteral (IntLiteral i) ->
                    "IND " ++ name ++ " " ++ show i ++ "\n"
                _ ->
                    compileParmCall env idx ++
                    "IND " ++ name ++ "\n"
        _ ->
            compileParmCall env base ++
            compileParmCall env idx ++
            "IND\n"

compileParmCall _ _ = ""

compileCallWithNamedParams :: Env -> Identifier -> [ParmCallDecl] -> String
compileCallWithNamedParams env fname args =
    let argsCode = concatMap (compileParmCall env) args
        fenv = eFuncs env
    in case Map.lookup fname fenv of
        Nothing ->
            argsCode ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"

        Just params ->
            let storeCode =
                    concatMap (\p -> "STORE " ++ p ++ "\n") (reverse params)
            in argsCode ++ storeCode ++
               "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"

compileLiteral :: Literal -> String
compileLiteral (CharLiteral c)  = "PUSH_CHAR " ++ show c ++ "\n"
compileLiteral (IntLiteral i)   = "PUSH_INT " ++ show i ++ "\n"
compileLiteral (BoolLiteral b)  =
    "PUSH_BOOL " ++ if b then "True\n" else "False\n"
compileLiteral (FloatLiteral f) = "PUSH_FLOAT " ++ show f ++ "\n"
compileLiteral (ListLiteral e)  =
    concatMap compileLiteral e ++
    "PUSH_LIST " ++ show (length e) ++ "\n"
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
