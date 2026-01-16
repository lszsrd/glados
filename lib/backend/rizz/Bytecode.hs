{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/backend/rizz/Bytecode.hs
-}

module Bytecode (
    compileDecl,
    compileFunctionDecl
) where

import Ast
import Tokens
import Data.Map (Map)
import qualified Data.Map as Map


type FuncEnv = Map Identifier [Identifier]

buildFuncEnv :: [Decl] -> FuncEnv
buildFuncEnv = foldr collect Map.empty
  where
    collect (FunctionDecl name params _ _) acc =
        Map.insert name (map paramName params) acc
    collect _ acc = acc

    paramName :: ParmVarDeclExpr -> Identifier
    paramName (ParmVarDeclExpr _ ident) = ident


compileDecl :: [Decl] -> String
compileDecl decls =
    let env = buildFuncEnv decls
    in concatMap (compileDeclWithEnv env) decls

compileDeclWithEnv :: FuncEnv -> Decl -> String
compileDeclWithEnv env (FunctionDecl name params body _ret) =
    compileFunctionWithEnv env name params body
compileDeclWithEnv _ _ = ""


compileFunctionWithEnv :: FuncEnv -> Identifier -> [ParmVarDeclExpr] ->
    CompoundStmt -> String
compileFunctionWithEnv env name params (CompoundStmt stmts) =
    let header = "FUNC " ++ name ++ " " ++ show (length params) ++ "\n"
        body = concatMap (compileStmt env) stmtsWithNames
        footer = "ENDFUNC\n"
        stmtsWithNames = map id stmts
    in header ++ body ++ footer

compileFunctionDecl :: Identifier -> [ParmVarDeclExpr] -> CompoundStmt
    -> String
compileFunctionDecl name params =
    compileFunctionWithEnv (Map.singleton name
        (map (\(ParmVarDeclExpr _ ident) -> ident) params)) name params


compileStmt :: FuncEnv -> Stmt -> String
compileStmt env (DeclVarExpr (VarDeclStmt _ ident _ rhs)) =
    compileParmCall env rhs ++
    "STORE " ++ ident ++ "\n"

compileStmt env (DeclStmt (DeclAssignStmtLiteral ident _ rhs)) =
    compileParmCall env rhs ++
    "STORE " ++ ident ++ "\n"

compileStmt _ (DeclStmt (DeclAssignStmtUnary (UnaryOperatorExpr ident op))) =
    case op of
        IdentIncrement ->
            "LOAD " ++ ident ++ "\n" ++
            "PUSH_INT 1\n" ++
            "ADD\n" ++ "STORE " ++ ident ++ "\n"
        IdentDecrement ->
            "LOAD " ++ ident ++ "\n" ++
            "PUSH_INT 1\n" ++
            "SUB\n" ++ "STORE " ++ ident ++ "\n"

compileStmt env (CallExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args

compileStmt env (BinaryOperator expr) =
    compileBinaryOpExpr env expr ++
    "POP\n"

compileStmt env (RetStmt (Just expr)) =
    compileBinaryOpExpr env expr ++
    "RET\n"

compileStmt env (IfStmt cond (CompoundStmt body) mElse) =
    let condCode = compileBinaryOpExpr env cond
        thenCode = concatMap (compileStmt env) body
        elseCode = maybe ""
            (\(CompoundStmt b) -> concatMap (compileStmt env) b) mElse
        endif = "LABEL endif\n"
    in condCode ++ "JMP_IF_FALSE endif\n" ++ thenCode ++ endif ++ elseCode

compileStmt env (WhileStmt cond (CompoundStmt body)) =
    "LABEL while_start\n" ++
    compileBinaryOpExpr env cond ++
    "JMP_IF_FALSE while_end\n" ++
    concatMap (compileStmt env) body ++
    "JMP while_start\n" ++
    "LABEL while_end\n"

compileStmt env (ForStmt mInit mCond mStep (CompoundStmt body)) =
    compileForInit env mInit ++
    "LABEL for_start\n" ++
    compileForCond env mCond ++
    concatMap (compileStmt env) body ++
    compileForStep env mStep ++
    "JMP for_start\n" ++
    "LABEL for_end\n"

compileStmt _ _ = "NOP\n"


compileCallWithNamedParams :: FuncEnv -> Identifier -> [ParmCallDecl] -> String
compileCallWithNamedParams env fname args =
    let argsCode = concatMap (compileParmCall env) args
    in case Map.lookup fname env of
        Nothing ->
            argsCode ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"
        Just params ->
            let storeCode = concatMap (\p -> "STORE " ++ p ++ "\n")
                    (reverse params)
            in argsCode ++ storeCode ++ "CALL " ++ fname ++ " " ++
                show (length args) ++ "\n"


compileBinaryOpExpr :: FuncEnv -> BinaryOpExpr -> String
compileBinaryOpExpr env (BinaryOpConst p) =
    compileParmCall env p
compileBinaryOpExpr env (BinaryOpExpr l op r) =
    compileBinaryOpParm env l ++
    compileBinaryOpParm env r ++
    opToInstr op ++ "\n"

compileBinaryOpParm :: FuncEnv -> BinaryOpParm -> String
compileBinaryOpParm env (BinaryOpParm p) = compileParmCall env p
compileBinaryOpParm env (BinaryOpParmBOp expr) = compileBinaryOpExpr env expr


compileParmCall :: FuncEnv -> ParmCallDecl -> String
compileParmCall _   (ParmCallDeclLiteral lit) = compileLiteral lit
compileParmCall _   (ParmCallDeclIdent ident) = "LOAD " ++ ident ++ "\n"
compileParmCall env (ParmCallDeclExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args

-- TODO ; Manage Structs
compileParmCall env (ParmCallDeclList l) = "\n"
compileParmCall env (ParmCallBExpr l op r) =
    compileBinaryOpParm env l ++
    compileBinaryOpParm env r ++
    opToInstr op ++ "\n"


compileForInit :: FuncEnv -> Maybe VarDeclStmt -> String
compileForInit _   Nothing = ""
compileForInit env (Just vds) = compileStmt env (DeclVarExpr vds)

compileForCond :: FuncEnv -> Maybe BinaryOpExpr -> String
compileForCond _   Nothing = ""
compileForCond env (Just cond) = compileBinaryOpExpr env
    cond ++ "JMP_IF_FALSE for_end\n"

compileForStep :: FuncEnv -> Maybe DeclStmt -> String
compileForStep _   Nothing = ""
compileForStep env (Just ds) = compileStmt env (DeclStmt ds)


compileLiteral :: Literal -> String
compileLiteral (IntLiteral i)   = "PUSH_INT " ++ show i ++ "\n"
compileLiteral (BoolLiteral b)  = "PUSH_BOOL " ++
    (if b then "true\n" else "false\n")
compileLiteral (FloatLiteral f) = "PUSH_FLOAT " ++ show f ++ "\n"
compileLiteral (ListLiteral elems) =
    concatMap compileLiteral elems ++
    "PUSH_LIST " ++ show (length elems) ++ "\n"
compileLiteral _                = "PUSH_UNKNOWN\n"


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
