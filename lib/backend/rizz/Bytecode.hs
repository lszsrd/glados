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
import Data.List (isInfixOf, isPrefixOf, tails)

-- | Function environment: maps function name -> parameter names (in order)
type FuncEnv   = Map Identifier [Identifier]
-- | Struct environment: maps struct name -> field names (in order)
type StructEnv = Map Identifier [Identifier]

-- | Global compilation environment: both function and struct maps,
--   plus a simple counter to generate unique labels.
data Env = Env
    { eFuncs  :: FuncEnv
    , eNextId :: Int
    }

-- | Loop context used to generate per-loop labels (continue / break)
data LoopCtx = LoopCtx { lblStart :: String, lblContinue :: String, lblEnd :: String }

-- | Build function environment from top-level declarations
buildFuncEnv :: [Decl] -> FuncEnv
buildFuncEnv = foldr collect Map.empty
  where
    collect (FunctionDecl name params _ _) acc =
        Map.insert name (map parmName params) acc
    collect _ acc = acc

    parmName :: ParmVarDeclExpr -> Identifier
    parmName (ParmVarDeclExpr _ ident) = ident
    parmName _ = ""  -- fallback for other ParmVarDeclExpr constructors

-- | Build struct environment from top-level declarations (RecordDecl)
buildStructEnv :: [Decl] -> StructEnv
buildStructEnv = foldr collect Map.empty
  where
    collect (RecordDecl (RecordDeclExpr name fields)) acc =
        Map.insert name (map parmName fields) acc
    collect _ acc = acc

    parmName :: ParmVarDeclExpr -> Identifier
    parmName (ParmVarDeclExpr _ ident) = ident
    parmName _ = ""

-- | Top-level compile entry: build envs and compile each decl
compileDecl :: [Decl] -> String
compileDecl decls =
    let fenv = buildFuncEnv decls
        -- senv currently unused but kept for future features
        _senv = buildStructEnv decls
        env  = Env fenv 0
    in concatMap (compileDeclWithEnv env) decls

-- | Compile a single declaration using the environment
compileDeclWithEnv :: Env -> Decl -> String
compileDeclWithEnv env d@(FunctionDecl _ _ _ _) =
    compileFunctionWithEnv env d
-- Emit a STRUCT descriptor for RecordDecl (informational)
compileDeclWithEnv _ (RecordDecl (RecordDeclExpr name fields)) =
    "STRUCT " ++ name ++ " " ++ show (length fields) ++ " "
        ++ unwords (map (\p -> case p of { ParmVarDeclExpr _ ident -> ident; _ -> "" }) fields) ++ "\n"
-- Top-level var declaration (if any)
compileDeclWithEnv env (VarDecl vds) =
    fst (compileVarDecl env vds, env)  -- var decl returns String; keep env unchanged
compileDeclWithEnv _ _ = ""

-- | Compile a function declaration (wrap / dispatch)
--   We keep header: FUNC <name> <param1> <param2> ...
compileFunctionWithEnv :: Env -> Decl -> String
compileFunctionWithEnv env (FunctionDecl name params (CompoundStmt stmts) _mret) =
    let env' = env { eNextId = eNextId env }
        paramNames = map (\p -> case p of { ParmVarDeclExpr _ ident -> ident; _ -> "" }) params
        header =
            "FUNC " ++ name ++
            (if null paramNames then "" else " " ++ unwords paramNames) ++ "\n"
        (body, _envFinal) = compileStmtSeq env' Nothing stmts
        footer = "ENDFUNC\n"
    in header ++ body ++ footer
compileFunctionWithEnv _ _ = ""

-- helpers for simple textual parsing of parser debug strings (for indexed target)
findSubIndex :: String -> String -> Maybe Int
findSubIndex sub s =
    case filter (isPrefixOf sub) (tails s) of
        (x:_) -> Just (length s - length x)
        []    -> Nothing

extractIdentWithPos :: String -> String -> Maybe (String, Int)
extractIdentWithPos pattern s =
    case findSubIndex pattern s of
        Nothing -> Nothing
        Just pos ->
            let start = pos + length pattern
                rest  = drop start s
                ident = takeWhile (/= '"') (dropWhile (== '"') rest)
            in Just (ident, start)

extractIntWithPos :: String -> String -> Maybe (Int, Int)
extractIntWithPos pattern s =
    case findSubIndex pattern s of
        Nothing -> Nothing
        Just pos ->
            let start = pos + length pattern
                rest  = drop start s
                numStr = takeWhile (\c -> c /= ')' && c /= ' ' ) rest
            in case reads numStr :: [(Int,String)] of
                ((n,_):_) -> Just (n, start)
                _         -> Nothing

findSecondIdentAfter :: String -> Maybe String
findSecondIdentAfter s =
    case extractIdentWithPos "(ParmCallDeclIdent \"" s of
        Nothing -> Nothing
        Just (_, pos1) ->
            let rest = drop (pos1 + length "(ParmCallDeclIdent \"") s
            in case extractIdentWithPos "(ParmCallDeclIdent \"" rest of
                Nothing -> Nothing
                Just (ident2, _) -> Just ident2

parseIndexedTarget :: String -> Maybe (String, String)
parseIndexedTarget s =
    if not ("ParmCallDeclIdx" `isInfixOf` s)
        then Nothing
        else
            case findSubIndex "ParmCallDeclIdx" s of
                Nothing -> Nothing
                Just idx0 ->
                    let afterIdx = drop (idx0 + length "ParmCallDeclIdx") s
                        mbBase = extractIdentWithPos "(ParmCallDeclIdent \"" afterIdx
                        mbIdxLit = extractIntWithPos "(ParmCallDeclLiteral (IntLiteral " afterIdx
                    in case mbBase of
                        Nothing -> Nothing
                        Just (base, _) ->
                            case mbIdxLit of
                                Just (n, _) -> Just (base, show n)
                                Nothing ->
                                    case findSecondIdentAfter afterIdx of
                                        Just idxIdent -> Just (base, idxIdent)
                                        Nothing -> Nothing

-- Statement compilation (threading Env)
-- compileStmt returns (code, updatedEnv)
-- compileStmtSeq compiles a sequential list of statements, threading env

compileStmtSeq :: Env -> Maybe LoopCtx -> [Stmt] -> (String, Env)
compileStmtSeq env _ [] = ("", env)
compileStmtSeq env ctx (s:ss) =
    let (c1, env1) = compileStmt env ctx s
        (crest, env2) = compileStmtSeq env1 ctx ss
    in (c1 ++ crest, env2)

compileStmt :: Env -> Maybe LoopCtx -> Stmt -> (String, Env)

-- Typed variable declaration (possibly struct or list)
compileStmt env _ (DeclVarExpr vds) = (compileVarDecl env vds, env)

-- Assignment without type: identifier := rhs
-- supports indexed target encoded as textual parser debug ("ParmCallDeclIdx ...")
compileStmt env _ (DeclStmt (DeclAssignStmtLiteral target op rhs)) =
    case parseIndexedTarget target of
        Just (base, idx) ->
            -- store into index: compute rhs, load base, load index, STORE_INDEX
            ( compileParmCall env rhs
              ++ "LOAD " ++ base ++ "\n"
              ++ "LOAD " ++ idx ++ "\n"
              ++ "STORE_INDEX\n"
            , env )
        Nothing ->
            if op `elem` [AddEqual, SubEqual, MulEqual, DivEqual, ModEqual]
            then
                ( "LOAD " ++ target ++ "\n"
                  ++ compileParmCall env rhs
                  ++ opAssignToInstr op ++ "\n"
                  ++ "STORE " ++ target ++ "\n"
                , env )
            else
                ( compileParmCall env rhs ++ "STORE " ++ target ++ "\n"
                , env )

-- Unary assignment (x++ / x--)
compileStmt env _ (DeclStmt (DeclAssignStmtUnary (UnaryOperatorExpr ident op))) =
    case op of
        IdentIncrement -> ("LOAD " ++ ident ++ "\nPUSH_INT 1\nADD\nSTORE "
            ++ ident ++ "\n", env)
        IdentDecrement -> ("LOAD " ++ ident ++ "\nPUSH_INT 1\nSUB\nSTORE "
            ++ ident ++ "\n", env)
        _              -> ("NOP\n", env)

-- Function call as statement
compileStmt env _ (CallExpr (CallExprDecl fname args)) =
    (compileCallWithNamedParams env fname args, env)

-- Binary expression used as a statement
compileStmt env _ (BinaryOperator expr) =
    (compileBinaryOpExpr env expr ++ "POP\n", env)

-- Return statement (maybe void)
compileStmt env _ (RetStmt mexpr) =
    case mexpr of
        Nothing   -> ("RET\n", env)
        Just expr -> (compileBinaryOpExpr env expr ++ "RET\n", env)

-- If / else statement with unique labels: reserve ids then thread env through bodies
compileStmt env ctx (IfStmt cond (CompoundStmt body) mElse) =
    let nid = eNextId env
        elseLbl = mkLabel "else" nid
        endLbl  = mkLabel "endif" (nid + 1)
        envAfterLabels = env { eNextId = nid + 2 }

        condCode = compileBinaryOpExpr envAfterLabels cond

        (thenCode, envAfterThen) = compileStmtSeq envAfterLabels ctx body

        (elseCode, envAfterElse) = case mElse of
            Nothing -> ("", envAfterThen)
            Just (CompoundStmt b) -> compileStmtSeq envAfterThen ctx b

        assembled = condCode
            ++ "JMP_IF_FALSE " ++ elseLbl ++ "\n"
            ++ thenCode
            ++ "JMP " ++ endLbl ++ "\n"
            ++ "LABEL " ++ elseLbl ++ "\n"
            ++ elseCode
            ++ "LABEL " ++ endLbl ++ "\n"
    in (assembled, envAfterElse)

-- While loop with unique labels and loop ctx
compileStmt env _ (WhileStmt cond (CompoundStmt body)) =
    let nid = eNextId env
        startLbl = mkLabel "while_start" nid
        endLbl   = mkLabel "while_end" (nid + 1)
        contLbl  = mkLabel "while_continue" (nid + 2)
        envAfterLabels = env { eNextId = nid + 3 }
        ctx = LoopCtx startLbl contLbl endLbl

        condCode = compileBinaryOpExpr envAfterLabels cond
        (bodyCode, envAfterBody) = compileStmtSeq envAfterLabels (Just ctx) body

        assembled = "LABEL " ++ startLbl ++ "\n"
                 ++ condCode ++ "JMP_IF_FALSE " ++ endLbl ++ "\n"
                 ++ bodyCode
                 ++ "LABEL " ++ contLbl ++ "\n"
                 ++ "JMP " ++ startLbl ++ "\n"
                 ++ "LABEL " ++ endLbl ++ "\n"
    in (assembled, envAfterBody)

-- For loop (init ; cond ; step) with unique labels — pass explicit for_end to cond compiler
compileStmt env _ (ForStmt mInit mCond mStep (CompoundStmt body)) =
    let nid = eNextId env
        startLbl = mkLabel "for_start" nid
        contLbl  = mkLabel "for_continue" (nid + 1)
        endLbl   = mkLabel "for_end" (nid + 2)
        envAfterLabels = env { eNextId = nid + 3 }

        initCode = compileForInit envAfterLabels mInit
        condCode = compileForCond envAfterLabels mCond endLbl
        stepCode = compileForStep envAfterLabels mStep

        ctx = LoopCtx startLbl contLbl endLbl

        (bodyCode, envAfterBody) = compileStmtSeq envAfterLabels (Just ctx) body

        assembled = initCode
                 ++ "LABEL " ++ startLbl ++ "\n"
                 ++ condCode
                 ++ bodyCode
                 ++ "LABEL " ++ contLbl ++ "\n"
                 ++ stepCode
                 ++ "JMP " ++ startLbl ++ "\n"
                 ++ "LABEL " ++ endLbl ++ "\n"
    in (assembled, envAfterBody)

-- Foreach: create an internal index var and store current element into the loop variable each iteration
compileStmt env _ (ForeachStmt var list (CompoundStmt body)) =
    let nid = eNextId env
        startLbl = mkLabel "foreach_start" nid
        contLbl  = mkLabel "foreach_continue" (nid + 1)
        endLbl   = mkLabel "foreach_end" (nid + 2)
        envAfterLabels = env { eNextId = nid + 3 }

        idxName = "_foreach_idx_" ++ show nid
        initCode = "PUSH_INT 0\nSTORE " ++ idxName ++ "\n"
        preBody = "LOAD " ++ list ++ "\nLOAD " ++ idxName ++ "\nIND\nSTORE "
            ++ var ++ "\n"
        ctx = LoopCtx startLbl contLbl endLbl
        (bodyCode, envAfterBody) = compileStmtSeq envAfterLabels (Just ctx) body
        stepCode = "LOAD " ++ idxName ++ "\nPUSH_INT 1\nADD\nSTORE "
            ++ idxName ++ "\n"
        assembled = initCode
                 ++ "LABEL " ++ startLbl ++ "\n"
                 ++ "LOAD " ++ list ++ "\nLOAD " ++ idxName ++ "\nIND\n"
                 ++ "JMP_IF_FALSE " ++ endLbl ++ "\n"
                 ++ preBody
                 ++ bodyCode
                 ++ "LABEL " ++ contLbl ++ "\n"
                 ++ stepCode
                 ++ "JMP " ++ startLbl ++ "\n"
                 ++ "LABEL " ++ endLbl ++ "\n"
    in (assembled, envAfterBody)

compileStmt env mctx (LoopControlStmt kw) =
    case (kw, mctx) of
        (Continue, Just ctx) -> ("JMP " ++ lblContinue ctx ++ "\n", env)
        (Break,    Just ctx) -> ("JMP " ++ lblEnd ctx ++ "\n", env)
        _                    -> ("NOP\n", env)

compileStmt env _ _ = ("NOP\n", env)

-- | compileVarDecl: handle typed var declarations including Struct and List
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

-- For init/cond/step helpers
compileForInit :: Env -> Maybe VarDeclStmt -> String
compileForInit _ Nothing = ""
compileForInit env (Just vds) = compileVarDecl env vds

-- cond now accepts explicit end label to jump to
compileForCond :: Env -> Maybe BinaryOpExpr -> String -> String
compileForCond _ Nothing _ = ""
compileForCond env (Just cond) endLbl =
    compileBinaryOpExpr env cond ++ "JMP_IF_FALSE " ++ endLbl ++ "\n"

compileForStep :: Env -> Maybe DeclStmt -> String
compileForStep _ Nothing = ""
compileForStep env (Just ds) = compileDeclStmt env ds

compileDeclStmt :: Env -> DeclStmt -> String
compileDeclStmt env (DeclAssignStmtLiteral target _ rhs) =
    case parseIndexedTarget target of
        Just (base, idx) ->
            compileParmCall env rhs ++
            "LOAD " ++ base ++ "\n" ++
            "LOAD " ++ idx ++ "\n" ++
            "STORE_INDEX\n"
        Nothing ->
            compileParmCall env rhs ++
            "STORE " ++ target ++ "\n"
compileDeclStmt env (DeclAssignStmtUnary u) =
    let (s, _) = compileStmt env Nothing (DeclStmt (DeclAssignStmtUnary u))
    in s

-- | Binary expression compilation (pure, does not alter env)
compileBinaryOpExpr :: Env -> BinaryOpExpr -> String
compileBinaryOpExpr env (BinaryOpConst p) = compileParmCall env p
compileBinaryOpExpr env (BinaryOpExpr l op r) =
    compileBinaryOpParm env l ++
    compileBinaryOpParm env r ++
    opToInstr op ++ "\n"

compileBinaryOpParm :: Env -> BinaryOpParm -> String
compileBinaryOpParm env (BinaryOpParm p)   = compileParmCall env p
compileBinaryOpParm env (BinaryOpParmBOp b) = compileBinaryOpExpr env b

-- | compileParmCall: supports all ParmCallDecl variants, including indexing
compileParmCall :: Env -> ParmCallDecl -> String
compileParmCall _   (ParmCallDeclLiteral lit) = compileLiteral lit
compileParmCall _   (ParmCallDeclIdent ident) = "LOAD " ++ ident ++ "\n"
compileParmCall env (ParmCallDeclExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args
compileParmCall env (ParmCallDeclList elems) =
    concatMap (compileParmCall env) elems ++ "PUSH_LIST "
        ++ show (length elems) ++ "\n"
compileParmCall env (ParmCallBExpr l op r) =
    compileBinaryOpParm env l ++ compileBinaryOpParm env r
        ++ opToInstr op ++ "\n"

-- Indexing: compile base then index then IND instruction.
-- On the VM we expect: <base> <index> IND -> pushes element
compileParmCall env (ParmCallDeclIdx base idx) =
    compileParmCall env base ++ compileParmCall env idx ++ "IND\n"

-- | Call with named params: push args, optionally STORE into param names (reversed), then CALL
compileCallWithNamedParams :: Env -> Identifier -> [ParmCallDecl] -> String
compileCallWithNamedParams env fname args =
    let argsCode = concatMap (compileParmCall env) args
    in case Map.lookup fname (eFuncs env) of
        Nothing -> argsCode ++ "CALL " ++ fname ++ " "
            ++ show (length args) ++ "\n"
        Just params ->
            let storeCode = concatMap (\p -> "STORE " ++ p ++ "\n") (reverse params)
            in argsCode ++ storeCode ++ "CALL " ++ fname ++ " "
                ++ show (length args) ++ "\n"

-- | Literals
compileLiteral :: Literal -> String
compileLiteral (CharLiteral c)   = "PUSH_CHAR " ++ show c ++ "\n"
compileLiteral (IntLiteral i)    = "PUSH_INT " ++ show i ++ "\n"
compileLiteral (BoolLiteral b)   = "PUSH_BOOL "
    ++ (if b then "True\n" else "False\n")
compileLiteral (FloatLiteral f)  = "PUSH_FLOAT " ++ show f ++ "\n"
compileLiteral (ListLiteral elems) =
    concatMap compileLiteral elems ++ "PUSH_LIST "
        ++ show (length elems) ++ "\n"

-- | Operators
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

opAssignToInstr :: AssignOp -> String
opAssignToInstr AddEqual = "ADD"
opAssignToInstr SubEqual = "SUB"
opAssignToInstr MulEqual = "MUL"
opAssignToInstr DivEqual = "DIV"
opAssignToInstr ModEqual = "MOD"

-- | Label generator using numeric id
mkLabel :: String -> Int -> String
mkLabel base i = base ++ "_" ++ show i
