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
import Data.List (isPrefixOf, tails)

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
    ++ unwords
         (map (\p -> case p of
               ParmVarDeclExpr _ ident -> ident
               _ -> "") fields)
    ++ "\n"

-- Top-level var declaration (if any)
compileDeclWithEnv env (VarDecl vds) =
    fst (compileVarDecl env vds, env)  -- var decl returns String; keep env unchanged
compileDeclWithEnv _ _ = ""

-- | Compile a function declaration (wrap / dispatch)
--   We keep header: FUNC <name> <param1> <param2> ...
compileFunctionWithEnv :: Env -> Decl -> String
compileFunctionWithEnv env (FunctionDecl n ps (CompoundStmt ss) _) =
    let names = map (\p -> case p of { ParmVarDeclExpr _ i -> i; _ -> "" }) ps
        header = "FUNC " ++ n ++
            (if null names then "" else " " ++ unwords names) ++ "\n"
        (body, _) = compileStmtSeq env Nothing ss
    in header ++ body ++ "ENDFUNC\n"
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
parseIndexedTarget s
    | "ParmCallDeclIdx" `notElem` words s = Nothing
    | otherwise = do
        idx0 <- findSubIndex "ParmCallDeclIdx" s
        let after = drop (idx0 + length "ParmCallDeclIdx") s
        parseIndexedBase after

parseIndexedBase :: String -> Maybe (String, String)
parseIndexedBase afterIdx = do
    (base, _) <- extractIdentWithPos "(ParmCallDeclIdent \"" afterIdx
    case extractIntWithPos "(ParmCallDeclLiteral (IntLiteral " afterIdx of
        Just (n, _) -> Just (base, show n)
        Nothing      -> fmap (\i -> (base, i)) (findSecondIdentAfter afterIdx)

-- Statement compilation (threading Env)
-- compileStmt returns (code, updatedEnv)
-- compileStmtSeq compiles a sequential list of statements, threading env

compileStmtSeq :: Env -> Maybe LoopCtx -> [Stmt] -> (String, Env)
compileStmtSeq env _ [] = ("", env)
compileStmtSeq env ctx (s:ss) =
    let (c1, env1) = compileStmt env ctx s
        (crest, env2) = compileStmtSeq env1 ctx ss
    in (c1 ++ crest, env2)

-- compileStmt and small helpers
compileStmt :: Env -> Maybe LoopCtx -> Stmt -> (String, Env)
compileStmt env _ (DeclVarExpr vds) = (compileVarDecl env vds, env)

compileStmt env _ (DeclStmt (DeclAssignStmtLiteral tgt op rhs)) =
    case parseIndexedTarget tgt of
        Just (base, idx) -> storeIndex env rhs base idx
        Nothing
            | op `elem` [AddEqual, SubEqual, MulEqual, DivEqual, ModEqual] ->
                opAssignStmt env tgt op rhs
            | otherwise -> simpleStore env tgt rhs

compileStmt env _ (DeclStmt (DeclAssignStmtUnary u)) =
    unaryAssignStmt env u

compileStmt env _ (CallExpr (CallExprDecl f args)) =
    (compileCallWithNamedParams env f args, env)

compileStmt env _ (BinaryOperator b) =
    (compileBinaryOpExpr env b ++ "POP\n", env)

compileStmt env _ (RetStmt m) = retStmt env m

compileStmt env ctx (IfStmt cond (CompoundStmt body) mElse) =
    let nid = eNextId env
        eLbl = mkLabel "else" nid
        end  = mkLabel "endif" (nid + 1)
        env2 = env { eNextId = nid + 2 }
        condCode = compileBinaryOpExpr env2 cond
        (thenC, envThen) = compileStmtSeq env2 ctx body
        (elseC, envEnd) = case mElse of
            Nothing -> ("", envThen)
            Just (CompoundStmt b) -> compileStmtSeq envThen ctx b
        assembled = condCode
                 ++ "JMP_IF_FALSE " ++ eLbl ++ "\n"
                 ++ thenC ++ "JMP " ++ end ++ "\n"
                 ++ "LABEL " ++ eLbl ++ "\n"
                 ++ elseC ++ "LABEL " ++ end ++ "\n"
    in (assembled, envEnd)

compileStmt env _ (WhileStmt cond (CompoundStmt body)) =
    let nid = eNextId env
        s = mkLabel "while_start" nid
        c = mkLabel "while_continue" (nid + 1)
        e = mkLabel "while_end" (nid + 2)
        env2 = env { eNextId = nid + 3 }
        condCode = compileBinaryOpExpr env2 cond
        (bodyC, envB) = compileStmtSeq env2 (Just (LoopCtx s c e)) body
        assembled = "LABEL " ++ s ++ "\n"
                 ++ condCode ++ "JMP_IF_FALSE " ++ e ++ "\n"
                 ++ bodyC ++ "LABEL " ++ c ++ "\n"
                 ++ "JMP " ++ s ++ "\n" ++ "LABEL " ++ e ++ "\n"
    in (assembled, envB)

compileStmt env _ (ForStmt mInit mCond mStep (CompoundStmt body)) =
    let nid = eNextId env
        s = mkLabel "for_start" nid
        c = mkLabel "for_continue" (nid + 1)
        e = mkLabel "for_end" (nid + 2)
        env2 = env { eNextId = nid + 3 }
        initC = compileForInit env2 mInit
        condC = compileForCond env2 mCond e
        stepC = compileForStep env2 mStep
        (bodyC, envB) = compileStmtSeq env2 (Just (LoopCtx s c e)) body
        assembled = initC ++ "LABEL " ++ s ++ "\n" ++ condC
                 ++ bodyC ++ "LABEL " ++ c ++ "\n"
                 ++ stepC ++ "JMP " ++ s ++ "\n" ++ "LABEL " ++ e ++ "\n"
    in (assembled, envB)

compileStmt env _ (ForeachStmt var list (CompoundStmt body)) =
    foreachStmt env var list body

compileStmt env mctx (LoopControlStmt kw) =
    case (kw, mctx) of
        (Continue, Just ctx) -> ("JMP " ++ lblContinue ctx ++ "\n", env)
        (Break,    Just ctx) -> ("JMP " ++ lblEnd ctx ++ "\n", env)
        _ -> ("NOP\n", env)

compileStmt env _ _ = ("NOP\n", env)


-- helpers (each < 10 lines, ≤80 chars)

storeIndex :: Env -> ParmCallDecl -> String -> String -> (String, Env)
storeIndex env rhs base idx =
    ( compileParmCall env rhs
      ++ "LOAD " ++ base ++ "\n"
      ++ "LOAD " ++ idx ++ "\n"
      ++ "STORE_INDEX\n"
    , env )

opAssignStmt :: Env -> String -> AssignOp -> ParmCallDecl -> (String, Env)
opAssignStmt env target op rhs =
    ( "LOAD " ++ target ++ "\n"
      ++ compileParmCall env rhs
      ++ opAssignToInstr op ++ "\n"
      ++ "STORE " ++ target ++ "\n"
    , env )

simpleStore :: Env -> String -> ParmCallDecl -> (String, Env)
simpleStore env target rhs =
    ( compileParmCall env rhs ++ "STORE " ++ target ++ "\n", env )

unaryAssignStmt :: Env -> UnaryOperatorExpr -> (String, Env)
unaryAssignStmt env (UnaryOperatorExpr iden op) =
    case op of
        IdentIncrement -> ("LOAD " ++ iden ++ "\nPUSH_INT 1\nADD\nSTORE "
                          ++ iden ++ "\n", env)
        IdentDecrement -> ("LOAD " ++ iden ++ "\nPUSH_INT 1\nSUB\nSTORE "
                          ++ iden ++ "\n", env)

retStmt :: Env -> Maybe BinaryOpExpr -> (String, Env)
retStmt env Nothing = ("RET\n", env)
retStmt env (Just e) = (compileBinaryOpExpr env e ++ "RET\n", env)

foreachStmt :: Env -> Identifier -> Identifier -> [Stmt] -> (String, Env)
foreachStmt env var list body =
    let nid = eNextId env
        s = mkLabel "foreach_start" nid
        c = mkLabel "foreach_continue" (nid + 1)
        e = mkLabel "foreach_end" (nid + 2)
        env2 = env { eNextId = nid + 3 }
        idx = "_foreach_idx_" ++ show nid
        initC = "PUSH_INT 0\nSTORE " ++ idx ++ "\nLABEL " ++ s ++ "\n"
        chkCon = "LOAD " ++ idx ++ "\nLOAD " ++ list ++ "\nCALL length 1\nLT\n"
        crtIt = "LOAD " ++ list ++ "\nLOAD " ++ idx ++ "\nIND\nSTORE " ++ var
        inc = "PUSH_INT 1\nLOAD " ++ idx ++ "\nADD\nSTORE " ++ idx ++ "\n"
        (bodyC, envB) = compileStmtSeq env2 (Just (LoopCtx s c e)) body
        assembled = initC ++ chkCon ++ "JMP_IF_FALSE " ++ e ++ "\n" ++ crtIt
            ++ "\n" ++ bodyC ++ inc ++ "JMP " ++ s ++ "\nLABEL " ++ e ++ "\n"
    in (assembled, envB)

-- | compileVarDecl: handle typed var declarations including Struct and List
compileVarDecl :: Env -> VarDeclStmt -> String
compileVarDecl env (VarDeclStmt typ ident _ rhs) =
    case (typ, rhs) of
        (Ast.Struct s, ParmCallDeclList es) -> buildStruct env s es ident
        (ListType _, ParmCallDeclList es)   -> buildList env es ident
        _                                   -> helperStruct env rhs ident

-- helpers
buildStruct :: Env -> Identifier -> [ParmCallDecl] -> Identifier -> String
buildStruct env s elems ident =
    concatMap (compileParmCall env) elems
    ++ "BUILD_STRUCT " ++ s ++ " " ++ show (length elems) ++ "\n"
    ++ "STORE " ++ ident ++ "\n"

buildList :: Env -> [ParmCallDecl] -> Identifier -> String
buildList env elems ident =
    concatMap (compileParmCall env) elems
    ++ "PUSH_LIST " ++ show (length elems) ++ "\n"
    ++ "STORE " ++ ident ++ "\n"

helperStruct :: Env -> ParmCallDecl -> Identifier -> String
helperStruct env rhs ident = compileParmCall env rhs 
    ++ "STORE " ++ ident ++ "\n"

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
        call = "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"
    in case Map.lookup fname (eFuncs env) of
        Nothing -> argsCode ++ call
        Just ps -> argsCode
                    ++ concatMap (\p -> "STORE " ++ p ++ "\n") (reverse ps)
                    ++ call

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
opAssignToInstr Equal = ""

-- | Label generator using numeric id
mkLabel :: String -> Int -> String
mkLabel base i = base ++ "_" ++ show i
