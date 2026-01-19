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
import Data.List (isInfixOf, isPrefixOf, tails)

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
compileDeclWithEnv _ (RecordDecl (RecordDeclExpr name fields)) =
    "STRUCT " ++ name ++ " " ++ show (length fields) ++ " "
        ++ unwords (map (\(ParmVarDeclExpr _ ident) -> ident) fields) ++ "\n"

-- Top-level var declaration (if any)
compileDeclWithEnv env (VarDecl vds) =
    compileVarDecl env vds
compileDeclWithEnv _ _ = ""

-- | Compile a function declaration (wrap / dispatch)
--   We keep header: FUNC <name> <param1> <param2> ...
compileFunctionWithEnv :: Env -> Decl -> String
compileFunctionWithEnv env (FunctionDecl name params (CompoundStmt stmts) _mret) =
    let env' = env { eNextId = eNextId env }
        paramNames = map (\(ParmVarDeclExpr _ ident) -> ident) params
        header =
            "FUNC " ++ name ++
            (if null paramNames then "" else " " ++ unwords paramNames)
            ++ "\n"
        body = concatMap (\s -> compileStmt env' s) stmts
        footer = "ENDFUNC\n"
    in header ++ body ++ footer
compileFunctionWithEnv _ _ = ""

-- Loop context used to generate per-loop labels (continue / break)
data LoopCtx = LoopCtx { lblStart :: String, lblContinue :: String, lblEnd :: String }


-- find first position where a substring occurs (returns index into the string)
findSubIndex :: String -> String -> Maybe Int
findSubIndex sub s =
    case filter (isPrefixOf sub) (tails s) of
        (x:_) -> Just (length s - length x)
        []    -> Nothing

-- extract an identifier after a pattern like '(ParmCallDeclIdent "name")'
extractIdentWithPos :: String -> String -> Maybe (String, Int)
extractIdentWithPos pattern s =
    case findSubIndex pattern s of
        Nothing -> Nothing
        Just pos ->
            let start = pos + length pattern
                rest  = drop start s
                ident = takeWhile (/= '"') (dropWhile (== '"') rest)
            in Just (ident, start)

-- extract integer literal after '(ParmCallDeclLiteral (IntLiteral '
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

-- indexAsString: either the identifier or the decimal literal string
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
                        mbIdxIdent =
                            case mbBase of
                                Nothing -> Nothing
                                Just (_, basePosRel) ->
                                    let relStart = basePosRel
                                        restAfterBase = drop relStart afterIdx
                                    in fmap (\(i,_) -> i) (extractIdentWithPos "(ParmCallDeclIdent \"" restAfterBase >>= (\(x,p)->Just (x,p)))
                    in case mbBase of
                        Nothing -> Nothing
                        Just (base, _) ->
                            case mbIdxLit of
                                Just (n, _) -> Just (base, show n)
                                Nothing ->
                                    case findSecondIdentAfter afterIdx of
                                        Just idxIdent -> Just (base, idxIdent)
                                        Nothing -> Nothing

-- find second occurrence of "(ParmCallDeclIdent \"" and return the identifier
findSecondIdentAfter :: String -> Maybe String
findSecondIdentAfter s =
    case extractIdentWithPos "(ParmCallDeclIdent \"" s of
        Nothing -> Nothing
        Just (_, pos1) ->
            let rest = drop (pos1 + length "(ParmCallDeclIdent \"") s
            in case extractIdentWithPos "(ParmCallDeclIdent \"" rest of
                Nothing -> Nothing
                Just (ident2, _) -> Just ident2

compileStmt :: Env -> Stmt -> String

-- Typed variable declaration (possibly struct or list)
compileStmt env (DeclVarExpr vds) = compileVarDecl env vds

-- If the parser produced a textual representation for an indexed target,
-- detect it and emit STORE_INDEX, otherwise simple STORE.
compileStmt env (DeclStmt (DeclAssignStmtLiteral target _ rhs))
    | "ParmCallDeclIdx" `isInfixOf` target =
        case words target of
            (_ : "(ParmCallDeclIdent" : base : ")" :
                 "(ParmCallDeclIdent" : idx : ")" : _) ->
                let clean x = filter (\c -> c /= '"' && c /= ')') x
                    b = clean base
                    i = clean idx
                in compileParmCall env rhs ++
                   "LOAD " ++ b ++ "\n" ++
                   "LOAD " ++ i ++ "\n" ++
                   "STORE_INDEX\n"
            _ ->
                "NOP\n"

compileStmt env (DeclStmt (DeclAssignStmtLiteral target op rhs))
    | op `elem` [AddEqual, SubEqual, MulEqual, DivEqual, ModEqual] =
        "LOAD " ++ target ++ "\n" ++
        compileParmCall env rhs ++
        opAssignToInstr op ++ "\n" ++
        "STORE " ++ target ++ "\n"

compileStmt env (DeclStmt (DeclAssignStmtLiteral target _ rhs)) =
    compileParmCall env rhs ++
    "STORE " ++ target ++ "\n"


-- Unary assignment (x++ / x--)
compileStmt _ (DeclStmt (DeclAssignStmtUnary (UnaryOperatorExpr ident op))) =
    case op of
        IdentIncrement -> "LOAD " ++ ident ++ "\nPUSH_INT 1\nADD\nSTORE "
            ++ ident ++ "\n"
        IdentDecrement -> "LOAD " ++ ident ++ "\nPUSH_INT 1\nSUB\nSTORE "
            ++ ident ++ "\n"
        _              -> "NOP\n"

-- Function call used as statement
compileStmt env (CallExpr (CallExprDecl fname args)) =
    compileCallWithNamedParams env fname args

-- Binary expression used as a statement
compileStmt env (BinaryOperator expr) =
    compileBinaryOpExpr env expr ++ "POP\n"

-- Return statement (maybe void)
compileStmt env (RetStmt mexpr) =
    case mexpr of
        Nothing   -> "RET\n"
        Just expr -> compileBinaryOpExpr env expr ++ "RET\n"

-- If / else statement with unique labels
compileStmt env (IfStmt cond (CompoundStmt body) mElse) =
    let nid = eNextId env
        elseLbl = mkLabel "else" nid
        endLbl  = mkLabel "endif" (nid + 1)
        env' = env { eNextId = nid + 2 }
        condCode = compileBinaryOpExpr env' cond
        thenCode = concatMap (compileStmt env') body
        elseCode = maybe "" (\(CompoundStmt b) -> concatMap (compileStmt env') b) mElse
    in condCode ++
       "JMP_IF_FALSE " ++ elseLbl ++ "\n" ++
       thenCode ++
       "JMP " ++ endLbl ++ "\n" ++
       "LABEL " ++ elseLbl ++ "\n" ++
       elseCode ++
       "LABEL " ++ endLbl ++ "\n"

-- While loop with unique labels (and optional support for break/continue via compileStmtWithLoop)
compileStmt env (WhileStmt cond (CompoundStmt body)) =
    let nid = eNextId env
        startLbl = mkLabel "while_start" nid
        endLbl   = mkLabel "while_end" (nid + 1)
        contLbl  = mkLabel "while_continue" (nid + 2)
        env' = env { eNextId = nid + 3 }
        ctx  = LoopCtx startLbl contLbl endLbl
    in "LABEL " ++ startLbl ++ "\n"
       ++ compileBinaryOpExpr env' cond ++ "JMP_IF_FALSE " ++ endLbl ++ "\n"
       ++ concatMap (compileStmtWithLoop env' ctx) body
       ++ "LABEL " ++ contLbl ++ "\n"
       ++ "JMP " ++ startLbl ++ "\n"
       ++ "LABEL " ++ endLbl ++ "\n"

-- For loop (init ; cond ; step) with unique labels — pass explicit for_end label to cond compiler
compileStmt env (ForStmt mInit mCond mStep (CompoundStmt body)) =
    let nid = eNextId env
        startLbl = mkLabel "for_start" nid
        contLbl  = mkLabel "for_continue" (nid + 1)
        endLbl   = mkLabel "for_end" (nid + 2)
        env' = env { eNextId = nid + 3 }
        initCode = compileForInit env' mInit
        condCode = compileForCond env' mCond endLbl
        stepCode = compileForStep env' mStep
        ctx = LoopCtx startLbl contLbl endLbl
    in initCode
       ++ "LABEL " ++ startLbl ++ "\n"
       ++ condCode
       ++ concatMap (compileStmtWithLoop env' ctx) body
       ++ "LABEL " ++ contLbl ++ "\n"
       ++ stepCode
       ++ "JMP " ++ startLbl ++ "\n"
       ++ "LABEL " ++ endLbl ++ "\n"

-- Foreach fallback (not implemented fully)
compileStmt env (ForeachStmt _ _ (CompoundStmt body)) =
    concatMap (compileStmt env) body

-- Fallback
compileStmt _ _ = "NOP\n"

-- | compileStmtWithLoop: variant that handles loop control statements
compileStmtWithLoop :: Env -> LoopCtx -> Stmt -> String
compileStmtWithLoop env ctx (LoopControlStmt kw) =
    case kw of
        Continue -> "JMP " ++ lblContinue ctx ++ "\n"
        Break    -> "JMP " ++ lblEnd ctx ++ "\n"
        _        -> "NOP\n"
compileStmtWithLoop env _ st = compileStmt env st

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
    compileStmt env (DeclStmt (DeclAssignStmtUnary u))

-- | Binary expression compilation
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
    concatMap (compileParmCall env) elems ++ "PUSH_LIST " ++ show (length elems) ++ "\n"
compileParmCall env (ParmCallBExpr l op r) =
    compileBinaryOpParm env l ++ compileBinaryOpParm env r ++ opToInstr op ++ "\n"

-- Indexing: compile base then index then IND instruction.
compileParmCall env (ParmCallDeclIdx base idx) =
    compileParmCall env base ++ compileParmCall env idx ++ "IND\n"

-- | Call with named params: push args, optionally STORE into param names (reversed), then CALL
compileCallWithNamedParams :: Env -> Identifier -> [ParmCallDecl] -> String
compileCallWithNamedParams env fname args =
    let argsCode = concatMap (compileParmCall env) args
    in case Map.lookup fname (eFuncs env) of
        Nothing -> argsCode ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"
        Just params ->
            let storeCode = concatMap (\p -> "STORE " ++ p ++ "\n") (reverse params)
            in argsCode ++ storeCode ++ "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"

-- | Literals
compileLiteral :: Literal -> String
compileLiteral (CharLiteral c)   = "PUSH_CHAR " ++ show c ++ "\n"
compileLiteral (IntLiteral i)    = "PUSH_INT " ++ show i ++ "\n"
compileLiteral (BoolLiteral b)   = "PUSH_BOOL " ++ (if b then "True\n" else "False\n")
compileLiteral (FloatLiteral f)  = "PUSH_FLOAT " ++ show f ++ "\n"
compileLiteral (ListLiteral elems) =
    concatMap compileLiteral elems ++ "PUSH_LIST " ++ show (length elems) ++ "\n"
compileLiteral _ = "PUSH_UNKNOWN\n"

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
opToInstr _   = "NOP"

opAssignToInstr :: AssignOp -> String
opAssignToInstr AddEqual = "ADD"
opAssignToInstr SubEqual = "SUB"
opAssignToInstr MulEqual = "MUL"
opAssignToInstr DivEqual = "DIV"
opAssignToInstr ModEqual = "MOD"
opAssignToInstr _        = "NOP"


-- | Label generator using eNextId values (stringify the numeric id)
mkLabel :: String -> Int -> String
mkLabel base i = base ++ "_" ++ show i
