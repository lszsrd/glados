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

-- Convert a  Decl into bytecode, only FunctionDecl is handled
-- other decls return an empty string
compileDecl :: [Decl] -> String
compileDecl ((FunctionDecl name params body _): x) =
    compileFunctionDecl name params body ++ compileDecl x
compileDecl _ = ""

-- Create a function header, compile each statement in the body and close the func
-- Format: FUNC <name> <arity>\n ...function body... ENDFUNC\n
compileFunctionDecl :: Identifier -> [ParmVarDeclExpr] -> CompoundStmt -> String
compileFunctionDecl name params (CompoundStmt stmts) =
    "FUNC " ++ name ++ " " ++ show (length params) ++ "\n"
    ++ concatMap (`compileStmt` name) stmts
    ++ "ENDFUNC\n"


-- Compile a single statement; second argument is the enclosing function name
-- (used for labels or context if needed)
compileStmt :: Stmt -> Identifier -> String

-- Typed variable declaration ex; Int y = <expr>
-- Compile the right-hand side value, then STORE into the variable name
compileStmt (DeclVarExpr (VarDeclStmt _ ident _ rhs)) _ =
    compileParmCall rhs ++
    "STORE " ++ ident ++ "\n"

-- Assignment without type ex: y = <expr>
compileStmt (DeclStmt (DeclAssignStmtLiteral ident _ rhs)) _ =
    compileParmCall rhs ++
    "STORE " ++ ident ++ "\n"

-- Unary assignment (x++ / x--)
compileStmt (DeclStmt (DeclAssignStmtUnary (UnaryOperatorExpr ident op))) _ =
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


-- Function call used as statement: compile args (push them) then CALL
compileStmt (CallExpr (CallExprDecl fname args)) _ =
    concatMap compileParmCall args ++
    "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"

-- Binary expression used as a statement: evaluate then POP result
compileStmt (BinaryOperator expr) fname =
    compileBinaryOpExpr expr fname ++
    "POP\n"

-- Return statement then RET
compileStmt (RetStmt expr) fname =
    compileBinaryOpExpr expr fname ++
    "RET\n"

-- If statement (no else support here)
-- Evaluate condition, jump to endif if false, compile body, do label
compileStmt (IfStmt cond (CompoundStmt body) _) fname =
    compileBinaryOpExpr cond fname ++
    "JMP_IF_FALSE endif\n" ++
    concatMap (`compileStmt` fname) body ++
    "LABEL endif\n"

-- While loop
compileStmt (WhileStmt cond (CompoundStmt body)) fname =
    "LABEL while_start\n" ++
    compileBinaryOpExpr cond fname ++
    "JMP_IF_FALSE while_end\n" ++
    concatMap (`compileStmt` fname) body ++
    "JMP while_start\n" ++
    "LABEL while_end\n"

-- For loop
compileStmt (ForStmt init cond step (CompoundStmt body)) fname =
    compileForInit init fname ++
    "LABEL for_start\n" ++
    compileForCond cond fname ++
    concatMap (`compileStmt` fname) body ++
    compileForStep step fname ++
    "JMP for_start\n" ++
    "LABEL for_end\n"


-- Fallback if statement unknown (like for/while )
compileStmt _ _ = "NOP\n"

-- Expressions

compileBinaryOpExpr :: BinaryOpExpr -> Identifier -> String

compileBinaryOpExpr (BinaryOpConst p) _ =
    compileParmCall p

-- BinaryOpExpr: compile left parm, right parm, then do op instruction
compileBinaryOpExpr (BinaryOpExpr l op r) fname =
    compileBinaryOpParm l fname ++
    compileBinaryOpParm r fname ++
    opToInstr op ++ "\n"

compileBinaryOpParm :: BinaryOpParm -> Identifier -> String

compileBinaryOpParm (BinaryOpParm p) _ =
    compileParmCall p

compileBinaryOpParm (BinaryOpParmBOp expr) fname =
    compileBinaryOpExpr expr fname

-- ParmCallDecl

-- Translate a parameter (literal, identifier, or call) into bytecode
compileParmCall :: ParmCallDecl -> String

compileParmCall (ParmCallDeclLiteral lit) =
    compileLiteral lit

compileParmCall (ParmCallDeclIdent ident) =
    "LOAD " ++ ident ++ "\n"

compileParmCall (ParmCallDeclExpr (CallExprDecl fname args)) =
    concatMap compileParmCall args ++
    "CALL " ++ fname ++ " " ++ show (length args) ++ "\n"

-- While / For (Work in progress)

compileForInit :: Maybe VarDeclStmt -> Identifier -> String
compileForInit Nothing _ = ""
compileForInit (Just vds) fname =
    compileStmt (DeclVarExpr vds) fname


compileForCond :: Maybe BinaryOpExpr -> Identifier -> String
compileForCond Nothing _ = ""
compileForCond (Just cond) fname =
    compileBinaryOpExpr cond fname ++
    "JMP_IF_FALSE for_end\n"


compileForStep :: Maybe DeclStmt -> Identifier -> String
compileForStep Nothing _ = ""
compileForStep (Just ds) fname =
    compileStmt (DeclStmt ds) fname


-- Literals

-- PUSH instruction for supported literal
compileLiteral :: Literal -> String
compileLiteral (IntLiteral i)   = "PUSH_INT " ++ show i ++ "\n"
compileLiteral (BoolLiteral b)  = "PUSH_BOOL " ++ show b ++ "\n"
compileLiteral (FloatLiteral f)= "PUSH_FLOAT " ++ show f ++ "\n"
compileLiteral _               = "PUSH_UNKNOWN\n"

-- Operators

-- Map AST binary operator to bytecode instruction
-- NOP = fallback
opToInstr :: BinaryOp -> String
opToInstr Add = "ADD"
opToInstr Sub = "SUB"
opToInstr Mul = "MUL"
opToInstr Div = "DIV"
opToInstr Mod = "MOD"
opToInstr Eq  = "EQ"
opToInstr Lt  = "LT"
opToInstr Gt  = "GT"
opToInstr LEq = "LT_EQ"
opToInstr GEq = "GT_EQ"
opToInstr NEq = "NOT_EQ"
opToInstr And = "AND"
opToInstr Or  = "OR"
opToInstr _   = "NOP"
