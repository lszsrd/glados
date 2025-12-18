{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/types/Ast.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Ast
-- Description : Defines the abstract syntax tree structure for the rizz language.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- The abstract syntax tree serves to defines how rizz tokens forms a valid grammar in order to represent a correct language expression.
-- This grammar is heavely inspired from the clang C programming language.
-- For further information, refer to the [Microsoft C grammar definition] (https://learn.microsoft.com/en-us/cpp/c-language/phrase-structure-grammar)
-- and the [Clang AST](https://clang.llvm.org/docs/IntroductionToTheClangAST.html) documentation.
-------------------------------------------------------------------------------
module Ast (
    -- * Top level definitions
    Decl                    (..)
    , Stmt                  (..)

    -- * Declarations
    , CompoundStmt          (..)
    , ParmVarDeclExpr       (..)
    , BuiltinType           (..)
    , VarDeclStmt           (..)

    -- * Statements
    , DeclStmt              (..)
    , BinaryOpParm          (..)
    , BinaryOpExpr          (..)
    , ParmCallDecl          (..)
    , CallExprDecl          (..)
) where

import Token
import Control.Applicative (optional)

-- | Defines @'Decl'@ as a primary node containing language declarations.
data Decl
    = FunctionDecl Identifier [ParmVarDeclExpr] CompoundStmt (Maybe BuiltinType)    -- FunctionDecl Nothing "foo" [ParmVarDeclExpr Integer "x"] (CompoundStmt [])
    -- ^ function declaration, expressed in rizz code like @'fn foo(Char: bar) -> Int {}'@.
    --
    -- A @'FunctionDecl'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - the @'Fn'@ @'Keyword'@.
    --
    --  - function's name as an @'Identifier'@.
    --
    --  - function's __optional__ parameters enclosed in @'OpenRBracket'@ and @'CloseRBracket'@ expressed as @'ParmVarDeclExpr'@.
    --
    --  - function's __optional__ return type. If the function __does__ returns, the return type is expressed as an @'Arrow'@ @'Keyword'@ and a @'BuiltinType'@.
    --  - function's body within a @'CompoundStmt'@.
    | ParmVarDecl ParmVarDeclExpr   -- ParmVarDeclExpr Integer "foo"
    -- ^ #ParmVarDeclLabel#
    -- function parameter, expressed in rizz code like @\`Bool: baz\`@.
    --
    -- A @'ParmVarDecl'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - the parameter's type as a @'BuiltinType'@.
    --
    --  - the parameter's name as an @'Identifier'@.
    --
    --  - a trailing @'Comma'@ if and __only__ if the parameter is __not__ the last one in the list.
    --
    --  Note that a standalone @'ParmVarDecl'@ (not in a @'FunctionDecl'@ expression) is a __grammar violation__!
    | VarDecl VarDeclStmt   -- VarDecl (VarDeclStmt Boolean "foo" (ParmCallDeclLiteral (BoolLiteral True)))
    -- ^ variable (with its type) declaration, expressed is rizz code like @'Float pi = 3.14'@.
    --
    -- A @'VarDecl'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - the variable's type as a @'BuiltinType'@.
    --
    --  - the variable's name as an @'Identifier'@.
    --
    -- Note that a variable that initializes itself is a __semantic violation__!

    deriving (
        Show
        -- ^ Allows @'Decl'@ to be printed.
        , Eq
        -- ^ Allows @'Decl'@ to be compared, needed for unit tests.
    )

data Stmt
    = DeclStmt DeclStmt     -- DeclStmt (DeclStmtLiteral "var" MulEqual (ParmCallDeclLiteral (BoolLiteral True)))
    | UnaryOperator Identifier UnaryOp  -- UnaryOperator "x" IdentIncrement
    | BinaryOperator BinaryOpExpr   -- BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))
    | IfStmt BinaryOpExpr CompoundStmt (Maybe CompoundStmt) -- IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)]))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))) (CompoundStmt []) (Just (CompoundStmt []))
    | WhileStmt BinaryOpExpr CompoundStmt   -- WhileStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) NEq (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral False)))) (CompoundStmt [])
    | ForStmt (Maybe VarDeclStmt) (Maybe BinaryOpExpr) (Maybe DeclStmt) CompoundStmt    -- ForStmt (Just (VarDeclStmt Integer "i" (ParmCallDeclExpr (CallExprDecl "foo" [])))) (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "x")) Lt (BinaryOpParm (ParmCallDeclIdent "y")))) Nothing (CompoundStmt [])
    | ForeachStmt Identifier Identifier CompoundStmt    -- ForeachStmt "foo" "it" (CompoundStmt [])
    | CallExpr CallExprDecl -- CallExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)])
    | RetStmt BinaryOpExpr  -- RetStmt (BinaryOpConst (ParmCallDeclIdent "foo"))

    deriving (
        Show
        -- ^ Allows @'Stmt'@ to be printed.
        , Eq
        -- ^ Allows @'Stmt'@ to be compared, needed for unit tests.
    )

-- | Defines @'CompoundStmt'@ type as a list of @'Stmt'@ expressions.
newtype CompoundStmt
    = CompoundStmt [Stmt]
    -- ^ compound statement block, expressed in rizz code as @\`{...}\`@ (any code enclosed in a @'OpenCBracket'@ and a @'CloseCBracket'@).
    --
    -- A @'CompoundStmt'@ is used for special cases:
    --
    --  - a function body
    --
    --  - a conditional body
    --
    --  - a loop body
    --
    --  - an unnamed scope

    deriving (
        Show
        -- ^ Allows @'CompoundStmt'@ to be printed.
        , Eq
        -- ^ Allows @'CompoundStmt'@ to be compared, needed for unit tests.
    )

data ParmVarDeclExpr
    = ParmVarDeclExpr BuiltinType Identifier    -- ParmVarDecl Boolean "x"
    -- ^ Function's parameter, see [ParmVarDecl](Ast#ParmVarDeclLabel) definition.
    

    deriving (
        Show
        -- ^ Allows @'ParmVarDecl'@ to be printed.
        , Eq
        -- ^ Allows @'ParmVarDecl'@ to be compared, needed for unit tests.
    )

data BuiltinType
    = Boolean
    | Character
    | Integer
    | SinglePrecision

    deriving (
        Show
        -- ^ Allows @'BuiltinType'@ to be printed.
        , Eq
        -- ^ Allows @'BuiltinType'@ to be compared, needed for unit tests.
    )

data VarDeclStmt
    = VarDeclStmt BuiltinType Identifier ParmCallDecl
    

    deriving (
        Show
        -- ^ Allows @'VarDeclStmt'@ to be printed.
        , Eq
        -- ^ Allows @'VarDeclStmt'@ to be compared, needed for unit tests.
    )

data DeclStmt
    = DeclStmtLiteral Identifier AssignOp ParmCallDecl
    -- DeclStmtLiteral "var" DivEqual (ParmCallDeclLiteral (BoolLiteral True))

    deriving (
        Show
        -- ^ Allows @'DeclStmt'@ to be printed.
        , Eq
        -- ^ Allows @'DeclStmt'@ to be compared, needed for unit tests.
    )

data BinaryOpParm
    = BinaryOpParm ParmCallDecl
    | BinaryOpParmBOp BinaryOpExpr

    deriving (
        Show
        -- ^ Allows @'BinaryOpParm'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOpParm'@ to be compared, needed for unit tests.
    )

data BinaryOpExpr
    = BinaryOpExpr BinaryOpParm BinaryOp BinaryOpParm
    | BinaryOpConst ParmCallDecl

    deriving (
        Show
        -- ^ Allows @'BinaryOpExpr'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOpExpr'@ to be compared, needed for unit tests.
    )

data ParmCallDecl
    = ParmCallDeclLiteral Literal
    | ParmCallDeclIdent Identifier
    | ParmCallDeclExpr CallExprDecl

    deriving (
        Show
        -- ^ Allows @'ParmCallDecl'@ to be printed.
        , Eq
        -- ^ Allows @'ParmCallDecl'@ to be compared, needed for unit tests.
    )

data CallExprDecl
    = CallExprDecl Identifier [ParmCallDecl]

    deriving (
        Show
        -- ^ Allows @'CallExprDecl'@ to be printed.
        , Eq
        -- ^ Allows @'CallExprDecl'@ to be compared, needed for unit tests.
    )
