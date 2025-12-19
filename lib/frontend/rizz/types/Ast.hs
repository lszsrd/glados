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
-- This grammar is heavely inspired from the Clang C's (programming language) AST.
--
-- For further informations, refer to the [Microsoft C grammar definition] (https://learn.microsoft.com/en-us/cpp/c-language/phrase-structure-grammar)
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
    , UnaryOperatorExpr     (..)
    , BinaryOpParm          (..)
    , BinaryOpExpr          (..)
    , ParmCallDecl          (..)
    , CallExprDecl          (..)
) where

import Token
import GHC.IO.SubSystem (conditional)

-- | Defines @'Decl'@ as a primary Ast node containing language declarations.
--
-- === __Examples__
--
-- Some GHCI syntax examples for every @'Decl'@ constructors:
--
-- >>> FunctionDecl "foo" [ParmVarDeclExpr Integer "x"] (CompoundStmt []) (Just Integer)
-- >>> ParmVarDeclExpr Integer "foo"
-- >>> VarDecl (VarDeclStmt Boolean "foo" Equal (ParmCallDeclLiteral (BoolLiteral True)))
data Decl
    = FunctionDecl Identifier [ParmVarDeclExpr] CompoundStmt (Maybe BuiltinType)
    -- ^ function declaration, expressed in rizz code like @\`fn foo(Char: bar) -> Int {}\`@.
    --
    -- A @'FunctionDecl'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - a leading @'Fn'@.
    --  - function's name as an @'Identifier'@.
    --  - function's __optional__ parameters enclosed in @'OpenRBracket'@ and @'CloseRBracket'@ expressed as @'ParmVarDeclExpr'@ (both round brackets are stil required even if no arguments are provided to the function).
    --  - function's __optional__ return type. If the function __does__ returns, the return type is expressed as an @'Arrow'@ @'Keyword'@ and a @'BuiltinType'@.
    --  - function's body within a @'CompoundStmt'@.
    | ParmVarDecl ParmVarDeclExpr
    -- ^ function parameter, expressed in rizz code like @\`Bool: baz\`@.
    --
    -- A @'ParmVarDecl'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - the parameter's type as a @'BuiltinType'@.
    --  - the parameter's name as an @'Identifier'@.
    --  - a trailing @'Comma'@ if and __only__ if the parameter is __not__ the last one in the list.
    --
    --  Note that a standalone @'ParmVarDecl'@ (not in a @'FunctionDecl'@ expression) is a __grammar violation__!
    | VarDecl VarDeclStmt
    -- ^ variable (with its type) declaration, expressed in rizz code like @\`Float pi = 3.14;\`@.
    --
    -- A @'VarDecl'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - the variable's type as a @'BuiltinType'@.
    --  - the variable's name as an @'Identifier'@.
    --  - any @'AssignOp'@ assignment operator.
    --  - any @'ParmCallDecl'@ (see the definition).
    --  - a trailing @'Semicolon'@ to end the expression.
    --
    -- Note that a variable that initializes itself is a __semantic violation__!

    deriving (
        Show
        -- ^ Allows @'Decl'@ to be printed.
        , Eq
        -- ^ Allows @'Decl'@ to be compared, needed for unit tests.
    )

-- | Defines @'Stmt'@ as the second-primary Ast node containing language statements.
--
-- === __Examples__
--
-- Some GHCI syntax examples for every @'Stmt'@ constructors:
--
-- >>> DeclStmt (DeclStmtLiteral "var" MulEqual (ParmCallDeclLiteral (BoolLiteral True)))
-- >>> UnaryOperator "x" IdentIncrement
-- >>> BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))
-- >>> IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)]))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))) (CompoundStmt []) (Just (CompoundStmt []))
-- >>> WhileStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) NEq (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral False)))) (CompoundStmt [])
-- >>> ForStmt (Just (VarDeclStmt Integer "i" (ParmCallDeclExpr (CallExprDecl "foo" [])))) (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "x")) Lt (BinaryOpParm (ParmCallDeclIdent "y")))) Nothing (CompoundStmt [])
-- >>> ForeachStmt "foo" "it" (CompoundStmt [])
-- >>> CallExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)])
-- >>> RetStmt (BinaryOpConst (ParmCallDeclIdent "foo"))
data Stmt
    = DeclStmt DeclStmt     
    -- ^ variable (without its type) assignment, expressed in rizz code like @\`foo = bar;\`@ or @\`baz++;\`@.
    --
    -- A @'Stmt.DeclStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - the variable's name as an @'Identifier'@.
    --  - any @'AssignOp'@ assignment operator.
    --  - any @'ParmCallDecl'@ (see the definition).
    --  - a trailing @'Semicolon'@ to end the expression.
    --
    -- /TODO/: Rename this to 'AssignStmt' to better describe it.
    -- Note that this statement differs from @'VarDecl'@ as it does not declare the variable's type, it only assigns it a new value.
    | UnaryOperator UnaryOperatorExpr
    -- ^ unary operator, which are @'Identifier'@ self increment or decrement, expressed in rizz code as @\`foo++;\`@ or @\`bar--\`@.
    --
    -- A @'Stmt.DeclStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - the variable's name as an @'Identifier'@.
    --  - one of the @'UnaryOp'@ operator.
    --  - a trailing @'Semicolon'@ to end the expression.
    | BinaryOperator BinaryOpExpr
    -- ^ binary operation, expressed in rizz code like @\`a < 42\`@ or @\`True == foo(bar, baz)\`@ (non exhaustive list).
    --
    -- For further details, see @'BinaryOpExpr'@.
    | IfStmt BinaryOpExpr CompoundStmt (Maybe CompoundStmt)
    -- ^ conditional branching, expressed in rizz code like @\`if (foo == bar) {...}\`@.
    --
    -- A @'IfStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - a leading @'If'@.
    --  - condition enclosed in @'OpenRBracket'@ and @'CloseRBracket'@ expressed as @'BinaryOpExpr'@.
    --  - conditional's body within a @'CompoundStmt'@.
    --  - a second __optional__ @'CompoundStmt'@ to represent an @'Else'@ block, expressed in rizz code as @\`else {...}\`@.
    | WhileStmt BinaryOpExpr CompoundStmt
    -- ^ while loop, expressed in rizz code like @\`while (True) {...}\`@.
    --
    -- A @'WhileStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - a leading @'While'@.
    --  - condition enclosed in @'OpenRBracket'@ and @'CloseRBracket'@ expressed as @'BinaryOpExpr'@.
    --  - while's body within a @'CompoundStmt'@.
    | ForStmt (Maybe VarDeclStmt) (Maybe BinaryOpExpr) (Maybe DeclStmt) CompoundStmt
    -- ^ for loop, expressed in rizz code like @\`for (Int i = 0; i < 10; i++) {...}\`@.
    --
    -- A @'ForeachStmt'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - a leading @'for'@.
    --  - an @'OpenRBracket'@.
    --  - an __optional__ @'VarDeclStmt'@.
    --  - a @'Semicolon'@ @'Punctuator'@.
    --  - an __optional__ @'BinaryOpExpr'@.
    --  - a @'Semicolon'@ @'Punctuator'@.
    --  - an __optional__ @'DeclStmt'@.
    --  - a @'CloseRBracket'@.
    --  - for's body within a @'CompoundStmt'@.
    | ForeachStmt Identifier Identifier CompoundStmt
    -- ^ foreach loop to iterate over a list, expressed in rizz code like @\`foreach (foo : it) {...}\`@.
    --
    -- A @'ForeachStmt'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - a leading @'Foreach'@.
    --  - an @'OpenRBracket'@.
    --  - the variable's name as an @'Identifier'@.
    --  - a @'Colon'@ @'Punctuator'@.
    --  - the iterator's name as an @'Identifier'@.
    --  - a @'CloseRBracket'@.
    --  - foreach's body within a @'CompoundStmt'@.
    | CallExpr CallExprDecl 
    -- ^ function call, expressed in rizz code like @\`foo(bar, baz);\`@.
    --
    -- A @'CallExprDecl'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - function's name as an @'Identifier'@.
    --  - function's parameters enclosed in @'OpenRBracket'@ and @'CloseRBracket'@ expressed as @'ParmCallDecl'@ (both round brackets are stil required even if no arguments are provided to the function).
    --  - a trailing @'Semicolon'@ to end the expression.
    | RetStmt BinaryOpExpr
    -- ^ return from function, expressed in rizz code like @\`ret 42;\`@.
    --
    -- A @'RetStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - a leading @'Ret'@.
    --  - @'BinaryOpExpr'@ which suites all needs, from constants to aritmethics or even function call.
    --  - a trailing @'Semicolon'@ to end the expression.

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
    --  - a conditional body
    --  - a loop body
    --  - an unnamed scope

    deriving (
        Show
        -- ^ Allows @'CompoundStmt'@ to be printed.
        , Eq
        -- ^ Allows @'CompoundStmt'@ to be compared, needed for unit tests.
    )

-- | Defines the @'ParmVarDeclExpr'@ expression (extended constructor for @'ParmVarDecl'@).
--
-- === __Example__
--
-- A GHCI syntax example to declare a @'ParmVarDecl'@:
-- >>> ParmVarDecl Boolean "x"
data ParmVarDeclExpr
    = ParmVarDeclExpr BuiltinType Identifier
    -- ^ function's parameter, see @'ParmVarDecl'@ definition.

    deriving (
        Show
        -- ^ Allows @'ParmVarDecl'@ to be printed.
        , Eq
        -- ^ Allows @'ParmVarDecl'@ to be compared, needed for unit tests.
    )

-- | Defines the @'BuiltinType'@ expression which lists available rizz data types (implementating Keyword @'Token'@ types).
data BuiltinType
    = Boolean
    -- ^ boolean type keyword, see Bool @'Keyword'@
    | Character
    -- ^ character type keyword, see Char @'Keyword'@
    | Integer
    -- ^ integer type keyword, see Int @'Keyword'@
    | SinglePrecision
    -- ^ float type keyword, see Float @'Keyword'@

    deriving (
        Show
        -- ^ Allows @'BuiltinType'@ to be printed.
        , Eq
        -- ^ Allows @'BuiltinType'@ to be compared, needed for unit tests.
    )

-- | Defines the @'VarDeclStmt'@ expression (extended constructor for @'VarDecl'@).
data VarDeclStmt
    = VarDeclStmt BuiltinType Identifier AssignOp ParmCallDecl
    -- ^ variable declaration, see @'VarDecl'@ definition.
    
    deriving (
        Show
        -- ^ Allows @'VarDeclStmt'@ to be printed.
        , Eq
        -- ^ Allows @'VarDeclStmt'@ to be compared, needed for unit tests.
    )

-- | Defines the @'DeclStmt'@ expression (extended constructor for @'DeclStmt'@).
--
-- === __Example__
--
-- >>> DeclStmtLiteral "var" DivEqual (ParmCallDeclLiteral (BoolLiteral True))
data DeclStmt
    = DeclStmtLiteral Identifier AssignOp ParmCallDecl
    -- ^ variable assignment, expressed in rizz code like @\`foo = bar\`@.
    | DeclStmtUnary UnaryOperatorExpr
    -- ^ variable with a @'UnaryOp'@, expressed in rizz code like @\`foo++\`@.

    deriving (
        Show
        -- ^ Allows @'DeclStmt'@ to be printed.
        , Eq
        -- ^ Allows @'DeclStmt'@ to be compared, needed for unit tests.
    )

-- | Defines the @'UnaryOperatorExpr'@ expression (extended constructor for @'UnaryOperator'@).
data UnaryOperatorExpr
    = UnaryOperatorExpr Identifier UnaryOp
    -- ^ variable declaration, see @'UnaryOperator'@ definition.

    deriving (
        Show
        -- ^ Allows @'UnaryOperatorExpr'@ to be printed.
        , Eq
        -- ^ Allows @'UnaryOperatorExpr'@ to be compared, needed for unit tests.BinaryOp
    )

-- | Defines @'BinaryOp'@'s parameters.
data BinaryOpParm
    = BinaryOpParm ParmCallDecl
    -- ^ function call which should be interpreted and evaluated.
    | BinaryOpParmBOp BinaryOpExpr
    -- ^ any other @'BinaryOpExpr'@.

    deriving (
        Show
        -- ^ Allows @'BinaryOpParm'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOpParm'@ to be compared, needed for unit tests.
    )

-- | Defines @'BinaryOpExpr'@ as any expression that can be evaluated by equality or aritmethic operators.
data BinaryOpExpr
    = BinaryOpExpr BinaryOpParm BinaryOp BinaryOpParm
    -- ^ two @'BinaryOpParm'@ being compared with any @'BinaryOp'@ operator.
    | BinaryOpConst ParmCallDecl
    -- ^ any literal value defined by @'ParmCallDecl'@.

    deriving (
        Show
        -- ^ Allows @'BinaryOpExpr'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOpExpr'@ to be compared, needed for unit tests.
    )

-- | Defines @'ParmCallDecl'@ as any value that can be evaluated. It also binds to function call's parameters.
data ParmCallDecl
    = ParmCallDeclLiteral Literal
    -- ^ literal value defined by Literal @'Token'@, expressed in rizz code like @\`42\`@.
    | ParmCallDeclIdent Identifier
    -- ^ identifier, which is a bound variable, expressed in rizz code like @\`foo\`@.
    | ParmCallDeclExpr CallExprDecl
    -- ^ function call, defined by @'CallExprDecl'@, see @'CallExprDecl'@.

    deriving (
        Show
        -- ^ Allows @'ParmCallDecl'@ to be printed.
        , Eq
        -- ^ Allows @'ParmCallDecl'@ to be compared, needed for unit tests.
    )

-- | Defines @'CallExprDecl'@ as a function call expression.
data CallExprDecl
    = CallExprDecl Identifier [ParmCallDecl]
    -- ^ function call expression, actually 'calling' another function.

    deriving (
        Show
        -- ^ Allows @'CallExprDecl'@ to be printed.
        , Eq
        -- ^ Allows @'CallExprDecl'@ to be compared, needed for unit tests.
    )
