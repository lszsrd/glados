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
-- The abstract syntax tree serves to define how rizz tokens forms a valid grammar in order to represent a correct language expression.
-- This grammar is heavily inspired from the Clang C's (programming language) AST.
--
-- For further information, refer to the [Microsoft C grammar definition] (https://learn.microsoft.com/en-us/cpp/c-language/phrase-structure-grammar)
-- and the [Clang AST](https://clang.llvm.org/docs/IntroductionToTheClangAST.html) documentation.
--
-- === __Example__
-- Taking those simple functions, expressed in rizz code:
--
-- @fn foo(Int: x) -> Int
--{
--    Int y = 42;
-- \
--    if (x == 0) {
--        ret 0;
--    }
--    ret y / x;
--}
-- \
--fn bar(Int: x) -> Int
--{
--    Int y = 1;
-- \
--    for (; y < x; y++) {
--        if (foo(y) == 0) {
--            ret 10;
--        }
--    }
--    ret y;
--}
-- \
--fn baz()
--{
--    bar(foo(100));
--}@
--
-- will be translated (each) as these following @'Decl'@:
--
-- >>> FunctionDecl "foo" [ParmVarDeclExpr Integer "x"] (CompoundStmt [DeclVarExpr (VarDeclStmt Integer "y" Equal (ParmCallDeclLiteral (IntLiteral 42))), IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "x")) Eq (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 0)))) (CompoundStmt [RetStmt (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 0)))]) Nothing, RetStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "y")) Div (BinaryOpParm (ParmCallDeclIdent "x")))]) (Just Integer)
--
-- >>> FunctionDecl "bar" [ParmVarDeclExpr Integer "x"] (CompoundStmt [DeclVarExpr (VarDeclStmt Integer "y" Equal (ParmCallDeclLiteral (IntLiteral 1))),ForStmt Nothing (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "y")) Lt (BinaryOpParm (ParmCallDeclIdent "x")))) (Just (DeclAssignStmtUnary (UnaryOperatorExpr "y" IdentIncrement))) (CompoundStmt [IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclIdent "y"]))) Eq (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 0)))) (CompoundStmt [RetStmt (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 0)))]) Nothing]),RetStmt (BinaryOpConst (ParmCallDeclIdent "y"))]) (Just Integer)
--
-- >>> FunctionDecl "baz" [] (CompoundStmt [CallExpr (CallExprDecl "bar" [ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 100)])])]) Nothing
--
-- and will be translated as a compilation unit (a @'[Decl]'@) as follow:
--
-- >>> [FunctionDecl "foo" [ParmVarDeclExpr Integer "x"] (CompoundStmt [DeclVarExpr (VarDeclStmt Integer "y" Equal (ParmCallDeclLiteral (IntLiteral 42))), IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "x")) Eq (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 0)))) (CompoundStmt [RetStmt (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 0)))]) Nothing, RetStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "y")) Div (BinaryOpParm (ParmCallDeclIdent "x")))]) (Just Integer), FunctionDecl "bar" [ParmVarDeclExpr Integer "x"] (CompoundStmt [DeclVarExpr (VarDeclStmt Integer "y" Equal (ParmCallDeclLiteral (IntLiteral 1))),ForStmt Nothing (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "y")) Lt (BinaryOpParm (ParmCallDeclIdent "x")))) (Just (DeclAssignStmtUnary (UnaryOperatorExpr "y" IdentIncrement))) (CompoundStmt [IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclIdent "y"]))) Eq (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 0)))) (CompoundStmt [RetStmt (BinaryOpConst (ParmCallDeclLiteral (IntLiteral 0)))]) Nothing]),RetStmt (BinaryOpConst (ParmCallDeclIdent "y"))]) (Just Integer), FunctionDecl "baz" [] (CompoundStmt [CallExpr (CallExprDecl "bar" [ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 100)])])]) Nothing]
-------------------------------------------------------------------------------
module Ast (
    -- * BNF definition
    -- $bnf

    -- * Top level definitions
    Decl                    (..)
    , Stmt                  (..)

    -- * Declarations
    , CompoundStmt          (..)
    , ParmVarDeclExpr       (..)
    , BuiltinType           (..)
    , VarDeclStmt           (..)
    , RecordDeclExpr        (..)

    -- * Statements
    , DeclStmt              (..)
    , UnaryOperatorExpr     (..)
    , BinaryOpParm          (..)
    , BinaryOpExpr          (..)
    , ParmCallDecl          (..)
    , CallExprDecl          (..)
) where

import Tokens

-- $bnf
-- For the full rizz grammar syntax definition, see the [BNF definition](https://github.com/lszsrd/glados/blob/main/docs/BNF/rizz-grammar.md) here.

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
    -- ^ Function declaration, expressed in rizz code like @\`fn foo(Char: bar) -> Int {}\`@.
    --
    -- A @'FunctionDecl'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - A leading @'Fn'@.
    --  - Function's name as an @'Identifier'@.
    --  - Function's __optional__ parameters enclosed in @'OpenRBracket'@ and @'CloseRBracket'@ expressed as @'ParmVarDeclExpr'@ (both round brackets are still required even if no arguments are provided to the function).
    --  - Function's __optional__ return type. If the function __does__ returns, the return type is expressed as an @'Arrow'@ @'Keyword'@ and a @'BuiltinType'@.
    --  - Function's body within a @'CompoundStmt'@.
    | ParmVarDecl ParmVarDeclExpr
    -- ^ Function parameter, expressed in rizz code like @\`Bool: baz\`@.
    --
    -- A @'ParmVarDecl'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - The parameter's type as a @'BuiltinType'@.
    --  - A @'Colon'@.
    --  - The parameter's name as an @'Identifier'@.
    --  - A trailing @'Comma'@ if and __only__ if the parameter is __not__ the last one in the list.
    --
    --  Note that a standalone @'ParmVarDecl'@ (not in a @'FunctionDecl'@ expression) is a __grammar violation__!
    | VarDecl VarDeclStmt
    -- ^ Variable (with its type) declaration, expressed in rizz code like @\`Float pi = 3.14;\`@.
    --
    -- A @'VarDecl'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - The variable's type as a @'BuiltinType'@.
    --  - The variable's name as an @'Identifier'@.
    --  - Any @'AssignOp'@ assignment operator.
    --  - Any @'ParmCallDecl'@ (see the definition).
    --  - A trailing @'Semicolon'@ to end the expression.
    --
    --  /TODO/: Merge both @'VarDecl'@ and @'Stmt.DeclStmt'@ types as they are almost identical.
    --
    -- Note that a variable that initializes itself is a __semantic violation__!
    | RecordDecl RecordDeclExpr
    -- ^ Structure declaration, expressed in rizz code like @\`struct Foo {Int: x; Int y; Bool: alive;}\`@.
    --
    -- A @'RecordDecl'@ rizz grammar in-code is as follow, in the given order:
    --
    --

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
-- >>> DeclVarExpr (VarDeclStmt Boolean "foo" Equal (ParmCallDeclLiteral (BoolLiteral True)))
-- >>> DeclStmt (DeclAssignStmtLiteral "var" MulEqual (ParmCallDeclLiteral (BoolLiteral True)))
-- >>> UnaryOperatorExpr "x" IdentIncrement
-- >>> BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))
-- >>> IfStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)]))) Lt (BinaryOpParm (ParmCallDeclIdent "a"))) (CompoundStmt []) (Just (CompoundStmt []))
-- >>> WhileStmt (BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral True))) NEq (BinaryOpParm (ParmCallDeclLiteral (BoolLiteral False)))) (CompoundStmt [])
-- >>> ForStmt (Just (VarDeclStmt Integer "i" Equal (ParmCallDeclExpr (CallExprDecl "foo" [])))) (Just (BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "x")) Lt (BinaryOpParm (ParmCallDeclIdent "y")))) Nothing (CompoundStmt [])
-- >>> ForeachStmt "foo" "it" (CompoundStmt [])
-- >>> CallExpr (CallExprDecl "foo" [ParmCallDeclLiteral (IntLiteral 42)])
-- >>> RetStmt (BinaryOpConst (ParmCallDeclIdent "foo"))
data Stmt
    = DeclVarExpr VarDeclStmt
    -- ^ @'VarDeclStmt'@ alias, allows a variable typed declaration to be a @'Stmt'@.
    | DeclStmt DeclStmt
    -- ^ Variable (without its type) assignment, expressed in rizz code like @\`foo = bar;\`@ or @\`baz++;\`@.
    --
    -- A @'Stmt.DeclStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - The variable's name as an @'Identifier'@.
    --  - Any @'AssignOp'@ assignment operator.
    --  - Any @'ParmCallDecl'@ (see the definition).
    --  - A trailing @'Semicolon'@ to end the expression.
    --
    -- /TODO/: Rename this to 'AssignStmt' to better describe it.
    --
    -- Note that this statement differs from @'VarDecl'@ as it does not declare the variable's type, it only assigns a new value to it.
    | UnaryOperator UnaryOperatorExpr
    -- ^ Unary operator, which are @'Identifier'@ self increment or decrement, expressed in rizz code as @\`foo++;\`@ or @\`bar--\`@.
    --
    -- A @'Stmt.DeclStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - The variable's name as an @'Identifier'@.
    --  - One of the @'UnaryOp'@ operator.
    --  - A trailing @'Semicolon'@ to end the expression.
    | BinaryOperator BinaryOpExpr
    -- ^ Binary operation, expressed in rizz code like @\`a < 42\`@ or @\`True == foo(bar, baz)\`@ (non exhaustive list).
    --
    -- For further details, see @'BinaryOpExpr'@.
    | IfStmt BinaryOpExpr CompoundStmt (Maybe CompoundStmt)
    -- ^ Conditional branching, expressed in rizz code like @\`if (foo == bar) {...}\`@.
    --
    -- A @'IfStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - A leading @'If'@.
    --  - Condition enclosed in @'OpenRBracket'@ and @'CloseRBracket'@ expressed as @'BinaryOpExpr'@.
    --  - Conditional's body within a @'CompoundStmt'@.
    --  - A second __optional__ @'CompoundStmt'@ to represent an @'Else'@ block, expressed in rizz code as @\`else {...}\`@.
    | WhileStmt BinaryOpExpr CompoundStmt
    -- ^ While loop, expressed in rizz code like @\`while (True) {...}\`@.
    --
    -- A @'WhileStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - A leading @'While'@.
    --  - Condition enclosed in @'OpenRBracket'@ and @'CloseRBracket'@ expressed as @'BinaryOpExpr'@.
    --  - While's body within a @'CompoundStmt'@.
    | ForStmt (Maybe VarDeclStmt) (Maybe BinaryOpExpr) (Maybe DeclStmt) CompoundStmt
    -- ^ For loop, expressed in rizz code like @\`for (Int i = 0; i < 10; i++) {...}\`@.
    --
    -- A @'ForeachStmt'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - A leading @'for'@.
    --  - An @'OpenRBracket'@.
    --  - An __optional__ @'VarDeclStmt'@.
    --  - A @'Semicolon'@ @'Punctuator'@.
    --  - An __optional__ @'BinaryOpExpr'@.
    --  - A @'Semicolon'@ @'Punctuator'@.
    --  - An __optional__ @'DeclStmt'@.
    --  - A @'CloseRBracket'@.
    --  - For's body within a @'CompoundStmt'@.
    | ForeachStmt Identifier Identifier CompoundStmt
    -- ^ Foreach loop to iterate over a list, expressed in rizz code like @\`foreach (foo : it) {...}\`@.
    --
    -- A @'ForeachStmt'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - A leading @'Foreach'@.
    --  - An @'OpenRBracket'@.
    --  - The variable's name as an @'Identifier'@.
    --  - A @'Colon'@ @'Punctuator'@.
    --  - The iterator's name as an @'Identifier'@.
    --  - A @'CloseRBracket'@.
    --  - Foreach's body within a @'CompoundStmt'@.
    | CallExpr CallExprDecl 
    -- ^ Function call, expressed in rizz code like @\`foo(bar, baz);\`@.
    --
    -- A @'CallExprDecl'@'s rizz grammar in-code is as follow, in the given order:
    --
    --  - Function's name as an @'Identifier'@.
    --  - Function's parameters enclosed in @'OpenRBracket'@ and @'CloseRBracket'@ expressed as @'ParmCallDecl'@ (both round brackets are still required even if no arguments are provided to the function).
    --  - A trailing @'Semicolon'@ to end the expression.
    | RetStmt (Maybe BinaryOpExpr)
    -- ^ Return from function, expressed in rizz code like @\`ret 42;\`@.
    --
    -- A @'RetStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - A leading @'Ret'@.
    --  - @'BinaryOpExpr'@ which suits all needs, from constants to arithmetics or even function call.
    --  - A trailing @'Semicolon'@ to end the expression.

    | LoopControlStmt Keyword
    -- ^ Control exec from loop instruction, expressed in rizz code like @\`continue;\`@ or @\`break;\`@.
    --
    -- A @'LoopControlStmt'@ rizz grammar in-code is as follow, in the given order:
    --
    --  - A leading @'continue'@ or @'break'@.
    --  - A trailing @'Semicolon'@ to end the expression.


    deriving (
        Show
        -- ^ Allows @'Stmt'@ to be printed.
        , Eq
        -- ^ Allows @'Stmt'@ to be compared, needed for unit tests.
    )

-- | Defines @'CompoundStmt'@ type as a list of @'Stmt'@ expressions.
newtype CompoundStmt
    = CompoundStmt [Stmt]
    -- ^ Compound statement block, expressed in rizz code as @\`{...}\`@ (any code enclosed in a @'OpenCBracket'@ and a @'CloseCBracket'@).
    --
    -- A @'CompoundStmt'@ is used for the following special cases:
    --
    --  - A function body
    --  - A conditional body
    --  - A loop body
    --  - An unnamed scope

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
--
-- >>> ParmVarDeclExpr Boolean "x"
data ParmVarDeclExpr
    = ParmVarDeclExpr BuiltinType Identifier
    -- ^ Function's parameter, see @'ParmVarDecl'@ definition.
    | ParmVarRecord RecordDeclExpr

    deriving (
        Show
        -- ^ Allows @'ParmVarDecl'@ to be printed.
        , Eq
        -- ^ Allows @'ParmVarDecl'@ to be compared, needed for unit tests.
    )

data RecordDeclExpr
    = RecordDeclExpr Identifier [ParmVarDeclExpr]
    -- ^ Structure declaration

    deriving (
        Show
        -- ^ Allows @'ParmVarDecl'@ to be printed.
        , Eq
        -- ^ Allows @'ParmVarDecl'@ to be compared, needed for unit tests.
    )

-- | Defines the @'BuiltinType'@ expression which lists available rizz data types (implementating Keyword @'Token'@ types).
data BuiltinType
    = Boolean
    -- ^ Boolean type keyword, see Bool @'Keyword'@
    | Character
    -- ^ Character type keyword, see Char @'Keyword'@
    | String
    -- ^ Sugar for [Char] type keyword, see String @'Keyword'@
    | Integer
    -- ^ Integer type keyword, see Int @'Keyword'@
    | SinglePrecision
    -- ^ Float type keyword, see Float @'Keyword'@
    | ListType BuiltinType
    -- ^ List type, expressed in rizz code as @\`[Int]\`@, @\`[[Int]]\`@, etc.
    | Struct Identifier

    deriving (
        Show
        -- ^ Allows @'BuiltinType'@ to be printed.
        , Eq
        -- ^ Allows @'BuiltinType'@ to be compared, needed for unit tests.
    )

-- | Defines the @'VarDeclStmt'@ expression (extended constructor for @'VarDecl'@).
data VarDeclStmt
    = VarDeclStmt BuiltinType Identifier AssignOp ParmCallDecl
    -- ^ Variable declaration, see @'VarDecl'@ definition.
    
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
-- A GHCI syntax example to declare a @'ParmVarDecl'@:
--
-- >>> DeclAssignStmtLiteral "var" DivEqual (ParmCallDeclLiteral (BoolLiteral True))
-- >>> DeclAssignStmtUnary (UnaryOperatorExpr "var" IdentIncrement)
data DeclStmt
    = DeclAssignStmtLiteral Identifier AssignOp ParmCallDecl
    -- ^ Variable assignment, expressed in rizz code like @\`foo = bar\`@.
    | DeclAssignStmtUnary UnaryOperatorExpr
    -- ^ Variable assignment via a @'UnaryOp'@, expressed in rizz code like @\`foo++\`@.

    deriving (
        Show
        -- ^ Allows @'DeclStmt'@ to be printed.
        , Eq
        -- ^ Allows @'DeclStmt'@ to be compared, needed for unit tests.
    )

-- | Defines the @'UnaryOperatorExpr'@ expression (extended constructor for @'UnaryOperator'@).
--
-- === __Example__
--
-- A GHCI syntax example to declare a @'UnaryOperatorExpr'@:
--
-- >>> UnaryOperatorExpr "foo" IdentIncrement
data UnaryOperatorExpr
    = UnaryOperatorExpr Identifier UnaryOp
    -- ^ Variable declaration, see @'UnaryOperator'@ definition.

    deriving (
        Show
        -- ^ Allows @'UnaryOperatorExpr'@ to be printed.
        , Eq
        -- ^ Allows @'UnaryOperatorExpr'@ to be compared, needed for unit tests.BinaryOp
    )

-- | Defines @'BinaryOp'@'s parameters.
--
-- === __Examples__
--
-- A GHCI syntax example to declare a @'BinaryOpParm'@:
--
-- >>> BinaryOpParm (ParmCallDeclLiteral (IntLiteral 42))
data BinaryOpParm
    = BinaryOpParm ParmCallDecl
    -- ^ Function call which should be interpreted and evaluated.
    | BinaryOpParmBOp BinaryOpExpr
    -- ^ Any other @'BinaryOpExpr'@.

    deriving (
        Show
        -- ^ Allows @'BinaryOpParm'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOpParm'@ to be compared, needed for unit tests.
    )

-- | Defines @'BinaryOpExpr'@ as any expression that can be evaluated by equality or arithmetic operators.
--
-- === __Examples__
--
-- A GHCI syntax example to declare a @'BinaryOpExpr'@:
--
-- >>> BinaryOpExpr (BinaryOpParm (ParmCallDeclIdent "x")) Div (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 42)))
-- >>> BinaryOpConst (ParmCallDeclLiteral (IntLiteral 84))
-- >>> BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 10))) Add (BinaryOpParmBOp (BinaryOpExpr (BinaryOpParm (ParmCallDeclLiteral (IntLiteral 45))) Add (BinaryOpParm (ParmCallDeclExpr (CallExprDecl "foo" [ParmCallDeclIdent "bar"])))))
data BinaryOpExpr
    = BinaryOpExpr BinaryOpParm BinaryOp BinaryOpParm
    -- ^ Two @'BinaryOpParm'@ being compared with any @'BinaryOp'@ operator.
    | BinaryOpConst ParmCallDecl
    -- ^ Any literal value defined by @'ParmCallDecl'@.

    deriving (
        Show
        -- ^ Allows @'BinaryOpExpr'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOpExpr'@ to be compared, needed for unit tests.
    )

-- | Defines @'ParmCallDecl'@ as any value that can be evaluated. It also binds to function call's parameters.
data ParmCallDecl
    = ParmCallDeclLiteral Literal
    -- ^ Literal value defined by Literal @'Token'@, expressed in rizz code like @\`42\`@.
    | ParmCallDeclIdent Identifier
    -- ^ Identifier, which is a bound variable, expressed in rizz code like @\`foo\`@.
    | ParmCallDeclExpr CallExprDecl
    -- ^ Function call, defined by @'CallExprDecl'@, see @'CallExprDecl'@.
    | ParmCallBExpr BinaryOpParm BinaryOp BinaryOpParm
    -- ^ Binary expression, an exact copy of @'BinaryOpExpr'@ without self calling @'BinaryOpConst'@ allowing expression in rizz code like @\`x - 1\`@ in function calls.
    | ParmCallDeclIdx ParmCallDecl ParmCallDecl
    -- ^ List index access, like @`list[0]`@ or @`arr[i]`@.
    | ParmCallDeclList [ParmCallDecl]

    deriving (
        Show
        -- ^ Allows @'ParmCallDecl'@ to be printed.
        , Eq
        -- ^ Allows @'ParmCallDecl'@ to be compared, needed for unit tests.
    )

-- | Defines @'CallExprDecl'@ as a function call expression.
data CallExprDecl
    = CallExprDecl Identifier [ParmCallDecl]
    -- ^ Function call expression, actually 'calling' another function.

    deriving (
        Show
        -- ^ Allows @'CallExprDecl'@ to be printed.
        , Eq
        -- ^ Allows @'CallExprDecl'@ to be compared, needed for unit tests.
    )
