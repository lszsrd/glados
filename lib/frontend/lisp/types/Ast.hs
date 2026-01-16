{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/lisp/types/Ast.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Ast
-- Description : Defines the abstract syntax tree structure for the lisp language.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- The abstract syntax tree serves to define how lisp tokens forms a valid grammar in order to represent a correct language expression.
--
--
-- === __Example__
-- Taking those simple functions, expressed in rizz code:
--
-- @(eq? (+ 3.0 2) (/ 6 2))
--
-- (+ (- 1 2) (* 3 (/ 4 (remainder 5 6))))
--
-- (define abs
--   (lambda (x)
--     (if (< x 0) (- 0 x) x)))
--
-- will be translated (each) as these following @'Expr'@:
--
-- >>> BinaryOp (CondExpr OpEq) (BinaryOp (ArithExpr OpAdd) (Const (SPrecision 3.0)) (Const (Int 2))) (BinaryOp (ArithExpr OpDiv) (Const (Int 6)) (Const (Int 2)))
--
-- >>> BinaryOp (ArithExpr OpAdd) (BinaryOp (ArithExpr OpSub) (Const (Int 1)) (Const (Int 2))) (BinaryOp (ArithExpr OpMul) (Const (Int 3)) (BinaryOp (ArithExpr OpDiv) (Const (Int 4)) (BinaryOp (ArithExpr OpMod) (Const (Int 5)) (Const (Int 6)))))
--
-- >>> Defun (Define "abs" (Defun (Lambda ["x"] (If OpLt (BinaryOp (ArithExpr OpSub) (Const (Int 0)) (Const (Identifier "x"))) (Const (Identifier "x"))))))
--
-- and will be translated as a compilation unit (a @'[Expr]'@) as follow:
-- >>> [BinaryOp (CondExpr OpEq) (BinaryOp (ArithExpr OpAdd) (Const (SPrecision 3.0)) (Const (Int 2))) (BinaryOp (ArithExpr OpDiv) (Const (Int 6)) (Const (Int 2))), BinaryOp (ArithExpr OpAdd) (BinaryOp (ArithExpr OpSub) (Const (Int 1)) (Const (Int 2))) (BinaryOp (ArithExpr OpMul) (Const (Int 3)) (BinaryOp (ArithExpr OpDiv) (Const (Int 4)) (BinaryOp (ArithExpr OpMod) (Const (Int 5)) (Const (Int 6))))), Defun (Define "abs" (Defun (Lambda ["x"] (If OpLt (BinaryOp (ArithExpr OpSub) (Const (Int 0)) (Const (Identifier "x"))) (Const (Identifier "x"))))))]
-------------------------------------------------------------------------------
module Ast (
    -- * Basic type
    Identifier

    -- * Expression definitions
    , Decl                  (..)
    , Expr                  (..)
    , Number                (..)

    -- * Operators definitions
    , ArithOperator         (..)
    , CondOperator          (..)
    , BinaryOperator        (..)
) where

-- | Defines @'Identifier'@ type as a string representing a variable (bound to an Atom).
type Identifier = String

-- | Defines @'Decl'@ as any declaration that can be evaluated using either @\`define\`@ or @\`lambda\`@ keywords.
data Decl
    = Define Identifier Expr
    -- ^ @'Identifier'@ definition, expressed in lisp code like @\`(define foo bar)\`@.
    | Func [Identifier] Expr
    -- ^ Function definition, expressed in lisp code like @\`(define (add a b) (+ a b))\`@.
    | Lambda [Identifier] Expr
    -- ^ Lambda definition, expressed in lisp code like @\`(define add (lambda (a b) (+ a b)))\`@.

    deriving (
        Show
        -- ^ Allows @'Decl'@ to be printed.
        , Eq
        -- ^ Allows @'Decl'@ to be compared, needed for unit tests.
    )

-- | Defines @'Expr'@ as any lisp expressions including @'Decl'@.
data Expr
    = Const Number
    -- ^ Defines constant numbers as lisp variables can not be muted.
    | If CondOperator Expr Expr
    -- ^ Defines @\`if\`@ expression with its else block.
    | Call Identifier [Expr]
    -- ^ Defines @'Identifier'@ call expression.
    | BinaryOp BinaryOperator Expr Expr
    -- ^ Defines generic expressions.
    | Defun Decl
    -- ^ Defines @'Decl'@ as expression.

    deriving (
        Show
        -- ^ Allows @'Expr'@ to be printed.
        , Eq
        -- ^ Allows @'Expr'@ to be compared, needed for unit tests.
    )

-- | Defines @'Number'@ as any base value which can be mapped to an @'Identifier'@.
data Number
    = Boolean Bool
    -- ^ Defines @'Boolean'@, expressed in lisp as either @\`#f\`@ or @\`#t\`@.
    | Int Integer
    -- ^ Defines @'Int'@, expressed in lisp like @\`42\`@.
    | SPrecision Float
    -- ^ Defines @'Float'@, expressed in lisp like @\`3.14\`@.
    | Identifier Identifier
    -- ^ Defines @'Identifier'@, expressed in lisp like @\`foo\`@.

    deriving (
        Show
        -- ^ Allows @'Number'@ to be printed.
        , Eq
        -- ^ Allows @'Number'@ to be compared, needed for unit tests.
    )

-- | Defines @'ArithOperator'@ as any mathematical expression.
data ArithOperator
    = OpAdd
    -- ^ Defines @'OpAdd'@, expressed in lisp like @\`+ 1 2\`@.
    | OpSub
    -- ^ Defines @'OpSub'@, expressed in lisp like @\`- 2 1\`@.
    | OpMul
    -- ^ Defines @'OpMul'@, expressed in lisp like @\`* 2 5\`@.
    | OpDiv
    -- ^ Defines @'OpDiv'@, expressed in lisp like @\`div 2 1\`@ or @\`/ 10 2\`@.
    | OpMod
    -- ^ Defines @'OpMod'@, expressed in lisp like @\`remainder 4 1\`@ or @\`% 1 10\`@.

    deriving (
        Show
        -- ^ Allows @'ArithOperator'@ to be printed.
        , Eq
        -- ^ Allows @'ArithOperator'@ to be compared, needed for unit tests.
    )

-- | Defines @'CondOperator'@ as any conditional expression.
data CondOperator
    = OpLt
    -- ^ Defines @'OpLt'@, expressed in lisp like @\`< 10 20\`@.
    | OpEq
    -- ^ Defines @'OpEq'@, expressed in lisp like @\`eq? #t #f\`@.
    | OpBool Bool
    -- ^ Defines @'OpBool'@ as a @'Boolean'@ value.
    | OpIdentifier Identifier
    -- ^ Defines @'OpIdentifier'@ as an @'Identifier'@.

    deriving (
        Show
        -- ^ Allows @'CondOperator'@ to be printed.
        , Eq
        -- ^ Allows @'CondOperator'@ to be compared, needed for unit tests.
    )

-- | Defines @'BinaryOperator'@ as either a mathematical or conditional expression.
data BinaryOperator
    = ArithExpr ArithOperator
    -- ^ Defines @'ArithOperator'@ as a mathematical expression.
    | CondExpr CondOperator
    -- ^ Defines @'ArithOperator'@ as a conditional expression.

    deriving (
        Show
        -- ^ Allows @'BinaryOperator'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOperator'@ to be compared, needed for unit tests.
    )
