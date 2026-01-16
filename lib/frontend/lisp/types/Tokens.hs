{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/lisp/types/Tokens.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Tokens
-- Description : Defines Tokens used to represent the lisp language syntax.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- Sets different categories to which each token belongs.
-------------------------------------------------------------------------------

module Tokens (
    Stream
    , Tokens

    , Token                 (..)
    , RBracket              (..)
    , Atom                  (..)
    , Operator              (..)
) where

-- $bnf
-- For the full lisp lexical syntax definition, see the [BNF definition](https://github.com/lszsrd/glados/blob/main/docs/BNF/lisp-lexical.bnf) here.

-- | Defines @'Stream'@ type as a string representing a finite byte array.
type Stream = String

-- | Defines @'Lexeme'@ type as a string representing a way to identify a specific @'Token'@.
type Tokens = [(Token, (Int, Int))]

instance Show Token where
    show (RBracket Open) = "'('"
    show (RBracket Close) = "')'"
    show (Atom (Bool False)) = "'#f'"
    show (Atom (Bool True)) = "'#t'"
    show (Atom (Integer x)) = show $ show x
    show (Atom (Float x)) = show $ show x
    show (Atom (Identifier x)) = show x
    show (Atom (Operator Define)) = "'define'"
    show (Atom (Operator Lambda)) = "'lambda'"
    show (Atom (Operator Add)) = "'+'"
    show (Atom (Operator Sub)) = "'-'"
    show (Atom (Operator Mul)) = "'*'"
    show (Atom (Operator Div)) = "'div'"
    show (Atom (Operator Mod)) = "'mod'"
    show (Atom (Operator Lt)) = "'<'"
    show (Atom (Operator Eq)) = "'eq?'"
    show (Atom (Operator If)) = "'if'"

-- | Defines @'Token'@ which is bound by its corresponding representation in
-- the bytes @'Stream'@.
--
-- It can only be of one type and serves as a way to represent a series of bytes in a more abstract way.
data Token
    = RBracket RBracket
    -- ^ @'RBracket'@ definition.
    | Atom Atom
    -- ^ @'Atom'@ definition.

    deriving (
        Eq
        -- ^ Allows @'Token'@ to be compared, needed for unit tests.
    )

data RBracket
    = Open
    -- ^ open bracket token, expressed in lisp code as @\`(\`@.
    | Close
    -- ^ close bracket token, expressed in lisp code as @\`)\`@.

    deriving (
        Show
        -- ^ Allows @'RBracket'@ to be shown.
        , Eq
        -- ^ Allows @'RBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'Atom'@ that are the main component of lisp language.
data Atom
    = Bool Bool
    -- ^ Boolean atom, expressed in lisp language as @\`#t\`@ or @\`#f\`@.
    | Integer Integer
    -- ^ Integer atom, expressed in lisp language as @\`5\`@.
    | Float Float
    -- ^ Float atom, expressed in lisp language as @\`42.04\`@.
    | Identifier String
    -- ^ Identifier atom, expressed in lisp language as @\`foo\`@.
    | Operator Operator
    -- ^ Operator atom.

    deriving (
        Show
        -- ^ Allows @'RBracket'@ to be shown.
        , Eq
        -- ^ Allows @'RBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'Operator@ that are the basics functions in lisp language.
data Operator
    = Define
    -- ^ Define operator keyword, expressed in lisp language as @\`define\`@.
    | Lambda
    -- ^ Define lambda keyword, expressed in lisp language as @\`lambda\`@.
    | Add
    -- ^ Define add operator, expressed in lisp language as @\`+\`@.
    | Sub
    -- ^ Define sub operator, expressed in lisp language as @\`-\`@.
    | Mul
    -- ^ Define mul operator, expressed in lisp language as @\`*\`@.
    | Div
    -- ^ Define div operator, expressed in lisp language as @\`/\`@.
    | Mod
    -- ^ Define modulo operator, expressed in lisp language as @\`%\`@.
    | Eq
    -- ^ Define equal operator, expressed in lisp language as @\`eq?\`@.
    | Lt
    -- ^ Define less operator, expressed in lisp language as @\`<\`@.
    | If
    -- ^ Define if operator, expressed in lisp language as @\`if\`@.

    deriving (
        Show
        -- ^ Allows @'RBracket'@ to be shown.
        , Eq
        -- ^ Allows @'RBracket'@ to be compared, needed for unit tests.
    )
