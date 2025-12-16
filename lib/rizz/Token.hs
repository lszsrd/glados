{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/rizz/Token.hs
-}

module Token (
    Identifier
    , Keyword               (..)
    , BuiltinType           (..)
    , Literal               (..)
    , SBracket              (..)
    , RBracket              (..)
    , CBracket              (..)
    , UnaryOp               (..)
    , BinaryOp              (..)
    , AssignOp              (..)
    , Punctuator            (..)
    , Token                 (..)
) where

type Identifier = String

data Keyword
    = Bool                  -- Bool
    | Char                  -- Char
    | Int                   -- Int
    | Float                 -- Float
    | Double                -- Double
    | Fn                    -- fn
    | If                    -- if
    | Else                  -- else
    | While                 -- while
    | For                   -- for
    | Foreach               -- foreach
    | Ret                   -- ret
    
    deriving (
        Show
        , Eq
    )

data BuiltinType
    = Boolean
    | Character
    | Integer
    | SinglePrecision

    deriving (
        Show
        , Eq
    )

data Literal
    = BoolLiteral Bool      -- boolean constant
    | CharLiteral Char      -- character constant
    | IntLiteral Integer    -- integer constant
    | FloatLiteral Float    -- floating point constant
    
    deriving (
        Show
        , Eq
    )

data SBracket
    = OpenSBracket          -- [
    | CloseSBracket         -- ]
    
    deriving (
        Show
        , Eq
    )

data RBracket
    = OpenRBracket          -- (
    | CloseRBracket         -- )
    
    deriving (
        Show
        , Eq
    )

data CBracket
    = OpenCBracket          -- {
    | CloseCBracket         -- }

    deriving (
        Show
        , Eq
    )

data UnaryOp
    = IdentIncrement        -- ++
    | IdentDecrement        -- --
    
    deriving (
        Show
        , Eq
    )

data BinaryOp
    = Mul                   -- *
    | Add                   -- +
    | Sub                   -- -
    | Div                   -- /
    | Mod                   -- %
    | Lt                    -- <
    | Gt                    -- >
    | LEq                   -- <=
    | GEq                   -- >=
    | Eq                    -- ==
    | NEq                   -- !=
    | And                   -- &&
    | Or                    -- ||
    
    deriving (
        Show
        , Eq
    )

data AssignOp
    = MulEqual              -- *=
    | DivEqual              -- /=
    | ModEqual              -- %=
    | AddEqual              -- +=
    | SubEqual              -- -=

    deriving (
        Show
        , Eq
    )

data Punctuator
    = SBracket SBracket
    | RBracket RBracket
    | CBracket CBracket
    | Dot                   -- .
    | Arrow                 -- ->
    | UnaryOp UnaryOp
    | BinaryOp BinaryOp
    | Colon                 -- :
    | Semicolon             -- ;
    | Comma                 -- ,
    | Equal                 -- =
    | AssignOp AssignOp
    | Hashtag               -- #

    deriving (
        Show
        , Eq
    )

data Token
    = Keyword Keyword
    | Identifier Identifier
    | Literal Literal
    | Punctuator Punctuator

    deriving (
        Show
        , Eq
    )
