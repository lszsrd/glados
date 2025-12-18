{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/rizz/Token.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Token
-- Description : Defines Tokens used to represent the rizz language syntax.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- Sets different categories in which each token belong. The tokens are based
-- on the C programming language. For further informations, refers to the
-- [Microsoft lexical grammar definition]
-- (https://learn.microsoft.com/en-us/cpp/c-language/lexical-grammar) from which
-- this definition is based.
-------------------------------------------------------------------------------
module Token (
    -- * Basic type
    Stream
    , Lexeme
    , Identifier

    -- * Tokens kind
    , Keyword               (..)
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

-- | Defines @'Stream'@ type as a string which represents a finite byte array.
type Stream = String

-- | Defines @'Lexeme'@ type as a string representing a way to identify a
-- specific @'Token'@.
type Lexeme = String

-- | Defines @'Identifier'@ type as a string which represents a variable.
type Identifier = String

-- | Defines @'Keyword'@ which are reserved words used by the language.
data Keyword
    = Bool                  -- Bool
    -- ^ boolean type keyword, expressed in rizz code as @\`Bool\`@.
    | Char                  -- Char
    -- ^ character type keyword, expressed in rizz code as @\`Char\`@.
    | Int                   -- Int
    -- ^ integer type keyword, expressed in rizz code as @\`Int\`@.
    | Float                 -- Float
    -- ^ float type keyword, expressed in rizz code as @\`Float\`@.
    | Double                -- Double
    -- ^ double type keyword, expressed in rizz code as @\`Double\`@.
    | Fn                    -- fn
    -- ^ function declaration keyword, expressed in rizz code as @\`fn\`@.
    | If                    -- if
    -- ^ if keyword, expressed in rizz code as @\`if\`@.
    | Else                  -- else
    -- ^ else keyword, expressed in rizz code as @\`else\`@.
    | While                 -- while
    -- ^ while loop type keyword, expressed in rizz code as @\`while\`@.
    | For                   -- for
    -- ^ for loop keyword, expressed in rizz code as @\`for\`@.
    | Foreach               -- foreach
    -- ^ foreach loop keyword, expressed in rizz code as @\`foreach\`@.
    | Ret                   -- ret
    -- ^ return (from function) keyword, expressed in rizz code as @\`ret\`@.
    
    deriving (
        Show
        -- ^ Allows @'Keyword'@ to be printed.
        , Eq
        -- ^ Allows @'Keyword'@ to be compared, needed for unit tests.
    )

-- | Defines @'Literal`@ which is a way to represent a constant value not
-- bound to a variable.
data Literal
    = BoolLiteral Bool      -- boolean constant
    -- ^ boolean literal, expressed in rizz code as @\`True\`@ or @\`False\`@.
    | CharLiteral Char      -- character constant
    -- ^ character literal, expressed in rizz code like @\`\'x\'\`@.
    | IntLiteral Integer    -- integer constant
    -- ^ integer literal, expressed in rizz code like @\`42\`@.
    | FloatLiteral Float    -- floating point constant
    -- ^ single floating point literal, expressed in rizz code like @\`3.14\`@.
    
    deriving (
        Show
        -- ^ Allows @'Literal'@ to be printed.
        , Eq
        -- ^ Allows @'Literal'@ to be compared, needed for unit tests.
    )

-- | Defines @'SBracket`@ which represents both opening and closing square
-- brackets \`[]\`.
data SBracket
    = OpenSBracket          -- [
    -- ^ opening square bracket, expressed in rizz code as @\`[\`@.
    | CloseSBracket         -- ]
    -- ^ closing square bracket, expressed in rizz code as @\`]\`@.
    
    deriving (
        Show
        -- ^ Allows @'SBracket'@ to be printed.
        , Eq
        -- ^ Allows @'SBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'RBracket`@ which represents both opening and closing round
-- brackets \`()\`.
data RBracket
    = OpenRBracket          -- (
    -- ^ opening round bracket, expressed in rizz code as @\`(\`@.
    | CloseRBracket         -- )
    -- ^ closing round bracket, expressed in rizz code as @\`)\`@.
    
    deriving (
        Show
        -- ^ Allows @'RBracket'@ to be printed.
        , Eq
        -- ^ Allows @'RBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'RBracket`@ which represents both opening and closing curly
-- brackets \`{}\`.
data CBracket
    = OpenCBracket          -- {
    -- ^ opening curly bracket, expressed in rizz code as @\`{\`@.
    | CloseCBracket         -- }
    -- ^ closing curly bracket, expressed in rizz code as @\`}\`@.

    deriving (
        Show
        -- ^ Allows @'CBracket'@ to be printed.
        , Eq
        -- ^ Allows @'CBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'UnaryOp`@ which represents both increment and decrement
-- unary operators.
data UnaryOp
    = IdentIncrement        -- ++
    -- ^ unary increment operator, expressed in rizz code as @\`++\`@.
    | IdentDecrement        -- --
    -- ^ unary decrement operator, expressed in rizz code as @\`--\`@.
    
    deriving (
        Show
        -- ^ Allows @'UnaryOp'@ to be printed.
        , Eq
        -- ^ Allows @'UnaryOp'@ to be compared, needed for unit tests.
    )

-- | Defines @'BinaryOp`@ which represents aritmethic and equality operators.
data BinaryOp
    = Mul                   -- *
    -- ^ multiplication operator, expressed in rizz code as @\`\*`@.
    | Add                   -- +
    -- ^ addition operator, expressed in rizz code as @\`+\`@.
    | Sub                   -- -
    -- ^ substraction operator, expressed in rizz code as @\`-\`@.
    | Div                   -- /
    -- ^ division operator, expressed in rizz code as @\`/\`@.
    | Mod                   -- %
    -- ^ modulo operator, expressed in rizz code as @\`%\`@.
    | Lt                    -- <
    -- ^ less than operator, expressed in rizz code as @\`<\`@.
    | Gt                    -- >
    -- ^ greater than operator, expressed in rizz code as @\`>\`@.
    | LEq                   -- <=
    -- ^ less than or equal to operator, expressed in rizz code as @\`<=\`@.
    | GEq                   -- >=
    -- ^ greater than or equal to operator, expressed in rizz code as @\`>=\`@.
    | Eq                    -- ==
    -- ^ equal operator, expressed in rizz code as @\`==\`@.
    | NEq                   -- !=
    -- ^ not equal operator, expressed in rizz code as @\`!=\`@.
    | And                   -- &&
    -- ^ and operator, expressed in rizz code as @\`&&\`@.
    | Or                    -- ||
    -- ^ or operator, expressed in rizz code as @\`||\`@.
    
    deriving (
        Show
        -- ^ Allows @'BinaryOp'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOp'@ to be compared, needed for unit tests.
    )

-- | Defines @'AssignOp`@ which represents assignement operators with
-- aritmethic operation on it.
data AssignOp
    = MulEqual              -- *=
    -- ^ multiplication and assignement operator, expressed in rizz code as
    -- @\`*=\`@.
    | DivEqual              -- /=
    -- ^ division and assignement operator, expressed in rizz code as @\`/=\`@.
    | ModEqual              -- %=
    -- ^ modulo and assignement operator, expressed in rizz code as @\`%=\`@.
    | AddEqual              -- +=
    -- ^ addition and assignement operator, expressed in rizz code as @\`+=\`@.
    | SubEqual              -- -=
    -- ^ substraction and assignement operator, expressed in rizz code as
    -- @\`-=\`@.

    deriving (
        Show
        -- ^ Allows @'AssignOp'@ to be printed.
        , Eq
        -- ^ Allows @'AssignOp'@ to be compared, needed for unit tests.
    )

-- | Defines @'Punctuator'@ which is a large category containing non keywords,
-- non literals and non identifiers tokens.
data Punctuator
    = SBracket SBracket
    -- ^ both square brackets, used to index lists. 
    | RBracket RBracket
    -- ^ both curly brackets, used to declare or call functions.
    | CBracket CBracket
    -- ^ both curly brackets, used to define a scope.
    | Dot                   -- .
    -- ^ standalone dot @'Punctuator'@ , used to declare floating point
    -- constants and expressed in rizz code as @\`.\`@.
    | Arrow                 -- ->
    -- ^ standalone arrow @'Punctuator'@, used to specify the return type of a
    -- function and expressed in rizz code as @\`+=\`@.
    | UnaryOp UnaryOp
    -- ^ both unary operators.
    | BinaryOp BinaryOp
    -- ^ all of binary operators.
    | Colon                 -- :
    -- ^ standalone colon @'Punctuator'@, used to specify the type of an
    -- @'Identifier'@ and expressed in rizz code as @\`:\`@.
    | Semicolon             -- ;
    -- ^ standalone semicolon @'Punctuator'@, used to specify the end of a
    -- statement and expressed in rizz code as @\`:\`@.
    | Comma                 -- ,
    -- ^ standalone comma @'Punctuator'@, used to space functions' arguments
    -- and expressed in rizz code as @\`,\`@.
    | Equal                 -- =
    -- ^ standalone equal @'Punctuator'@, used to assign a variable and
    -- expressed in rizz code as @\`,\`@.
    | AssignOp AssignOp
    -- ^ All of assignement operators.

    deriving (
        Show
        -- ^ Allows @'Punctuator'@ to be printed.
        , Eq
        -- ^ Allows @'Punctuator'@ to be compared, needed for unit tests.
    )

-- | Defines @'Token'@ which is bound by its corresponding representation in
-- the bytes @'Stream'@.
--
-- It can only be of one type and serves as a way to represent a series
-- of bytes in a more abstract way.
data Token
    = Keyword Keyword
    -- ^ @'Keyword'@ definition.
    | Identifier Identifier
    -- ^ @'Identifier'@ definition.
    | Literal Literal
    -- ^ @'Literal'@ definition.
    | Punctuator Punctuator
    -- ^ @'Punctuator'@ definition.

    deriving (
        Show
        -- ^ Allows @'Token'@ to be printed.
        , Eq
        -- ^ Allows @'Token'@ to be compared, needed for unit tests.
    )
