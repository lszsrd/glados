{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/types/Token.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Token
-- Description : Defines Tokens used to represent the rizz language syntax.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- Sets different categories to which each token belongs.
-- The tokens are based on the C programming language. For further information, refer to the
-- [Microsoft C lexical grammar definition](https://learn.microsoft.com/en-us/cpp/c-language/lexical-grammar) from which this definition is based.
-------------------------------------------------------------------------------
module Token (
    -- * Basic type
    Stream
    , Lexeme
    , Identifier

    -- * Tokens kind
    , Token                 (..)
    , Keyword               (..)
    , Literal               (..)
    , SBracket              (..)
    , RBracket              (..)
    , CBracket              (..)
    , UnaryOp               (..)
    , BinaryOp              (..)
    , AssignOp              (..)
    , Punctuator            (..)
) where

-- | Defines @'Stream'@ type as a string representing a finite byte array.
type Stream = String

-- | Defines @'Lexeme'@ type as a string representing a way to identify a specific @'Token'@.
type Lexeme = String

-- | Defines @'Identifier'@ type as a string representing a variable.
type Identifier = String

-- | Defines @'Token'@ which is bound by its corresponding representation in
-- the bytes @'Stream'@.
--
-- It can only be of one type and serves as a way to represent a series of bytes in a more abstract way.
data Token
    = Keyword Keyword
    -- ^ keyword definition.
    | Identifier Identifier
    -- ^ identifier definition.
    | Literal Literal
    -- ^ literal definition.
    | Punctuator Punctuator
    -- ^ punctuator definition.

    deriving (
        Show
        -- ^ Allows @'Token'@ to be printed.
        , Eq
        -- ^ Allows @'Token'@ to be compared, needed for unit tests.
    )

-- | Defines @'Keyword'@ which are reserved words used by the language.
data Keyword
    = Bool
    -- ^ boolean type keyword, expressed in rizz code as @\`Bool\`@.
    | Char
    -- ^ character type keyword, expressed in rizz code as @\`Char\`@.
    | Int
    -- ^ integer type keyword, expressed in rizz code as @\`Int\`@.
    | Float
    -- ^ float type keyword, expressed in rizz code as @\`Float\`@.
    | Double
    -- ^ double type keyword, expressed in rizz code as @\`Double\`@.
    | Fn
    -- ^ function declaration keyword, expressed in rizz code as @\`fn\`@.
    | If
    -- ^ if keyword, expressed in rizz code as @\`if\`@.
    | Else
    -- ^ else keyword, expressed in rizz code as @\`else\`@.
    | While
    -- ^ while loop type keyword, expressed in rizz code as @\`while\`@.
    | For
    -- ^ for loop keyword, expressed in rizz code as @\`for\`@.
    | Foreach
    -- ^ foreach loop keyword, expressed in rizz code as @\`foreach\`@.
    | Ret
    -- ^ return (from function) keyword, expressed in rizz code as @\`ret\`@.
    
    deriving (
        Show
        -- ^ Allows Keywords to be printed.
        , Eq
        -- ^ Allows Keywords to be compared, needed for unit tests.
    )

-- | Defines @'Literal`@ which is a way to represent a constant value not bound to a variable.
data Literal
    = BoolLiteral Bool
    -- ^ boolean literal, expressed in rizz code as @\`True\`@ or @\`False\`@.
    | CharLiteral Char
    -- ^ character literal, expressed in rizz code like @\`\'x\'\`@.
    | IntLiteral Integer
    -- ^ integer literal, expressed in rizz code like @\`42\`@.
    | FloatLiteral Float
    -- ^ single floating point literal, expressed in rizz code like @\`3.14\`@.
    
    deriving (
        Show
        -- ^ Allows Literals to be printed.
        , Eq
        -- ^ Allows Literals to be compared, needed for unit tests.
    )

-- | Defines @'SBracket`@ representing both opening and closing square brackets \`[]\`.
data SBracket
    = OpenSBracket
    -- ^ opening square bracket, expressed in rizz code as @\`[\`@.
    | CloseSBracket
    -- ^ closing square bracket, expressed in rizz code as @\`]\`@.
    
    deriving (
        Show
        -- ^ Allows @'SBracket'@ to be printed.
        , Eq
        -- ^ Allows @'SBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'RBracket`@ representing both opening and closing round brackets \`()\`.
data RBracket
    = OpenRBracket
    -- ^ opening round bracket, expressed in rizz code as @\`(\`@.
    | CloseRBracket
    -- ^ closing round bracket, expressed in rizz code as @\`)\`@.
    
    deriving (
        Show
        -- ^ Allows @'RBracket'@ to be printed.
        , Eq
        -- ^ Allows @'RBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'RBracket`@ representing both opening and closing curly brackets \`{}\`.
data CBracket
    = OpenCBracket
    -- ^ opening curly bracket, expressed in rizz code as @\`{\`@.
    | CloseCBracket
    -- ^ closing curly bracket, expressed in rizz code as @\`}\`@.

    deriving (
        Show
        -- ^ Allows @'CBracket'@ to be printed.
        , Eq
        -- ^ Allows @'CBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'UnaryOp`@ representing both increment and decrement unary operators.
data UnaryOp
    = IdentIncrement
    -- ^ unary increment operator, expressed in rizz code as @\`++\`@.
    | IdentDecrement
    -- ^ unary decrement operator, expressed in rizz code as @\`--\`@.
    
    deriving (
        Show
        -- ^ Allows @'UnaryOp'@ to be printed.
        , Eq
        -- ^ Allows @'UnaryOp'@ to be compared, needed for unit tests.
    )

-- | Defines @'BinaryOp`@ representing arithmetic and equality operators.
data BinaryOp
    = Mul
    -- ^ multiplication operator, expressed in rizz code as @\`\*`@.
    | Add
    -- ^ addition operator, expressed in rizz code as @\`+\`@.
    | Sub
    -- ^ subtraction operator, expressed in rizz code as @\`-\`@.
    | Div
    -- ^ division operator, expressed in rizz code as @\`/\`@.
    | Mod
    -- ^ modulo operator, expressed in rizz code as @\`%\`@.
    | Lt
    -- ^ less than operator, expressed in rizz code as @\`<\`@.
    | Gt
    -- ^ greater than operator, expressed in rizz code as @\`>\`@.
    | LEq
    -- ^ less than or equal to operator, expressed in rizz code as @\`<=\`@.
    | GEq
    -- ^ greater than or equal to operator, expressed in rizz code as @\`>=\`@.
    | Eq
    -- ^ equal operator, expressed in rizz code as @\`==\`@.
    | NEq
    -- ^ not equal operator, expressed in rizz code as @\`!=\`@.
    | And
    -- ^ and operator, expressed in rizz code as @\`&&\`@.
    | Or
    -- ^ or operator, expressed in rizz code as @\`||\`@.
    
    deriving (
        Show
        -- ^ Allows @'BinaryOp'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOp'@ to be compared, needed for unit tests.
    )

-- | Defines @'AssignOp`@ representing assignment operators with arithmetic operation on it.
data AssignOp
    = MulEqual
    -- ^ multiplication and assignment operator, expressed in rizz code as @\`*=\`@.
    | DivEqual
    -- ^ division and assignment operator, expressed in rizz code as @\`/=\`@.
    | ModEqual
    -- ^ modulo and assignment operator, expressed in rizz code as @\`%=\`@.
    | AddEqual
    -- ^ addition and assignment operator, expressed in rizz code as @\`+=\`@.
    | SubEqual
    -- ^ subtraction and assignment operator, expressed in rizz code as @\`-=\`@.

    deriving (
        Show
        -- ^ Allows @'AssignOp'@ to be printed.
        , Eq
        -- ^ Allows @'AssignOp'@ to be compared, needed for unit tests.
    )

-- | Defines @'Punctuator'@ which is a large category containing non keywords, non literals and non identifiers tokens.
data Punctuator
    = SBracket SBracket
    -- ^ both square brackets, used to index lists. 
    | RBracket RBracket
    -- ^ both curly brackets, used to declare or call functions.
    | CBracket CBracket
    -- ^ both curly brackets, used to define a scope.
    | Dot
    -- ^ standalone dot @'Punctuator'@ , used to declare floating point constants and expressed in rizz code as @\`.\`@.
    | Arrow
    -- ^ standalone arrow @'Punctuator'@, used to specify the return type of a function and expressed in rizz code as @\`+=\`@.
    | UnaryOp UnaryOp
    -- ^ both unary operators.
    | BinaryOp BinaryOp
    -- ^ all of binary operators.
    | Colon
    -- ^ standalone colon @'Punctuator'@, used to specify the type of an @'Identifier'@ and expressed in rizz code as @\`:\`@.
    | Semicolon
    -- ^ standalone semicolon @'Punctuator'@, used to specify the end of a statement and expressed in rizz code as @\`:\`@.
    | Comma
    -- ^ standalone comma @'Punctuator'@, used to space functions' arguments and expressed in rizz code as @\`,\`@.
    | Equal
    -- ^ standalone equal @'Punctuator'@, used to assign a variable and expressed in rizz code as @\`,\`@.
    | AssignOp AssignOp
    -- ^ All of assignment operators.

    deriving (
        Show
        -- ^ Allows Punctuators to be printed.
        , Eq
        -- ^ Allows Punctuators to be compared, needed for unit tests.
    )
