{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- lib/frontend/rizz/types/Tokens.hs
-}

-------------------------------------------------------------------------------
-- |
-- Module      : Tokens
-- Description : Defines Tokens used to represent the rizz language syntax.
-- License     : MIT
-- Maintainer  : laszlo.serdet@epitech.eu
--
-- Sets different categories to which each token belongs.
-- The tokens are based on the C programming language.
--
-- For further informations, refer to the [Microsoft C lexical grammar definition](https://learn.microsoft.com/en-us/cpp/c-language/lexical-grammar) from which this definition is based.
-------------------------------------------------------------------------------
module Tokens (
    -- * BNF definition
    -- $bnf

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

-- $bnf
-- For the full rizz lexical syntax definition, see the [BNF definition](https://github.com/lszsrd/glados/blob/main/docs/BNF/rizz-lexical.md) here.

{- instance Show Token where
    show (Keyword Bool) = "Bool"
    show (Keyword Char) = "Char"
    show (Keyword Int) = "Int"
    show (Keyword Float) = "Float"
    show (Keyword Double) = "Double"
    show (Keyword Fn) = "fn"
    show (Keyword If) = "if"
    show (Keyword Else) = "else"
    show (Keyword While) = "while"
    show (Keyword For) = "for"
    show (Keyword Foreach) = "foreach"
    show (Keyword Ret) = "ret"
    show (Keyword Struct) = "struct"

    show (Identifier x) = x

    show (Literal (BoolLiteral True)) = "True"
    show (Literal (BoolLiteral False)) = "False"
    show (Literal (CharLiteral x)) = show x
    show (Literal (IntLiteral x)) = show x
    show (Literal (FloatLiteral x)) = show x
    show (Literal (ListLiteral x)) = show x

    show (Punctuator (SBracket OpenSBracket)) = "["
    show (Punctuator (SBracket CloseSBracket)) = "]"
    show (Punctuator (RBracket OpenRBracket)) = "("
    show (Punctuator (RBracket CloseRBracket)) = ")"
    show (Punctuator (CBracket OpenCBracket)) = "{"
    show (Punctuator (CBracket CloseCBracket)) = "}"
    show (Punctuator Dot) = "."
    show (Punctuator Arrow) = "->"
    show (Punctuator (UnaryOp IdentIncrement)) = "++"
    show (Punctuator (UnaryOp IdentDecrement)) = "--"
    show (Punctuator (BinaryOp Mul)) = "*"
    show (Punctuator (BinaryOp Add)) = "+"
    show (Punctuator (BinaryOp Sub)) = "-"
    show (Punctuator (BinaryOp Div)) = "/"
    show (Punctuator (BinaryOp Mod)) = "%"
    show (Punctuator (BinaryOp Lt)) = "<"
    show (Punctuator (BinaryOp Gt)) = ">"
    show (Punctuator (BinaryOp LEq)) = "<="
    show (Punctuator (BinaryOp GEq)) = ">="
    show (Punctuator (BinaryOp Eq)) = "=="
    show (Punctuator (BinaryOp NEq)) = "!="
    show (Punctuator (BinaryOp And)) = "&&"
    show (Punctuator (BinaryOp Or)) = "||"
    show (Punctuator Colon) = ":"
    show (Punctuator Semicolon) = ";"
    show (Punctuator Comma) = ","
    show (Punctuator QMark) = "?"
    show (Punctuator (AssignOp Equal)) = "="
    show (Punctuator (AssignOp MulEqual)) = "*="
    show (Punctuator (AssignOp DivEqual)) = "/="
    show (Punctuator (AssignOp ModEqual)) = "%="
    show (Punctuator (AssignOp AddEqual)) = "+="
    show (Punctuator (AssignOp SubEqual)) = "-=" -}

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
    -- ^ Keyword definition.
    | Identifier Identifier
    -- ^ Identifier definition.
    | Literal Literal
    -- ^ Literal definition.
    | Punctuator Punctuator
    -- ^ Punctuator definition.

    deriving (
        Show,
        Eq
        -- ^ Allows @'Token'@ to be compared, needed for unit tests.
    )

-- | Defines @'Keyword'@ which are reserved words used by the language.
data Keyword
    = Bool
    -- ^ Boolean type keyword, expressed in rizz code as @\`Bool\`@.
    | Char
    -- ^ Character type keyword, expressed in rizz code as @\`Char\`@.
    | Int
    -- ^ Integer type keyword, expressed in rizz code as @\`Int\`@.
    | Float
    -- ^ Float type keyword, expressed in rizz code as @\`Float\`@.
    | Double
    -- ^ Double type keyword, expressed in rizz code as @\`Double\`@.
    | Fn
    -- ^ Function declaration keyword, expressed in rizz code as @\`fn\`@.
    | If
    -- ^ If keyword, expressed in rizz code as @\`if\`@.
    | Else
    -- ^ Else keyword, expressed in rizz code as @\`else\`@.
    | While
    -- ^ While loop type keyword, expressed in rizz code as @\`while\`@.
    | For
    -- ^ For loop keyword, expressed in rizz code as @\`for\`@.
    | Foreach
    -- ^ Foreach loop keyword, expressed in rizz code as @\`foreach\`@.
    | Ret
    -- ^ Return (from function) keyword, expressed in rizz code as @\`ret\`@.
    | Struct
    -- ^ Struct keyword, expressed in rizz code as @\`struct Foo = {...}\`@.
    
    deriving (
        Show
        -- ^ Allows Keywords to be printed.
        , Eq
        -- ^ Allows Keywords to be compared, needed for unit tests.
    )

-- | Defines @'Literal`@ which is a way to represent a constant value not bound to a variable.
data Literal
    = BoolLiteral Bool
    -- ^ Boolean literal, expressed in rizz code as @\`True\`@ or @\`False\`@.
    | CharLiteral Char
    -- ^ Character literal, expressed in rizz code like @\`\'x\'\`@.
    | IntLiteral Integer
    -- ^ Integer literal, expressed in rizz code like @\`42\`@.
    | FloatLiteral Float
    -- ^ Single floating point literal, expressed in rizz code like @\`3.14\`@.
    | ListLiteral [Literal]
    -- ^ List literal, expressed in rizz code like @\`[1, 2, 3]\`@.

    deriving (
        Show
        -- ^ Allows Literals to be printed.
        , Eq
        -- ^ Allows Literals to be compared, needed for unit tests.
    )

-- | Defines @'SBracket`@ representing both opening and closing square brackets \`[]\`.
data SBracket
    = OpenSBracket
    -- ^ Opening square bracket, expressed in rizz code as @\`[\`@.
    | CloseSBracket
    -- ^ Closing square bracket, expressed in rizz code as @\`]\`@.
    
    deriving (
        Show
        -- ^ Allows @'SBracket'@ to be printed.
        , Eq
        -- ^ Allows @'SBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'RBracket`@ representing both opening and closing round brackets \`()\`.
data RBracket
    = OpenRBracket
    -- ^ Opening round bracket, expressed in rizz code as @\`(\`@.
    | CloseRBracket
    -- ^ Closing round bracket, expressed in rizz code as @\`)\`@.
    
    deriving (
        Show
        -- ^ Allows @'RBracket'@ to be printed.
        , Eq
        -- ^ Allows @'RBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'RBracket`@ representing both opening and closing curly brackets \`{}\`.
data CBracket
    = OpenCBracket
    -- ^ Opening curly bracket, expressed in rizz code as @\`{\`@.
    | CloseCBracket
    -- ^ Closing curly bracket, expressed in rizz code as @\`}\`@.

    deriving (
        Show
        -- ^ Allows @'CBracket'@ to be printed.
        , Eq
        -- ^ Allows @'CBracket'@ to be compared, needed for unit tests.
    )

-- | Defines @'UnaryOp`@ representing both increment and decrement unary operators.
data UnaryOp
    = IdentIncrement
    -- ^ Unary increment operator, expressed in rizz code as @\`++\`@.
    | IdentDecrement
    -- ^ Unary decrement operator, expressed in rizz code as @\`--\`@.
    
    deriving (
        Show
        -- ^ Allows @'UnaryOp'@ to be printed.
        , Eq
        -- ^ Allows @'UnaryOp'@ to be compared, needed for unit tests.
    )

-- | Defines @'BinaryOp`@ representing arithmetic and equality operators.
data BinaryOp
    = Mul
    -- ^ Multiplication operator, expressed in rizz code as @\`\*`@.
    | Add
    -- ^ Addition operator, expressed in rizz code as @\`+\`@.
    | Sub
    -- ^ Subtraction operator, expressed in rizz code as @\`-\`@.
    | Div
    -- ^ Division operator, expressed in rizz code as @\`/\`@.
    | Mod
    -- ^ Modulo operator, expressed in rizz code as @\`%\`@.
    | Lt
    -- ^ Less than operator, expressed in rizz code as @\`<\`@.
    | Gt
    -- ^ Greater than operator, expressed in rizz code as @\`>\`@.
    | LEq
    -- ^ Less than or equal to operator, expressed in rizz code as @\`<=\`@.
    | GEq
    -- ^ Greater than or equal to operator, expressed in rizz code as @\`>=\`@.
    | Eq
    -- ^ Equal operator, expressed in rizz code as @\`==\`@.
    | NEq
    -- ^ Not equal operator, expressed in rizz code as @\`!=\`@.
    | And
    -- ^ And operator, expressed in rizz code as @\`&&\`@.
    | Or
    -- ^ Or operator, expressed in rizz code as @\`||\`@.
    
    deriving (
        Show
        -- ^ Allows @'BinaryOp'@ to be printed.
        , Eq
        -- ^ Allows @'BinaryOp'@ to be compared, needed for unit tests.
    )

-- | Defines @'AssignOp`@ representing assignment operators with arithmetic operation on it.
data AssignOp
    = Equal
    -- ^ Assignment operator, expressed in rizz code as @\`=\`@.
    | MulEqual
    -- ^ Multiplication and assignment operator, expressed in rizz code as @\`*=\`@.
    | DivEqual
    -- ^ Division and assignment operator, expressed in rizz code as @\`/=\`@.
    | ModEqual
    -- ^ Modulo and assignment operator, expressed in rizz code as @\`%=\`@.
    | AddEqual
    -- ^ Addition and assignment operator, expressed in rizz code as @\`+=\`@.
    | SubEqual
    -- ^ Subtraction and assignment operator, expressed in rizz code as @\`-=\`@.

    deriving (
        Show
        -- ^ Allows @'AssignOp'@ to be printed.
        , Eq
        -- ^ Allows @'AssignOp'@ to be compared, needed for unit tests.
    )

-- | Defines @'Punctuator'@ which is a large category containing non keywords, non literals and non identifiers tokens.
data Punctuator
    = SBracket SBracket
    -- ^ Both square brackets, used to index lists. 
    | RBracket RBracket
    -- ^ Both curly brackets, used to declare or call functions.
    | CBracket CBracket
    -- ^ Both curly brackets, used to define a scope.
    | Dot
    -- ^ Standalone dot @'Punctuator'@ , used to declare floating point constants and expressed in rizz code as @\`.\`@.
    | Arrow
    -- ^ Standalone arrow @'Punctuator'@, used to specify the return type of a function and expressed in rizz code as @\`+=\`@.
    | UnaryOp UnaryOp
    -- ^ Both unary operators.
    | BinaryOp BinaryOp
    -- ^ all of binary operators.
    | Colon
    -- ^ Standalone colon @'Punctuator'@, used to specify the type of an @'Identifier'@ and expressed in rizz code as @\`:\`@.
    | Semicolon
    -- ^ Standalone semicolon @'Punctuator'@, used to specify the end of a statement and expressed in rizz code as @\`;\`@.
    | Comma
    -- ^ Standalone comma @'Punctuator'@, used to space functions' arguments and expressed in rizz code as @\`,\`@.
    | QMark
    -- ^ Standalone question mark @'Punctuator'@, used to mark a ternary condition and expressed in rizz code like @\`True ? {} : {}\`@.
    | AssignOp AssignOp
    -- ^ All of assignment operators.

    deriving (
        Show
        -- ^ Allows Punctuators to be printed.
        , Eq
        -- ^ Allows Punctuators to be compared, needed for unit tests.
    )
