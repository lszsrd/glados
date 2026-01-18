{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/types/OpCodes.hs
-}

module OpCodes (
    Identifier
    , Label

    , Operand               (..)
    , Instruction           (..)
    
    , OpCodes.div
    , OpCodes.mod
    , showList'
) where

type Identifier = String
type Label = String

instance Show Operand where
    show (Bool False) = "False"
    show (Bool True) = "True"
    show (Char x) = [x]
    show (Integer x) = show x
    show (Float x) = show x
    show (List x) = showList' x
    show (Struct x y z) = x ++ ": " ++ show (zip y z)

instance Num Operand where
    Bool True - Bool False = Bool True
    Bool True - Bool True = Bool False
    Bool _ - Bool _ = Bool False
    Char x - Bool y = Integer (toInteger (fromEnum x - fromEnum y))
    Bool x - Char y = Integer (toInteger (fromEnum x - fromEnum y))
    Integer x - Bool y = Integer (x - toInteger (fromEnum y))
    Bool x - Integer y = Integer (toInteger (fromEnum x) - y)
    Float x - Bool y = Float (x - fromIntegral (fromEnum y))
    Bool x - Float y = Float (fromIntegral (fromEnum x) - y)
    Char x - Char y = Integer (toInteger (fromEnum x - fromEnum y))
    Integer x - Char y = Integer (toInteger x - toInteger (fromEnum y))
    Char x - Integer y = Integer (toInteger (fromEnum x) - toInteger y)
    Char x - Float y = Float (fromIntegral (fromEnum x) - y)
    Float x - Char y = Float (x - fromIntegral (fromEnum y))
    Integer x - Integer y = Integer (x - y)
    Float x - Integer y = Float (x - fromIntegral y)
    Integer x - Float y = Float (fromIntegral x - y)
    Float x - Float y = Float (x - y)
    _ - _ = Integer 0
    Bool x * Bool y = Bool (toEnum (fromEnum x * fromEnum y))
    Char x * Bool y = Integer (toInteger (fromEnum x * fromEnum y))
    Bool x * Char y = Integer (toInteger (fromEnum x * fromEnum y))
    Integer x * Bool y = Integer (x * toInteger (fromEnum y))
    Bool x * Integer y = Integer (toInteger (fromEnum x) * y)
    Float x * Bool y = Float (x * fromIntegral (fromEnum y))
    Bool x * Float y = Float (fromIntegral (fromEnum x) * y)
    Char x * Char y = Integer (toInteger (fromEnum x * fromEnum y))
    Integer x * Char y = Integer (toInteger x * toInteger (fromEnum y))
    Char x * Integer y = Integer (toInteger (fromEnum x) * toInteger y)
    Char x * Float y = Float (fromIntegral (fromEnum x) * y)
    Float x * Char y = Float (x * fromIntegral (fromEnum y))
    Integer x * Integer y = Integer (x * y)
    Float x * Integer y = Float (x * fromIntegral y)
    Integer x * Float y = Float (fromIntegral x * y)
    Float x * Float y = Float (x * y)
    _ * _ = Integer 0
    Bool True + Bool True = Bool True
    Bool _ + Bool _ = Bool False
    Char x + Bool y = Integer (toInteger (fromEnum x + fromEnum y))
    Bool x + Char y = Integer (toInteger (fromEnum x + fromEnum y))
    Integer x + Bool y = Integer (x + toInteger (fromEnum y))
    Bool x + Integer y = Integer (toInteger (fromEnum x) + y)
    Float x + Bool y = Float (x + fromIntegral (fromEnum y))
    Bool x + Float y = Float (fromIntegral (fromEnum x) + y)
    Char x + Char y = Integer (toInteger (fromEnum x + fromEnum y))
    Integer x + Char y = Integer (toInteger x + toInteger (fromEnum y))
    Char x + Integer y = Integer (toInteger (fromEnum x) + toInteger y)
    Char x + Float y = Float (fromIntegral (fromEnum x) + y)
    Float x + Char y = Float (x + fromIntegral (fromEnum y))
    Integer x + Integer y = Integer (x + y)
    Float x + Integer y = Float (x + fromIntegral y)
    Integer x + Float y = Float (fromIntegral x + y)
    Float x + Float y = Float (x + y)
    List x + List y = List (x ++ y)
    _ + _ = Integer 0
    abs x = if x < 0 then x * (-1) else x
    signum x
        | x < 0 = -1
        | x > 0 = 1
        | otherwise = 0
    fromInteger n = let a = fromInteger n in Integer a

instance Eq Operand where
    Bool x == Bool y = x == y
    Char x == Bool y = fromEnum x == fromEnum y
    Bool True == Char x = fromEnum x /= 0
    Bool x == Char y = fromEnum x == fromEnum y
    Integer x == Bool y = x == toInteger (fromEnum y)
    Bool True == Integer x = x /= 0
    Bool x == Integer y = toInteger (fromEnum x) == y
    Float x == Bool y = x == fromIntegral (fromEnum y)
    Bool True == Float x = x /= 0
    Bool x == Float y = fromIntegral (fromEnum x) == y
    Char x == Char y = fromEnum x == fromEnum y
    Integer x == Char y = toInteger x == toInteger (fromEnum y)
    Char x == Integer y = toInteger (fromEnum x) == toInteger y
    Char x == Float y = fromIntegral (fromEnum x) == y
    Float x == Char y = x == fromIntegral (fromEnum y)
    Integer x == Integer y = x == y
    Float x == Integer y = x == fromIntegral y
    Integer x == Float y = fromIntegral x == y
    Float x == Float y = x == y
    List x == List y = x == y
    Struct x _ y == Struct x' _ y' = x == x' && y == y'
    _ == _ = False

instance Ord Operand where
    Bool x <= Bool y = x <= y
    Char x <= Bool y = fromEnum x <= fromEnum y
    Bool x <= Char y = fromEnum x <= fromEnum y
    Integer x <= Bool y = x <= toInteger (fromEnum y)
    Bool x <= Integer y = toInteger (fromEnum x) <= y
    Float x <= Bool y = x <= fromIntegral (fromEnum y)
    Bool x <= Float y = fromIntegral (fromEnum x) <= y
    Char x <= Char y = fromEnum x <= fromEnum y
    Integer x <= Char y = toInteger x <= toInteger (fromEnum y)
    Char x <= Integer y = toInteger (fromEnum x) <= toInteger y
    Char x <= Float y = fromIntegral (fromEnum x) <= y
    Float x <= Char y = x <= fromIntegral (fromEnum y)
    Integer x <= Integer y = x <= y
    Float x <= Integer y = x <= fromIntegral y
    Integer x <= Float y = fromIntegral x <= y
    Float x <= Float y = x <= y
    _ <= _ = False
    Bool x >= Bool y = x >= y
    Char x >= Bool y = fromEnum x >= fromEnum y
    Bool x >= Char y = fromEnum x >= fromEnum y
    Integer x >= Bool y = x >= toInteger (fromEnum y)
    Bool x >= Integer y = toInteger (fromEnum x) >= y
    Float x >= Bool y = x >= fromIntegral (fromEnum y)
    Bool x >= Float y = fromIntegral (fromEnum x) >= y
    Char x >= Char y = fromEnum x >= fromEnum y
    Integer x >= Char y = toInteger x >= toInteger (fromEnum y)
    Char x >= Integer y = toInteger (fromEnum x) >= toInteger y
    Char x >= Float y = fromIntegral (fromEnum x) >= y
    Float x >= Char y = x >= fromIntegral (fromEnum y)
    Integer x >= Integer y = x >= y
    Float x >= Integer y = x >= fromIntegral y
    Integer x >= Float y = fromIntegral x >= y
    Float x >= Float y = x >= y
    _ >= _ = False
    Bool x < Bool y = x < y
    Char x < Bool y = fromEnum x < fromEnum y
    Bool x < Char y = fromEnum x < fromEnum y
    Integer x < Bool y = x < toInteger (fromEnum y)
    Bool x < Integer y = toInteger (fromEnum x) < y
    Float x < Bool y = x < fromIntegral (fromEnum y)
    Bool x < Float y = fromIntegral (fromEnum x) < y
    Char x < Char y = fromEnum x < fromEnum y
    Integer x < Char y = toInteger x < toInteger (fromEnum y)
    Char x < Integer y = toInteger (fromEnum x) < toInteger y
    Char x < Float y = fromIntegral (fromEnum x) < y
    Float x < Char y = x < fromIntegral (fromEnum y)
    Integer x < Integer y = x < y
    Float x < Integer y = x < fromIntegral y
    Integer x < Float y = fromIntegral x < y
    Float x < Float y = x < y
    _ < _ = False

data Operand
    = Bool Bool
    | Char Char
    | Integer Integer
    | Float Float
    | List [Operand]
    | Struct Identifier [Identifier] [Operand]

data Instruction
    = Nop
    | Call Identifier Int
    | Load Identifier
    | Store Identifier
    | Push Operand
    | PushList Int
    | Pop
    | Jump Label
    | JumpFalse Label
    | JumpTrue Label
    | Label Identifier
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Lt
    | Gt
    | LEq
    | GEq
    | Eq
    | NEq
    | And
    | Or
    | Ret

    deriving (
        Show
        , Eq
    )

div :: Operand -> Operand -> Either String Float
div x (Bool _) = case x + Float 0.0 of
    Float y -> Right y
    _ -> Right 0
div (Char x) (Char y)
    = Right (fromIntegral (fromEnum x) / fromIntegral (fromEnum y))
div (Char x) (Integer y) = Right (fromIntegral (fromEnum x) / fromIntegral y)
div (Integer y) (Char x) = Right (fromIntegral (fromEnum x) / fromIntegral y)
div (Char x) (Float y) = Right (fromIntegral (fromEnum x) / y)
div (Float y) (Char x) = Right (fromIntegral (fromEnum x) / y)
div (Integer x) (Integer y) = Right (fromIntegral x / fromIntegral y)
div (Integer x) (Float y) = Right (fromIntegral x / y)
div (Float x) (Integer y) = Right (x / fromIntegral y)
div (Float x) (Float y) = Right (x / y)
div _ _ = Left "DIV: non-numeric operands"

mod :: Operand -> Operand -> Either String Integer
mod (Char x) (Char y) = Right (fromIntegral (fromEnum x) `Prelude.mod` fromIntegral (fromEnum y))
mod (Char x) (Integer y) = Right (fromIntegral (fromEnum x) `Prelude.mod` fromIntegral y)
mod (Integer y) (Char x) = Right (fromIntegral (fromEnum x) `Prelude.mod` fromIntegral y)
mod (Char x) (Float y) = Right (fromIntegral (fromEnum x) `Prelude.mod` round y)
mod (Float y) (Char x) = Right (fromIntegral (fromEnum x) `Prelude.mod` round y)
mod (Integer x) (Integer y) = Right (fromIntegral x `Prelude.mod` fromIntegral y)
mod (Integer x) (Float y) = Right (fromIntegral x `Prelude.mod` round y)
mod (Float x) (Integer y) = Right (round x `Prelude.mod` y)
mod (Float x) (Float y) = Right (round x `Prelude.mod` round y)
mod _ _ = Left "MOD: non-numeric operands"

showList' :: [Operand] -> String
showList' [] = []
showList' [x] = show x
showList' ((Char x): xs) = x: showList' xs
showList' (x: xs) = show x ++ showList' xs
