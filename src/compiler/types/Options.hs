{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/compiler/types/Options.hs
-}

module Options (
    Options                 (..)
) where

data Options = Options  {
    dumpToks :: Bool
    , dumpAst :: Bool
                        }

    deriving (
        Show
        , Eq
    )
