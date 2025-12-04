{- EPITECH PROJECT, 2025
   GENERIC LANGUAGE AND DATA OPERAND SYNTAX
   File description:
   src/Interpretor.hs
-}

module UserDefined (
      applyUser
) where

import AbstractTree
import Debug.Trace

applyUser :: Maybe Expr -> [Expr] -> Maybe Expr
applyUser e f = trace (show e ++ "\n" ++ show f) Nothing
