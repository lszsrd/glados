{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- src/vm/Parser.hs
-}

module Parser (
    parseFunctions
    , hParseFunctions
    , parseInstructions
    , hParseInstruction
) where

import Text.Read (readMaybe)

import OpCodes (Operand (..), Instruction (..))
import Function (Function)

parseFunctions :: [String] -> Either String [Function]
parseFunctions [] = Right []
parseFunctions (x: xs) = case words x of
    [] -> parseFunctions xs
    ("FUNC": y: ys) -> hParseFunctions xs y ys
    ("FUNC": _) -> Left "FUNC: missing function name"
    _ -> Left ("not a function: " ++ x)

hParseFunctions :: [String] -> String -> [String] -> Either String [Function]
hParseFunctions xs fnName args = case parseInstructions xs of
    Left e -> Left e
    Right (_, []) -> Left ("FUNC: missing ENDFUNC at EOF: " ++ fnName)
    Right (fnBody, z: z') -> if z /= "ENDFUNC"
        then Left ("FUNC: missing ENDFUNC: " ++ fnName)
        else case parseFunctions z' of
            Left e -> Left e
            Right functions -> Right $ (fnName, args, fnBody): functions

parseInstructions :: [String] -> Either String ([Instruction], [String])
parseInstructions [] = Right ([], [])
parseInstructions instructions@("ENDFUNC": _) = Right ([], instructions)
parseInstructions (x: xs) = case parseInstruction (words x) of
    Left e -> Left e
    Right opcode -> case parseInstructions xs of
        Left e -> Left e
        Right (opcodes, instructions) -> Right (opcode: opcodes, instructions)

parseInstruction :: [String] -> Either String Instruction
parseInstruction [] = Right Nop
parseInstruction ((';': _): _) = Right Nop
parseInstruction ("NOP": x) = case hParseInstruction "NOP" x 0 of
    Left e -> Left e
    Right _ -> Right Nop
parseInstruction ("CALL": x) = case hParseInstruction "CALL" x 2 of
    Left e -> Left e
    Right (y: z: _) -> case readMaybe z :: Maybe Int of
        Nothing -> Left ("CALL: invalid operand: " ++ z)
        Just operand -> Right $ Call y operand
    Right _ -> Left "CALL: the impossible happened: found a pattern hole"
parseInstruction ("LOAD": x) = case hParseInstruction "LOAD" x 1 of
    Left e -> Left e
    Right y -> Right $ Load (head y)
parseInstruction ("STORE": x) = case hParseInstruction "STORE" x 1 of
    Left e -> Left e
    Right y -> Right $ Store (head y)
parseInstruction ("PUSH_BOOL": x) = case hParseInstruction "PUSH_BOOL" x 1 of
    Left e -> Left e
    Right y -> case readMaybe (head y) :: Maybe Bool of
        Nothing -> Left ("PUSH_BOOL: invalid operand: " ++ unwords y)
        Just operand -> Right $ Push (Bool operand)
parseInstruction ("PUSH_BOOL": x) = case hParseInstruction "PUSH_BOOL" x 1 of
    Left e -> Left e
    Right y -> case readMaybe (head y) :: Maybe Bool of
        Nothing -> Left ("PUSH_BOOL: invalid operand: " ++ unwords y)
        Just operand -> Right $ Push (Bool operand)
parseInstruction ["PUSH_CHAR", "'", "'"] = Right $ Push (Char ' ')
parseInstruction ("PUSH_CHAR": x) = case hParseInstruction "PUSH_CHAR" x 1 of
    Left e -> Left (e ++ ": " ++ show x)
    Right y -> case readMaybe (head y) :: Maybe Char of
        Nothing -> Left ("PUSH_BOOL: invalid operand: " ++ unwords y)
        Just operand -> Right $ Push (Char operand)
parseInstruction ("PUSH_INT": x) = case hParseInstruction "PUSH_INT" x 1 of
    Left e -> Left e
    Right y -> case readMaybe (head y) :: Maybe Integer of
        Nothing -> Left ("PUSH_INT: invalid operand: " ++ unwords y)
        Just operand -> Right $ Push (Integer operand)
parseInstruction ("PUSH_FLOAT": x) = case hParseInstruction "PUSH_FLOAT" x 1 of
    Left e -> Left e
    Right y -> case readMaybe (head y) :: Maybe Float of
        Nothing -> Left ("PUSH_FLOAT: invalid operand: " ++ unwords y)
        Just operand -> Right $ Push (Float operand)
parseInstruction ("PUSH_LIST": x) = case hParseInstruction "PUSH_LIST" x 1 of
    Left e -> Left e
    Right y -> case readMaybe (head y) :: Maybe Int of
        Nothing -> Left ("PUSH_FLOAT: invalid operand: " ++ unwords y)
        Just operand -> Right $ PushList operand
parseInstruction ("POP": x) = case hParseInstruction "POP" x 0 of
    Left e -> Left e
    Right _ -> Right Pop
parseInstruction ("JMP": x) = case hParseInstruction "JMP" x 1 of
    Left e -> Left e
    Right y -> Right $ Jump (head y)
parseInstruction ("JMP_IF_FALSE": x) =
    case hParseInstruction "JMP_IF_FALSE" x 1 of
        Left e -> Left e
        Right y -> Right $ JumpFalse (head y)
parseInstruction ("JMP_IF_TRUE": x) = case hParseInstruction "JMP_IF_TRUE" x 1 of
    Left e -> Left e
    Right y -> Right $ JumpTrue (head y)
parseInstruction ("LABEL": x) = case hParseInstruction "LABEL" x 1 of
    Left e -> Left e
    Right y -> Right $ Label (head y)
parseInstruction ("MUL": x) = case hParseInstruction "MUL" x 0 of
    Left e -> Left e
    Right _ -> Right Mul
parseInstruction ("ADD": x) = case hParseInstruction "ADD" x 0 of
    Left e -> Left e
    Right _ -> Right Add
parseInstruction ("SUB": x) = case hParseInstruction "SUB" x 0 of
    Left e -> Left e
    Right _ -> Right Sub
parseInstruction ("DIV": x) = case hParseInstruction "DIV" x 0 of
    Left e -> Left e
    Right _ -> Right Div
parseInstruction ("MOD": x) = case hParseInstruction "MOD" x 0 of
    Left e -> Left e
    Right _ -> Right Mod
parseInstruction ("LT": x) = case hParseInstruction "LT" x 0 of
    Left e -> Left e
    Right _ -> Right Lt
parseInstruction ("GT": x) = case hParseInstruction "GT" x 0 of
    Left e -> Left e
    Right _ -> Right Gt
parseInstruction ("LE": x) = case hParseInstruction "LE" x 0 of
    Left e -> Left e
    Right _ -> Right LEq
parseInstruction ("GE": x) = case hParseInstruction "GE" x 0 of
    Left e -> Left e
    Right _ -> Right GEq
parseInstruction ("EQ": x) = case hParseInstruction "EQ" x 0 of
    Left e -> Left e
    Right _ -> Right Eq
parseInstruction ("NEQ": x) = case hParseInstruction "NEQ" x 0 of
    Left e -> Left e
    Right _ -> Right NEq
parseInstruction ("AND": x) = case hParseInstruction "AND" x 0 of
    Left e -> Left e
    Right _ -> Right And
parseInstruction ("OR": x) = case hParseInstruction "OR" x 0 of
    Left e -> Left e
    Right _ -> Right Or
parseInstruction ("RET": x) = case hParseInstruction "RET" x 0 of
    Left e -> Left e
    Right _ -> Right Ret
parseInstruction (x: _) = Left ("unknown opcode: " ++ x)

hParseInstruction :: String -> [String] -> Int -> Either String [String]
hParseInstruction x y z
    | length y < z = Left (x ++ ": not enough operands")
    | length y > z = Left (x ++ ": too many operands")
    | otherwise = Right y
