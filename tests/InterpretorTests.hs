{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- tests/LexerTests.hs
-}

module InterpretorTests (
      interpretorTestsSimpleSubjet
    , interpretorTestsBuiltins 
    , interpretorTestsEnv
) where

import Test.HUnit
import AbstractTree
import Interpretor
import Builtins
import EnvStoreRetrieve

interpretorTestsSimpleSubjet = TestList    [
    interpretEmptyList
    , interpretSimpleInt
    , interpretDefineVar 
    , interpretDefineNUse
    , interpretCallBuiltin
    , interpretLambda
    , interpretDefinedLambda
    , interpretBuiltins1
    , interpretBuiltins2
    , interpretBuiltins3
    , interpretSimpleIf1
    , interpretSimpleIf2
    , interpretComplexIf
                                        ]

interpretEmptyList = TestCase (assertEqual "interpret []"
    Nothing
    (interpret [] []) )
interpretSimpleInt = TestCase (assertEqual "interpret Int 2"
    (Just $ Expression (Int 2))
    (interpret [Expression (Int 2)] []) )
interpretDefineVar = TestCase (assertEqual "interpret Define foo 2"
    Nothing
    (interpret [ Define "foo" (Int 2) ] []) )
interpretDefineNUse = TestCase (assertEqual "interpret Define foo 2, foo"
    (Just $ Expression (Int 2))
    (interpret [Define "foo" (Int 2), Expression $ Var "foo"] []) )
interpretCallBuiltin = TestCase (assertEqual "interpret Call + 2 4"
    (Just $ Expression (Int 6))
    (interpret [ Expression $ Call (Var "+") [Int 2, Int 4] ] []) )
interpretLambda = TestCase (assertEqual "interpret Call (Lambda a b) 1 2"
    (Just $ Expression (Int 3))
    (interpret [ Expression (Call (Lambda ["a", "b"] (Call (Var "+")  [Var "a", Var "b"])) [Int 1, Int 2] ) ] []) )
interpretDefinedLambda = TestCase (assertEqual "interpret Define foo (lambda a b) Call foo 1 2"
    (Just $ Expression (Int 47))
    (interpret [ Define "naouel" (Lambda ["a", "b"] (Call (Var "+") [Var "a", Var "b"]) ), Expression (Call (Var "naouel") [Int 1, Int 46])  ] []) )
interpretBuiltins1 = TestCase (assertEqual "interpret Call + (Call * 2 3) (Call div 10 2)"
    (Just $ Expression (Int 11))
    (interpret [ Expression ( Call (Var "+") [ Call (Var "*") [Int 2, Int 3], Call (Var "div") [Int 10, Int 2] ]  )] []) )
interpretBuiltins2 = TestCase (assertEqual "interpret Call eq? (Call * 2 5) (Call - 11 1)"
    (Just $ Expression (Boolean True))
    (interpret [ Expression ( Call (Var "eq?") [ Call (Var "*") [Int 2, Int 5], Call (Var "-") [Int 11, Int 1] ]  )] []) )
interpretBuiltins3 = TestCase (assertEqual "interpret Call < 1 (Call mod 10 3)"
    (Just $ Expression (Boolean False))
    (interpret [ Expression ( Call (Var "<") [ Int 1, Call (Var "mod") [Int 10, Int 3] ]  )] []) )
interpretSimpleIf1 = TestCase (assertEqual "interpret If True 1 2"
    (Just $ Expression (Int 1))
    (interpret [ Expression ( If (Boolean True) (Int 1) (Int 2)) ] []) )
interpretSimpleIf2 = TestCase (assertEqual "interpret If False 1 2"
    (Just $ Expression (Int 2))
    (interpret [ Expression ( If (Boolean False) (Int 1) (Int 2)) ] []) )
interpretComplexIf = TestCase (assertEqual "interpret Define foo 42, If < foo 10 then (* foo 3) else (div foo 2)"
    (Just $ Expression (Int 21))
    (interpret [ Define "foo" (Int 42), Expression $ If (Call (Var "<") [Var "foo", Int 10]) (Call (Var "*") [Var "foo", Int 3]) (Call (Var "div") [Var "foo", Int 2]) ] []) )

{-
(define (fact x)
(if (eq? x 1)
1
(* x (fact (- x 1)))))
(fact 10)               -- Cannot implement with current parser 

-}

interpretorTestsBuiltins = TestList    [
      interpretDivisionNull
    , interpretModuloNull
    , interpretWrongArgsMinus
    , interpretWrongArgsDiv
    , interpretWrongArgsMod
    , interpretWrongArgsInf
    , interpretWrongArgsEq
    , interpretWrongArgsType
    , interpretMinusInt
                                        ]


interpretDivisionNull = TestCase (assertEqual "interpret div 42 0"
    Nothing
    (interpret [ Expression $ Call (Var "div") [Int 42, Int 0] ] []) )
interpretModuloNull = TestCase (assertEqual "interpret mod 42 0"
    Nothing
    (interpret [ Expression $ Call (Var "mod") [Int 42, Int 0] ] []) )
interpretWrongArgsMinus = TestCase (assertEqual "interpret -"
    Nothing
    (interpret [ Expression $ Call (Var "-") [] ] []) )
interpretWrongArgsDiv = TestCase (assertEqual "interpret div 1"
    Nothing
    (interpret [ Expression $ Call (Var "div") [Int 1] ] []) )
interpretWrongArgsMod = TestCase (assertEqual "interpret mod 1"
    Nothing
    (interpret [ Expression $ Call (Var "mod") [Int 1] ] []) )
interpretWrongArgsInf = TestCase (assertEqual "interpret < 1"
    Nothing
    (interpret [ Expression $ Call (Var "<") [Int 1] ] []) )
interpretWrongArgsEq = TestCase (assertEqual "interpret eq 1"
    Nothing
    (interpret [ Expression $ Call (Var "eq?") [Int 1] ] []) )
interpretWrongArgsType = TestCase (assertEqual "interpret div 42 True"
    Nothing
    (interpret [ Expression $ Call (Var "div") [Int 42, Boolean True] ] []) )
interpretMinusInt = TestCase (assertEqual "interpret - 1"
    (Just $ Expression (Int (-1)))
    (interpret [ Expression $ Call (Var "-") [Int 1] ] []) )
interpretNothing = TestCase (assertEqual "applyBuiltin feur (SHOULD NOT EVEN HAPPEN)"
    Nothing
    (applyBuiltin "feur" []))


interpretorTestsEnv = TestList    [
    checkCallTokenWrong
                                        ]


checkCallTokenWrong = TestCase (assertEqual "checkCallToken [] () []"
    Nothing
    (checkCallToken [] (If (Var "urmom") (Call (Var "thepolice") []) (Int 17) ) []) )
-- interpretBoolDefined = TestCase (assertEqual "checkCallToken [Var feur Bool] (Call feur) []"
    -- (Just $ Expression (Boolean True))
    -- (checkCallToken [] ) )
