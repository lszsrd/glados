{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- tests/LexerTests.hs
-}

module InterpretorTests (
      interpretorTestsSimpleSubjet
    , interpretorExtended
    , interpretorTestsBuiltins 
    , interpretorTestsEnv
) where

import Test.HUnit
import AbstractTree
import Interpretor
import Builtins
import EnvStoreRetrieve
import Control.Exception (try, SomeException, evaluate)

evalInterpret :: [Ast] -> [Env] -> IO (Maybe Ast) 
evalInterpret a b = do
    res <- try (evaluate $ interpret a b)
        :: IO (Either SomeException (Maybe Ast))
    case res of
        Left _ -> return Nothing
        Right content -> return content


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

interpretEmptyList = TestCase $ do
    res <- evalInterpret [] []
    assertEqual "interpret []" Nothing res
interpretSimpleInt = TestCase (assertEqual "interpret Int 2"
    (Just $ Expression (Int 2))
    (interpret [ Expression (Int 2)] []) )
interpretDefineVar = TestCase $ do
    res <- evalInterpret [ Define "foo" (Int 2) ] [] 
    assertEqual "interpret Define foo 2" Nothing res
interpretDefineNUse = TestCase (assertEqual "interpret Define foo 2, foo"
    (Just $ Expression (Int 2))
    (interpret [ Define "foo" (Int 2), Expression $ Var "foo"] []) )
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
(fact 10)               -- Cannot implement with current ast 

-}


interpretorExtended = TestList          [
      interpretDefineBoolean
    , interpretDefineBooleanNUse
    , interpretDefine2
    , interpretLambdaWBools
    , interpretLambdaNoParams
                                        ]

interpretDefineBoolean = TestCase $ do
    res <- evalInterpret [Define "foo" (Boolean True)] []
    assertEqual "interpret [Define fef (Boolean True))]" Nothing res
interpretDefineBooleanNUse = TestCase (assertEqual "interpret [Define fef (Boolean True)), Var fef]"
    (Just $ Expression (Boolean True))
    (interpret [Define "foo" (Boolean True), Expression $ Var "foo"] []) )
interpretDefine2 = TestCase $ do
    res <- evalInterpret [Define "a" (Int 14), Define "foo" (Boolean True), Expression $ Call (Var "+") [Var "a", Var "fef"]] []
    assertEqual "interpret [Define a .., Define fef .., Call +  a fef]" Nothing res
interpretLambdaWBools = TestCase (assertEqual "interpret [Call Lamba (a) (if a b c) [true]]"
    (Just (Expression (Int 2)))
    (interpret [Expression $ Call (Lambda ["a"] (If (Var "a") (Int 2) (Boolean False))) [Boolean True] ] []) )
interpretLambdaNoParams = TestCase (assertEqual "interpret [Call Lamba () (if a b c) []]"
    (Just (Expression (Boolean False)))
    (interpret [Expression $ Call (Lambda [] (If (Boolean False) (Int 2) (Boolean False))) [] ] []) )


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


interpretDivisionNull = TestCase $ do
    res <- evalInterpret [ Expression $ Call (Var "div") [Int 42, Int 0] ] []
    assertEqual "interpret div 42 0" Nothing res
interpretModuloNull = TestCase $ do
    res <- evalInterpret [ Expression $ Call (Var "mod") [Int 42, Int 0] ] []
    assertEqual "interpret mod 42 0" Nothing res
interpretWrongArgsMinus = TestCase $ do
    res <- evalInterpret [ Expression $ Call (Var "-") [] ] []
    assertEqual "interpret -" Nothing res
interpretWrongArgsDiv = TestCase $ do
    res <- evalInterpret [ Expression $ Call (Var "div") [Int 1] ] []
    assertEqual "interpret div 1" Nothing res
interpretWrongArgsMod = TestCase $ do
    res <- evalInterpret [ Expression $ Call (Var "mod") [Int 1] ] []
    assertEqual "interpret mod 1" Nothing res
interpretWrongArgsInf = TestCase $ do
    res <- evalInterpret [ Expression $ Call (Var "<") [Int 1] ] []
    assertEqual "interpret < 1" Nothing res
interpretWrongArgsEq = TestCase $ do
    res <- evalInterpret [ Expression $ Call (Var "eq?") [Int 1] ] []
    assertEqual "interpret eq 1" Nothing res
interpretWrongArgsType = TestCase $ do
    res <- evalInterpret [ Expression $ Call (Var "div") [Int 42, Boolean True] ] []
    assertEqual "interpret div 42 True" Nothing res
interpretMinusInt = TestCase (assertEqual "interpret - 1"
    (Just $ Expression (Int (-1)))
    (interpret [ Expression $ Call (Var "-") [Int 1] ] []) )
interpretNothing = TestCase (assertEqual "applyBuiltin feur (SHOULD NOT EVEN HAPPEN)"
    Nothing
    (applyBuiltin "feur" []))


interpretorTestsEnv = TestList    [
      checkCallTokenWrong
    , interpretBoolDefined
    , interpretNotDefined
                                        ]


checkCallTokenWrong = TestCase (assertEqual "checkCallToken [] () []"
    Nothing
    (checkCallToken [] (If (Var "urmom") (Call (Var "thepolice") []) (Int 17) ) []) )
interpretBoolDefined = TestCase (assertEqual "checkCallToken [Var feur Bool] (feur) []"
    (Just $ Boolean True)
    (checkCallToken [Variable "feur" (Right True)] ( Var "feur" ) [] ) )
interpretNotDefined = TestCase (assertEqual "checkCallToken [Var bozo Bool] (feur) []"
    Nothing
    (checkCallToken [Variable "bozo" (Right True)] ( Var "feur" ) [] ) )
