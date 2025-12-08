{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- tests/ParserTests.hs
-}

module ParserTests (
      parserTestsSimple
) where

import Test.HUnit
import AbstractTree
import Parser
import Lexer (Token(..))
import Error (ErrorT(..))

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

assertParseOk :: String -> [Token] -> [Ast] -> Assertion
assertParseOk msg toks expected =
  case parsor toks of
    Left err ->
      assertFailure (msg ++ ": expected Right, got Left " ++ show err)
    Right asts ->
      assertEqual msg expected asts

parserTestsSimple :: Test
parserTestsSimple = TestList
    [ badKeyword
    , badDefine
    , badIf
    , badLambda
    , badCall
    , validDefine
    , validIf
    , validLambda
    , validCall
    , validDefineLambda
    , badDefineNotIdentifier
    , badLambdaNoParen
    , badIfTooMany
    ]

badKeyword :: Test
badKeyword = TestCase $
    assertBool "parse bare keyword should fail"
      (isLeft $ parsor [Keyword "define"])

badDefine :: Test
badDefine = TestCase $
    assertBool "parse (define foo) should fail"
      (isLeft $
        parsor [ Delimiter "("
               , Keyword "define"
               , Identifier "foo"
               , Delimiter ")"
               ])

badIf :: Test
badIf = TestCase $
    assertBool "parse (if #t 1) should fail"
      (isLeft $
        parsor [ Delimiter "("
               , Keyword "if"
               , Lexer.Boolean True
               , Constant 1
               , Delimiter ")"
               ])

badLambda :: Test
badLambda = TestCase $
    assertBool "parse (lambda (a b) body  ; missing final ')' ) should fail"
      (isLeft $
        parsor [ Delimiter "("
               , Keyword "lambda"
               , Delimiter "("
               , Identifier "a"
               , Identifier "b"
               , Delimiter ")"
               , Identifier "a"
               -- No Delimiter ")"
               ])

badCall :: Test
badCall = TestCase $
    assertBool "parse (+ 1 2  ; missing ')' ) should fail"
      (isLeft $
        parsor [ Delimiter "("
               , Operator "+"
               , Constant 1
               , Constant 2
               -- No Delimiter ")"
               ])

validDefine :: Test
validDefine = TestCase $
    assertParseOk "parse (define foo 2)"
      [ Delimiter "("
        , Keyword "define"
        , Identifier "foo"
        , Constant 2
        , Delimiter ")"
      ]
      [Define "foo" (Int 2)]

validIf :: Test
validIf = TestCase $
    assertParseOk "parse (if #t 1 2)"
    [ Delimiter "("
      , Keyword "if"
      , Lexer.Boolean True
      , Constant 1
      , Constant 2
      , Delimiter ")"
    ]
      [Expression (If (AbstractTree.Boolean True) (Int 1) (Int 2))]

validLambda :: Test
validLambda = TestCase $
    assertParseOk "parse (lambda (a) a)"
      [ Delimiter "("
        , Keyword "lambda"
        , Delimiter "("
        , Identifier "a"
        , Delimiter ")"
        , Identifier "a"
        , Delimiter ")"
      ]
      [Expression (Lambda ["a"] (Var "a"))]

validCall :: Test
validCall = TestCase $
    assertParseOk "parse (+ 1 2)"
      [ Delimiter "("
        , Operator "+"
        , Constant 1
        , Constant 2
        , Delimiter ")"
      ]
      [Expression (Call (Var "+") [Int 1, Int 2])]

validDefineLambda :: Test
validDefineLambda = TestCase $
  assertParseOk "parse (define (naouel a b) (+ a b))"
    [ Delimiter "("
    , Keyword "define"
    , Delimiter "("
    , Identifier "naouel"
    , Identifier "a"
    , Identifier "b"
    , Delimiter ")"
    , Delimiter "("
    , Identifier "+"
    , Identifier "a"
    , Identifier "b"
    , Delimiter ")"
    , Delimiter ")"
    ]
    [ Define "naouel"
        (Lambda ["a","b"]
          (Call (Var "+") [Var "a", Var "b"]))
    ]

badDefineNotIdentifier :: Test
badDefineNotIdentifier = TestCase $
  assertBool "parse (define 42 1) should fail"
    (isLeft $
      parsor [ Delimiter "("
             , Keyword "define"
             , Constant 42
             , Constant 1
             , Delimiter ")"
             ])

badLambdaNoParen :: Test
badLambdaNoParen = TestCase $
  assertBool "parse (lambda a a) should fail"
    (isLeft $
      parsor [ Delimiter "("
             , Keyword "lambda"
             , Identifier "a"
             , Identifier "a"
             , Delimiter ")"
             ])

badIfTooMany :: Test
badIfTooMany = TestCase $
  assertBool "parse (if #t 1 2 3) should fail"
    (isLeft $
      parsor [ Delimiter "("
             , Keyword "if"
             , Lexer.Boolean True
             , Constant 1
             , Constant 2
             , Constant 3
             , Delimiter ")"
             ])
