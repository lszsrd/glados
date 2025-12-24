{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- tests/hunit/lib/frontend/TestsLexer.hs
-}

module TestsLexer (
    tests
    , testsParseDigit
    , testsParseNonDigit
    , testsParseIdentifier
    , testsParseEscapeSequence
    , testsParseChar
    , testsParseCharacterConstant
    , testsParseDigitSequence
    , testsParseFloatingConstant
    , testsParseDecimalConstant
    , testsParseSChar
    , testsParseSCharSequence
    , testsParseStringLiteral
    , testsParseMultiLineComment
    , testsParsePunctuator
    , testsParseLiteral
    , testsParseKeyword
    , testsParseBooleanConstant
    , testsLexer
) where

import Test.HUnit

import Lexer

tests = TestList                        [
    testsParseDigit, testsParseNonDigit, testsParseIdentifier
    , testsParseEscapeSequence, testsParseChar, testsParseCharacterConstant
    , testsParseDigitSequence, testsParseFloatingConstant, testsParseDecimalConstant
    , testsParseSChar, testsParseSCharSequence, testsParseStringLiteral
    , testsParseMultiLineComment, testsParsePunctuator, testsParseLiteral
    , testsParseKeyword, testsParseBooleanConstant, testsLexer
                                        ]

testsLexer = TestList                   [
                                        ]

testsParseBooleanConstant = TestList    [
                                        ]

testsParseKeyword = TestList            [
                                        ]

testsParseLiteral = TestList            [
                                        ]

testsParsePunctuator = TestList         [
                                        ]

testsParseMultiLineComment = TestList   [
                                        ]

testsParseStringLiteral = TestList      [
                                        ]

testsParseSCharSequence = TestList      [
                                        ]

testsParseSChar = TestList              [
                                        ]

testsParseDecimalConstant = TestList    [
                                        ]

testsParseFloatingConstant = TestList   [
                                        ]

testsParseDigitSequence = TestList      [
                                        ]

testsParseCharacterConstant = TestList  [
                                        ]

testsParseChar = TestList               [
                                        ]

testsParseEscapeSequence = TestList     [
                                        ]

testsParseIdentifier = TestList         [
                                        ]

testsParseNonDigit = TestList           [
                                        ]

testsParseDigit = TestList              [
                                        ]


