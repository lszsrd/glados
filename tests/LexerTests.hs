{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- tests/LexerTests.hs
-}

module LexerTests (
    lexerTestsToken
    , lexerTestsIdentifier
    , lexerTestsConstant
    , lexerTestsUInteger
) where

import Test.HUnit
import Lexer

lexerTestsToken :: Test
lexerTestsToken = TestList      [
                                ]

lexerTestsIdentifier :: Test
lexerTestsIdentifier = TestList [
    parseIdentifierStartsWithAnInvalidChar
    , parseIdentifierStartsWithADigit
    , parseIdentifierStartsWithDelimiter1
    , parseIdentifierStartsWithDelimiter2
    , parseIdentifierStartsWithOperator1
    , parseIdentifierStartsWithOperator2
    , parseIdentifierStartsWithOperator3
    , parseIdentifierStartsWithOperator4
    , parseIdentifierStartsWithOperator5
    , parseIdentifierStartsWithOperator6
    , parseIdentifierStartsWithOperator7
    , parseIdentifierValidName
    , parseIdentifierValidNameEndingWithASpace
                                ]

lexerTestsConstant :: Test
lexerTestsConstant = TestList   [
    parseConstantStartsWithAnInvalidChar
    , parseConstantBooleanFalse
    , parseConstantBooleanTrue
    , parseConstantMalformedBoolean
    , parseConstantInvalidBoolean
    , parseConstantNegativeDigit
    , parseConstantNegativeNumber
    , parseConstantMalformedNegativeNumber
    , parseConstantDigit
    , parseConstantNumber
    , parseConstantBigNumber
                                ]

lexerTestsUInteger :: Test
lexerTestsUInteger = TestList [
    parseUIntegerStartsWithAnInvalidChar
    , parseUIntegerDigit
    , parseUIntegerNumber
    , parseUIntegerBigNumber
    , parseUIntegerNegativeDigit
    , parseUIntegerNegativeNumber
    , parseUIntegerNotADigit
    , parseUIntegerEmptyString
                                ]

-- Testing `parseIdentifier` function from Lexer module
parseIdentifierStartsWithAnInvalidChar :: Test
parseIdentifierStartsWithAnInvalidChar = TestCase (assertEqual "parseIdentifier \" foo\"" Nothing (parseIdentifier " foo"))

parseIdentifierStartsWithADigit :: Test
parseIdentifierStartsWithADigit = TestCase (assertEqual "parseIdentifier \"2foo\"" Nothing (parseIdentifier "2foo"))

parseIdentifierStartsWithOperator1 :: Test
parseIdentifierStartsWithOperator1 = TestCase (assertEqual "parseIdentifier \"+foo\"" Nothing (parseIdentifier "+foo"))

parseIdentifierStartsWithOperator2 :: Test
parseIdentifierStartsWithOperator2 = TestCase (assertEqual "parseIdentifier \"-foo\"" Nothing (parseIdentifier "-foo"))

parseIdentifierStartsWithOperator3 :: Test
parseIdentifierStartsWithOperator3 = TestCase (assertEqual "parseIdentifier \"*foo\"" Nothing (parseIdentifier "*foo"))

parseIdentifierStartsWithOperator4 :: Test
parseIdentifierStartsWithOperator4 = TestCase (assertEqual "parseIdentifier \"/foo\"" Nothing (parseIdentifier "/foo"))

parseIdentifierStartsWithOperator5 :: Test
parseIdentifierStartsWithOperator5 = TestCase (assertEqual "parseIdentifier \">foo\"" Nothing (parseIdentifier ">foo"))

parseIdentifierStartsWithOperator6 :: Test
parseIdentifierStartsWithOperator6 = TestCase (assertEqual "parseIdentifier \"<foo\"" Nothing (parseIdentifier "<foo"))

parseIdentifierStartsWithOperator7 :: Test
parseIdentifierStartsWithOperator7 = TestCase (assertEqual "parseIdentifier \"eq?foo\"" Nothing (parseIdentifier "eq?foo"))

parseIdentifierStartsWithDelimiter1 :: Test
parseIdentifierStartsWithDelimiter1 = TestCase (assertEqual "parseIdentifier \"(foo\"" Nothing (parseIdentifier "(foo"))

parseIdentifierStartsWithDelimiter2 :: Test
parseIdentifierStartsWithDelimiter2 = TestCase (assertEqual "parseIdentifier \")foo\"" Nothing (parseIdentifier ")foo"))

parseIdentifierValidName :: Test
parseIdentifierValidName = TestCase (assertEqual "parseIdentifier \"foo\"" (Just ("foo", "")) (parseIdentifier "foo"))

parseIdentifierValidNameEndingWithASpace :: Test
parseIdentifierValidNameEndingWithASpace = TestCase (assertEqual "parseIdentifier \"foo bar\"" (Just ("foo", " bar")) (parseIdentifier "foo bar"))

-- Testing `parseConstant` function from Lexer module
parseConstantStartsWithAnInvalidChar :: Test
parseConstantStartsWithAnInvalidChar = TestCase (assertEqual "parseConstant \" 6\"" Nothing (parseConstant " 6"))

parseConstantBooleanFalse :: Test
parseConstantBooleanFalse = TestCase (assertEqual "parseConstant \"#f no\"" (Just (Boolean False, "")) (parseConstant "#f"))

parseConstantBooleanTrue :: Test
parseConstantBooleanTrue = TestCase (assertEqual "parseConstant \"#tesla\"" (Just (Boolean True, "esla")) (parseConstant "#tesla"))

parseConstantMalformedBoolean :: Test
parseConstantMalformedBoolean = TestCase (assertEqual "parseConstant \"#\"" Nothing (parseConstant "#"))

parseConstantInvalidBoolean :: Test
parseConstantInvalidBoolean = TestCase (assertEqual "parseConstant \"#z\"" Nothing (parseConstant "#z"))

parseConstantNegativeDigit :: Test
parseConstantNegativeDigit = TestCase (assertEqual "parseConstant \"-9\"" (Just (Constant (-9), "")) (parseConstant "-9"))

parseConstantNegativeNumber :: Test
parseConstantNegativeNumber = TestCase (assertEqual "parseConstant \"-84\"" (Just (Constant (-84), "")) (parseConstant "-84"))

parseConstantMalformedNegativeNumber :: Test
parseConstantMalformedNegativeNumber = TestCase (assertEqual "parseConstant \"- 12\"" Nothing (parseConstant "- 12"))

parseConstantDigit :: Test
parseConstantDigit = TestCase (assertEqual "parseConstant \"1\"" (Just (Constant (1), "")) (parseConstant "1"))

parseConstantNumber :: Test
parseConstantNumber = TestCase (assertEqual "parseConstant \"1024dirts\"" (Just (Constant 1024, "dirts")) (parseConstant "1024dirts"))

parseConstantBigNumber :: Test
parseConstantBigNumber = TestCase (assertEqual "parseConstant \"91234567891011121314151617 big one\"" (Just (Constant (91234567891011121314151617), " big one")) (parseConstant "91234567891011121314151617 big one"))

-- Testing `parseUInteger` function from Lexer module
parseUIntegerStartsWithAnInvalidChar :: Test
parseUIntegerStartsWithAnInvalidChar = TestCase (assertEqual "parseUInteger \" 4\"" Nothing (parseUInteger " 4"))

parseUIntegerDigit :: Test
parseUIntegerDigit = TestCase (assertEqual "parseUInteger \"8\"" (Just ("8", "")) (parseUInteger "8"))

parseUIntegerNumber :: Test
parseUIntegerNumber = TestCase (assertEqual "parseUInteger \"42bytes\"" (Just ("42", "bytes")) (parseUInteger "42bytes"))

parseUIntegerBigNumber :: Test
parseUIntegerBigNumber = TestCase (assertEqual "parseUInteger \"1234567891011121314151617 big one\"" (Just ("1234567891011121314151617", " big one")) (parseUInteger "1234567891011121314151617 big one"))

parseUIntegerNegativeDigit :: Test
parseUIntegerNegativeDigit = TestCase (assertEqual "parseUInteger \"-4\"" Nothing (parseUInteger "-4"))

parseUIntegerNegativeNumber :: Test
parseUIntegerNegativeNumber = TestCase (assertEqual "parseUInteger \"-21\"" Nothing (parseUInteger "-21"))

parseUIntegerNotADigit :: Test
parseUIntegerNotADigit = TestCase (assertEqual "parseUInteger \"A1\"" Nothing (parseUInteger "A1"))

parseUIntegerEmptyString :: Test
parseUIntegerEmptyString = TestCase (assertEqual "parseUInteger \"\"" Nothing (parseUInteger ""))
