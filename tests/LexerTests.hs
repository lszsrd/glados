{- 
-- EPITECH PROJECT, 2025
-- Generic Language And Data Operand Syntax
-- File description:
-- tests/LexerTests.hs
-}

module LexerTests (
    lexerTestsLexemes
    , lexerTestsLexer
    , lexerTestsAnyToken
    , lexerTestsToken
    , lexerTestsIdentifier
    , lexerTestsConstant
    , lexerTestsUInteger
) where

import Test.HUnit
import Lexer

-- Testing lexemes from Lexer module
lexerTestsLexemes = TestList    [
    keywordsDefinitions
    , operatorsDefinitions
    , delimitersDefinitions
                                ]

keywordsDefinitions = TestCase (assertEqual "keywords = [define, lambda, if]" ["define", "lambda", "if"] keywords)
operatorsDefinitions = TestCase (assertEqual "operators = [+, -, *, /, >, <, eq?]" ["+", "-", "*", "/", ">", "<", "eq?"] operators)
delimitersDefinitions = TestCase (assertEqual "delimiters = [(, )]" ["(", ")"] delimiters)

-- Testing `lexer` function from Lexer module
lexerTestsLexer = TestList      [
    lexerEmptyString
    , lexerNotTokenNorIdentifier
    , lexerCommentedString
    , lexerSimpleDefine
    , lexerSimpleIf
    , lexerSimpleOperator
    , lexerNegativeConstant
                                ]

lexerEmptyString = TestCase (assertEqual "lexer \"\"" [] (lexer ""))
lexerNotTokenNorIdentifier = TestCase (assertEqual "lexer \"         \"" [] (lexer "         "))
lexerCommentedString = TestCase (assertEqual "lexer \"; foo bar baz\n\"" [] (lexer "; foo bar baz\n"))
lexerSimpleDefine = TestCase (assertEqual "lexer \"(define foo 42)\"" [Delimiter "(", Keyword "define", Identifier "foo", Constant 42, Delimiter ")"] (lexer "(define foo 42)"))
lexerSimpleIf = TestCase (assertEqual "lexer \"(if #t 4 2)\"" [Delimiter "(", Keyword "if", Boolean True, Constant 4, Constant 2, Delimiter ")"] (lexer "(if #t 4 2)"))
lexerSimpleOperator = TestCase (assertEqual "lexer \"(eq? (*2 5) (- 11 1))\"" [Delimiter "(", Operator "eq?", Delimiter "(", Operator "*", Constant 2, Constant 5, Delimiter ")", Delimiter "(", Operator "-", Constant 11, Constant 1, Delimiter ")", Delimiter ")"] (lexer "(eq? (*2 5) (- 11 1))"))
lexerNegativeConstant = TestCase (assertEqual "lexer \"(-42)\"" [Delimiter "(", Constant (-42), Delimiter ")"] (lexer "(-42)"))

-- Testing `parseAnyToken` function from Lexer module
lexerTestsAnyToken = TestList   [
    parseAnyTokenEmptyString
    , parseAnyTokenDelimiter1
    , parseAnyTokenDelimiter2
    , parseAnyTokenInvalidDelimiter
    , parseAnyTokenOperator1
    , parseAnyTokenOperator2
    , parseAnyTokenOperator3
    , parseAnyTokenOperator4
    , parseAnyTokenOperator5
    , parseAnyTokenOperator6
    , parseAnyTokenOperator7
    , parseAnyTokenInvalidOperator
    , parseAnyTokenKeyword1
    , parseAnyTokenKeyword2
    , parseAnyTokenKeyword3
    , parseAnyTokenInvalidKeyword
                                ]

parseAnyTokenEmptyString = TestCase (assertEqual "parseAnyToken \"\"" Nothing (parseAnyToken ""))
parseAnyTokenDelimiter1 = TestCase (assertEqual "parseAnyToken \"(foo\"" (Just (Delimiter "(", "foo")) (parseAnyToken "(foo"))
parseAnyTokenDelimiter2 = TestCase (assertEqual "parseAnyToken \")foo\"" (Just (Delimiter ")", "foo")) (parseAnyToken ")foo"))
parseAnyTokenInvalidDelimiter = TestCase (assertEqual "parseAnyToken \")foo\"" Nothing (parseAnyToken "foo"))
parseAnyTokenOperator1 = TestCase (assertEqual "parseAnyToken \"+ bar\"" (Just (Operator "+", " bar")) (parseAnyToken "+ bar"))
parseAnyTokenOperator2 = TestCase (assertEqual "parseAnyToken \"- bar\"" (Just (Operator "-", " bar")) (parseAnyToken "- bar"))
parseAnyTokenOperator3 = TestCase (assertEqual "parseAnyToken \"*bar\"" (Just (Operator "*", "bar")) (parseAnyToken "*bar"))
parseAnyTokenOperator4 = TestCase (assertEqual "parseAnyToken \"/bar\"" (Just (Operator "/", "bar")) (parseAnyToken "/bar"))
parseAnyTokenOperator5 = TestCase (assertEqual "parseAnyToken \">\"" (Just (Operator ">", "")) (parseAnyToken ">"))
parseAnyTokenOperator6 = TestCase (assertEqual "parseAnyToken \"<\"" (Just (Operator "<", "")) (parseAnyToken "<"))
parseAnyTokenOperator7 = TestCase (assertEqual "parseAnyToken \"eq? bar\"" (Just (Operator "eq?", " bar")) (parseAnyToken "eq? bar"))
parseAnyTokenInvalidOperator = TestCase (assertEqual "parseAnyToken \"bar\"" Nothing (parseAnyToken "bar"))
parseAnyTokenKeyword1 = TestCase (assertEqual "parseAnyToken \"define baz\"" (Just (Keyword "define", " baz")) (parseAnyToken "define baz"))
parseAnyTokenKeyword2 = TestCase (assertEqual "parseAnyToken \"lambda   baz\"" (Just (Keyword "lambda", "   baz")) (parseAnyToken "lambda   baz"))
parseAnyTokenKeyword3 = TestCase (assertEqual "parseAnyToken \"if \"" (Just (Keyword "if", "")) (parseAnyToken "if"))
parseAnyTokenInvalidKeyword = TestCase (assertEqual "parseAnyToken \"baz\"" Nothing (parseAnyToken "baz"))

-- Testing `parseToken` function from Lexer module
lexerTestsToken = TestList      [
    parseTokenDelimiter1
    , parseTokenDelimiter2
    , parseTokenInvalidDelimiter
    , parseTokenOperator1
    , parseTokenOperator2
    , parseTokenOperator3
    , parseTokenOperator4
    , parseTokenOperator5
    , parseTokenOperator6
    , parseTokenOperator7
    , parseTokenInvalidOperator
    , parseTokenKeyword1
    , parseTokenKeyword2
    , parseTokenKeyword3
    , parseTokenInvalidKeyword
                                ]

parseTokenDelimiter1 = TestCase (assertEqual "parseToken \"(foo\"" (Just ("(", "foo")) (parseToken "(foo" delimiters))
parseTokenDelimiter2 = TestCase (assertEqual "parseToken \")foo\"" (Just (")", "foo")) (parseToken ")foo" delimiters))
parseTokenInvalidDelimiter = TestCase (assertEqual "parseToken \")foo\"" Nothing (parseToken "foo" delimiters))
parseTokenOperator1 = TestCase (assertEqual "parseToken \"+ bar\"" (Just ("+", " bar")) (parseToken "+ bar" operators))
parseTokenOperator2 = TestCase (assertEqual "parseToken \"- bar\"" (Just ("-", " bar")) (parseToken "- bar" operators))
parseTokenOperator3 = TestCase (assertEqual "parseToken \"*bar\"" (Just ("*", "bar")) (parseToken "*bar" operators))
parseTokenOperator4 = TestCase (assertEqual "parseToken \"/bar\"" (Just ("/", "bar")) (parseToken "/bar" operators))
parseTokenOperator5 = TestCase (assertEqual "parseToken \">\"" (Just (">", "")) (parseToken ">" operators))
parseTokenOperator6 = TestCase (assertEqual "parseToken \"<\"" (Just ("<", "")) (parseToken "<" operators))
parseTokenOperator7 = TestCase (assertEqual "parseToken \"eq? bar\"" (Just ("eq?", " bar")) (parseToken "eq? bar" operators))
parseTokenInvalidOperator = TestCase (assertEqual "parseToken \"bar\"" Nothing (parseToken "bar" operators))
parseTokenKeyword1 = TestCase (assertEqual "parseToken \"define baz\"" (Just ("define", " baz")) (parseToken "define baz" keywords))
parseTokenKeyword2 = TestCase (assertEqual "parseToken \"lambda   baz\"" (Just ("lambda", "   baz")) (parseToken "lambda   baz" keywords))
parseTokenKeyword3 = TestCase (assertEqual "parseToken \"if \"" (Just ("if", "")) (parseToken "if" keywords))
parseTokenInvalidKeyword = TestCase (assertEqual "parseToken \"baz\"" Nothing (parseToken "baz" keywords))

-- Testing `parseIdentifier` function from Lexer module
lexerTestsIdentifier = TestList [
    parseIdentifierStartsWithAnInvalidChar
    -- , parseIdentifierStartsWithADigit
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

parseIdentifierStartsWithAnInvalidChar = TestCase (assertEqual "parseIdentifier \" foo\"" Nothing (parseIdentifier " foo"))
-- parseIdentifierStartsWithADigit = TestCase (assertEqual "parseIdentifier \"2foo\"" Nothing (parseIdentifier "2foo"))
parseIdentifierStartsWithOperator1 = TestCase (assertEqual "parseIdentifier \"+foo\"" Nothing (parseIdentifier "+foo"))
parseIdentifierStartsWithOperator2 = TestCase (assertEqual "parseIdentifier \"-foo\"" Nothing (parseIdentifier "-foo"))
parseIdentifierStartsWithOperator3 = TestCase (assertEqual "parseIdentifier \"*foo\"" Nothing (parseIdentifier "*foo"))
parseIdentifierStartsWithOperator4 = TestCase (assertEqual "parseIdentifier \"/foo\"" Nothing (parseIdentifier "/foo"))
parseIdentifierStartsWithOperator5 = TestCase (assertEqual "parseIdentifier \">foo\"" Nothing (parseIdentifier ">foo"))
parseIdentifierStartsWithOperator6 = TestCase (assertEqual "parseIdentifier \"<foo\"" Nothing (parseIdentifier "<foo"))
parseIdentifierStartsWithOperator7 = TestCase (assertEqual "parseIdentifier \"eq?foo\"" Nothing (parseIdentifier "eq?foo"))
parseIdentifierStartsWithDelimiter1 = TestCase (assertEqual "parseIdentifier \"(foo\"" Nothing (parseIdentifier "(foo"))
parseIdentifierStartsWithDelimiter2 = TestCase (assertEqual "parseIdentifier \")foo\"" Nothing (parseIdentifier ")foo"))
parseIdentifierValidName = TestCase (assertEqual "parseIdentifier \"foo\"" (Just ("foo", "")) (parseIdentifier "foo"))
parseIdentifierValidNameEndingWithASpace = TestCase (assertEqual "parseIdentifier \"foo bar\"" (Just ("foo", " bar")) (parseIdentifier "foo bar"))

-- Testing `parseConstant` function from Lexer module
lexerTestsConstant = TestList   [
    parseConstantEmptyString
    , parseConstantStartsWithAnInvalidChar
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

parseConstantEmptyString = TestCase (assertEqual "parseConstant \"\"" Nothing (parseConstant ""))
parseConstantStartsWithAnInvalidChar = TestCase (assertEqual "parseConstant \" 6\"" Nothing (parseConstant " 6"))
parseConstantBooleanFalse = TestCase (assertEqual "parseConstant \"#f no\"" (Just (Boolean False, "")) (parseConstant "#f"))
parseConstantBooleanTrue = TestCase (assertEqual "parseConstant \"#tesla\"" (Just (Boolean True, "esla")) (parseConstant "#tesla"))
parseConstantMalformedBoolean = TestCase (assertEqual "parseConstant \"#\"" Nothing (parseConstant "#"))
parseConstantInvalidBoolean = TestCase (assertEqual "parseConstant \"#z\"" Nothing (parseConstant "#z"))
parseConstantNegativeDigit = TestCase (assertEqual "parseConstant \"-9\"" (Just (Constant (-9), "")) (parseConstant "-9"))
parseConstantNegativeNumber = TestCase (assertEqual "parseConstant \"-84\"" (Just (Constant (-84), "")) (parseConstant "-84"))
parseConstantMalformedNegativeNumber = TestCase (assertEqual "parseConstant \"- 12\"" Nothing (parseConstant "- 12"))
parseConstantDigit = TestCase (assertEqual "parseConstant \"1\"" (Just (Constant 1, "")) (parseConstant "1"))
parseConstantNumber = TestCase (assertEqual "parseConstant \"1024dirts\"" (Just (Constant 1024, "dirts")) (parseConstant "1024dirts"))
parseConstantBigNumber = TestCase (assertEqual "parseConstant \"91234567891011121314151617 big one\"" (Just (Constant 91234567891011121314151617, " big one")) (parseConstant "91234567891011121314151617 big one"))

-- Testing `parseUInteger` function from Lexer module
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

parseUIntegerStartsWithAnInvalidChar = TestCase (assertEqual "parseUInteger \" 4\"" Nothing (parseUInteger " 4"))
parseUIntegerDigit = TestCase (assertEqual "parseUInteger \"8\"" (Just ("8", "")) (parseUInteger "8"))
parseUIntegerNumber = TestCase (assertEqual "parseUInteger \"42bytes\"" (Just ("42", "bytes")) (parseUInteger "42bytes"))
parseUIntegerBigNumber = TestCase (assertEqual "parseUInteger \"1234567891011121314151617 big one\"" (Just ("1234567891011121314151617", " big one")) (parseUInteger "1234567891011121314151617 big one"))
parseUIntegerNegativeDigit = TestCase (assertEqual "parseUInteger \"-4\"" Nothing (parseUInteger "-4"))
parseUIntegerNegativeNumber = TestCase (assertEqual "parseUInteger \"-21\"" Nothing (parseUInteger "-21"))
parseUIntegerNotADigit = TestCase (assertEqual "parseUInteger \"A1\"" Nothing (parseUInteger "A1"))
parseUIntegerEmptyString = TestCase (assertEqual "parseUInteger \"\"" Nothing (parseUInteger ""))
