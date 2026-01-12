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
import Tokens

tests = TestList                        [
    testsParseDigit, testsParseNonDigit, testsParseIdentifier
    , testsParseEscapeSequence, testsParseChar, testsParseCharacterConstant
    , testsParseDigitSequence, testsParseFloatingConstant, testsParseDecimalConstant
    , testsParseSChar, testsParseSCharSequence, testsParseStringLiteral
    , testsParseMultiLineComment, testsParsePunctuator, testsParseLiteral
    , testsParseKeyword, testsParseBooleanConstant, testsLexer
                                        ]

testsLexer = TestList                   [
    lexerTest1, lexerTest2, lexerTest3
    , lexerTest4, lexerTest5, lexerTest6
    , lexerTest7, lexerTest8, lexerTest9
    , lexerTest10, lexerTest11, lexerTest12
                                        ]

lexerTest1 = TestCase (assertEqual "lexer \"\"" (Right []) (lexer ""))
lexerTest2 = TestCase (assertEqual "lexer \"if (y == 0) {}\"" (Right [(Keyword If,(1,1)),(Punctuator (RBracket OpenRBracket),(1,4)),(Identifier "y",(1,5)),(Punctuator (BinaryOp Eq),(1,7)),(Literal (IntLiteral 0),(1,10)),(Punctuator (RBracket CloseRBracket),(1,11)),(Punctuator (CBracket OpenCBracket),(1,13)),(Punctuator (CBracket CloseCBracket),(1,14))]) (lexer "if (y == 0) {}"))
lexerTest3 = TestCase (assertEqual "lexer \"if /**/ (\"" (Right [(Keyword If,(1,1)),(Punctuator (RBracket OpenRBracket),(1,9))]) (lexer "if /**/ ("))
lexerTest4 = TestCase (assertEqual "lexer \"fn      foo(Int: x, Int: y) -> Int\"" (Right [(Keyword Fn,(1,1)),(Identifier "foo",(1,9)),(Punctuator (RBracket OpenRBracket),(1,12)),(Keyword Int,(1,13)),(Punctuator Colon,(1,16)),(Identifier "x",(1,18)),(Punctuator Comma,(1,19)),(Keyword Int,(1,21)),(Punctuator Colon,(1,24)),(Identifier "y",(1,26)),(Punctuator (RBracket CloseRBracket),(1,27)),(Punctuator Arrow,(1,29)),(Keyword Int,(1,32))]) (lexer "fn      foo(Int: x, Int: y) -> Int"))
lexerTest5 = TestCase (assertEqual "lexer \"\n/\"" (Right []) (lexer "\n"))
lexerTest6 = TestCase (assertEqual "lexer \"/\"" (Right [(Punctuator (BinaryOp Div),(1,1))]) (lexer "/"))
lexerTest7 = TestCase (assertEqual "lexer \"Int   \nx\"" (Right [(Keyword Int,(1,1)),(Identifier "x",(2,1))]) (lexer "Int   \nx"))
lexerTest8 = TestCase (assertEqual "lexer \"//\n//x\"" (Right []) (lexer "//\n//\n"))
lexerTest9 = TestCase (assertEqual "lexer \"if /*\"" (Left "1:6: \ESC[1;31merror\ESC[0m: unterminated comment block, missing '\\*'\n    1 | if /*\n      |      \ESC[1;32m^~ here\ESC[0m") (lexer "if /*"))
lexerTest10 = TestCase (assertEqual "lexer \"x @\"" (Left "1:3: \ESC[1;31merror\ESC[0m: unexpected character '@'\n    1 | x @\n      |   \ESC[1;32m^ here\ESC[0m") (lexer "x @"))
lexerTest11 = TestCase (assertEqual "lexer \"/**/&\"" (Left "1:5: \ESC[1;31merror\ESC[0m: unexpected character '&'\n    1 | /**/&\n      |     \ESC[1;32m^ here\ESC[0m") (lexer "/**/&"))
lexerTest12 = TestCase (assertEqual "lexer \"//\n&\"" (Left "2:1: \ESC[1;31merror\ESC[0m: unexpected character '&'\n    2 | &\n      | \ESC[1;32m^ here\ESC[0m") (lexer "//\n&"))

testsParseBooleanConstant = TestList    [
    parseBooleanConstantTest1, parseBooleanConstantTest2, parseBooleanConstantTest3
    , parseBooleanConstantTest4
                                        ]

parseBooleanConstantTest1 = TestCase (assertEqual "parseBooleanConstant \"True elle\"" (Just (Literal (BoolLiteral True), 4, " elle")) (parseBooleanConstant "True elle"))
parseBooleanConstantTest2 = TestCase (assertEqual "parseBooleanConstant \"False\"" (Just (Literal (BoolLiteral False), 5, [])) (parseBooleanConstant "False"))
parseBooleanConstantTest3 = TestCase (assertEqual "parseBooleanConstant \"false\"" Nothing (parseBooleanConstant "false"))
parseBooleanConstantTest4 = TestCase (assertEqual "parseBooleanConstant \"\"" Nothing (parseBooleanConstant ""))

testsParseKeyword = TestList            [
    parseKeywordTest1, parseKeywordTest2, parseKeywordTest3
    , parseKeywordTest4, parseKeywordTest5, parseKeywordTest6
    , parseKeywordTest7, parseKeywordTest8, parseKeywordTest9
    , parseKeywordTest10, parseKeywordTest11, parseKeywordTest12
    , parseKeywordTest13, parseKeywordTest14
                                        ]

parseKeywordTest1 = TestCase (assertEqual "parseKeyword \"Bool\"" (Just (Keyword Bool, 4, [])) (parseKeyword "Bool"))
parseKeywordTest2 = TestCase (assertEqual "parseKeyword \"Char i\"" (Just (Keyword Char, 4, " i")) (parseKeyword "Char i"))
parseKeywordTest3 = TestCase (assertEqual "parseKeyword \"Int\"" (Just (Keyword Int, 3, [])) (parseKeyword "Int"))
parseKeywordTest4 = TestCase (assertEqual "parseKeyword \"Float foo\"" (Just (Keyword Float, 5, " foo")) (parseKeyword "Float foo"))
parseKeywordTest5 = TestCase (assertEqual "parseKeyword \"Double d\"" (Just (Keyword Double, 6, " d")) (parseKeyword "Double d"))
parseKeywordTest6 = TestCase (assertEqual "parseKeyword \"fn\"" (Just (Keyword Fn, 2, [])) (parseKeyword "fn"))
parseKeywordTest7 = TestCase (assertEqual "parseKeyword \"if (\"" (Just (Keyword If, 2, " (")) (parseKeyword "if ("))
parseKeywordTest8 = TestCase (assertEqual "parseKeyword \"else\"" (Just (Keyword Else, 4, [])) (parseKeyword "else"))
parseKeywordTest9 = TestCase (assertEqual "parseKeyword \"while\"" (Just (Keyword While, 5, [])) (parseKeyword "while"))
parseKeywordTest10 = TestCase (assertEqual "parseKeyword \"foreach  \"" (Just (Keyword Foreach, 7, "  ")) (parseKeyword "foreach  "))
parseKeywordTest11 = TestCase (assertEqual "parseKeyword \"for\"" (Just (Keyword For, 3, [])) (parseKeyword "for"))
parseKeywordTest12 = TestCase (assertEqual "parseKeyword \"ret 1\"" (Just (Keyword Ret, 3, " 1")) (parseKeyword "ret 1"))
parseKeywordTest13 = TestCase (assertEqual "parseKeyword \"Boolfoo\"" Nothing (parseKeyword "Boolfoo"))
parseKeywordTest14 = TestCase (assertEqual "parseKeyword \"\"" Nothing (parseKeyword ""))

testsParseLiteral = TestList            [
    parseLiteralTest1, parseLiteralTest2, parseLiteralTest3
                                        ]

parseLiteralTest1 = TestCase (assertEqual "parseLiteral \"'a'foo\"" (Just (Literal (CharLiteral 'a'), 3, "foo")) (parseLiteral "'a'foo"))
parseLiteralTest2 = TestCase (assertEqual "parseLiteral \"1234\"" (Just (Literal (IntLiteral 1234), 4, [])) (parseLiteral "1234"))
parseLiteralTest3 = TestCase (assertEqual "parseLiteral \"3.14\"" (Just (Literal (FloatLiteral 3.14), 4, [])) (parseLiteral "3.14"))

testsParsePunctuator = TestList         [
    parsePunctuatorTest1, parsePunctuatorTest2, parsePunctuatorTest3
    , parsePunctuatorTest4, parsePunctuatorTest5, parsePunctuatorTest6
    , parsePunctuatorTest7, parsePunctuatorTest8, parsePunctuatorTest9
    , parsePunctuatorTest10, parsePunctuatorTest11, parsePunctuatorTest12
    , parsePunctuatorTest13, parsePunctuatorTest14, parsePunctuatorTest15
    , parsePunctuatorTest16, parsePunctuatorTest17, parsePunctuatorTest18
    , parsePunctuatorTest19, parsePunctuatorTest20, parsePunctuatorTest21
    , parsePunctuatorTest22, parsePunctuatorTest23, parsePunctuatorTest24
    , parsePunctuatorTest25, parsePunctuatorTest26, parsePunctuatorTest27
    , parsePunctuatorTest28, parsePunctuatorTest29, parsePunctuatorTest30
    , parsePunctuatorTest31, parsePunctuatorTest32, parsePunctuatorTest33
    , parsePunctuatorTest34, parsePunctuatorTest35
                                        ]

parsePunctuatorTest1 = TestCase (assertEqual "parsePunctuator \"[\"" (Just (Punctuator (SBracket OpenSBracket), 1, [])) (parsePunctuator "["))
parsePunctuatorTest2 = TestCase (assertEqual "parsePunctuator \"] \"" (Just (Punctuator (SBracket CloseSBracket), 1, " ")) (parsePunctuator "] "))
parsePunctuatorTest3 = TestCase (assertEqual "parsePunctuator \"(foo\"" (Just (Punctuator (RBracket OpenRBracket), 1, "foo")) (parsePunctuator "(foo"))
parsePunctuatorTest4 = TestCase (assertEqual "parsePunctuator \")  \"" (Just (Punctuator (RBracket CloseRBracket), 1, "  ")) (parsePunctuator ")  "))
parsePunctuatorTest5 = TestCase (assertEqual "parsePunctuator \"{\"" (Just (Punctuator (CBracket OpenCBracket), 1, [])) (parsePunctuator "{"))
parsePunctuatorTest6 = TestCase (assertEqual "parsePunctuator \"}\"" (Just (Punctuator (CBracket CloseCBracket), 1, [])) (parsePunctuator "}"))
parsePunctuatorTest7 = TestCase (assertEqual "parsePunctuator \".\"" (Just (Punctuator Dot, 1, [])) (parsePunctuator "."))
parsePunctuatorTest8 = TestCase (assertEqual "parsePunctuator \"->yes\"" (Just (Punctuator Arrow, 2, "yes")) (parsePunctuator "->yes"))
parsePunctuatorTest9 = TestCase (assertEqual "parsePunctuator \"++\"" (Just (Punctuator (UnaryOp IdentIncrement), 2, [])) (parsePunctuator "++"))
parsePunctuatorTest10 = TestCase (assertEqual "parsePunctuator \"---\"" (Just (Punctuator (UnaryOp IdentDecrement), 2, "-")) (parsePunctuator "---"))
parsePunctuatorTest11 = TestCase (assertEqual "parsePunctuator \"*= foo\"" (Just (Punctuator (AssignOp MulEqual), 2, " foo")) (parsePunctuator "*= foo"))
parsePunctuatorTest12 = TestCase (assertEqual "parsePunctuator \"/=\"" (Just (Punctuator (AssignOp DivEqual), 2, [])) (parsePunctuator "/="))
parsePunctuatorTest13 = TestCase (assertEqual "parsePunctuator \"%= \"" (Just (Punctuator (AssignOp ModEqual), 2, " ")) (parsePunctuator "%= "))
parsePunctuatorTest14 = TestCase (assertEqual "parsePunctuator \"+=  \"" (Just (Punctuator (AssignOp AddEqual), 2, "  ")) (parsePunctuator "+=  "))
parsePunctuatorTest15 = TestCase (assertEqual "parsePunctuator \"-=\"" (Just (Punctuator (AssignOp SubEqual), 2, [])) (parsePunctuator "-="))
parsePunctuatorTest16 = TestCase (assertEqual "parsePunctuator \"<=\"" (Just (Punctuator (BinaryOp LEq), 2, [])) (parsePunctuator "<="))
parsePunctuatorTest17 = TestCase (assertEqual "parsePunctuator \">=\"" (Just (Punctuator (BinaryOp GEq), 2, [])) (parsePunctuator ">="))
parsePunctuatorTest18 = TestCase (assertEqual "parsePunctuator \"==\"" (Just (Punctuator (BinaryOp Eq), 2, [])) (parsePunctuator "=="))
parsePunctuatorTest19 = TestCase (assertEqual "parsePunctuator \"!=\"" (Just (Punctuator (BinaryOp NEq), 2, [])) (parsePunctuator "!="))
parsePunctuatorTest20 = TestCase (assertEqual "parsePunctuator \"&&&\"" (Just (Punctuator (BinaryOp And), 2, "&")) (parsePunctuator "&&&"))
parsePunctuatorTest21 = TestCase (assertEqual "parsePunctuator \"|||\"" (Just (Punctuator (BinaryOp Or), 2, "|")) (parsePunctuator "|||"))
parsePunctuatorTest22 = TestCase (assertEqual "parsePunctuator \"* \"" (Just (Punctuator (BinaryOp Mul), 1, " ")) (parsePunctuator "* "))
parsePunctuatorTest23 = TestCase (assertEqual "parsePunctuator \"+  \"" (Just (Punctuator (BinaryOp Add), 1, "  ")) (parsePunctuator "+  "))
parsePunctuatorTest24 = TestCase (assertEqual "parsePunctuator \"- -\"" (Just (Punctuator (BinaryOp Sub), 1, " -")) (parsePunctuator "- -"))
parsePunctuatorTest25 = TestCase (assertEqual "parsePunctuator \"/\"" (Just (Punctuator (BinaryOp Div), 1, [])) (parsePunctuator "/"))
parsePunctuatorTest26 = TestCase (assertEqual "parsePunctuator \"%\"" (Just (Punctuator (BinaryOp Mod), 1, [])) (parsePunctuator "%"))
parsePunctuatorTest27 = TestCase (assertEqual "parsePunctuator \"< \"" (Just (Punctuator (BinaryOp Lt), 1, " ")) (parsePunctuator "< "))
parsePunctuatorTest28 = TestCase (assertEqual "parsePunctuator \">a\"" (Just (Punctuator (BinaryOp Gt), 1, "a")) (parsePunctuator ">a"))
parsePunctuatorTest29 = TestCase (assertEqual "parsePunctuator \":42\"" (Just (Punctuator Colon, 1, "42")) (parsePunctuator ":42"))
parsePunctuatorTest30 = TestCase (assertEqual "parsePunctuator \";;\"" (Just (Punctuator Semicolon, 1, ";")) (parsePunctuator ";;"))
parsePunctuatorTest31 = TestCase (assertEqual "parsePunctuator \",,,\"" (Just (Punctuator Comma, 1, ",,")) (parsePunctuator ",,,"))
parsePunctuatorTest32 = TestCase (assertEqual "parsePunctuator \"=\"" (Just (Punctuator (AssignOp Equal), 1, [])) (parsePunctuator "="))
parsePunctuatorTest33 = TestCase (assertEqual "parsePunctuator \"" Nothing (parsePunctuator ""))
parsePunctuatorTest34 = TestCase (assertEqual "parsePunctuator @" Nothing (parsePunctuator "@"))
parsePunctuatorTest35 = TestCase (assertEqual "parsePunctuator |" Nothing (parsePunctuator "|"))

testsParseMultiLineComment = TestList   [
    parseMultiLineCommentTest1, parseMultiLineCommentTest2, parseMultiLineCommentTest3
    , parseMultiLineCommentTest4, parseMultiLineCommentTest5, parseMultiLineCommentTest6
    , parseMultiLineCommentTest7, parseMultiLineCommentTest8
                                        ]

parseMultiLineCommentTest1 = TestCase (assertEqual "parseMultiLineComment \"*/\"" (Right ("", (1, 3))) (parseMultiLineComment "/*" "*/" (1, 1) (1, 1)))
parseMultiLineCommentTest2 = TestCase (assertEqual "parseMultiLineComment \"foo\n*/bar\"" (Right ("bar", (2, 3))) (parseMultiLineComment "/*" "foo\n*/bar" (1, 1) (1, 1)))
parseMultiLineCommentTest3 = TestCase (assertEqual "parseMultiLineComment \"*/bar\"" (Right (" ", (1, 6))) (parseMultiLineComment "/*" "   */ " (1, 1) (1, 1)))
parseMultiLineCommentTest4 = TestCase (assertEqual "parseMultiLineComment \"\n*/\"" (Right ("", (2, 3))) (parseMultiLineComment "/*" "\n*/" (1, 1) (1, 1)))
parseMultiLineCommentTest5 = TestCase (assertEqual "parseMultiLineComment \"\"" (Left "1:1: \ESC[1;31merror\ESC[0m: unterminated comment block, missing '\\*'\n    1 | /*\n      | \ESC[1;32m^~ here\ESC[0m") (parseMultiLineComment "/*" "" (1, 1) (1, 1)))
parseMultiLineCommentTest6 = TestCase (assertEqual "parseMultiLineComment \"/*2nd comment\"" (Left "1:1: \ESC[1;31merror\ESC[0m: unterminated comment block upon creation of a new one\n    1 | /*\n      | \ESC[1;32m^~ here\ESC[0m") (parseMultiLineComment "/*" "/*2nd comment" (1, 1) (1, 1)))
parseMultiLineCommentTest7 = TestCase (assertEqual "parseMultiLineComment \"\n\"" (Left "1:1: \ESC[1;31merror\ESC[0m: unterminated comment block, missing '\\*'\n    1 | /*\n      | \ESC[1;32m^~ here\ESC[0m") (parseMultiLineComment "/*" "\n" (1, 1) (1, 1)))
parseMultiLineCommentTest8 = TestCase (assertEqual "parseMultiLineComment \"a\"" (Left "1:1: \ESC[1;31merror\ESC[0m: unterminated comment block, missing '\\*'\n    1 | /*\n      | \ESC[1;32m^~ here\ESC[0m") (parseMultiLineComment "/*" "a" (1, 1) (1, 1)))

testsParseStringLiteral = TestList      [
    parseStringLiteralTest1, parseStringLiteralTest2, parseStringLiteralTest3
    , parseStringLiteralTest4, parseStringLiteralTest5, parseStringLiteralTest6
    , parseStringLiteralTest7
                                        ]

parseStringLiteralTest1 = TestCase (assertEqual "parseStringLiteral \"\"\"\"" (Just ("", 2, [])) (parseStringLiteral "\"\""))
parseStringLiteralTest2 = TestCase (assertEqual "parseStringLiteral \"\"x\"foo\"" (Just ("x", 3, "foo")) (parseStringLiteral "\"x\"foo"))
parseStringLiteralTest3 = TestCase (assertEqual "parseStringLiteral \"\"foo\"\" bar" (Just ("foo", 5, " bar")) (parseStringLiteral "\"foo\" bar"))
parseStringLiteralTest4 = TestCase (assertEqual "parseStringLiteral \"\"foo\"" Nothing (parseStringLiteral "\"foo"))
parseStringLiteralTest5 = TestCase (assertEqual "parseStringLiteral \"foo\"\"" Nothing (parseStringLiteral "foo\""))
parseStringLiteralTest6 = TestCase (assertEqual "parseStringLiteral \"bar\"" Nothing (parseStringLiteral "bar"))
parseStringLiteralTest7 = TestCase (assertEqual "parseStringLiteral \"\"" Nothing (parseStringLiteral ""))

testsParseSCharSequence = TestList      [
    parseSCharSequenceTest1, parseSCharSequenceTest2, parseSCharSequenceTest3
    , parseSCharSequenceTest4
                                        ]

parseSCharSequenceTest1 = TestCase (assertEqual "parseSCharSequence \"abc\"" (Just ("a foo", 5, [])) (parseSCharSequence "a foo"))
parseSCharSequenceTest2 = TestCase (assertEqual "parseSCharSequence \"abc\"" (Just ("abc", 3, [])) (parseSCharSequence "abc"))
parseSCharSequenceTest3 = TestCase (assertEqual "parseSCharSequence \"a\"c\"" (Just ("a", 1, "\"c")) (parseSCharSequence "a\"c"))
parseSCharSequenceTest4 = TestCase (assertEqual "parseSCharSequence \"\"" Nothing (parseSCharSequence []))

testsParseSChar = TestList              [
    parseSCharTest1, parseSCharTest2, parseSCharTest3
    , parseSCharTest4, parseSCharTest5, parseSCharTest6
    , parseSCharTest7, parseSCharTest8, parseSCharTest9
                                        ]

parseSCharTest1 = TestCase (assertEqual "parseSChar \"a\"" (Just ('a', 1, [])) (parseSChar "a"))
parseSCharTest2 = TestCase (assertEqual "parseSChar \"z \"" (Just ('z', 1, " ")) (parseSChar "z "))
parseSCharTest3 = TestCase (assertEqual "parseSChar \"AAA\"" (Just ('A', 1, "AA")) (parseSChar "AAA"))
parseSCharTest4 = TestCase (assertEqual "parseSChar \"Z1\"" (Just ('Z', 1, "1")) (parseSChar "Z1"))
parseSCharTest5 = TestCase (assertEqual "parseSChar \"'\"" (Just ('\'', 1, [])) (parseSChar "'"))
parseSCharTest6 = TestCase (assertEqual "parseSChar \"~home\"" (Just ('@', 1, "home")) (parseSChar "@home"))
parseSCharTest7 = TestCase (assertEqual "parseSChar \"\'a\'\"" (Just ('\'', 1, "a\'")) (parseSChar "\'a\'"))
parseSCharTest8 = TestCase (assertEqual "parseSChar \"\\\"" Nothing (parseSChar "\\"))
parseSCharTest9 = TestCase (assertEqual "parseSChar \"\"" Nothing (parseSChar ""))

testsParseDecimalConstant = TestList    [
    parseDecimalConstantTest1, parseDecimalConstantTest2, parseDecimalConstantTest3
    , parseDecimalConstantTest4, parseDecimalConstantTest5, parseDecimalConstantTest6
                                        ]

parseDecimalConstantTest1 = TestCase (assertEqual "parseDecimalConstant \"0\"" (Just ("0", 1, [])) (parseDecimalConstant "0"))
parseDecimalConstantTest2 = TestCase (assertEqual "parseDecimalConstant \"1x\"" (Just ("1", 1, "x")) (parseDecimalConstant "1x"))
parseDecimalConstantTest3 = TestCase (assertEqual "parseDecimalConstant \"123foo\"" (Just ("123", 3, "foo")) (parseDecimalConstant "123foo"))
parseDecimalConstantTest4 = TestCase (assertEqual "parseDecimalConstant \"123456789123456789 bar\"" (Just ("123456789123456789", 18, " bar")) (parseDecimalConstant "123456789123456789 bar"))
parseDecimalConstantTest5 = TestCase (assertEqual "parseDecimalConstant \"foo bar\"" Nothing (parseDecimalConstant "foo bar"))
parseDecimalConstantTest6 = TestCase (assertEqual "parseDecimalConstant \"\"" Nothing (parseDecimalConstant ""))

testsParseFloatingConstant = TestList   [
    parseFloatingConstantTest1, parseFloatingConstantTest2, parseFloatingConstantTest3
    , parseFloatingConstantTest4, parseFloatingConstantTest5, parseFloatingConstantTest6
    , parseFloatingConstantTest7, parseFloatingConstantTest8
                                        ]

parseFloatingConstantTest1 = TestCase (assertEqual "parseFloatingConstant \"0.0\"" (Just ("0.0", 3, [])) (parseFloatingConstant "0.0"))
parseFloatingConstantTest2 = TestCase (assertEqual "parseFloatingConstant \"3.1415pi\"" (Just ("3.1415", 6, "pi")) (parseFloatingConstant "3.1415pi"))
parseFloatingConstantTest3 = TestCase (assertEqual "parseFloatingConstant \"4242.8484.foo\"" (Just ("4242.8484", 9, ".foo")) (parseFloatingConstant "4242.8484.foo"))
parseFloatingConstantTest4 = TestCase (assertEqual "parseFloatingConstant \"foo.bar\"" Nothing (parseFloatingConstant "foo.bar"))
parseFloatingConstantTest5 = TestCase (assertEqual "parseFloatingConstant \"21.\"" Nothing (parseFloatingConstant "21."))
parseFloatingConstantTest6 = TestCase (assertEqual "parseFloatingConstant \".84\"" Nothing (parseFloatingConstant ".84"))
parseFloatingConstantTest7 = TestCase (assertEqual "parseFloatingConstant \"just foo\"" Nothing (parseFloatingConstant "just foo"))
parseFloatingConstantTest8 = TestCase (assertEqual "parseFloatingConstant \"\"" Nothing (parseFloatingConstant ""))

testsParseDigitSequence = TestList      [
    parseDigitSequenceTest1, parseDigitSequenceTest2, parseDigitSequenceTest3
    , parseDigitSequenceTest4, parseDigitSequenceTest5, parseDigitSequenceTest6
    , parseDigitSequenceTest7, parseDigitSequenceTest8, parseDigitSequenceTest9
    , parseDigitSequenceTest10
                                        ]

parseDigitSequenceTest1 = TestCase (assertEqual "parseDigitSequence \"0\"" (Just ("0", 1, [])) (parseDigitSequence "0"))
parseDigitSequenceTest2 = TestCase (assertEqual "parseDigitSequence \"123\"" (Just ("123", 3, [])) (parseDigitSequence "123"))
parseDigitSequenceTest3 = TestCase (assertEqual "parseDigitSequence \"1 2\"" (Just ("1", 1, " 2")) (parseDigitSequence "1 2"))
parseDigitSequenceTest4 = TestCase (assertEqual "parseDigitSequence \"123 456\"" (Just ("123", 3, " 456")) (parseDigitSequence "123 456"))
parseDigitSequenceTest5 = TestCase (assertEqual "parseDigitSequence \"123456789123456789foo\"" (Just ("123456789123456789", 18, "foo")) (parseDigitSequence "123456789123456789foo"))
parseDigitSequenceTest6 = TestCase (assertEqual "parseDigitSequence \"42a42\"" (Just ("42", 2, "a42")) (parseDigitSequence "42a42"))
parseDigitSequenceTest7 = TestCase (assertEqual "parseDigitSequence \"foo\"" Nothing (parseDigitSequence "foo"))
parseDigitSequenceTest8 = TestCase (assertEqual "parseDigitSequence \"A\"" Nothing (parseDigitSequence "A"))
parseDigitSequenceTest9 = TestCase (assertEqual "parseDigitSequence \"\"" Nothing (parseDigitSequence ""))
parseDigitSequenceTest10 = TestCase (assertEqual "parseDigitSequence \"-1\"" (Just ("-1", 2, [])) (parseDigitSequence "-1"))

testsParseCharacterConstant = TestList  [
    parseCharacterConstantTest1, parseCharacterConstantTest2, parseCharacterConstantTest3
    , parseCharacterConstantTest4, parseCharacterConstantTest5, parseCharacterConstantTest6
    , parseCharacterConstantTest7, parseCharacterConstantTest8
                                        ]

parseCharacterConstantTest1 = TestCase (assertEqual "parseCharacterConstant \"'a'\"" (Just ('a', 3, [])) (parseCharacterConstant "'a'"))
parseCharacterConstantTest2 = TestCase (assertEqual "parseCharacterConstant \"'a'\"" (Just ('z', 3, " ")) (parseCharacterConstant "'z' "))
parseCharacterConstantTest3 = TestCase (assertEqual "parseCharacterConstant \"'a'\"" (Just ('A', 3, "foo")) (parseCharacterConstant "'A'foo"))
parseCharacterConstantTest4 = TestCase (assertEqual "parseCharacterConstant \"'a'\"" (Just ('Z', 3, " bar")) (parseCharacterConstant "'Z' bar"))
parseCharacterConstantTest5 = TestCase (assertEqual "parseCharacterConstant \"'foo'\"" Nothing (parseCharacterConstant "'foo'"))
parseCharacterConstantTest6 = TestCase (assertEqual "parseCharacterConstant \"'z\"" Nothing (parseCharacterConstant "'z"))
parseCharacterConstantTest7 = TestCase (assertEqual "parseCharacterConstant \"\"" Nothing (parseCharacterConstant ""))
parseCharacterConstantTest8 = TestCase (assertEqual "parseCharacterConstant \"''' failure\"" Nothing (parseCharacterConstant "''' failure"))

testsParseChar = TestList               [
    parseCharTest1, parseCharTest2, parseCharTest3
    , parseCharTest4, parseCharTest5, parseCharTest6
    , parseCharTest7, parseCharTest8, parseCharTest9
                                        ]

parseCharTest1 = TestCase (assertEqual "parseChar \"a\"" (Just ('a', 1, [])) (parseChar "a"))
parseCharTest2 = TestCase (assertEqual "parseChar \"z \"" (Just ('z', 1, " ")) (parseChar "z "))
parseCharTest3 = TestCase (assertEqual "parseChar \"AAA\"" (Just ('A', 1, "AA")) (parseChar "AAA"))
parseCharTest4 = TestCase (assertEqual "parseChar \"Z1\"" (Just ('Z', 1, "1")) (parseChar "Z1"))
parseCharTest5 = TestCase (assertEqual "parseSChar \"\"\"" (Just ('"', 1, [])) (parseChar "\""))
parseCharTest6 = TestCase (assertEqual "parseChar \"~home\"" (Just ('~', 1, "home")) (parseChar "~home"))
parseCharTest7 = TestCase (assertEqual "parseChar \"\'a\'\"" Nothing (parseChar "\'a\'"))
parseCharTest8 = TestCase (assertEqual "parseChar \"\\\"" Nothing (parseChar "\\"))
parseCharTest9 = TestCase (assertEqual "parseChar \"\"" Nothing (parseChar ""))

testsParseEscapeSequence = TestList     [
    parseEscapeSequenceTest1, parseEscapeSequenceTest2, parseEscapeSequenceTest3
    , parseEscapeSequenceTest4, parseEscapeSequenceTest5, parseEscapeSequenceTest6
    , parseEscapeSequenceTest7, parseEscapeSequenceTest8, parseEscapeSequenceTest9
    , parseEscapeSequenceTest10, parseEscapeSequenceTest11, parseEscapeSequenceTest12
    , parseEscapeSequenceTest13
                                        ]

parseEscapeSequenceTest1 = TestCase (assertEqual "parseEscapeSequence \" \"" (Just (' ', 1, [])) (parseEscapeSequence " "))
parseEscapeSequenceTest2 = TestCase (assertEqual "parseEscapeSequence \"\t\"" (Just ('\t', 1, [])) (parseEscapeSequence "\t"))
parseEscapeSequenceTest3 = TestCase (assertEqual "parseEscapeSequence \"\a\"" (Just ('\a', 1, [])) (parseEscapeSequence "\a"))
parseEscapeSequenceTest4 = TestCase (assertEqual "parseEscapeSequence \"\b\"" (Just ('\b', 1, [])) (parseEscapeSequence "\b"))
parseEscapeSequenceTest5 = TestCase (assertEqual "parseEscapeSequence \"\f\"" (Just ('\f', 1, [])) (parseEscapeSequence "\f"))
parseEscapeSequenceTest6 = TestCase (assertEqual "parseEscapeSequence \"\n\"" (Just ('\n', 1, [])) (parseEscapeSequence "\n"))
parseEscapeSequenceTest7 = TestCase (assertEqual "parseEscapeSequence \"\r\"" (Just ('\r', 1, [])) (parseEscapeSequence "\r"))
parseEscapeSequenceTest8 = TestCase (assertEqual "parseEscapeSequence \"\v\"" (Just ('\v', 1, [])) (parseEscapeSequence "\v"))
parseEscapeSequenceTest9 = TestCase (assertEqual "parseEscapeSequence \"\\\"" (Just ('\\', 1, [])) (parseEscapeSequence "\\"))
parseEscapeSequenceTest10 = TestCase (assertEqual "parseEscapeSequence \"\'x\"" (Just ('\'', 1, "x")) (parseEscapeSequence "\'x"))
parseEscapeSequenceTest11 = TestCase (assertEqual "parseEscapeSequence \"\" foo\"" (Just ('\"', 1, " foo")) (parseEscapeSequence "\" foo"))
parseEscapeSequenceTest12 = TestCase (assertEqual "parseEscapeSequence \"foo\"" Nothing (parseEscapeSequence "foo"))
parseEscapeSequenceTest13 = TestCase (assertEqual "parseEscapeSequence \"\"" Nothing (parseEscapeSequence ""))

testsParseIdentifier = TestList         [
    parseIdentifierTest1, parseIdentifierTest2, parseIdentifierTest3
    , parseIdentifierTest4, parseIdentifierTest5, parseIdentifierTest6
    , parseIdentifierTest7
                                        ]

parseIdentifierTest1 = TestCase (assertEqual "parseIdentifier \"foo\"" (Just ("foo", 3, [])) (parseIdentifier "foo"))
parseIdentifierTest2 = TestCase (assertEqual "parseIdentifier \"Bar baz\"" (Just ("Bar", 3, " baz")) (parseIdentifier "Bar baz"))
parseIdentifierTest3 = TestCase (assertEqual "parseIdentifier \"_\"" (Just ("_", 1, "")) (parseIdentifier "_"))
parseIdentifierTest4 = TestCase (assertEqual "parseIdentifier \"fn name\"" (Just ("fn", 2, " name")) (parseIdentifier "fn name"))
parseIdentifierTest5 = TestCase (assertEqual "parseIdentifier \"foo1bar2 \"" (Just ("foo1bar2", 8, " ")) (parseIdentifier "foo1bar2 "))
parseIdentifierTest6 = TestCase (assertEqual "parseIdentifier \"1foo\"" Nothing (parseIdentifier "1foo"))
parseIdentifierTest7 = TestCase (assertEqual "parseIdentifier \"\"" Nothing (parseIdentifier ""))

testsParseNonDigit = TestList           [
    parseNonDigitTest1, parseNonDigitTest2, parseNonDigitTest3
    , parseNonDigitTest4, parseNonDigitTest5, parseNonDigitTest6
    , parseNonDigitTest7, parseNonDigitTest8, parseNonDigitTest9
                                        ]

parseNonDigitTest1 = TestCase (assertEqual "parseNonDigit \"_\"" (Just ('_', 1, [])) (parseNonDigit "_"))
parseNonDigitTest2 = TestCase (assertEqual "parseNonDigit \"abc\"" (Just ('a', 1, "bc")) (parseNonDigit "abc"))
parseNonDigitTest3 = TestCase (assertEqual "parseNonDigit \"z$\"" (Just ('z', 1, "$")) (parseNonDigit "z$"))
parseNonDigitTest4 = TestCase (assertEqual "parseNonDigit \"A\"" (Just ('A', 1, "")) (parseNonDigit "A"))
parseNonDigitTest5 = TestCase (assertEqual "parseNonDigit \"Zircon\"" (Just ('Z', 1, "ircon")) (parseNonDigit "Zircon"))
parseNonDigitTest6 = TestCase (assertEqual "parseNonDigit \"@foo\"" Nothing (parseNonDigit "@foo"))
parseNonDigitTest7 = TestCase (assertEqual "parseNonDigit \"42\"" Nothing (parseNonDigit "42"))
parseNonDigitTest8 = TestCase (assertEqual "parseNonDigit \"-\"" Nothing (parseNonDigit "-"))
parseNonDigitTest9 = TestCase (assertEqual "parseNonDigit \"\"" Nothing (parseNonDigit ""))

testsParseDigit = TestList              [
    parseDigitTest1, parseDigitTest2, parseDigitTest3
    , parseDigitTest4, parseDigitTest5, parseDigitTest6
    , parseDigitTest7, parseDigitTest8, parseDigitTest9
    , parseDigitTest10, parseDigitTest11, parseDigitTest12
                                        ]

parseDigitTest1 = TestCase (assertEqual "parseDigit \"0\"" (Just ('0', 1, [])) (parseDigit "0"))
parseDigitTest2 = TestCase (assertEqual "parseDigit \"1foo\"" (Just ('1', 1, "foo")) (parseDigit "1foo"))
parseDigitTest3 = TestCase (assertEqual "parseDigit \"2 bar\"" (Just ('2', 1, " bar")) (parseDigit "2 bar"))
parseDigitTest4 = TestCase (assertEqual "parseDigit \"3    baz\"" (Just ('3', 1, "    baz")) (parseDigit "3    baz"))
parseDigitTest5 = TestCase (assertEqual "parseDigit \"4\"" (Just ('4', 1, [])) (parseDigit "4"))
parseDigitTest6 = TestCase (assertEqual "parseDigit \"5\"" (Just ('5', 1, [])) (parseDigit "5"))
parseDigitTest7 = TestCase (assertEqual "parseDigit \"6\"" (Just ('6', 1, [])) (parseDigit "6"))
parseDigitTest8 = TestCase (assertEqual "parseDigit \"7\"" (Just ('7', 1, [])) (parseDigit "7"))
parseDigitTest9 = TestCase (assertEqual "parseDigit \"8\"" (Just ('8', 1, [])) (parseDigit "8"))
parseDigitTest10 = TestCase (assertEqual "parseDigit \"9\"" (Just ('9', 1, [])) (parseDigit "9"))
parseDigitTest11 = TestCase (assertEqual "parseDigit \"foo\"" Nothing (parseDigit "foo"))
parseDigitTest12 = TestCase (assertEqual "parseDigit \"\"" Nothing (parseDigit ""))
