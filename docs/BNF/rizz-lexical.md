\<keyword>                  ::= \<builtin-type> | "fn" | "if" | "else" | "while" | "for" | "foreach" | "ret"

\<builtin-type>             ::= "Bool" | "Char" | "Int" | "Float" | "Double"

\<string-literal>           ::= "\"" "\""
                            | "\"" \<s-char-sequence> "\""

\<s-char-sequence>          ::= \<s-char>
                            | \<s-char-sequence> \<s-char>

\<s-char>                   ::= any member of the source character set except the double-quotation mark ("), backslash (\\), or new-line character

\<constant>                 ::= \<decimal-constant>
                            | \<floating-constant>
                            | \<character-constant>

\<decimal-constant>         ::= \<decimal-constant> \<digit>

\<nonzero-digit>            ::= "0" | \<digit>

\<floating-constant>        ::= \<digit-sequence> "." \<digit-sequence>

\<digit-sequence>           ::= \<digit>
                            | \<digit-sequence> \<digit>

\<character-constant>       ::= "'" \<char> "'"

\<char>                     ::= Any member of the source character set except the single quotation mark ('), backslash (\\), or new-line character

\<escape-sequence>          ::= " " | "\t" | "\a" | "\b" | "\f" | "\n" | "\r" | "\t" | "\v" | "\'" | "\"" | "\\\"

\<identifier>               ::= \<nondigit>
                            | \<identifier> \<nondigit>
                            | \<identifier> \<digit>

\<nondigit>                 ::= "_" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"

\<digit>                    ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

\<punctuator>               ::= "[" | "]" | "(" | ")" | "{" | "}" | "." | "->" | "++" | "--" | "*" | "+" | "-" | "/" | "%" | "<" | ">" | "<=" | ">=" | "==" | "!=" | "&&" | "||" | ":" | ";" | "," | "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "//" | "/*" | "*\"
