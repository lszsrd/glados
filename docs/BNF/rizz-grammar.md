\<function-decl>            ::= "fn" \<s-char-sequence> "(" { \<parm-var-decl> } ")" { "->" \<builtin-type> } \<compound-statement>

\<parm-var-decl>            ::= \<builtin-type> ":" \<s-char-sequence>\
                            | \<parm-var-decl> "," \<parm-var-decl>

\<compound-stmt>            ::= "{" { \<compound-stmt-block> } "}"

\<compound-stmt-block>      ::= \<stmt>
                            | \<stmt> \<compound-stmt-block>

\<stmt>                     ::= \<decl-var-expr> ";"
                            | \<decl-stmt> ";"
                            | \<unary-operator> ";"
                            | \<binary-operator>
                            | "if" "(" \<binary-operator> ")" \<compound-statement> { "else" \<compound-statement> }
                            | "while" "(" \<binary-operator> ")" \<compound-statement>
                            | "for" "(" { decl-var-expr } ";" { \<binary-operator> } ";" \<decl-stmt> ")" \<compound-statement>
                            | "foreach" "(" \<s-char-sequence> ":" \<s-char-sequence> ")" \<compound-statement>
                            | "ret" \<binary-operator> ";"

\<decl-var-expr>            ::= \<builtin-type> \<s-char-sequence> \<assign-op> \<parm-call-decl>

\<decl-stmt>                ::= \<s-char-sequence> \<assign-op> \<parm-call-decl>
                            | \<unary-operator>

\<unary-operator>           ::= \<s-char-sequence> "++"
                            | \<s-char-sequence> "--"

\<binary-operator>          ::= \<binary-op-parm> \<binary-op> \<binary-op-parm>

\<binary-op-parm>           ::= \<parm-call-decl> | \<binary-operator>

\<parm-call-decl>           ::= \<constant> | \<s-char-sequence> | \<call-expr-decl>

\<call-expr-decl>           ::= \<s-char-sequence> "(" { \<call-expr-parm> | \<binary-operator>} ")"

\<call-expr-parm>           ::= \<parm-call-decl> | \<call-expr-parm> ","

\<var-decl>                 ::= \<builtin-type> \<s-char-sequence> \<assign-op> \<parm-call-decl>

\<assign-op>                ::= "=" | "*=" | "/=" | "%=" | "+=" | "-="

\<record-decl-expr>           ::= "struct" |           \<identifier> "{" { \<parm-var-decl-expr-block> } "}"