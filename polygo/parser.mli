type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | LBRACKET
  | RBRACKET
  | LLBRACKET
  | RRBRACKET
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | PLUSONE
  | MINUSONE
  | MODULUS
  | VB
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | RETURN
  | IF
  | ELSE
  | FOR
  | FOREACH
  | IN
  | WHILE
  | INT
  | FLOAT
  | BOOL
  | COMPLEX
  | POLY
  | STRING
  | VOID
  | INTLIT of (int)
  | FLOATLIT of (float)
  | ID of (string)
  | STRINGLIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
