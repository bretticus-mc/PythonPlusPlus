type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | ASSIGN
  | MULT
  | DIV
  | EQ
  | NEQ
  | LT
  | GT
  | AND
  | OR
  | NOT
  | DOT
  | PLUS_EQ
  | MINUS_EQ
  | MULT_EQ
  | DIV_EQ
  | IF
  | ELSE
  | WHILE
  | INT
  | STRING
  | BOOL
  | REM
  | RANGLE
  | ARROW
  | EXCLAMATION
  | EQEQUAL
  | NOTEQUAL
  | TRUE
  | FALSE
  | NONE
  | COLON
  | DEF
  | FOR
  | IN
  | NEWLINE
  | RETURN
  | COMMA
  | FLOAT of (float)
  | INT_LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | STRING_LITERAL of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
