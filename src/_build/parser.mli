type token =
  | INT
  | FLOAT
  | BOOL
  | STRING
  | ASSIGN
  | PLUS
  | MINUS
  | MULT
  | DIV
  | EQ
  | NOT_EQ
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
  | EXCLAMATION
  | EQ_COMPARISON
  | NOTEQUAL
  | IN
  | COLON
  | IF
  | ELSE
  | WHILE
  | FOR
  | DEF
  | RETURN
  | COMMA
  | NEWLINE
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | TRUE
  | FALSE
  | NONE
  | MOD
  | RANGLE
  | ARROW
  | FLOAT_LITERAL of (string)
  | INT_LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | STRING_LITERAL of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
