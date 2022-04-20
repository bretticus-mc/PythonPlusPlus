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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 59 "parser.ml"
let yytransl_const = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* BOOL *);
  260 (* STRING *);
  261 (* ASSIGN *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* MULT *);
  265 (* DIV *);
  266 (* EQ *);
  267 (* NOT_EQ *);
  268 (* LT *);
  269 (* GT *);
  270 (* AND *);
  271 (* OR *);
  272 (* NOT *);
  273 (* DOT *);
  274 (* PLUS_EQ *);
  275 (* MINUS_EQ *);
  276 (* MULT_EQ *);
  277 (* DIV_EQ *);
  278 (* EXCLAMATION *);
  279 (* EQ_COMPARISON *);
  280 (* NOTEQUAL *);
  281 (* IN *);
  282 (* COLON *);
  283 (* IF *);
  284 (* ELSE *);
  285 (* WHILE *);
  286 (* FOR *);
  287 (* DEF *);
  288 (* RETURN *);
  289 (* COMMA *);
  290 (* NEWLINE *);
  291 (* SEMI *);
  292 (* LPAREN *);
  293 (* RPAREN *);
  294 (* LBRACE *);
  295 (* RBRACE *);
  296 (* TRUE *);
  297 (* FALSE *);
  298 (* NONE *);
  299 (* MOD *);
  300 (* RANGLE *);
  301 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  302 (* FLOAT_LITERAL *);
  303 (* INT_LITERAL *);
  304 (* BLIT *);
  305 (* ID *);
  306 (* STRING_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\006\000\006\000\006\000\
\006\000\006\000\003\000\007\000\007\000\009\000\009\000\008\000\
\008\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\011\000\000\000\001\000\001\000\003\000\000\000\
\002\000\002\000\002\000\003\000\011\000\007\000\003\000\003\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\005\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\025\000\027\000\000\000\028\000\048\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\003\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000\019\000\000\000\000\000\000\000\023\000\024\000\
\041\000\017\000\020\000\000\000\006\000\008\000\007\000\009\000\
\010\000\000\000\000\000\000\000\045\000\030\000\031\000\032\000\
\033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\000\000\000\000\043\000\
\000\000\000\000\000\000\000\000\000\000\000\000\047\000\000\000\
\000\000\005\000\015\000\000\000\000\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000\011\000"

let yydgoto = "\002\000\
\014\000\015\000\016\000\017\000\075\000\058\000\076\000\025\000\
\077\000\018\000\060\000\061\000"

let yysindex = "\002\000\
\107\255\000\000\225\254\241\254\237\254\039\255\039\255\131\255\
\000\000\000\000\000\000\017\255\000\000\000\000\032\000\107\255\
\107\255\084\000\039\255\039\255\255\254\097\000\059\255\131\255\
\253\254\039\255\007\255\039\255\000\000\000\000\000\000\039\255\
\039\255\039\255\039\255\039\255\039\255\039\255\039\255\039\255\
\039\255\000\000\000\000\096\255\109\255\244\254\000\000\000\000\
\000\000\000\000\000\000\176\255\000\000\000\000\000\000\000\000\
\000\000\029\255\011\255\009\255\000\000\000\000\000\000\000\000\
\000\000\207\255\119\255\119\255\198\255\189\255\207\255\025\255\
\038\255\043\255\045\255\042\255\000\000\039\255\039\255\000\000\
\049\255\056\255\007\255\244\254\031\255\176\255\000\000\131\255\
\131\255\000\000\000\000\007\255\065\255\000\000\068\255\069\255\
\064\255\072\255\131\255\131\255\078\255\000\000\000\000"

let yyrindex = "\000\000\
\099\000\000\000\000\000\000\000\000\000\000\000\000\000\074\255\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\099\000\
\099\000\000\000\000\000\000\000\000\000\000\000\000\000\229\254\
\000\000\000\000\000\000\077\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\092\255\000\000\000\000\
\000\000\000\000\000\000\031\000\000\000\000\000\000\000\000\000\
\000\000\000\000\093\255\000\000\000\000\000\000\000\000\000\000\
\000\000\038\000\006\000\033\000\045\000\047\000\040\000\000\000\
\000\000\000\000\094\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\052\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\101\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\254\255\000\000\248\255\000\000\177\255\000\000\234\255\
\053\000\022\000\000\000\061\000"

let yytablesize = 387
let yytable = "\024\000\
\029\000\050\000\001\000\090\000\019\000\036\000\016\000\053\000\
\054\000\055\000\056\000\016\000\095\000\030\000\031\000\024\000\
\032\000\033\000\034\000\035\000\020\000\036\000\037\000\038\000\
\039\000\040\000\026\000\022\000\023\000\021\000\040\000\029\000\
\037\000\041\000\046\000\051\000\074\000\035\000\078\000\034\000\
\044\000\045\000\027\000\079\000\038\000\080\000\039\000\052\000\
\057\000\059\000\081\000\042\000\028\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\082\000\
\032\000\033\000\034\000\035\000\083\000\036\000\037\000\038\000\
\039\000\040\000\007\000\092\000\101\000\084\000\085\000\093\000\
\094\000\041\000\088\000\043\000\009\000\010\000\011\000\012\000\
\013\000\089\000\024\000\102\000\096\000\097\000\098\000\049\000\
\048\000\099\000\002\000\086\000\059\000\032\000\033\000\034\000\
\035\000\100\000\036\000\037\000\038\000\039\000\040\000\103\000\
\016\000\044\000\032\000\033\000\034\000\035\000\041\000\036\000\
\037\000\038\000\039\000\040\000\032\000\033\000\034\000\035\000\
\012\000\046\000\014\000\041\000\072\000\003\000\016\000\004\000\
\091\000\005\000\006\000\087\000\000\000\000\000\007\000\000\000\
\008\000\073\000\000\000\000\000\000\000\000\000\000\000\000\000\
\009\000\010\000\011\000\012\000\013\000\003\000\000\000\004\000\
\000\000\000\000\006\000\000\000\000\000\000\000\007\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\009\000\010\000\011\000\012\000\013\000\032\000\033\000\034\000\
\035\000\000\000\036\000\037\000\038\000\039\000\040\000\000\000\
\000\000\000\000\032\000\033\000\034\000\035\000\041\000\036\000\
\037\000\038\000\039\000\032\000\033\000\034\000\035\000\000\000\
\036\000\037\000\038\000\041\000\032\000\033\000\034\000\035\000\
\000\000\000\000\037\000\038\000\041\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\029\000\
\029\000\029\000\000\000\029\000\029\000\029\000\029\000\029\000\
\036\000\036\000\036\000\036\000\036\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\036\000\000\000\000\000\000\000\
\000\000\029\000\029\000\000\000\000\000\029\000\036\000\036\000\
\000\000\000\000\036\000\037\000\037\000\037\000\037\000\037\000\
\035\000\000\000\034\000\035\000\035\000\034\000\034\000\037\000\
\000\000\000\000\038\000\038\000\035\000\039\000\034\000\040\000\
\040\000\037\000\037\000\040\000\000\000\037\000\035\000\035\000\
\034\000\034\000\035\000\000\000\034\000\038\000\038\000\039\000\
\039\000\038\000\000\000\039\000\042\000\042\000\000\000\000\000\
\042\000\032\000\033\000\034\000\035\000\000\000\036\000\037\000\
\038\000\039\000\040\000\000\000\000\000\000\000\032\000\033\000\
\034\000\035\000\041\000\036\000\037\000\038\000\039\000\040\000\
\000\000\000\000\000\000\000\000\000\000\042\000\000\000\041\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000"

let yycheck = "\008\000\
\000\000\024\000\001\000\083\000\036\001\000\000\034\001\001\001\
\002\001\003\001\004\001\039\001\092\000\016\000\017\000\024\000\
\006\001\007\001\008\001\009\001\036\001\011\001\012\001\013\001\
\014\001\015\001\010\001\006\000\007\000\049\001\000\000\000\000\
\000\000\023\001\036\001\039\001\049\001\000\000\010\001\000\000\
\019\000\020\000\026\001\033\001\000\000\037\001\000\000\026\000\
\042\001\028\000\026\001\000\000\036\001\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\026\001\
\006\001\007\001\008\001\009\001\026\001\011\001\012\001\013\001\
\014\001\015\001\036\001\045\001\099\000\033\001\037\001\088\000\
\089\000\023\001\034\001\000\000\046\001\047\001\048\001\049\001\
\050\001\034\001\099\000\100\000\028\001\026\001\026\001\037\001\
\000\000\034\001\000\000\078\000\079\000\006\001\007\001\008\001\
\009\001\034\001\011\001\012\001\013\001\014\001\015\001\034\001\
\039\001\037\001\006\001\007\001\008\001\009\001\023\001\011\001\
\012\001\013\001\014\001\015\001\006\001\007\001\008\001\009\001\
\037\001\037\001\037\001\023\001\037\001\027\001\034\001\029\001\
\084\000\031\001\032\001\079\000\255\255\255\255\036\001\255\255\
\038\001\037\001\255\255\255\255\255\255\255\255\255\255\255\255\
\046\001\047\001\048\001\049\001\050\001\027\001\255\255\029\001\
\255\255\255\255\032\001\255\255\255\255\255\255\036\001\255\255\
\038\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\046\001\047\001\048\001\049\001\050\001\006\001\007\001\008\001\
\009\001\255\255\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\006\001\007\001\008\001\009\001\023\001\011\001\
\012\001\013\001\014\001\006\001\007\001\008\001\009\001\255\255\
\011\001\012\001\013\001\023\001\006\001\007\001\008\001\009\001\
\255\255\255\255\012\001\013\001\023\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\006\001\007\001\
\008\001\009\001\255\255\011\001\012\001\013\001\014\001\015\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\023\001\
\255\255\255\255\255\255\255\255\023\001\255\255\255\255\255\255\
\255\255\033\001\034\001\255\255\255\255\037\001\033\001\034\001\
\255\255\255\255\037\001\011\001\012\001\013\001\014\001\015\001\
\011\001\255\255\011\001\014\001\015\001\014\001\015\001\023\001\
\255\255\255\255\014\001\015\001\023\001\015\001\023\001\033\001\
\034\001\033\001\034\001\037\001\255\255\037\001\033\001\034\001\
\033\001\034\001\037\001\255\255\037\001\033\001\034\001\033\001\
\034\001\037\001\255\255\037\001\033\001\034\001\255\255\255\255\
\037\001\006\001\007\001\008\001\009\001\255\255\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\255\255\006\001\007\001\
\008\001\009\001\023\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\034\001\255\255\023\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\034\001"

let yynames_const = "\
  INT\000\
  FLOAT\000\
  BOOL\000\
  STRING\000\
  ASSIGN\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  EQ\000\
  NOT_EQ\000\
  LT\000\
  GT\000\
  AND\000\
  OR\000\
  NOT\000\
  DOT\000\
  PLUS_EQ\000\
  MINUS_EQ\000\
  MULT_EQ\000\
  DIV_EQ\000\
  EXCLAMATION\000\
  EQ_COMPARISON\000\
  NOTEQUAL\000\
  IN\000\
  COLON\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  DEF\000\
  RETURN\000\
  COMMA\000\
  NEWLINE\000\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  TRUE\000\
  FALSE\000\
  NONE\000\
  MOD\000\
  RANGLE\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  FLOAT_LITERAL\000\
  INT_LITERAL\000\
  BLIT\000\
  ID\000\
  STRING_LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 41 "parser.mly"
            ( _1 )
# 353 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                ( ([]) )
# 359 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 47 "parser.mly"
               ( (Func_def _1)::_2 )
# 367 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 48 "parser.mly"
              ( (Stmt _1)::_2 )
# 375 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 53 "parser.mly"
               ( (_1, _3) )
# 383 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
          ( Int   )
# 389 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
          ( Bool  )
# 395 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
           ( Float )
# 401 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
           ( String )
# 407 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
         ( None )
# 413 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 67 "parser.mly"
  (
    {
      rtyp= _7;
      fname= _2;
      formals= _4; 
      body= _10 
    }
  )
# 430 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
              ( [] )
# 436 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 79 "parser.mly"
                 ( _1 )
# 443 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 83 "parser.mly"
        ( [_1] )
# 450 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 84 "parser.mly"
                             ( _1::_3 )
# 458 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                ( [] )
# 464 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 88 "parser.mly"
                    ( _1::_2 )
# 472 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                                            ( Expr _1 )
# 479 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                                            ( Expr _1 )
# 486 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 93 "parser.mly"
                                            ( Block _2 )
# 493 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'stmt) in
    let _11 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "parser.mly"
                                                                        ( If(_3, _7, _11) )
# 502 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 96 "parser.mly"
                                                ( While (_3, _7)  )
# 510 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                                              ( Return _2   )
# 517 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                                           ( Return _2      )
# 524 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 103 "parser.mly"
                     ( Literal(_1)            )
# 531 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
                     ( FloatLit(_1)           )
# 538 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 105 "parser.mly"
                     ( BoolLit(_1)            )
# 545 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "parser.mly"
                     ( StringLit(_1) )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "parser.mly"
                     ( Id(_1)                 )
# 559 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 567 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 575 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( Binop(_1, Mult,   _3)   )
# 583 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 591 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                            ( Binop(_1, Eq_Compar, _3)   )
# 599 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                        ( Binop(_1, Neq, _3)     )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 615 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                     ( Binop(_1, Greater,  _3)   )
# 623 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 631 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 639 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                 ( Assign(_1, _3)         )
# 647 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                       ( _2                   )
# 654 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                         ( VariableInit(_1, _3, _5) )
# 663 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 122 "parser.mly"
                              ( Call (_1, _3)  )
# 671 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
              ( [] )
# 677 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 127 "parser.mly"
         ( _1 )
# 684 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
        ( [_1] )
# 691 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 131 "parser.mly"
                    ( _1::_3 )
# 699 "parser.ml"
               : 'args))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
