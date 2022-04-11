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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 58 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* ASSIGN *);
  265 (* MULT *);
  266 (* DIV *);
  267 (* EQ *);
  268 (* NEQ *);
  269 (* LT *);
  270 (* GT *);
  271 (* AND *);
  272 (* OR *);
  273 (* NOT *);
  274 (* DOT *);
  275 (* PLUS_EQ *);
  276 (* MINUS_EQ *);
  277 (* MULT_EQ *);
  278 (* DIV_EQ *);
  279 (* IF *);
  280 (* ELSE *);
  281 (* WHILE *);
  282 (* INT *);
  283 (* STRING *);
  284 (* BOOL *);
  285 (* REM *);
  286 (* RANGLE *);
  287 (* ARROW *);
  288 (* EXCLAMATION *);
  289 (* EQEQUAL *);
  290 (* NOTEQUAL *);
  291 (* TRUE *);
  292 (* FALSE *);
  293 (* NONE *);
  294 (* COLON *);
  295 (* DEF *);
  296 (* FOR *);
  297 (* IN *);
  298 (* NEWLINE *);
  299 (* RETURN *);
  300 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  301 (* FLOAT *);
  302 (* INT_LITERAL *);
  303 (* BLIT *);
  304 (* ID *);
  305 (* STRING_LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\006\000\006\000\006\000\
\006\000\006\000\003\000\007\000\007\000\009\000\009\000\008\000\
\008\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\011\000\000\000\001\000\001\000\003\000\000\000\
\002\000\002\000\002\000\003\000\007\000\005\000\003\000\003\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\005\000\004\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\026\000\000\000\027\000\045\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\003\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\019\000\
\038\000\017\000\020\000\000\000\000\000\000\000\023\000\024\000\
\000\000\000\000\042\000\000\000\006\000\009\000\007\000\010\000\
\008\000\000\000\029\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\040\000\000\000\000\000\022\000\000\000\000\000\000\000\
\044\000\000\000\000\000\005\000\015\000\000\000\021\000\000\000\
\000\000\000\000\000\000\011\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000\016\000\070\000\058\000\071\000\020\000\
\072\000\017\000\050\000\051\000"

let yysindex = "\002\000\
\055\255\000\000\028\255\065\255\006\255\017\255\231\254\028\255\
\000\000\000\000\016\255\000\000\000\000\025\000\055\255\055\255\
\095\000\116\255\065\255\024\255\028\255\028\255\035\255\106\000\
\028\255\028\255\089\255\000\000\000\000\000\000\028\255\028\255\
\028\255\028\255\028\255\028\255\028\255\028\255\000\000\000\000\
\000\000\000\000\000\000\130\255\144\255\251\254\000\000\000\000\
\254\254\053\255\000\000\155\255\000\000\000\000\000\000\000\000\
\000\000\047\255\000\000\000\000\078\255\078\255\059\255\059\255\
\020\255\166\255\065\255\065\255\022\255\018\255\067\255\000\000\
\028\255\000\000\028\255\044\255\000\000\089\255\251\254\050\255\
\000\000\078\255\065\255\000\000\000\000\089\255\000\000\045\255\
\040\255\065\255\051\255\000\000"

let yyrindex = "\000\000\
\096\000\000\000\000\000\092\255\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\096\000\096\000\
\000\000\000\000\011\255\000\000\000\000\000\000\000\000\000\000\
\096\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\097\255\000\000\000\000\
\102\255\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\041\000\055\000\007\000\021\000\
\044\000\038\000\000\000\000\000\000\000\104\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\061\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\068\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\056\000\000\000\252\255\000\000\198\255\000\000\239\255\
\030\000\014\000\000\000\045\000"

let yytablesize = 404
let yytable = "\019\000\
\028\000\042\000\001\000\031\000\032\000\037\000\033\000\021\000\
\033\000\034\000\035\000\036\000\037\000\038\000\019\000\016\000\
\018\000\025\000\022\000\084\000\034\000\024\000\023\000\026\000\
\028\000\031\000\032\000\088\000\043\000\003\000\033\000\034\000\
\035\000\036\000\044\000\045\000\046\000\036\000\049\000\052\000\
\031\000\073\000\069\000\035\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\016\000\027\000\032\000\074\000\
\003\000\075\000\004\000\078\000\039\000\079\000\076\000\077\000\
\031\000\032\000\003\000\083\000\004\000\080\000\029\000\030\000\
\091\000\009\000\010\000\011\000\012\000\005\000\087\000\006\000\
\086\000\090\000\089\000\031\000\032\000\019\000\049\000\005\000\
\082\000\006\000\035\000\036\000\092\000\007\000\040\000\002\000\
\016\000\008\000\041\000\012\000\009\000\010\000\011\000\012\000\
\043\000\048\000\014\000\008\000\085\000\016\000\009\000\010\000\
\011\000\012\000\053\000\054\000\055\000\081\000\041\000\000\000\
\000\000\031\000\032\000\000\000\000\000\056\000\033\000\034\000\
\035\000\036\000\037\000\038\000\067\000\057\000\000\000\031\000\
\032\000\000\000\000\000\000\000\033\000\034\000\035\000\036\000\
\037\000\038\000\068\000\000\000\000\000\031\000\032\000\000\000\
\000\000\000\000\033\000\034\000\035\000\036\000\037\000\038\000\
\031\000\032\000\000\000\000\000\000\000\033\000\034\000\035\000\
\036\000\037\000\038\000\031\000\032\000\000\000\000\000\000\000\
\033\000\034\000\035\000\036\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\000\000\028\000\028\000\
\037\000\033\000\000\000\028\000\028\000\028\000\028\000\028\000\
\028\000\033\000\033\000\033\000\033\000\033\000\033\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
\034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
\036\000\000\000\028\000\031\000\028\000\000\000\035\000\037\000\
\033\000\037\000\033\000\031\000\031\000\036\000\000\000\031\000\
\031\000\032\000\035\000\035\000\000\000\000\000\034\000\039\000\
\034\000\032\000\032\000\000\000\000\000\032\000\032\000\039\000\
\039\000\000\000\000\000\039\000\039\000\000\000\000\000\036\000\
\000\000\036\000\031\000\000\000\031\000\035\000\000\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\032\000\000\000\032\000\000\000\031\000\032\000\039\000\000\000\
\039\000\033\000\034\000\035\000\036\000\037\000\038\000\031\000\
\032\000\000\000\000\000\000\000\033\000\034\000\035\000\036\000\
\037\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000"

let yycheck = "\004\000\
\000\000\019\000\001\000\006\001\007\001\000\000\000\000\002\001\
\011\001\012\001\013\001\014\001\015\001\016\001\019\000\005\001\
\003\000\002\001\002\001\078\000\000\000\008\000\048\001\008\001\
\000\000\006\001\007\001\086\000\005\001\002\001\011\001\012\001\
\013\001\014\001\021\000\022\000\002\001\000\000\025\000\026\000\
\000\000\044\001\048\001\000\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\042\001\038\001\000\000\003\001\
\002\001\011\001\004\001\038\001\000\000\044\001\067\000\068\000\
\006\001\007\001\002\001\024\001\004\001\003\001\015\000\016\000\
\090\000\046\001\047\001\048\001\049\001\023\001\083\000\025\001\
\031\001\042\001\038\001\006\001\007\001\090\000\073\000\023\001\
\075\000\025\001\013\001\014\001\042\001\039\001\000\000\000\000\
\005\001\043\001\003\001\003\001\046\001\047\001\048\001\049\001\
\003\001\000\000\003\001\043\001\079\000\042\001\046\001\047\001\
\048\001\049\001\026\001\027\001\028\001\073\000\003\001\255\255\
\255\255\006\001\007\001\255\255\255\255\037\001\011\001\012\001\
\013\001\014\001\015\001\016\001\003\001\045\001\255\255\006\001\
\007\001\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\003\001\255\255\255\255\006\001\007\001\255\255\
\255\255\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\006\001\007\001\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\006\001\007\001\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\255\255\006\001\007\001\
\003\001\003\001\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\011\001\012\001\013\001\014\001\015\001\016\001\003\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\255\255\255\255\255\255\
\003\001\255\255\042\001\003\001\044\001\255\255\003\001\042\001\
\042\001\044\001\044\001\011\001\012\001\016\001\255\255\015\001\
\016\001\003\001\015\001\016\001\255\255\255\255\042\001\003\001\
\044\001\011\001\012\001\255\255\255\255\015\001\016\001\011\001\
\012\001\255\255\255\255\015\001\016\001\255\255\255\255\042\001\
\255\255\044\001\042\001\255\255\044\001\042\001\255\255\044\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\042\001\255\255\044\001\255\255\006\001\007\001\042\001\255\255\
\044\001\011\001\012\001\013\001\014\001\015\001\016\001\006\001\
\007\001\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\042\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\042\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  PLUS\000\
  MINUS\000\
  ASSIGN\000\
  MULT\000\
  DIV\000\
  EQ\000\
  NEQ\000\
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
  IF\000\
  ELSE\000\
  WHILE\000\
  INT\000\
  STRING\000\
  BOOL\000\
  REM\000\
  RANGLE\000\
  ARROW\000\
  EXCLAMATION\000\
  EQEQUAL\000\
  NOTEQUAL\000\
  TRUE\000\
  FALSE\000\
  NONE\000\
  COLON\000\
  DEF\000\
  FOR\000\
  IN\000\
  NEWLINE\000\
  RETURN\000\
  COMMA\000\
  EOF\000\
  "

let yynames_block = "\
  FLOAT\000\
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
# 34 "parser.mly"
            ( _1 )
# 351 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                ( ([]) )
# 357 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 40 "parser.mly"
               ( (Func_def _1)::_2 )
# 365 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 41 "parser.mly"
              ( (Stmt _1)::_2 )
# 373 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 46 "parser.mly"
               ( (_1, _3) )
# 381 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
          ( Int   )
# 387 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
          ( Bool  )
# 393 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 51 "parser.mly"
           ( Float )
# 400 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
           ( String )
# 406 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
         ( None )
# 412 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 60 "parser.mly"
  (
    {
      rtyp= _7;
      fname= _2;
      formals= _4; 
      body= _10 
    }
  )
# 429 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
              ( [] )
# 435 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 72 "parser.mly"
                 ( _1 )
# 442 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 76 "parser.mly"
        ( [_1] )
# 449 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 77 "parser.mly"
                             ( _1::_3 )
# 457 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                ( [] )
# 463 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 81 "parser.mly"
                    ( _1::_2 )
# 471 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                                            ( Expr _1 )
# 478 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                                            ( Expr _1 )
# 485 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 86 "parser.mly"
                                            ( Block _2 )
# 492 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 89 "parser.mly"
                                            ( If(_3, _5, _7) )
# 501 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 90 "parser.mly"
                                            ( While (_3, _5)  )
# 509 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                                               ( Return _2      )
# 516 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                                           ( Return _2      )
# 523 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 97 "parser.mly"
                     ( Literal(_1)            )
# 530 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 98 "parser.mly"
                     ( BoolLit(_1)            )
# 537 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "parser.mly"
                     ( StringLit(_1) )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
                     ( Id(_1)                 )
# 551 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 559 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 567 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 575 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( Binop(_1, Neq, _3)     )
# 583 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 591 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( Binop(_1, Greater,  _3)   )
# 599 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 615 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( Assign(_1, _3)         )
# 623 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                       ( _2                   )
# 630 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                         ( VariableInit(_1, _3, _5) )
# 639 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 113 "parser.mly"
                              ( Call (_1, _3)  )
# 647 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
              ( [] )
# 653 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 118 "parser.mly"
         ( _1 )
# 660 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
        ( [_1] )
# 667 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 122 "parser.mly"
                    ( _1::_3 )
# 675 "parser.ml"
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
