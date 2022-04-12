type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEG
  | GT
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | COMMA
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "microcparse.mly"
open Ast
# 39 "microcparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIVIDE *);
  266 (* ASSIGN *);
  267 (* NOT *);
  268 (* EQ *);
  269 (* NEQ *);
  270 (* LT *);
  271 (* LEG *);
  272 (* GT *);
  273 (* AND *);
  274 (* OR *);
  275 (* RETURN *);
  276 (* IF *);
  277 (* ELSE *);
  278 (* FOR *);
  279 (* WHILE *);
  280 (* INT *);
  281 (* BOOL *);
  282 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  283 (* LITERAL *);
  284 (* BLIT *);
  285 (* ID *);
  286 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\004\000\007\000\007\000\009\000\009\000\008\000\008\000\
\010\000\010\000\010\000\010\000\010\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\012\000\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\008\000\000\000\001\000\001\000\003\000\000\000\002\000\
\002\000\003\000\007\000\005\000\003\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\039\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\004\000\007\000\003\000\000\000\
\000\000\012\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\023\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\016\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\018\000\
\021\000\000\000\000\000\000\000\000\000\036\000\000\000\025\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\034\000\000\000\020\000\038\000\000\000\019\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\024\000\009\000\017\000\034\000\
\018\000\035\000\036\000\061\000\062\000"

let yysindex = "\002\000\
\030\255\000\000\000\000\000\000\000\000\005\000\076\255\030\255\
\236\254\000\000\030\255\030\255\000\000\000\000\000\000\013\255\
\038\255\000\000\030\255\039\255\000\000\030\255\031\255\017\255\
\030\255\033\255\017\255\033\255\049\255\051\255\000\000\000\000\
\028\255\054\255\017\255\131\255\000\000\158\255\059\255\145\255\
\033\255\033\255\033\255\033\255\000\000\000\000\000\000\033\255\
\033\255\033\255\033\255\033\255\033\255\033\255\000\000\000\000\
\000\000\171\255\184\255\113\255\068\255\000\000\193\255\000\000\
\000\000\006\255\006\255\078\255\122\255\202\255\017\255\017\255\
\033\255\000\000\072\255\000\000\000\000\017\255\000\000"

let yyrindex = "\000\000\
\090\000\000\000\000\000\000\000\000\000\000\000\000\000\090\000\
\000\000\000\000\090\000\094\255\000\000\000\000\000\000\096\255\
\000\000\000\000\000\000\000\000\000\000\029\255\000\000\097\255\
\029\255\000\000\097\255\000\000\000\000\000\000\000\000\000\000\
\062\255\000\000\097\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\104\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\107\255\000\000\000\000\003\255\000\000\
\000\000\088\255\091\255\069\255\095\255\115\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\245\255\000\000\086\000\000\000\000\000\015\000\
\096\000\251\255\230\255\000\000\049\000"

let yytablesize = 219
let yytable = "\038\000\
\016\000\040\000\001\000\032\000\010\000\032\000\013\000\016\000\
\014\000\015\000\023\000\048\000\049\000\023\000\058\000\059\000\
\060\000\063\000\026\000\052\000\027\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\032\000\043\000\005\000\025\000\
\005\000\005\000\026\000\028\000\029\000\044\000\019\000\030\000\
\020\000\039\000\022\000\031\000\032\000\033\000\060\000\005\000\
\005\000\046\000\041\000\005\000\042\000\003\000\004\000\005\000\
\005\000\005\000\045\000\031\000\032\000\033\000\024\000\056\000\
\024\000\075\000\076\000\024\000\024\000\029\000\074\000\029\000\
\079\000\024\000\024\000\024\000\011\000\012\000\024\000\024\000\
\029\000\029\000\029\000\048\000\049\000\029\000\029\000\024\000\
\027\000\002\000\027\000\028\000\078\000\028\000\029\000\030\000\
\011\000\030\000\013\000\027\000\027\000\015\000\028\000\028\000\
\027\000\027\000\035\000\028\000\028\000\037\000\037\000\030\000\
\030\000\027\000\021\000\031\000\028\000\031\000\048\000\049\000\
\030\000\077\000\000\000\000\000\050\000\051\000\052\000\048\000\
\049\000\053\000\054\000\047\000\031\000\050\000\051\000\052\000\
\048\000\049\000\073\000\000\000\031\000\000\000\050\000\051\000\
\052\000\057\000\000\000\053\000\054\000\000\000\048\000\049\000\
\000\000\000\000\000\000\000\000\050\000\051\000\052\000\000\000\
\055\000\053\000\054\000\048\000\049\000\000\000\000\000\000\000\
\000\000\050\000\051\000\052\000\000\000\071\000\053\000\054\000\
\048\000\049\000\000\000\000\000\000\000\000\000\050\000\051\000\
\052\000\000\000\072\000\053\000\054\000\048\000\049\000\000\000\
\000\000\000\000\000\000\050\000\051\000\052\000\048\000\049\000\
\053\000\054\000\000\000\000\000\050\000\051\000\052\000\048\000\
\049\000\053\000\054\000\000\000\000\000\050\000\051\000\052\000\
\000\000\000\000\053\000"

let yycheck = "\026\000\
\012\000\028\000\001\000\001\001\000\000\003\001\008\000\019\000\
\029\001\011\000\022\000\006\001\007\001\025\000\041\000\042\000\
\043\000\044\000\002\001\014\001\004\001\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\026\001\002\001\002\001\001\001\
\004\001\005\001\002\001\019\001\020\001\010\001\026\001\023\001\
\003\001\027\000\004\001\027\001\028\001\029\001\073\000\019\001\
\020\001\035\000\002\001\023\001\002\001\024\001\025\001\027\001\
\028\001\029\001\005\001\027\001\028\001\029\001\001\001\005\001\
\003\001\071\000\072\000\006\001\007\001\001\001\003\001\003\001\
\078\000\012\001\013\001\014\001\001\001\002\001\017\001\018\001\
\012\001\013\001\014\001\006\001\007\001\017\001\018\001\026\001\
\001\001\000\000\003\001\001\001\021\001\003\001\026\001\001\001\
\003\001\003\001\003\001\012\001\013\001\005\001\012\001\013\001\
\017\001\018\001\003\001\017\001\018\001\003\001\025\000\017\001\
\018\001\026\001\019\000\001\001\026\001\003\001\006\001\007\001\
\026\001\073\000\255\255\255\255\012\001\013\001\014\001\006\001\
\007\001\017\001\018\001\001\001\018\001\012\001\013\001\014\001\
\006\001\007\001\026\001\255\255\026\001\255\255\012\001\013\001\
\014\001\001\001\255\255\017\001\018\001\255\255\006\001\007\001\
\255\255\255\255\255\255\255\255\012\001\013\001\014\001\255\255\
\003\001\017\001\018\001\006\001\007\001\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\255\255\003\001\017\001\018\001\
\006\001\007\001\255\255\255\255\255\255\255\255\012\001\013\001\
\014\001\255\255\003\001\017\001\018\001\006\001\007\001\255\255\
\255\255\255\255\255\255\012\001\013\001\014\001\006\001\007\001\
\017\001\018\001\255\255\255\255\012\001\013\001\014\001\006\001\
\007\001\017\001\018\001\255\255\255\255\012\001\013\001\014\001\
\255\255\255\255\017\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEG\000\
  GT\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  COMMA\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "microcparse.mly"
            ( _1)
# 240 "microcparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "microcparse.mly"
                 ( ([], [])               )
# 246 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 36 "microcparse.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 254 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 37 "microcparse.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 262 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "microcparse.mly"
              ( [] )
# 268 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 41 "microcparse.mly"
                           (  _1 :: _3 )
# 276 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "microcparse.mly"
         ( (_1, _2) )
# 284 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "microcparse.mly"
          ( Int   )
# 290 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "microcparse.mly"
          ( Bool  )
# 296 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 54 "microcparse.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      locals=_6;
      body=_7
    }
  )
# 314 "microcparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "microcparse.mly"
              ( [] )
# 320 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 67 "microcparse.mly"
                 ( _1 )
# 327 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 70 "microcparse.mly"
        ( [_1] )
# 334 "microcparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 71 "microcparse.mly"
                             ( _1::_3 )
# 342 "microcparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "microcparse.mly"
                ( [] )
# 348 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 75 "microcparse.mly"
                    ( _1::_2 )
# 356 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 78 "microcparse.mly"
                                            ( Expr _1      )
# 363 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 79 "microcparse.mly"
                                            ( Block _2 )
# 370 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "microcparse.mly"
                                            ( If(_3, _5, _7) )
# 379 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "microcparse.mly"
                                            ( While (_3, _5)  )
# 387 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "microcparse.mly"
                                            ( Return _2      )
# 394 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "microcparse.mly"
                     ( Literal(_1)            )
# 401 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 89 "microcparse.mly"
                     ( BoolLit(_1)            )
# 408 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "microcparse.mly"
                     ( Id(_1)                 )
# 415 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "microcparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 423 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "microcparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 431 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "microcparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 439 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "microcparse.mly"
                     ( Binop(_1, Neq, _3)     )
# 447 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "microcparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 455 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "microcparse.mly"
                     ( Binop(_1, And,   _3)   )
# 463 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "microcparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 471 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "microcparse.mly"
                     ( Assign(_1, _3)         )
# 479 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 99 "microcparse.mly"
                       ( _2                   )
# 486 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 101 "microcparse.mly"
                              ( Call (_1, _3)  )
# 494 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "microcparse.mly"
              ( [] )
# 500 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 106 "microcparse.mly"
         ( _1 )
# 507 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "microcparse.mly"
        ( [_1] )
# 514 "microcparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 110 "microcparse.mly"
                    ( _1::_3 )
# 522 "microcparse.ml"
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
