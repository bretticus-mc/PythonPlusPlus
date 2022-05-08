%{
open Ast
%}

/* Types */
%token INT FLOAT BOOL STRING

/* Operators */
%token PLUS MINUS MULT DIV 

/* Comparators */
%token EQ NOT_EQ LT GT AND OR NOT DOT PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ EXCLAMATION EQ_COMPARISON NOTEQUAL IN COLON

%token IF ELSE WHILE FOR DEF RETURN COMMA NEWLINE
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token TRUE FALSE NONE DEDENT INDENT
%token MOD RANGLE ARROW LIST

%token <string> FLOAT_LITERAL
%token <int> INT_LITERAL
%token <bool> BLIT
%token <string> ID
%token <string> STRING_LITERAL
%token EOF

%start program
%type <Ast.program> program

%right EQ
%left OR
%left AND
%left EQ_COMPARISON NOT_EQ
%left LT GT
%left PLUS MINUS MULT DIV

%%

/* add function declarations*/
/* Returns List of Declarations */
program:
  decls EOF { $1 }

/* Returns Function Declarations and Statement Declarations */
/* WHERE ARE WE USING VDECL HERE? */
decls:
  /* nothing */ { ([]) }
 | fdecl decls { (Func_def $1)::$2 }
 | stmt decls { (Stmt $1)::$2 }


/* Variable Declaraiton */
vdecl:
  ID COLON typ { ($1, $3) } /* x: int = 50 */

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT  { Float }
  | STRING { String }
  | NONE { None }
  | LIST LBRACKET typ COMMA INT_LITERAL RBRACKET  { Array($3, $5) }

/* fdecl 
def main(x: int, y: int) -> None:
*/
fdecl:
  DEF ID LPAREN formals_opt RPAREN ARROW typ COLON NEWLINE INDENT stmt_list DEDENT 
  {
    {
      rtyp= $7;
      fname= $2;
      formals= $4; 
      body= $11
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

/* main(x: int, y: int) */
formals_list:
  vdecl { [$1] } /* Single Argument */
  | vdecl COMMA formals_list { $1::$3 } /* OR Multiple Arguments */

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr NEWLINE                            { Expr $1 }
  | expr EOF                                { Expr $1 }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition): /n stmt else /n stmt */
  | IF LPAREN expr RPAREN COLON NEWLINE INDENT stmt_list DEDENT ELSE COLON NEWLINE INDENT stmt_list DEDENT   { If($3, $8, $14) }
  | WHILE LPAREN expr RPAREN COLON NEWLINE INDENT stmt_list DEDENT { While ($3, $8)  }
  /* return */
  | RETURN expr NEWLINE                       { Return $2   }
  | RETURN expr EOF                        { Return $2      } /* Return the Expression */


expr:
    INT_LITERAL      { Literal($1)            }
  | FLOAT_LITERAL    { FloatLit($1)           }
  | BLIT             { BoolLit($1)            }
  | STRING_LITERAL   { StringLit($1) } 
  | ID               { Id($1)                 }
  | list_lit { $1 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr MULT expr   { Binop($1, Mult,   $3)   }
  | expr DIV expr    { Binop($1, Div,   $3)   }
  | expr EQ_COMPARISON expr { Binop($1, Eq_Compar, $3)   }
  | expr NOT_EQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID EQ expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  | ID COLON typ EQ expr { VariableInit($1, $3, $5) } /* Variable Declaration */
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  } /* args_opt = List of Arguments */
  | ID LBRACKET expr RBRACKET { ListAccess($1, $3) }
  | ID LBRACKET expr RBRACKET EQ expr { ListIndAssign($1, $3, $6) }

list_lit:
  LBRACKET args_opt RBRACKET { ListLiteral($2) }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
