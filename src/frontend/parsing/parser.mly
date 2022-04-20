%{
open Ast
%}

/* Types */
%token INT FLOAT BOOL STRING

/* Operators */
%token PLUS MINUS MULT DIV ASSIGN

/* Comparators */
%token NOT EQ NOT_EQ LT GT AND OR NOT DOT PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ EXCLAMATION EQ_COMPARISON IN COLON

%token IF ELSE WHILE FOR DEF RETURN COMMA NEWLINE
%token SEMI LPAREN RPAREN LBRACE RBRACE
%token TRUE FALSE NONE
%token MOD RANGLE ARROW

%token <string> FLOAT_LITERAL
%token <int> INT_LITERAL
%token <bool> BLIT
%token <string> ID
%token <string> STRING_LITERAL
%token EOF
%token Bit_And

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left EQ_COMPARISON NOT_EQ
%left LT GT
%left PLUS MINUS
%left MULT DIV
%right NOT Bit_And

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

/* fdecl 
def main(x: int, y: int) -> None:
*/
fdecl:
  DEF ID LPAREN formals_opt RPAREN ARROW typ COLON NEWLINE stmt_list NEWLINE
  {
    {
      rtyp= $7;
      fname= $2;
      formals= $4; 
      body= $10 
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
  | IF LPAREN expr RPAREN COLON NEWLINE stmt ELSE COLON NEWLINE stmt    { If($3, $7, $11) }
  | WHILE LPAREN expr RPAREN COLON NEWLINE stmt { While ($3, $7)  }
  /* return */
  | RETURN expr NEWLINE                       { Return $2   }
  | RETURN expr EOF                        { Return $2      } /* Return the Expression */


expr:
    INT_LITERAL      { Literal($1)            }
  | FLOAT_LITERAL    { FloatLit($1)           }
  | BLIT             { BoolLit($1)            }
  | STRING_LITERAL   { StringLit($1) } 
  | ID               { Id($1)                 }
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
  | NOT expr         { Unop(Not, $2)          }
  | ID EQ expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  | ID COLON typ EQ expr { VariableInit($1, $3, $5) } /* Variable Declaration */
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  } /* args_opt = List of Arguments */

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
