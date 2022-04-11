%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN MULT DIV
%token EQ NEQ LT GT AND OR NOT DOT PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ
%token IF ELSE WHILE INT STRING BOOL REM RANGLE ARROW
%token EXCLAMATION EQEQUAL NOTEQUAL TRUE FALSE NONE COLON
%token DEF FOR IN NEWLINE
/* return, COMMA token */
%token RETURN COMMA
%token <float> FLOAT
%token <int> INT_LITERAL
%token <bool> BLIT
%token <string> ID
%token <string> STRING_LITERAL
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT
%left PLUS MINUS

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
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr NEWLINE                        { Return $2      }
  | RETURN expr EOF                        { Return $2      } /* Return the Expression */


expr:
    INT_LITERAL      { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  | STRING_LITERAL   { StringLit($1) } 
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID ASSIGN expr   { Assign($1, $3)         }
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
