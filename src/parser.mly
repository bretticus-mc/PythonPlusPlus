%{
open Ast
%}
%token INT BOOL FLOAT STRING NONE POINTER
%token <string> ID
%token <int> INT_LITERAL
%token <string> FLOAT_LITERAL
%token <bool> BLIT
%token <string> STRING_LITERAL

%token NEWLINE %token %token EOF
%token TRUE    %token FALSE  %token NONE
%token IF      %token ELIF   %token ELSE
%token WHILE   %token BREAK  %token CONTINUE %token FOR %token DEL
%token DEF     %token RETURN %token SIZEOF  %token MALLOC
%token FREE    %token PASS
%token AND     %token OR    %token NOT  %token IS  %token IN  

%token PLUS    %token MINUS  %token MULT %token DIV %token MOD
%token EXP     %token ARROW   %token EQ_COMPARISON
%token NEQ     %token LT       %token GT      %token LEQ
%token GEQ     %token Bit_And   %token NOT
%token LPAREN   %token RPAREN   %token LBRACKET   %token RBRACKET
%token LBRACE  %token RBRACE   %token DOT        %token COMMA
%token COLON    %token SEMI     %token EXCLAMATION   %token ASSIG    

%start program
%type <Ast.program> program
%left OR
%left AND
%left IN NOT LT LEQ GT GEQ NEQ EQ
%left PLUS MINUS
%left MULT DIV
%right EXCLAMATION Bit_And
%right ASSIG EXP
%left DOT LBRACKET
%left LPAREN

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
  | FLOAT  { Float }
  | STRING { String }
  | BOOL {Bool}
  | NONE { None }
  | POINTER  typ {Pointer($2)}  



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
  | expr NEQ   expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | NOT expr         { Unop(Not, $2)          }
  | expr EQ expr   { Assign($1, $3)         }
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
