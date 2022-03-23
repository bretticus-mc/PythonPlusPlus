%{

%}

%token <int> LITERAL
%token <string> ID
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COMMA
%token DOT
%token SEMI
%token EQUAL
%token PLUS 
%token MINUS 
%token MULT 
%token DIV 
%token PLUS_EQ 
%token MINUS_EQ 
%token MULT_EQ 
%token REM 
%token LANGLE 
%token RANGLE 
%token AND 
%token OR 
%token NOT 
%token EQEQUAL 
%token NOTEQUAL 
%token TRUE 
%token FALSE 
%token DEF 
%token IF 
%token ELSE 
%token FOR 
%token WHILE 
%token IN 
%token EOF

%start program
%type <Ast.tokenseq> program

%%
program:
	tokens EOF { $1 }

tokens:
	{ [] }
| one_token tokens { $1 :: $2 }

one_token:
	| SEMI { "SEMI "}
	| LPAREN { "LPAREN "}
	| RPAREN
	| LBRACE
	| RBRACE
	| COMMA
	| DOT
	| SEMI
	| EQUAL
	| PLUS 
	| MINUS 
	| MULT 
	| DIV 
	| PLUS_EQ 
	| MINUS_EQ
	| MULT_EQ 
	| REM 
	| LANGLE 
	| RANGLE 
	| AND 
	| OR 
	| NOT 
	| EQEQUAL 
	| NOTEQUAL 
	| TRUE 
	| FALSE 
	| DEF 
	| IF 
	| ELSE 
	| FOR 
	| WHILE 
	| IN 
	| LITERAL { "LITERAL: " ^ string_of_int $1 }
	| ID { "ID: " ^ $1 }
