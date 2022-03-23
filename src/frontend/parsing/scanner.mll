{
	open Lexing
	open Parser

	exception SytaxError of string

	let next_line lexbuf = 
		let pos = lexbuf.lex_curr_p in 
		lexbuf.lex_curr_p <- 
			{ pos with pos_bol = lexbuf.lex_curr_pos;
				pos_lnum = pos.pos_lnum + 1
			}
}

(* 
	OCamllex Regex Syntax:
	https://ocaml.org/manual/lexyacc.html#ss:ocamllex-regexp 
*)

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let id = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* 
	Python 3.10 Tokens
	https://github.com/python/cpython/blob/3.10/Lib/token.py
*)

rule read_tokens = parse
	| "(" { LPAREN }
	| ")" { RPAREN }
	| "{" { LBRACE }
	| "}" { RBRACE }
	| "," { COMMA }
	| "." { DOT }
	| ";" { SEMI }
	| "=" { EQUAL }
	| "+" { PLUS }
	| "-" { MINUS }
	| "*" { MULT }
	| "/" { DIV }
	| "+=" { PLUS_EQ }
	| "-=" { MINUS_EQ }
	| "*=" { MULT_EQ }
	| "/=" { DIV_EQ }
	| "%" { REM }
	| "<" { LANGLE }
	| ">" { RANGLE }
	| "->" { ARROW }
	| "and" { AND }
	| "or" { OR }
	| "not" { NOT }
	| "!" { EXCLAMATION }
	| "==" { EQEQUAL }
	| "!=" { NOTEQUAL }
	| "True" { TRUE }
	| "False" { FALSE }
	| "def" { DEF }
	| "if" { IF }
	| "else" { ELSE }
	| "for" { FOR }
	| "while" { WHILE }
	| "in" { IN }
	| "int" { INT }
	| whitespace {read_tokens lexbuf }
	| "#" { SINGLE_LINE_COMMENT }
	| "\"\"\"" { MULTI_LINE_COMMENT }
	| int as lem {LITERAL (int_of_string lem )}
	| newline { next_line lexbuf; red_token lexbuf }
	| eof { EOF }
	| _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

	and read_single_line_comment = parse
		| "#" { read_tokens lexbuf }
		| newline { next_line lexbuf; read_multi_line_comment lexbuf }
		| eof { raise (SytaxError ("Lexer - Unexpected EOF"))}
		| _ { read_multi_line_comment lexbuf }
	
	and read_multi_line_comment = parse
		| "\"\"\"" { read_tokens lexbuf }
		| newline { next_line lexbuf; read_multi_line_comment lexbuf }
		| eof { raise (SyntaxError ("Lexer - Unexpected EOF"))}
		| _ { read_multi_line_comment lexbuf }

