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

	
	let indent_levels = Stack.create()
	let () = Stack.push 0 indent_levels; ()
	(* let scan_queue = Queue.create() *)
}

(* 
	OCamllex Regex Syntax:
	https://ocaml.org/manual/lexyacc.html#ss:ocamllex-regexp 
*)

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* 
	Python 3.10 Tokens
	https://github.com/python/cpython/blob/3.10/Lib/token.py
*)

rule scan_token = parse
(*	| [' ' '\t' '\r' '\n'] {scan_token lexbuf } *)
	| "(" { LPAREN }
	| ")" { RPAREN }
	| "{" { LBRACE }
	| "}" { RBRACE }
	| "," { COMMA }
	| "." { DOT }
	| ";" { SEMI }
	| "=" { EQ }
	| "+" { PLUS }
	| "-" { MINUS }
	| "*" { MULT }
	| "/" { DIV }
	| "+=" { PLUS_EQ }
	| "-=" { MINUS_EQ }
	| "*=" { MULT_EQ }
	| "/=" { DIV_EQ }
	| "%" { REM }
	| "<" { LT }
	| ">" { GT }
	| "->" { ARROW }
	| "and" { AND }
	| "or" { OR }
	| "not" { NOT }
	| "!" { EXCLAMATION }
	| "==" { EQEQUAL }
	| "!=" { NOTEQUAL }
	| "True" { BLIT(true)  }
	| "False" { BLIT(false) }
	| "def" { DEF }
	| "if" { IF }
	| "else" { ELSE }
	| "for" { FOR }
	| "while" { WHILE }
	| "in" { IN }
	| "int" { INT }
	| "#" { read_single_line_comment lexbuf }
	| "\"\"\"" { read_multi_line_comment lexbuf }
	| digit+ as lem  { INT_LITERAL(int_of_string lem) }
	| '"'['a'-'z' 'A'-'Z' ' ']*'"' as lem {STRING_LITERAL(lem)}
	| letter (digit | letter | '_')* as lem { ID(lem) }
	| ['\n']  { NEWLINE }
	| eof { EOF }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

	and read_single_line_comment = parse
		| newline { next_line lexbuf; scan_token lexbuf }
		| eof { raise (Failure("Unexpected EOF"))}
		| _ { read_single_line_comment lexbuf }
	
	and read_multi_line_comment = parse
		| "\"\"\"" { scan_token lexbuf }
		| newline { next_line lexbuf; read_multi_line_comment lexbuf }
		| eof { raise (Failure("Unexpected EOF"))}
		| _ { read_multi_line_comment lexbuf }
	



