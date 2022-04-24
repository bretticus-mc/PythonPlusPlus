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

	(* Create stacks and queue *)
	let indention_stack = Stack.create ()
	let () = Stack.push 0 indention_stack
	let queue_of_tokens = Queue.create ()

	let rec enqueue_dedents queue prev_indent_level curr_indent_level = 
		if prev_indent_level > curr_indent_level then
			(Queue.add DEDENT queue_of_tokens; enqueue_dedents queue (prev_indent_level - 4) curr_indent_level)

	let rec enqueue_indents queue prev_indent_level curr_indent_level = 
		if prev_indent_level < curr_indent_level then
			(Queue.add INDENT queue_of_tokens; enqueue_indents queue (prev_indent_level + 4) curr_indent_level)

	let count_whitespace str = 
		let parsed_string = List.init (String.length str) (String.get str) in
		let calculate_space accumulator = function
			'\t' -> accumulator + 4
			| ' ' -> accumulator + 1
			| _ -> accumulator
		in
		List.fold_left calculate_space 0 parsed_string
	
	let last_token_newline = ref true

}
(* 
	OCamllex Regex Syntax:
	https://ocaml.org/manual/lexyacc.html#ss:ocamllex-regexp 
*)

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let flt = digit*'.'digit+
let string_literal = ('"'[' '-'~']*'"')

let whitespace = [' ' '\t']+
let comment_after_whitespace = [' ' '\t']+'#'
let newline = '\r' | '\n' | "\r\n"

(* 
	Python 3.10 Tokens
	https://github.com/python/cpython/blob/3.10/Lib/token.py
*)

rule scan_token = parse
	| [' ' '\r' ] {scan_token lexbuf }
	| ['\n']+['\t' ' ']* {
		let curr_indent_level = count_whitespace (Lexing.lexeme lexbuf) in
		let prev_indent_level = Stack.top indention_stack in
		if curr_indent_level > prev_indent_level then
			((ignore(enqueue_indents queue_of_tokens prev_indent_level curr_indent_level);
			Stack.push curr_indent_level indention_stack;

			);
			NEWLINE
			)
		else if curr_indent_level = prev_indent_level then 
			NEWLINE
		else
			(ignore(enqueue_dedents queue_of_tokens prev_indent_level curr_indent_level;
			Stack.push curr_indent_level indention_stack); 
			NEWLINE)
	}
	| "(" { LPAREN }
	| ")" { RPAREN }
	| "{" { LBRACE }
	| "}" { RBRACE }
	| "," { COMMA }
	| "." { DOT }
	| ":" { COLON }
	| ";" { SEMI }
	| "=" { EQ }
	| "%" { MOD }
	| "+" { PLUS }
	| "-" { MINUS }
	| "*" { MULT }
	| "/" { DIV }
	| "+=" { PLUS_EQ }
	| "-=" { MINUS_EQ }
	| "*=" { MULT_EQ }
	| "/=" { DIV_EQ }
	| "<" { LT }
	| ">" { GT }
	| "->" { ARROW }
	| "and" { AND }
	| "or" { OR }
	| "not" { NOT }
	| "!" { EXCLAMATION }
	| "==" { EQ_COMPARISON }
	| "!=" { NOT_EQ }
	| "True" { BLIT(true)  }
	| "False" { BLIT(false) }
	| "return" { RETURN }
	| "def" { DEF }
	| "if" { IF }
	| "else" { ELSE }
	| "for" { FOR }
	| "while" { WHILE }
	| "in" { IN }
	| "int" { INT }
	| "String" { STRING }
	| "None" { NONE }
	| "return" { RETURN }
	| "float" { FLOAT }
	| "#" { read_single_line_comment lexbuf }
	| "\"\"\"" { read_multi_line_comment lexbuf }
	| digit+ as lem  { INT_LITERAL(int_of_string lem) }
	| '"'['a'-'z' 'A'-'Z' ' ']*'"' as lem {STRING_LITERAL(lem)}
	| flt as lem { FLOAT_LITERAL(lem)}
	| letter (digit | letter | '_')* as lem { ID(lem) }
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

	{
		let read lexbuf = 
			if Queue.is_empty queue_of_tokens then
				scan_token lexbuf
			else Queue.take queue_of_tokens
	}