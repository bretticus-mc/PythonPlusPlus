(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx

and sx =
    SLiteral of int
  | SBoolLit of bool
  | SId of string
  | SPointer_Ref of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  | SStringLit of string
  (* call *)
  | SCall of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  sbody: sstmt list;
}

type scode = 
  SFunc_def of sfunc_def
  | SStmt of sstmt

type sprogram = scode list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SStringLit(l) -> l
      | SPointer_Ref(p) -> p
      | SBoolLit(true) -> "True"
      | SBoolLit(false) -> "False"
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ "\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ "\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  "def " ^ fdecl.sfname ^ "(" ^ 
  String.concat ", " (List.map fst fdecl.sformals) ^
  ") -> " ^ string_of_typ fdecl.srtyp ^ ": \n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "\n"

let string_of_scode code = match code with
  SFunc_def(f) -> string_of_sfdecl f
| SStmt(s) -> string_of_sstmt s

let string_of_sprogram (code) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_scode code)

(* 
let string_of_sprogram (code) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
*)