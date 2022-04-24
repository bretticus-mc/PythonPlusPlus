(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx

and sx =
    SLiteral of int
  | SFloatLit of string
  | SBoolLit of bool
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  | SStringLit of string
  | SVariableInit of string * typ * sexpr
  (* call *)
  | SCall of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt list
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ; (* Function Return Type *)
  sfname: string; (* Function Name *)
  sformals: bind list; (* Function Arguments *) 
  sbody: sstmt list; (* Function Body *)
}

type scode = 
  SFunc_def of sfunc_def
  | SStmt of sstmt

type sprogram = scode list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SFloatLit(l) -> l
      | SStringLit(l) -> l
      | SBoolLit(true) -> "True"
      | SBoolLit(false) -> "False"
      | SId(s) -> s
      | SBinop(e1, o, e2) -> "Binop: " ^
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> "SAssign: " ^ v ^ " = " ^ string_of_sexpr e
      | SVariableInit(v, t, e) -> "SVariable Init: " ^ v ^ " : " ^ string_of_typ t ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "SBlock: {\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> "SExpr: "^ string_of_sexpr expr ^ "\n"
  | SReturn(expr) -> "SReturn: return " ^ string_of_sexpr expr ^ "\n"
  | SIf(e, s1, s2) ->  "SIf: if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, stmts) -> "Swhile: while " ^ string_of_sexpr e ^ ": " ^ "\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "\n" ^ "End of While Loop"

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