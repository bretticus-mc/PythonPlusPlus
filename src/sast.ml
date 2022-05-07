(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx

and sx =
    SLiteral of int
  | SStringLit of string
  | SFloatLit of string
  | SBoolLit of bool
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  (*| SCast of typ * sexpr 
  | SAlloc of string * typ *)
  | SVariableInit of string * typ * sexpr
  | SAssign of sexpr * sexpr
  (* call *)
  | SCall of string * sexpr list
  | SSubscript of sexpr * sexpr
  | SRefer of string
  | SDeref of sexpr
  (*| SNew of typ *)


type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt list * sstmt list
  | SWhile of sexpr * sstmt list
  (* | SFor of sexpr * sstmt *  sstmt *)
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
      | SBoolLit(b) -> if b then "True"  else "False"
      | SId(s) -> s
      | SBinop(e1, o, e2) -> "Binop: " ^
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
      | SAssign (v, e) -> string_of_sexpr v ^ " = " ^ string_of_sexpr e
      (* | SAlloc(s,t) -> "*"^s^ ":=" ^ string_of_typ t  *)
      | SVariableInit(v, t, e) -> "SVariable Init: " ^ v ^ " : " ^ string_of_typ t ^ " = " ^ string_of_sexpr e
      | SCall (f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      (* | SCast(e,t) -> "st" *)
      | SSubscript (e, s) -> string_of_sexpr e ^ "[" ^ string_of_sexpr s ^ "]"
      (* | SNew(t) ->  "new(" ^ string_of_typ t ^ ")\n" *)
      | SRefer s -> "SRefer: &" ^ s
      | SDeref e -> "SDeref: *" ^ string_of_sexpr e
    ) ^ ")"

  
let rec string_of_sstmt = function
    SBlock(stmts) ->
    "SBlock: {\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> "SExpr: "^ string_of_sexpr expr ^ "\n"
  | SReturn(expr) -> "SReturn: return " ^ string_of_sexpr expr ^ "\n"
  | SIf(e, stmt1, stmt2) ->  "SIf: if (" ^ string_of_sexpr e ^ ")\n" ^
          String.concat "" (List.map string_of_sstmt stmt1) ^ "else\n" ^ String.concat "" (List.map string_of_sstmt stmt2)
  | SWhile(e, stmts) -> "Swhile: while " ^ string_of_sexpr e ^ ": " ^ "\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "\n" ^ "End of While Loop"
  (* | SFor(i,e, s) -> "for"^ string_of_sexpr i ^"in"^ string_of_sstmt e ^ ":\n" ^
                           string_of_sstmt s *)

let string_of_sfdecl fdecl =
  "def " ^ fdecl.sfname ^ "(" ^ 
  String.concat ", " (List.map (fun f -> "Arg_name:("^ (fst f) ^ ") Arg_Type:(" ^ (string_of_typ (snd f)) ^") ") fdecl.sformals) ^ 
  ") -> " ^ string_of_typ fdecl.srtyp ^ ": \n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "\n"

let string_of_scode code = match code with
  SFunc_def(f) -> string_of_sfdecl f
| SStmt(s) -> string_of_sstmt s

let string_of_sprogram (code) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_scode code)