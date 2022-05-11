type op = Add | Sub | Mult | Div | Equal | Neq | Less | Greater | And | Or | Eq_Compar
  | Geq | Leq 
      
type uop = Neg | Not 

type typ = Int | Bool | Float | None | String | Pointer of typ | Array of typ * int


(* Defining what expressions can be *) 
type expr =
    Literal of int
  | FloatLit of string
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | VariableInit of string * typ * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  (* function call *)
  | Call of string * expr list (* Function Name and Function Arguments  *)
  | ListLiteral of expr list
  | ListAccess of string * expr
  | ListIndAssign of string * expr * expr
  | Subscript of expr * expr
  | Deref of expr
  | Refer of string
  | Assign of expr * expr

(* Defining what statements can be *)
type stmt =
  Block of stmt list
  | Expr of expr
  | If of expr * stmt list * stmt list
  | While of expr * stmt list
  (* return *)
  | Return of expr


(* x: int *)
type bind = string * typ 

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ; (* Function Return Type *)
  fname: string; (* Function Name *)
  formals: bind list;
  body: stmt list; (* Function Body *)
}

type pystruct = {structName : string; structFields: bind list}

type code = 
  Func_def of func_def
  | Stmt of stmt


type program = 
    code list (* global variables and then list of function declarations *) 

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "="
  | Div -> "/"
  | Mult -> "*"
  | Eq_Compar -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Geq -> ">="
  | Greater -> ">"
  | And -> "and"
  | Or -> "or"

let struct_of_decl dec = 
   "struct" ^ dec.structName  ^ "{" ^ String.concat ", " (List.map fst dec.structFields) ^"}\n"   

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "String"
  | None -> "None"
  | Array(t, n) -> (string_of_typ t) ^ "[" ^ (string_of_int n) ^ "]"
  | Pointer t ->  "Pointer: *" ^ string_of_typ t 

  let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

  
let rec string_of_expr = function
    Literal(l) -> "IntLit: " ^ string_of_int l
  | FloatLit(l) -> "FloatLit: " ^ l
  | BoolLit(b) -> if b then "True" else "False"
  | StringLit(s) -> "String: " ^ s 
  | Id(s) -> "Id: " ^ s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | VariableInit(v, t, e) -> "VariableInit: " ^ v ^ " : " ^ string_of_typ t ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ListLiteral(elem) -> "ListLiteral: [" ^ String.concat ", " (List.map string_of_expr elem) ^ "]"
  | ListAccess(i, e) -> "ListAccess: " ^ i ^ "[" ^ string_of_expr e ^ "]"
  | ListIndAssign(l, i, e) -> "ListIndAssign: " ^ l ^ "[" ^ string_of_expr i ^ "]" ^ " = " ^ string_of_expr e 
  | Subscript (e, s) -> string_of_expr e ^ "[" ^ string_of_expr s ^ "]"
  | Deref e ->  "Deref: *" ^ string_of_expr e
  | Refer s -> "Refer: &"^ s

let rec string_of_stmt = function
  Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ "\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ "\n"
  | If(e, stmt1, stmt2) ->  "if: (" ^ string_of_expr e ^ ")\n" ^
        String.concat "" (List.map string_of_stmt stmt1) ^ "else:\n" ^ String.concat "" (List.map string_of_stmt stmt2)
  | While(e, stmts) -> "while (" ^ string_of_expr e ^ ") " ^ "\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n"
  (* |   For(i,e, s) -> "for" ^ string_of_expr i ^"in"^ string_of_stmt e  ^ ":\n"^
                           string_of_stmt s      *)            

let string_of_fdecl fdecl =
    "def " ^ fdecl.fname ^ "(" ^ 
    String.concat ", " (List.map (fun f -> "Arg_name:("^ (fst f) ^ ") Arg_Type:(" ^ (string_of_typ (snd f)) ^") ") fdecl.formals) ^ 
    ") -> " ^ string_of_typ fdecl.rtyp ^ ": \n" ^
    String.concat "" (List.map string_of_stmt fdecl.body)


let string_of_scode code = match code with
  Func_def(f) -> string_of_fdecl f
| Stmt(s) -> string_of_stmt s

let string_of_program (code) =
  "\n\nParsed program: \n\n" ^
  String.concat "\n" (List.map string_of_scode code)
