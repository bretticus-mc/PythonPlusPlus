type op = Add
        | Sub
        | Mult
        | Div
        | Power
        | Equal
        | Neq
        | Less
        | Leq
        | Greater
        | Geq
        | And
        | Or
        | BitAnd
        | BitOr
        | Eq_Compar
        | Mod
      
type uop = Neg | Not | Bitty | Ampy


type typ = Int | Bool | Float | Pointer of typ  | None | String  


(* Defining what expressions can be *) 
type expr =
    Literal of int
  | StringLit of string
  | FloatLit of string
  | BoolLit of bool
  | Null
  | Id of string
  | VariableInit of string * typ * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list (* Function Name and Function Arguments  *)
  | Cast of typ * expr
  | Subscript of expr * expr
  (* function call *)
  | Alloc of string * typ
  | Deref of expr
  | Refer of string
  | Assign of expr * expr
  | New of typ
  | Noexpr
  

(* Defining what statements can be *)
type stmt =
   Expr of expr
  | Block of stmt list
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr *  stmt * stmt 
  (* return *)

 


(* x: int *)
type bind = string * typ 

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  (* Function Return Type *)
  fname: string; (* Function Name *)
  rtyp: typ; 
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
  | Mod -> "%"
  | BitAnd -> "&"
  | BitOr -> "|"
  



let struct_of_decl dec = 
   "struct" ^ dec.structName  ^ "{" ^ String.concat ", " (List.map fst dec.structFields) ^"}\n"
   

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | None -> "None"
  | String -> "String"
  | Pointer t ->  string_of_typ t ^"*"


  let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Bitty -> "*"
  | Ampy -> "&"

 
  
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FloatLit(l) -> l
  | BoolLit(b) -> if b then "True" else "False"
  | Null -> "Null"
  | StringLit(s) -> "\"" ^ s ^ "\""
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | VariableInit(v, t, e) -> v ^ " : " ^ string_of_typ t ^ " = " ^ string_of_expr e
  | Alloc(s,t) ->  "*"^s ^ ":=" ^ "(" ^ string_of_typ t ^")" 
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Subscript (e, s) -> string_of_expr e ^ "[" ^ string_of_expr s ^ "]"
  | Deref e ->  "*" ^ string_of_expr e 
  | Refer s -> "&"^ s 
  | New t -> "new(" ^ string_of_typ t ^")"
  | Noexpr -> " "

let rec string_of_stmt = function
    Expr(expr) -> string_of_expr expr ^ "\n"
  | Block(stmts) ->
    "\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
 | For(i,e, s) -> "for" ^ string_of_expr i ^"in"^ string_of_stmt e  ^ ":\n"^
                           string_of_stmt s
  | Return(expr) -> "return " ^ string_of_expr expr ^ "\n"

                 

let string_of_fdecl fdecl = 
    "def " ^ fdecl.fname ^ "(" ^ 
    String.concat ", " (List.map fst fdecl.formals) ^
    ") -> " ^ string_of_typ fdecl.rtyp ^ ": \n" ^
    String.concat "" (List.map string_of_stmt fdecl.body)


let string_of_scode code = match code with
  Func_def(f) -> string_of_fdecl f
| Stmt(s) -> string_of_stmt s


let string_of_program (code) =
  "\n\nParsed program: \n\n" ^
  String.concat "\n" (List.map string_of_scode code)
(* pointer rules:
if x is an expr and x has a type as pointer T type then x is assignable to T
 X type and T types can be int, floats, arrayy only. *)

(* 
x : int = 10 
ptr : *int
*ptr  = new(ptr)
ptr = &x
print ptr
*)



 