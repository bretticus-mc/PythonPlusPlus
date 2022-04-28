type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Geq | Greater |And | Or | Eq_Compar | Bit_And

type uop = Neg | Not 

type typ = Int | Bool | Float | String | None | Pointer of typ



(* Defining what expressions can be *) 
type expr =
    Literal of int
  | FloatLit of string
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | VariableInit of string * typ * expr
  (* function call *)
  | Call of string * expr list (* Function Name and Function Arguments  *)
  | Subscript of expr * expr
  | Refer of string
  | Deref of expr
  | Noexpr
  

(* Defining what statements can be *)
type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * stmt 
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
  | Bit_And -> "&"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "String"
  | None -> "None"
  | Pointer t -> "*"^string_of_typ t 


  let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
 
  
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FloatLit(l) -> l
  | BoolLit(true) -> "True"
  | BoolLit(false) -> "False"
  | StringLit(s) -> s
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | VariableInit(v, t, e) -> v ^ " : " ^ string_of_typ t ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Subscript (e, s) -> string_of_expr e ^ "[" ^ string_of_expr s ^ "]"
  | Refer s -> "&" ^ s
  | Deref e -> "*" ^ string_of_expr e
  | Noexpr -> " "

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ "\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ "\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(e,s) -> "for"^ string_of_expr e ^"in"^ string_of_stmt s  ^ ":"



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
