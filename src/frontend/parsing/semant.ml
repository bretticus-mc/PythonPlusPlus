(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

(* Create a Map where its keys are strings *)
module StringMap = Map.Make(String)
let var_map = Hashtbl.create 12345
(* module StringHash = Hashtbl.Make() *)
(* module VarMap = Map.Make(String) *)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (code) =

  (* Verify a list of declarations(E.g: x: int) has no duplicate names *)
  (* Input: String, List *)
  let check_binds (kind : string) (binds : (string * typ) list) =
    let rec dups = function
        [] -> ()
      |	((n1, _) :: (n2, _) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (a, _) (b, _) -> compare a b) binds)
  in

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    (* Add the key: "print" and value: Function Definition *)
    StringMap.add "print" {
      (* rtyp = Int; *)
      rtyp = String;
      fname = "print";
      (* formals = [(Int, "x")]; *)
      formals = [("x", String)];
      body = [] } StringMap.empty (* Add this key/value pair to an empty map*)
  in

  (* TODO: Create new function that creates new hashtable for user declared functions *)

  (* Add function name to symbol table *)
  (* Input: *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  let build_func_table map = function
    Func_def(f) -> add_func map f
    | _ -> map
  in

    (* Collect all function names into one symbol table 
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  *)
  let function_decls = List.fold_left build_func_table built_in_decls code
  in

  (* Return a function from symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

(* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
let check_assign lvaluet rvaluet err =
  if lvaluet = rvaluet then lvaluet else raise (Failure err)
in

(* Return a variable from our local symbol table *)
let type_of_identifier symbol_table s =
  try Hashtbl.find symbol_table s
  with Not_found -> raise (Failure ("undeclared identifier " ^ s))
in

(* Return a semantically-checked expression, i.e., with a type *)
(**)
let rec check_expr symbol_table = function
      Literal l -> (Int, SLiteral l)
    | BoolLit l -> (Bool, SBoolLit l)
    | StringLit l -> (String, SStringLit l)
    | Id var -> (type_of_identifier symbol_table var, SId var)
    | VariableInit(var, t, e) -> (* var = Variable Name, t = Type, e = Expression *)
      (* TODO: Check if Variable exists in Hashtable *)
        ignore(Hashtbl.add symbol_table var t);  (* Add Variable to Hashtable *)
      (t, SVariableInit(var, t, (check_expr symbol_table e))) (* Check if it is added properly *)
    | Assign(var, e) as ex ->
      let lt = type_of_identifier symbol_table var
      and (rt, e') = check_expr symbol_table e in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
    in
      (check_assign lt rt err, SAssign(var, (rt, e')))

    | Binop(e1, op, e2) as e ->
      let (t1, e1') = check_expr symbol_table e1
      and (t2, e2') = check_expr symbol_table e2 in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t = match op with
            Add | Sub when t1 = Int -> Int
          | Equal | Neq -> Bool
          | Less when t1 = Int -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else raise (Failure err)
    | Call(fname, args) as call ->
      let fd = find_func fname in
      let param_length = List.length fd.formals in
      if List.length args != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^
                        " arguments in " ^ string_of_expr call))
      else let check_call (_, ft) e =
              let (et, e') = check_expr symbol_table e in
              let err = "illegal argument found " ^ string_of_typ et ^
                        " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.rtyp, SCall(fname, args'))
  in

let rec check_top_stmt_list curr_symbol_table  = function
      [] -> []
    | Block sl :: sl'  -> check_top_stmt_list curr_symbol_table (sl @ sl') (* Flatten blocks *)
    | s :: sl -> check_top_stmt curr_symbol_table s :: check_top_stmt_list curr_symbol_table sl
  (* Return a semantically-checked statement i.e. containing sexprs *)
and check_top_stmt curr_symbol_table = function
    (* A block is correct if each statement is correct and nothing
        follows any Return statement.  Nested blocks are flattened. *)
      Block sl -> SBlock (check_top_stmt_list curr_symbol_table sl)
    | Expr e -> SExpr (check_expr curr_symbol_table e)
    | If(e, st1, st2) ->
      SIf(check_bool_expr curr_symbol_table e, check_top_stmt curr_symbol_table st1, check_top_stmt curr_symbol_table st2)
    | While(e, st) ->
      SWhile(check_bool_expr curr_symbol_table e, check_top_stmt curr_symbol_table st)
    | Return e ->
      raise (Failure("Return statement must be inside function"))
  and 
  check_bool_expr curr_symbol_table e =
    let (t, e') = check_expr curr_symbol_table e in
    match t with
    | Bool -> (t, e')
    |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

  let check_func curr_symbol_table func =
    (* Make sure no duplicate formal arguments in function declaration *)
    check_binds "formal" func.formals;
    (* 
    let updated_function_decls = add_func function_decls func in
    ignore(updated_function_decls);
    *)
    let local_symbol_table = Hashtbl.copy curr_symbol_table in 
    (* Hashtbl.add() *)
    let rec build_local_symbol_table table formals = 
      match formals with
      | [] -> table
      | hd::tl -> Hashtbl.add table (fst hd) (snd hd); build_local_symbol_table table tl
      (* *)
    in
    ignore(build_local_symbol_table local_symbol_table func.formals);
    (* Build local symbol table of variables for this function 
    let local_symbols = List.fold_left (fun m (name, ty) -> Hashtbl.add name ty m)
        (* StringMap.empty (globals @ func.formals @ func.locals ) *)
        curr_symbol_table (func.formals)
    in
    *)

     (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = check_top_stmt_list local_symbol_table func.body
    }
  in
  let check_code curr_symbol_table = function
    Func_def(f) -> SFunc_def (check_func curr_symbol_table f )
    | Stmt(s) -> SStmt (check_top_stmt curr_symbol_table s)
  in 

  List.map (check_code var_map) code