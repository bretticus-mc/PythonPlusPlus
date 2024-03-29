(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

(* Create a Map where its keys are strings *)
module StringMap = Map.Make(String)
let var_map = Hashtbl.create 12345


(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
 *)

(* code = func_def and stmnt list *)
let check (code) =

  (* Verify a list of declarations(E.g: x: int) has no duplicate names *)
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
    (* StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      formals = [("x", Int)];
      body = [] } StringMap.empty  *)
    let add_bind map (name, ty, rtyp) = StringMap.add name {
      fname = name; formals = [("x", ty)]; rtyp = ty; 
      body = [] } map
    in List.fold_left add_bind StringMap.empty [
                               ("print", Int, Int);
                               ("prints",String, None);
 (*                              ("printb", Bool);
                               ("printf", Float);
                               ("prints", String); *)
                               ("free", Pointer(Int), None)
                               ] 
    (* Add the key: "print" and value: Function Definition *)
  in

  (* Add function name to symbol table *)
  let add_func map function_def =
    let built_in_err = "function " ^ function_def.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ function_def.fname
    and make_err er = raise (Failure er)
    and n = function_def.fname (* Name of the function *)
    in match function_def with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n function_def map
  in

  (* Build symbol table *)
  let build_func_table map = function
    Func_def(f) -> add_func map f
    | _ -> map
  in

  (* Collect all function declarations into symbol table *)
  let function_decls = List.fold_left build_func_table built_in_decls code
  in
  let is_pointer p = match p with Pointer _ -> true | _ -> false in

  (* Return a function from symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
in

(* Raise an exception if the given rvalue type cannot be assigned to
   the given lvalue type *)
let check_assign lvaluet rvaluet err =
    let rvaluet' = match rvaluet with 
      Array(arr_type, _) -> arr_type
      | _ -> rvaluet
    in
    let matched_typ =
      match lvaluet with
      | Pointer None ->
          if is_pointer rvaluet' then rvaluet' else raise (Failure err)
      | Pointer p_typ ->
          if rvaluet' = Pointer None || rvaluet' = p_typ then lvaluet
            else raise (Failure err)
      | Array(arr_type, _) -> arr_type
      | _ -> if lvaluet = rvaluet' then lvaluet else raise (Failure err)
  in
  matched_typ
in
let check_args lvaluet rvaluet err = 
  if lvaluet = rvaluet then lvaluet else raise (Failure err)
in
(* Return a variable from our local symbol table *)
let type_of_identifier symbol_table s =
  try Hashtbl.find symbol_table s
  with Not_found -> raise (Failure ("undeclared identifier " ^ s))
in

let deref p =
    match p with
    | Pointer s -> s
    | _ -> raise (Failure "cannot dereference expression")
in
(* Return a semantically-checked expression, i.e., with a type *)
let rec check_expr symbol_table = function
      Literal l -> (Int, SLiteral l)
    | FloatLit l -> (Float, SFloatLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | StringLit l -> (String , SStringLit l)
    | Id var -> (type_of_identifier symbol_table var, SId var)
    | VariableInit(var_name, var_type, e) as ex -> (* var = Variable Name, t = Type, e = Expression *)
        let (right_hand_type, right_hand_expr) =  check_expr symbol_table e in
        let err = "illegal assignment " ^ string_of_typ var_type ^ " = " ^
        string_of_typ right_hand_type ^ " in " ^ string_of_expr ex in
        let _ = check_assign var_type right_hand_type err in
        ignore(Hashtbl.add symbol_table var_name var_type);  (* Add Variable to Hashtable *)
        (var_type, SVariableInit(var_name, var_type, (check_expr symbol_table e)))
    | Assign (e1, e2) as ex ->
        let t1, e1' = check_expr symbol_table e1 and (t2, e2') = check_expr symbol_table e2 in    
        let err =
             "illegal assignment " ^ string_of_typ t1 ^ " = " ^ string_of_typ t2
              ^ " in " ^ string_of_expr ex
             and vt =
                match e1 with
                | Id _ | Subscript (_, _) | Deref _ | ListAccess _ -> t1
                 | _ -> raise (Failure "left expression is not assignable")
             in
             (check_assign t1 t2 err, SAssign ((vt, e1'), (t2, e2')))
    | Unop(op, e) as ex -> 
          let (t, e') = check_expr symbol_table e in
          let ty = match op with
            Neg when t = Int || t = Float -> t 
          | Not when t = Bool -> Bool 
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))   
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
        let t = 
        match op with
          | Eq_Compar when t1 = t2 -> Bool
          | Add | Sub | Div | Mult when t1 = Int -> Int
          | Add | Sub | Div | Mult  when t1 = Float -> Float
          | (Equal | Neq) -> Bool
          | (Less | Leq | Greater | Geq) 
             when t1 = Int || t1 = Float || is_pointer t1  -> Bool
          | And | Or when t1 = Bool -> Bool
          (* pointer addition and subtraction *)
          | (Add | Sub ) when is_pointer t1 && t2 = Int -> t1
          | (Add | Sub ) when is_pointer t1 && t2 = Float -> t1
          | _ -> raise(Failure err)
            
        in
          (t, SBinop ((t1, e1'), op, (t2, e2')))
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
              in (check_args ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.rtyp, SCall(fname, args'))
    | ListLiteral values ->
      let check_types e (e1, _) = 
        if e != e1 then raise (Failure ("Different types not allowed in the same list")) 
        else e1
      and l' = List.map (check_expr symbol_table) values in
        (match l' with
        [] -> raise (Failure "empty literals not allowed")
      | _ -> 
        let first_elem = List.fold_left check_types (fst (List.hd l')) l' in 
        (Array(first_elem, List.length l'), SListLiteral(l')) )
    | ListAccess(l, i) ->
      let lt = (type_of_identifier symbol_table l)
      in let v = check_expr symbol_table i in (lt, SListAccess(l, v))
      (* subscript main expr must be a pointer and the subscript must be integer *)
    | Subscript (e, s) ->
          let te, e' = check_expr symbol_table e and ts, s' = check_expr  symbol_table  s in
          if ts != Int then raise (Failure "subscript expression not integral")
          else
            let ts =
              match te with
              | Pointer p -> p
              | _ -> raise (Failure "main expression not a pointer")
            in
            (ts, SSubscript ((te, e'), (ts, s')))
      | Refer s -> (Pointer (type_of_identifier symbol_table s), SRefer s)
      | Deref e ->
          let t, e' = check_expr symbol_table e in
          if is_pointer t then (deref t, SDeref (t, e'))
          else raise (Failure "cannot dereference expression")

  in

let rec check_stmt_list curr_symbol_table  = function
      [] -> []
    | Block sl :: sl'  -> check_stmt_list curr_symbol_table (sl @ sl') (* Flatten blocks *)
    | s :: sl -> check_top_stmt curr_symbol_table s :: check_stmt_list curr_symbol_table sl
  (* Return a semantically-checked statement i.e. containing sexprs *)
and check_top_stmt curr_symbol_table = function
    (* A block is correct if each statement is correct and nothing
        follows any Return statement.  Nested blocks are flattened. *)
      Block sl -> SBlock (check_stmt_list curr_symbol_table sl)
    | Expr e -> SExpr (check_expr curr_symbol_table e)
    | If(e, st1, st2) ->
      SIf(check_bool_expr curr_symbol_table e, check_stmt_list curr_symbol_table st1, check_stmt_list curr_symbol_table st2)
    | While(e, st) ->
      SWhile(check_bool_expr curr_symbol_table e, check_stmt_list curr_symbol_table st)
    | Return e -> raise (Failure ("return outside function" ))
    (* | For(e1, e2,st) ->
	    SFor(check_bool_expr curr_symbol_table e1,check_top_stmt curr_symbol_table e2, check_top_stmt curr_symbol_table st) *)
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

    (* Build local symbol table to check that there are no duplicate formals *)
    let local_symbol_table = Hashtbl.copy curr_symbol_table in

    let rec build_local_symbol_table table formals = 
      match formals with
      | [] -> table
      | hd::tl -> Hashtbl.add table (fst hd) (snd hd); build_local_symbol_table table tl
    in
    ignore(build_local_symbol_table local_symbol_table func.formals);

    let raise_return_error t e = raise (
        Failure ("return gives " ^ string_of_typ t ^ " expected " ^
              string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
    in

    let rec check_func_statements = function
      [] -> []
    | Return e :: sl -> let (t, e') = check_expr local_symbol_table e in
      let return_val = match t with
        Array(arr_typ, _) -> if arr_typ = func.rtyp then SReturn (t, e')
          else (raise_return_error t e)
        | _ -> if t = func.rtyp then SReturn (t, e')
          else (raise_return_error t e)
        in return_val :: check_func_statements sl
    | s :: sl -> check_top_stmt local_symbol_table s :: check_func_statements sl
    in

     (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = List.rev (check_func_statements (List.rev func.body))
    }
  in
  let check_code curr_symbol_table = function
    Func_def(f) -> SFunc_def (check_func curr_symbol_table f )
    | Stmt(s) -> SStmt (check_top_stmt curr_symbol_table s)
  in 

  List.map (check_code var_map) code