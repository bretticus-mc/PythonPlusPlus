(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)


module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
let var_map = Hashtbl.create 12345




(* translate : Sast.program -> Llvm.module *)
let translate (code: Sast.scode list) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "PythonPP" in
  (* Get types from the context *)
  let i32_t      = L.i32_type    context (* 32-bit int type *)
  and i8_t       = L.i8_type     context (* Characters *)
  and i1_t       = L.i1_type     context (* Boolean type *)
  and float_t    = L.double_type context (* Double/Float type *)
  and string_t   = L.pointer_type   (L.i8_type context) (* String type *)
  and none_t     = L.void_type   context in 
  let vpoint_t   = L.pointer_type i8_t
in

  (* Return the LLVM type for a PythonPP type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.None  -> none_t 
    | A.Float -> float_t
    | A.String -> string_t
    | A.Array(t, n) ->  L.array_type (ltype_of_typ t) n
    | A.Pointer p ->
      if p == A.None then vpoint_t else L.pointer_type (ltype_of_typ p)
  in  
 
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (_,t) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    let func_decl_intermediary map = function
      | SFunc_def(func_decl) -> function_decl map func_decl
      | SStmt(stmt) -> map
      in
    List.fold_left func_decl_intermediary StringMap.empty code in
(*  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in *)

  (* Return the value for a variable or formal argument.
      Check local names first, then global names *)
  let lookup symbol_table var_name = try Hashtbl.find symbol_table var_name
    with Not_found -> raise (Failure ("can't find variable"))
  in

 

  
  (* Construct code for an expression; return its value *)
  let rec build_expr curr_symbol_table builder ((_, e) : sexpr) = match e with
     SLiteral i  -> L.const_int i32_t i
    | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0) 
    | SFloatLit l -> L.const_float_of_string float_t l
    | SStringLit s -> L.build_global_stringptr s "str" builder
    | SId s       -> L.build_load (lookup curr_symbol_table s) s builder
    (* special handling for deref expr, subscript expr, and malloc *)
    | SAssign (e1, e2) ->
          let t1, s1 = e1 and e2' = build_expr curr_symbol_table builder e2 in
          let e =
            match s1 with
            | SId s ->
                ignore (L.build_store e2' (lookup curr_symbol_table  s) builder);
              e2'
           | SSubscript (s, i) ->
                let e1' =
                  let s' = build_expr curr_symbol_table builder s and i' = build_expr curr_symbol_table builder i in
                  L.build_in_bounds_gep s' (Array.of_list [i']) "tmp" builder
                in
                ignore (L.build_store e2' e1' builder) ;
                e2'
            | SDeref s ->
                let e1' = build_expr curr_symbol_table builder s in
                ignore (L.build_store e2' e1' builder) ;
                e2'
            | _ -> raise (Failure "error: failed to assign value")
            in e
    | SListLiteral list  ->
      let first_elem = fst (List.hd list) in
      let v = Array.map (fun e -> build_expr curr_symbol_table builder e) (Array.of_list list) in
      (L.const_array (ltype_of_typ first_elem) v) 
    | SListAccess(id, index) ->
      let ind = (build_expr curr_symbol_table builder index)
      in let value = L.build_gep (lookup curr_symbol_table id) [| (L.const_int i32_t 0); ind |] "tmp" builder
      in L.build_load value "tmp" builder
    | SListIndAssign (s, idx, e) -> 
      let e' = build_expr curr_symbol_table builder e and
      ind = build_expr curr_symbol_table builder idx
      in
      let el = L.build_gep (lookup curr_symbol_table s) [| (L.const_int i32_t 0); ind |] "" builder and
      e' = e'
      in
      ignore(L.build_store e' el builder); e'
    | SVariableInit(var_name, var_typ, s_expr) -> let s_expr' = build_expr curr_symbol_table builder s_expr in
      let _ = (match var_typ with 
        Pointer(s) -> 
          let malloc = L.build_array_malloc vpoint_t s_expr' "malloc" builder
          in 
          let bitcast = L.build_bitcast malloc (ltype_of_typ var_typ) "pointer_init" builder
          in
          let var_allocation = L.build_alloca (ltype_of_typ var_typ) var_name builder
          in 
          ignore(Hashtbl.add curr_symbol_table var_name var_allocation);
          ignore(L.build_store bitcast var_allocation builder);
          let var_load = L.build_load var_allocation var_name builder
          in
          L.build_store s_expr' var_load builder;
        | _ ->
          let var_allocation = L.build_alloca (ltype_of_typ var_typ) var_name builder
          in
          ignore(Hashtbl.add curr_symbol_table var_name var_allocation);
          L.build_store s_expr' var_allocation builder
      ) in
      s_expr';
    | SBinop ((A.Float,_ ) as e1, op, e2) ->
	      let e1' = build_expr curr_symbol_table builder e1
        and e2' = build_expr curr_symbol_table builder e2 in
        (match op with 
          A.Add     -> L.build_fadd
        | A.Sub     -> L.build_fsub
        | A.Mult    -> L.build_fmul
        | A.Div     -> L.build_fdiv
        | A.Eq_Compar -> L.build_fcmp L.Fcmp.Oeq (* Not sure on this *)
        | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
        | A.Neq     -> L.build_fcmp L.Fcmp.One
        | A.Less    -> L.build_fcmp L.Fcmp.Olt
        | A.Leq -> L.build_fcmp L.Fcmp.Ole
        | A.Greater -> L.build_fcmp L.Fcmp.Ogt
        | A.Geq -> L.build_fcmp L.Fcmp.Oge
        | A.And | A.Or ->
            raise (Failure "internal error: semant should have rejected and/or on float")
        ) e1' e2' "tmp" builder
    | SBinop (((A.Int, _) as e1), op, e2)
      |SBinop (((A.Bool, _) as e1), op, e2) ->
          let e1' = build_expr curr_symbol_table builder e1 
          and e2' = build_expr curr_symbol_table builder e2 in
          ( match op with
          | A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_sdiv
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Eq_Compar -> L.build_icmp L.Icmp.Eq (* No binop version on LLVM *)
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq -> L.build_icmp L.Icmp.Sge 
          | A.Leq -> L.build_icmp L.Icmp.Sle
          ) e1' e2' "tmp" builder
    | SUnop(op, ((t, _) as e)) ->
          let e' = build_expr curr_symbol_table builder e in
	        (match op with
	        | A.Neg when t = A.Float -> L.build_fneg 
	        | A.Neg   -> L.build_neg
          | A.Not  -> L.build_not) 
             e' "tmp" builder
    | SBinop (s, op, ((A.Int, _) as i)) ->
        let s' = build_expr curr_symbol_table builder s in
        let i' = 
        match op with
            | A.Add -> build_expr curr_symbol_table builder i
            | A.Sub -> build_expr curr_symbol_table builder (A.Int, SUnop (A.Neg, i))
            | _ -> raise (Failure "error: invalid pointer manipulation")
          in
          L.build_in_bounds_gep s' (Array.of_list [i']) "tmp" builder
    (* pointer comparison *)
      | SBinop (p1, op, p2) ->
          let p1' = build_expr curr_symbol_table builder p1 and p2' = build_expr curr_symbol_table builder p2 in
          ( match op with
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | A.Less -> L.build_icmp L.Icmp.Slt
          | A.Leq -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq -> L.build_icmp L.Icmp.Sge
          | _ -> raise (Failure "error: invalid pointer comparison") )
            p1' p2' "tmp" builder

      | SSubscript (s, i) ->
          let e =
            let s' = build_expr curr_symbol_table builder s and i' = build_expr curr_symbol_table builder i in
            L.build_in_bounds_gep s' (Array.of_list [i']) "gep" builder
          in
          L.build_load e "deref" builder
    | SDeref s -> L.build_load (build_expr curr_symbol_table builder s) "deref" builder
    | SRefer s -> lookup  curr_symbol_table s
    | SCall ("print", [e]) ->
         let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
          L.build_call printf_func [| int_format_str ; (build_expr curr_symbol_table builder e) |]
           "printf" builder
      | SCall ("prints", [e]) ->
          L.build_call printf_func [| (build_expr curr_symbol_table builder e) |]
          "prints" builder
      | SCall ("free", [e]) ->
          L.build_free (build_expr curr_symbol_table builder e) builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr curr_symbol_table builder) (List.rev args)) in
        let result  = (match (fdecl.srtyp) with 
          None -> "" 
          | _ -> f ^ "_result")
        in
        L.build_call fdef (Array.of_list llargs) result builder
  in

  (* LLVM insists each basic block end with exactly one "terminator"
    instruction that transfers control.  This function runs "instr builder"
    if the current block does not already have a terminator.  Used,
    e.g., to handle the "fall off the end of the function" case. *)
  let add_terminal builder instr =
  match L.block_terminator (L.insertion_block builder) with
    Some _ -> ()
  | None -> ignore (instr builder) in

 
  

  (* Build the code for the given statement; return the builder for
    the statement's successor (i.e., the next instruction will be built
    after the one generated by this call) *)
  let rec build_stmt the_function curr_symbol_table builder = function
    SBlock sl -> List.fold_left (build_stmt the_function curr_symbol_table) builder sl (* Need to check arguments passed*)
    | SExpr e -> ignore(build_expr curr_symbol_table builder e); builder
    | SReturn e -> ignore(L.build_ret (build_expr curr_symbol_table builder e) builder); builder
    | SIf (predicate, then_stmt, else_stmt) ->
      let bool_val = build_expr curr_symbol_table builder predicate in

      let then_bb = L.append_block context "then" the_function in
      ignore (List.fold_left (build_stmt the_function curr_symbol_table) (L.builder_at_end context then_bb) then_stmt);
      let else_bb = L.append_block context "else" the_function in
      ignore (List.fold_left (build_stmt the_function curr_symbol_table) (L.builder_at_end context else_bb) else_stmt);

      let end_bb = L.append_block context "if_end" the_function in
      let build_br_end = L.build_br end_bb in (* partial function *)
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      add_terminal (L.builder_at_end context else_bb) build_br_end;

      ignore(L.build_cond_br bool_val then_bb else_bb builder);
      L.builder_at_end context end_bb

    | SWhile (predicate, body) ->
      let while_bb = L.append_block context "while" the_function in
      let build_br_while = L.build_br while_bb in (* partial function *)
      ignore (build_br_while builder);
      let while_builder = L.builder_at_end context while_bb in
      let bool_val = build_expr curr_symbol_table while_builder predicate in

      let body_bb = L.append_block context "while_body" the_function in
      add_terminal (List.fold_left (build_stmt the_function curr_symbol_table) (L.builder_at_end context body_bb) body) build_br_while;

      let end_bb = L.append_block context "while_end" the_function in

      ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
      L.builder_at_end context end_bb

   in
  (* Fill in the body of the given function *)
  let build_function_body curr_symbol_table builder fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let func_builder = L.builder_at_end context (L.entry_block the_function) in

    (* Create a copy of the running symbol table to create a local 
    symbol table inside the function scope *)
    let local_symbol_table_init = Hashtbl.copy curr_symbol_table in 

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let add_formals_to_local_symbol_table =
      let add_formal map (var_name, var_typ) p =
        L.set_value_name var_name p;
        let local = L.build_alloca (ltype_of_typ var_typ) var_name func_builder in
        ignore (L.build_store p local func_builder);
        ignore(Hashtbl.add map var_name local);
        map
      in
      List.fold_left2 add_formal local_symbol_table_init fdecl.sformals
          (Array.to_list (L.params the_function))
    in
    let local_symbol_table = add_formals_to_local_symbol_table
  in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block func_builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    
    (* Build the code for each statement in the function *)
    let complete_func_builder = build_stmt the_function local_symbol_table func_builder (SBlock fdecl.sbody) in
    
    let () = (match fdecl.srtyp with
      None -> ignore(L.build_ret_void complete_func_builder);
      | _ -> (* Add a return if the last block falls off the end *)
        ignore(add_terminal complete_func_builder (L.build_ret (L.const_int i32_t 0)))); 
    in
    builder
  in

  (* Psuedo "main" to encapsulate top-level statements. Borrowed from Boomslang *)
  let main_t : L.lltype =
    L.var_arg_function_type i32_t [| |] in
  let main_func_llvalue : L.llvalue =
    L.define_function "main" main_t the_module in
  let main_builder = L.builder_at_end context (L.entry_block main_func_llvalue) in


  let translate_code curr_symbol_table builder program = match program with
    SStmt(stmt) -> build_stmt main_func_llvalue curr_symbol_table builder stmt
    | SFunc_def(func) -> build_function_body curr_symbol_table builder func
  in
  let build_code = List.fold_left (translate_code var_map) main_builder code
  in
  ignore(L.build_ret (L.const_int i32_t 0) build_code);
  (* List.map (translate_code var_map) code; *)
  the_module
