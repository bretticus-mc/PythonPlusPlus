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
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context 
  and none_t     = L.void_type   context
in

  (* Return the LLVM type for a PythonPP type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.None  -> none_t
  in

  (* Create a map of global variables after creating each 
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in
  *)

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

  (* Return the value for a variable or formal argument.
      Check local names first, then global names *)
  let lookup symbol_table var_name = try Hashtbl.find symbol_table var_name
    (* with Not_found -> StringMap.find n global_vars *)
    with Not_found -> raise (Failure ("can't find variable"))
  in
 
  (* Construct code for an expression; return its value *)
  let rec build_expr curr_symbol_table builder ((_, e) : sexpr) = match e with
    SLiteral i  -> L.const_int i32_t i
(*  | SStringLit s -> *)
    | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
    | SId s       -> L.build_load (lookup curr_symbol_table s) s builder
    | SAssign (s, e) -> let e' = build_expr curr_symbol_table builder e in
      ignore(L.build_store e' (lookup curr_symbol_table s) builder); e'
    | SVariableInit(var_name, var_typ, s_expr) -> let s_expr' = build_expr curr_symbol_table builder s_expr in
      let var_allocation = L.build_alloca (ltype_of_typ var_typ) var_name builder
      in
      ignore(Hashtbl.add curr_symbol_table var_name var_allocation);
      ignore(L.build_store s_expr' var_allocation builder); s_expr' (* TODO: Should s_expr' be return value? *)
    | SBinop (e1, op, e2) ->
      let e1' = build_expr curr_symbol_table builder e1
      and e2' = build_expr curr_symbol_table builder e2 in
      (match op with
        A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Equal   -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Less    -> L.build_icmp L.Icmp.Slt
      ) e1' e2' "tmp" builder
    | SCall ("print", [e]) ->
      let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
     (* L.build_call printf_func [|(build_expr curr_symbol_table builder e);|] *)
      L.build_call printf_func [| int_format_str ; (build_expr curr_symbol_table builder e) |]
        "printf" builder
    | SCall (f, args) ->
      let (fdef, fdecl) = StringMap.find f function_decls in
      let llargs = List.rev (List.map (build_expr curr_symbol_table builder) (List.rev args)) in
      let result = f ^ "_result" in
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
      ignore (build_stmt the_function curr_symbol_table (L.builder_at_end context then_bb) then_stmt);
      let else_bb = L.append_block context "else" the_function in
      ignore (build_stmt the_function curr_symbol_table (L.builder_at_end context else_bb) else_stmt);

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
      add_terminal (build_stmt the_function curr_symbol_table (L.builder_at_end context body_bb) body) build_br_while;

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

    (* Add a return if the last block falls off the end *)
    add_terminal complete_func_builder (L.build_ret (L.const_int i32_t 0)); 
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