module L = Llvm
module A = Ast
open Sast  

module StringMap = Map.Make(String)

let translate (globals, functions) =  
	let context =	L.global_context () in
	let the_module = L.create_module context "Hippograph" in 

	let void_t 	= 	L.void_type context
  and i32_t  = L.i32_type  context
  and i32_ptr_t  = L.pointer_type (L.i32_type context)
	and i8_t 	= 	L.i8_type context
  and i1_t   = L.i1_type   context
	and str_t	=	L.pointer_type (L.i8_type context)
  and void_ptr_t = L.pointer_type (L.i8_type context)
  in

	let ltype_of_typ = function
    | A.Void    -> void_t
    | A.Int     -> i32_t
    | A.Bool  	-> i1_t
    | A.String 	-> str_t
    | A.Node(_, _) -> void_ptr_t
    | A.Edge(_) -> void_ptr_t
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = 
        match t with
        | A.String -> L.const_bitcast (L.const_stringz context "") str_t
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* C Functions *)

  let print_t : L.lltype = L.var_arg_function_type void_t [| L.pointer_type i8_t |] in
  let print_func : L.llvalue = L.declare_function "printf" print_t the_module in

  let create_node_t : L.lltype = L.var_arg_function_type void_ptr_t [| |] in
  let create_node_func : L.llvalue = L.declare_function "create_node" create_node_t the_module in

  let create_edge_t : L.lltype = L.var_arg_function_type void_ptr_t [| |] in
  let create_edge_func : L.llvalue = L.declare_function "create_edge" create_edge_t the_module in

  let set_node_label_int_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; i32_t |] in
  let set_node_label_int_func : L.llvalue = L.declare_function "set_node_label_int" set_node_label_int_t the_module in

  let set_node_label_str_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; str_t |] in
  let set_node_label_str_func : L.llvalue = L.declare_function "set_node_label_str" set_node_label_str_t the_module in

  let set_node_label_void_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; void_ptr_t |] in
  let set_node_label_void_func : L.llvalue = L.declare_function "set_node_label_void" set_node_label_void_t the_module in

  let set_node_data_int_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; i32_t |] in
  let set_node_data_int_func : L.llvalue = L.declare_function "set_node_data_int" set_node_data_int_t the_module in

  let set_node_data_str_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; str_t |] in
  let set_node_data_str_func : L.llvalue = L.declare_function "set_node_data_str" set_node_data_str_t the_module in

  let set_node_data_void_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; void_ptr_t |] in
  let set_node_data_void_func : L.llvalue = L.declare_function "set_node_data_void" set_node_data_void_t the_module in

  let get_node_data_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let get_node_data_func : L.llvalue = L.declare_function "get_node_data" get_node_data_t the_module in

  let set_edge_src_int_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; void_ptr_t |] in
  let set_edge_src_int_func : L.llvalue = L.declare_function "set_edge_src_int" set_edge_src_int_t the_module in

  let set_edge_dest_int_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; void_ptr_t |] in
  let set_edge_dest_int_func : L.llvalue = L.declare_function "set_edge_dest_int" set_edge_dest_int_t the_module in

  let set_edge_w_int_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; i32_t |] in
  let set_edge_w_int_func : L.llvalue = L.declare_function "set_edge_w_int" set_edge_w_int_t the_module in

  let set_edge_w_str_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; str_t |] in
  let set_edge_w_str_func : L.llvalue = L.declare_function "set_edge_w_str" set_edge_w_str_t the_module in

  let set_edge_w_void_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; void_ptr_t |] in
  let set_edge_w_void_func : L.llvalue = L.declare_function "set_edge_w_void" set_edge_w_void_t the_module in

  let get_edge_w_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let get_edge_w_func : L.llvalue = L.declare_function "get_edge_w" get_edge_w_t the_module in

  let function_decls : (L.llvalue * sfdecl) StringMap.t =
    let function_decl m (sfdecl : sfdecl) =
      let name = sfdecl.sfname
      and formal_types = 
		    Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) sfdecl.sargs)
      in let ftype = 
        L.function_type (ltype_of_typ sfdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, sfdecl) m in
    List.fold_left function_decl StringMap.empty functions in

	let build_function_body (sfdecl : sfdecl) =
    let (the_function, _) = StringMap.find sfdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let add_arg m (t, n) p = L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m in

    let add_local builder m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m in

    let local_vars =
      List.fold_left2 add_arg StringMap.empty sfdecl.sargs
          (Array.to_list (L.params the_function)) in

    (* Return the value for a variable or formal argument *)
    let lookup vars n = try StringMap.find n vars
                        with Not_found -> StringMap.find n global_vars
    in

    let rec expr vars builder ((ty,e) : sexpr) = match e with
    | SStringlit s -> L.build_global_stringptr s "str" builder
    | SIntlit i -> L.const_int i32_t i
    | SBoollit b -> L.const_int i1_t (if b then 1 else 0)
    | SVar s -> L.build_load (lookup vars s) s builder
    | SUnop(op, e) ->
        let e' = expr vars builder e in
        (match op with
          A.Neg     -> L.build_neg
        | A.Not     -> L.build_not) e' "tmp" builder
    | SBinop (e1, op, e2) ->
        let e1' = expr vars builder e1
        and e2' = expr vars builder e2 in
        (match op with
          A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mul     -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Eq      -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Lt      -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Gt      -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
    | SFCall ("print", [e]) ->
       L.build_call print_func [| str_format_str ; ( expr vars builder e ) |] "" builder
    | SFCall ("print_int", [e]) | SFCall ("print_bool", [e]) ->
  		 L.build_call print_func [| int_format_str ; ( expr vars builder e ) |] "" builder
    | SFCall (f, act) ->
       let (fdef, sfdecl) = StringMap.find f function_decls in
       let actuals = List.rev (List.map (expr vars builder) (List.rev act)) in
       let result = (match sfdecl.styp with A.Void -> "" | _ -> f ^ "_result") in
       L.build_call fdef (Array.of_list actuals) result builder
    | SMCall (n, "get_data", []) ->
       let n_ptr = expr vars builder n in
       let ret = L.build_call get_node_data_func [| n_ptr |] "tmp_data" builder in
       (match ty with
       | String -> ret
       | Int -> L.build_load (L.build_bitcast ret i32_ptr_t "bitcast" builder) "deref" builder);
    | SMCall (n, "weight", []) ->
       let n_ptr = expr vars builder n in
       let ret = L.build_call get_edge_w_func [| n_ptr |] "tmp_data" builder in
       (match ty with
       | String -> ret
       | Int -> L.build_load (L.build_bitcast ret i32_ptr_t "bitcast" builder) "deref" builder
       | Void -> L.build_load (L.build_bitcast ret void_ptr_t "bitcast" builder) "deref" builder);
    | SAsn (s, e) ->
       let e' = expr vars builder e in
       ignore (L.build_store e' (lookup vars s) builder); e'
    | SEdgeExpr(s, d, w) ->
       let s' = expr vars builder s in
       let d' = expr vars builder d in
       let w' = expr vars builder w in
       let n = L.build_call create_edge_func [||] "create_edge" builder in
       (match s with
        | (A.Node(_,_), _) -> ignore (L.build_call set_edge_src_int_func [| n; s' |] "" builder)
        | _ -> ());
       (match d with
        | (A.Node(_,_), _) -> ignore (L.build_call set_edge_dest_int_func [| n; d' |] "" builder)
        | _ -> ());
       (match w with
        | (A.Int, v) -> if v = SNull then () else ignore (L.build_call set_edge_w_int_func [| n; w' |] "" builder)
        | (A.String, v) -> if v = SNull then () else ignore (L.build_call set_edge_w_str_func [| n; w' |] "" builder)
        | (A.Void, v) -> if v = SNull then () else ignore (L.build_call set_edge_w_void_func [| n; w' |] "" builder)
        | _ -> ());
       n
    | SNodeExpr (l, d) ->
       let l' = expr vars builder l in
       let d' = expr vars builder d in
       let n = L.build_call create_node_func [||] "create_node" builder in
       (match l with
        | (A.Int, _) -> ignore (L.build_call set_node_label_int_func [| n; l' |] "" builder)
        | (A.String, _) -> ignore (L.build_call set_node_label_str_func [| n; l' |] "" builder)
        | (A.Void, _) -> ignore (L.build_call set_node_label_void_func [| n; l' |] "" builder)
        | _ -> () (* TODO *));
       (match d with
        | (A.Int, v) -> if v = SNull then () else ignore (L.build_call set_node_data_int_func [| n; d' |] "" builder)
        | (A.String, v) -> if v = SNull then () else ignore (L.build_call set_node_data_str_func [| n; d' |] "" builder)
        | (A.Void, v) -> if v = SNull then () else ignore (L.build_call set_node_data_void_func [| n; d' |] "" builder)
        | _ -> () (* TODO *));
       n
    | SNull ->
       L.const_null i32_t
    | SNoexpr ->
       L.undef (L.void_type context) (* placeholder *)
  	in 

    let add_terminal builder instr =
      (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	    | Some _ -> ()
      | None -> ignore (instr builder)
    in

    let rec stmt (vars, builder) = function
	    | SBlock sl ->
        List.fold_left stmt (vars, builder) sl
      (* Generate code for this expression, return resulting builder *)
      | SExpr e ->
        let _ = expr vars builder e in (vars, builder)
      | SVdecl (ty, s, e) ->
        let vars' = add_local builder vars (ty, s) in
        let _ = expr vars' builder e in (vars', builder)
      | SReturn e ->
        let _ = match sfdecl.styp with
                (* Special "return nothing" instr *)
                | A.Void -> L.build_ret_void builder 
                (* Build return statement *)
                | _ -> L.build_ret (expr vars builder e) builder 
        in (vars, builder)
      | SIf (p, then_stmt, else_stmt) ->
        let bool_val = expr vars builder p in
        let merge_bb = L.append_block context "merge" the_function in

        let then_bb = L.append_block context "then" the_function in
        let _, builder' = stmt (vars, L.builder_at_end context then_bb) then_stmt in
        add_terminal builder' (L.build_br merge_bb);

        let else_bb = L.append_block context "else" the_function in
        let _, builder' = stmt (vars, L.builder_at_end context else_bb) else_stmt in
        add_terminal builder' (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        (vars, L.builder_at_end context merge_bb)

      | SWhile (p, body) ->
        let p_bb = L.append_block context "while" the_function in
        ignore (L.build_br p_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        let _, builder' = stmt (vars, L.builder_at_end context body_bb) body in
        add_terminal builder' (L.build_br p_bb);

        let p_builder = L.builder_at_end context p_bb in
        let bool_val = expr vars p_builder p in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb p_builder);
        (vars, L.builder_at_end context merge_bb)


        | SFor (e1, p, e2, body) -> stmt (vars, builder)
         (SBlock [SExpr e1 ; SWhile (p, SBlock [body ; SExpr e2]) ] )
    in

    let (_, builder) = stmt (local_vars, builder) (SBlock sfdecl.sbody) in

    add_terminal builder (match sfdecl.styp with
                            A.Void -> L.build_ret_void
                          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
