(* Authors:
  Benjamin Lewinter bsl2121
  Irina Mateescu    im2441
  Harry Smith       hs3061
  Yasunari Watanabe yw3239
*)

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

	let rec ltype_of_typ = function
    | A.Void    -> void_t
    | A.Int     -> i32_t
    | A.Bool  	-> i1_t
    | A.String 	-> str_t
    | A.Fun(ret_t, args) ->
      let formal_types = Array.of_list (List.map ltype_of_typ args) in
      L.pointer_type (L.function_type (ltype_of_typ ret_t) formal_types)
    | A.Node(_, _) -> void_ptr_t
    | A.Edge(_) -> void_ptr_t
    | A.Graph(_,_,_) -> void_ptr_t
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

  let strcmp_t : L.lltype = L.var_arg_function_type i32_t [| str_t; str_t |] in
  let strcmp_func : L.llvalue = L.declare_function "strcmp" strcmp_t the_module in

  let create_graph_t : L.lltype = L.var_arg_function_type void_ptr_t [| |] in
  let create_graph_func : L.llvalue = L.declare_function "create_graph" create_graph_t the_module in

  let add_node_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; void_ptr_t |] in
  let add_node_func : L.llvalue = L.declare_function "add_node" add_node_t the_module in

  let create_node_t : L.lltype = L.var_arg_function_type void_ptr_t [| |] in
  let create_node_func : L.llvalue = L.declare_function "create_node" create_node_t the_module in

  let clone_node_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let clone_node_func : L.llvalue = L.declare_function "clone_node" clone_node_t the_module in

  let create_edge_t : L.lltype = L.var_arg_function_type void_ptr_t [| |] in
  let create_edge_func : L.llvalue = L.declare_function "create_edge" create_edge_t the_module in

  let add_edge_int_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; void_ptr_t; i32_t; i32_t |] in
  let add_edge_int_func : L.llvalue = L.declare_function "add_edge_int" add_edge_int_t the_module in

  let add_edge_bool_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; void_ptr_t; i1_t; i1_t |] in
  let add_edge_bool_func : L.llvalue = L.declare_function "add_edge_bool" add_edge_bool_t the_module in

  let add_edge_str_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; void_ptr_t; str_t; str_t |] in
  let add_edge_str_func : L.llvalue = L.declare_function "add_edge_str" add_edge_str_t the_module in

  let set_node_label_int_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; i32_t |] in
  let set_node_label_int_func : L.llvalue = L.declare_function "set_node_label_int" set_node_label_int_t the_module in

  let set_node_label_bool_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; i1_t |] in
  let set_node_label_bool_func : L.llvalue = L.declare_function "set_node_label_bool" set_node_label_bool_t the_module in

  let set_node_label_str_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; str_t |] in
  let set_node_label_str_func : L.llvalue = L.declare_function "set_node_label_str" set_node_label_str_t the_module in

  let set_node_data_int_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; i32_t; i1_t |] in
  let set_node_data_int_func : L.llvalue = L.declare_function "set_node_data_int" set_node_data_int_t the_module in

  let set_node_data_bool_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; i1_t; i1_t |] in
  let set_node_data_bool_func : L.llvalue = L.declare_function "set_node_data_bool" set_node_data_bool_t the_module in

  let set_node_data_str_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; str_t; i1_t |] in
  let set_node_data_str_func : L.llvalue = L.declare_function "set_node_data_str" set_node_data_str_t the_module in

  let get_node_label_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let get_node_label_func : L.llvalue = L.declare_function "get_node_label" get_node_label_t the_module in

  let get_node_data_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let get_node_data_func : L.llvalue = L.declare_function "get_node_data" get_node_data_t the_module in

  let graph_has_node_int_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i32_t |] in
  let graph_has_node_int_func : L.llvalue = L.declare_function "graph_has_node_int" graph_has_node_int_t the_module in

  let graph_has_node_str_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; str_t |] in
  let graph_has_node_str_func : L.llvalue = L.declare_function "graph_has_node_str" graph_has_node_str_t the_module in

  let graph_has_node_bool_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i1_t |] in
  let graph_has_node_bool_func : L.llvalue = L.declare_function "graph_has_node_int" graph_has_node_bool_t the_module in

  let graph_set_edge_int_int_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i32_t; i32_t; i32_t |] in
  let graph_set_edge_int_int_func : L.llvalue = L.declare_function "graph_set_edge_int_int" graph_set_edge_int_int_t the_module in

  let graph_set_edge_bool_int_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i1_t; i1_t; i32_t |] in
  let graph_set_edge_bool_int_func : L.llvalue = L.declare_function "graph_set_edge_int_int" graph_set_edge_bool_int_t the_module in

  let graph_set_edge_str_bool_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; str_t; str_t; i1_t |] in
  let graph_set_edge_str_bool_func : L.llvalue = L.declare_function "graph_set_edge_str_int" graph_set_edge_str_bool_t the_module in

  let graph_set_edge_bool_str_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i1_t; i1_t; str_t |] in
  let graph_set_edge_bool_str_func : L.llvalue = L.declare_function "graph_set_edge_int_str" graph_set_edge_bool_str_t the_module in

  let graph_set_edge_int_bool_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i32_t; i32_t; i1_t |] in
  let graph_set_edge_int_bool_func : L.llvalue = L.declare_function "graph_set_edge_int_int" graph_set_edge_int_bool_t the_module in

  let graph_set_edge_bool_bool_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i1_t; i1_t; i1_t |] in
  let graph_set_edge_bool_bool_func : L.llvalue = L.declare_function "graph_set_edge_int_int" graph_set_edge_bool_bool_t the_module in

  let graph_set_edge_int_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i32_t; i32_t |] in
  let graph_set_edge_int_func : L.llvalue = L.declare_function "graph_set_edge_int" graph_set_edge_int_t the_module in

  let graph_set_edge_str_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; str_t; str_t |] in
  let graph_set_edge_str_func : L.llvalue = L.declare_function "graph_set_edge_str" graph_set_edge_str_t the_module in

  let graph_set_edge_bool_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i1_t; i1_t |] in
  let graph_set_edge_bool_func : L.llvalue = L.declare_function "graph_set_edge_int" graph_set_edge_bool_t the_module in

  let graph_set_edge_str_int_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; str_t; str_t; i32_t |] in
  let graph_set_edge_str_int_func : L.llvalue = L.declare_function "graph_set_edge_str_int" graph_set_edge_str_int_t the_module in

  let graph_set_edge_int_str_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i32_t; i32_t; str_t |] in
  let graph_set_edge_int_str_func : L.llvalue = L.declare_function "graph_set_edge_int_str" graph_set_edge_int_str_t the_module in

  let graph_set_edge_str_str_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; str_t; str_t; str_t |] in
  let graph_set_edge_str_str_func : L.llvalue = L.declare_function "graph_set_edge_str_str" graph_set_edge_str_str_t the_module in

  let get_node_by_label_int_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; i32_t |] in
  let get_node_by_label_int_func : L.llvalue = L.declare_function "get_node_by_label_int" get_node_by_label_int_t the_module in
  let get_node_by_label_int_opt_func : L.llvalue = L.declare_function "get_node_by_label_int_opt" get_node_by_label_int_t the_module in

  let get_node_by_label_bool_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; i1_t |] in
  let get_node_by_label_bool_opt_func : L.llvalue = L.declare_function "get_node_by_label_bool_opt" get_node_by_label_bool_t the_module in

  let get_node_by_label_str_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; str_t |] in
  let get_node_by_label_str_func : L.llvalue = L.declare_function "get_node_by_label_str" get_node_by_label_str_t the_module in
  let get_node_by_label_str_opt_func : L.llvalue = L.declare_function "get_node_by_label_str_opt" get_node_by_label_str_t the_module in

  let print_node_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t |] in
  let print_node_func : L.llvalue = L.declare_function "print_node" print_node_t the_module in

  let print_graph_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t |] in
  let print_graph_func : L.llvalue = L.declare_function "print_graph" print_graph_t the_module in

  let set_edge_w_int_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; i32_t; i1_t |] in
  let set_edge_w_int_func : L.llvalue = L.declare_function "set_edge_w_int" set_edge_w_int_t the_module in

  let set_edge_w_bool_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; i1_t; i1_t |] in
  let set_edge_w_bool_func : L.llvalue = L.declare_function "set_edge_w_bool" set_edge_w_bool_t the_module in

  let set_edge_w_str_t : L.lltype = L.var_arg_function_type void_t [| void_ptr_t; str_t; i1_t |] in
  let set_edge_w_str_func : L.llvalue = L.declare_function "set_edge_w_str" set_edge_w_str_t the_module in

  let get_edge_src_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let get_edge_src_func : L.llvalue = L.declare_function "get_edge_src" get_edge_src_t the_module in

  let get_edge_dst_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let get_edge_dst_func : L.llvalue = L.declare_function "get_edge_dst" get_edge_dst_t the_module in

  let get_edge_w_int_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t |] in
  let get_edge_w_int_func : L.llvalue = L.declare_function "get_edge_w_int" get_edge_w_int_t the_module in

  let get_edge_w_bool_t : L.lltype = L.var_arg_function_type i1_t [| void_ptr_t |] in
  let get_edge_w_bool_func : L.llvalue = L.declare_function "get_edge_w_int" get_edge_w_bool_t the_module in

  let get_edge_w_str_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let get_edge_w_str_func : L.llvalue = L.declare_function "get_edge_w_str" get_edge_w_str_t the_module in

  let graph_to_node_iterable_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let graph_to_node_iterable_func : L.llvalue = L.declare_function "graph_to_node_iterable" graph_to_node_iterable_t the_module in

  let graph_to_edge_iterable_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let graph_to_edge_iterable_func : L.llvalue = L.declare_function "graph_to_edge_iterable" graph_to_edge_iterable_t the_module in

  let get_graph_next_node_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let get_graph_next_node_func : L.llvalue = L.declare_function "get_graph_next_node" get_graph_next_node_t the_module in

  let get_graph_next_edge_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let get_graph_next_edge_func : L.llvalue = L.declare_function "get_graph_next_edge" get_graph_next_edge_t the_module in

  let graph_set_node_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; void_ptr_t |] in
  let graph_set_node_func : L.llvalue = L.declare_function "graph_set_node" graph_set_node_t the_module in

  let remove_edge_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; void_ptr_t |] in
  let remove_edge_func : L.llvalue = L.declare_function "remove_edge" remove_edge_t the_module in

  let remove_node_int_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i32_t |] in
  let remove_node_int_func : L.llvalue = L.declare_function "remove_node_int" remove_node_int_t the_module in

  let remove_node_str_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; str_t |] in
  let remove_node_str_func : L.llvalue = L.declare_function "remove_node_str" remove_node_str_t the_module in

  let remove_node_bool_t : L.lltype = L.var_arg_function_type i32_t [| void_ptr_t; i1_t |] in
  let remove_node_bool_func : L.llvalue = L.declare_function "remove_node_int" remove_node_bool_t the_module in

  let get_edge_by_src_and_dst_int_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; i32_t; i32_t |] in
  let get_edge_by_src_and_dst_int_func : L.llvalue = L.declare_function "get_edge_by_src_and_dst_int" get_edge_by_src_and_dst_int_t the_module in

  let get_edge_by_src_and_dst_bool_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; i1_t; i1_t |] in
  let get_edge_by_src_and_dst_bool_func : L.llvalue = L.declare_function "get_edge_by_src_and_dst_int" get_edge_by_src_and_dst_bool_t the_module in

  let get_edge_by_src_and_dst_str_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; void_ptr_t; void_ptr_t |] in
  let get_edge_by_src_and_dst_str_func : L.llvalue = L.declare_function "get_edge_by_src_and_dst_str" get_edge_by_src_and_dst_str_t the_module in

  let neighbors_one_arg_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t |] in
  let neighbors_one_arg_func : L.llvalue = L.declare_function "neighbors_one_arg" neighbors_one_arg_t the_module in

  let neighbors_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; i32_t; i1_t |] in
  let neighbors_func : L.llvalue = L.declare_function "neighbors" neighbors_t the_module in

  let find_data_int_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; i32_t |] in
  let find_data_int_func : L.llvalue = L.declare_function "find_data_int" find_data_int_t the_module in

  let find_data_bool_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; i1_t |] in
  let find_data_bool_func : L.llvalue = L.declare_function "find_data_int" find_data_bool_t the_module in

  let find_data_str_t : L.lltype = L.var_arg_function_type void_ptr_t [| void_ptr_t; str_t |] in
  let find_data_str_func : L.llvalue = L.declare_function "find_data_str" find_data_str_t the_module in

  let are_neighbors_int_t : L.lltype = L.var_arg_function_type i1_t [| void_ptr_t; i32_t; i32_t |] in
  let are_neighbors_int_func : L.llvalue = L.declare_function "are_neighbors_int" are_neighbors_int_t the_module in

  let are_neighbors_bool_t : L.lltype = L.var_arg_function_type i1_t [| void_ptr_t; i1_t; i1_t |] in
  let are_neighbors_bool_func : L.llvalue = L.declare_function "are_neighbors_int" are_neighbors_bool_t the_module in

  let are_neighbors_str_t : L.lltype = L.var_arg_function_type i1_t [| void_ptr_t; str_t; str_t |] in
  let are_neighbors_str_func : L.llvalue = L.declare_function "are_neighbors_str" are_neighbors_str_t the_module in

  let is_empty_t : L.lltype = L.var_arg_function_type i1_t [| void_ptr_t |] in
  let is_empty_func : L.llvalue = L.declare_function "is_empty" is_empty_t the_module in

  let dfs_int_t : L.lltype = L.function_type void_ptr_t [| void_ptr_t; i32_t |] in
  let dfs_int_func : L.llvalue = L.declare_function "dfs_int" dfs_int_t the_module in
  
  let dfs_str_t : L.lltype = L.function_type void_ptr_t [| void_ptr_t; str_t |] in
  let dfs_str_func : L.llvalue = L.declare_function "dfs_str" dfs_str_t the_module in
  
  let bfs_int_t : L.lltype = L.function_type void_ptr_t [| void_ptr_t; i32_t |] in
  let bfs_int_func : L.llvalue = L.declare_function "bfs_int" bfs_int_t the_module in
  
  let bfs_str_t : L.lltype = L.function_type void_ptr_t [| void_ptr_t; str_t |] in
  let bfs_str_func : L.llvalue = L.declare_function "bfs_str" bfs_str_t the_module in

  let global_fdecls : (L.llvalue * sfdecl) StringMap.t =
    let function_decl m (sfdecl : sfdecl) =
      let name = sfdecl.sfname
      and formal_types = 
		    Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) sfdecl.sargs)
      in let ftype = 
        L.function_type (ltype_of_typ sfdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, sfdecl) m in
    List.fold_left function_decl StringMap.empty functions in

	let build_function_body local_fdecls sfdecl =
    let (the_function, _) = try StringMap.find sfdecl.sfname local_fdecls
                            with Not_found -> StringMap.find sfdecl.sfname global_fdecls
                            in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let add_arg builder m (t, n) p = L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m in

    let add_local_var builder m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m in

    let local_vars =
      List.fold_left2 (add_arg builder) StringMap.empty sfdecl.sargs
          (Array.to_list (L.params the_function)) in

    (* Return the value for a variable or formal argument *)
    let lookup vars n = try StringMap.find n vars
                        with Not_found -> StringMap.find n global_vars in

    let funcs' = List.map (fun (ty, name) -> 
                            match ty with
                            | A.Fun(ret_t, args_t) ->
                               (ty, {styp = ret_t; sfname = name; sargs = List.map (fun t -> (t, "x")) args_t; sbody = []})
                            | _ -> raise A.Unsupported_constructor)
                         (List.filter (fun (ty, _) -> match ty with A.Fun(_) -> true | _ -> false) sfdecl.sargs) in

    let add_local_fdecl vars fdecls (t, n) =
      match t with
      | A.Fun(ret_t, args_t) ->
        StringMap.add n (lookup vars n, {styp = ret_t; sfname = n; sargs = List.map (fun t -> (t, "x")) args_t; sbody = []}) fdecls
      | _ -> raise A.Unsupported_constructor in

    let local_fdecls = 
      List.fold_left (fun m (_, sfdecl) ->
                        StringMap.add sfdecl.sfname (lookup local_vars sfdecl.sfname, sfdecl) m) StringMap.empty funcs' in


    let lookup_func fdecls n = try StringMap.find n fdecls
                              with Not_found -> StringMap.find n global_fdecls
    in
    
    let rec expr fdecls vars builder ((ty,e) : sexpr) = match e with
      | SStringlit s -> L.build_global_stringptr s "str" builder
      | SIntlit i -> L.const_int i32_t i
      | SBoollit b -> L.const_int i1_t (if b then 1 else 0)
      | SVar s -> L.build_load (lookup vars s) s builder
      | SUnop(op, e) ->
          let e' = expr fdecls vars builder e in
          (match op with
            A.Neg     -> L.build_neg
          | A.Not     -> L.build_not) e' "tmp" builder
      | SBinop ((e1_t, e1), op, e2) ->
          let e1' = expr fdecls vars builder (e1_t, e1)
          and e2' = expr fdecls vars builder e2 in
          (match e1_t with
          | A.String ->
            let e1' = L.build_call strcmp_func [| e1'; e2' |] "strcmp" builder in
            let e2' = L.const_int i32_t 0 in
            (match op with
            | A.Eq      -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Lt      -> L.build_icmp L.Icmp.Slt
            | A.Leq     -> L.build_icmp L.Icmp.Sle
            | A.Gt      -> L.build_icmp L.Icmp.Sgt
            | A.Geq     -> L.build_icmp L.Icmp.Sge
            | _         -> raise A.Unsupported_constructor)
            e1' e2' "tmp" builder
          | _ ->
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
            ) e1' e2' "tmp" builder)
      | SFunsig (t, bl, _) -> 
        let t_list = List.map fst bl in 
        let new_fun_t = L.function_type (ltype_of_typ t) (Array.of_list (List.map ltype_of_typ t_list)) in
        L.define_function "temp" new_fun_t the_module 
      | SFCall ("print", [e]) ->
         L.build_call print_func [| str_format_str ; ( expr fdecls vars builder e ) |] "" builder
      | SFCall ("print_int", [e]) | SFCall ("print_bool", [e]) ->
         L.build_call print_func [| int_format_str ; ( expr fdecls vars builder e ) |] "" builder
      | SFCall (f, act) ->
         let (fdef, sfdecl) = lookup_func fdecls f in
         let actuals = List.rev (List.map (expr fdecls vars builder) (List.rev act)) in
         let result = (match sfdecl.styp with A.Void -> "" | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
      | SMCall (e, s, args) ->
         handle_mcall_expr fdecls vars builder ty e args s
      | SAsn (s, (t, v)) ->
         (* If e is SNull, change to default value for type s *)
         let v = match v with
         | SNull -> (match t with
           | A.Int -> SIntlit 0
           | A.Bool -> SBoollit false
           | A.String -> SStringlit ""
           | _ -> raise A.Unsupported_constructor)
         | _ -> v
         in
         let e' = expr fdecls vars builder (t, v) in
         (match t with
         | A.Fun(_) -> ignore (L.build_store e' (lookup vars s) builder); e'
         | _ -> ignore (L.build_store e' (lookup vars s) builder); e')
      | SGraphExpr(nlist, elist) ->
         let g = L.build_call create_graph_func [||] "create_graph" builder in
         ignore (List.map (fun n -> L.build_call add_node_func [| g; expr fdecls vars builder n |] "add_node" builder) nlist);
         ignore (List.map (fun e -> let f, src', dst' =
                             match e with
                             | (_, SEdgeExpr(src, dst, _)) ->
                                (match src with
                                 | (A.Int, _) -> add_edge_int_func
                                 | (A.Bool, _) -> add_edge_bool_func
                                 | (A.String, _) -> add_edge_str_func
                                 | _ -> raise A.Unsupported_constructor),
                                expr fdecls vars builder src,
                                expr fdecls vars builder dst
                             | _ -> raise A.Unsupported_constructor
                             in L.build_call f [| g; expr fdecls vars builder e; src'; dst' |] "add_edge" builder) elist);
         g
      | SEdgeExpr(_, _, w) ->
         let e = L.build_call create_edge_func [||] "edge" builder in
         let w' = expr fdecls vars builder w in
         (match w with
         | (A.Int, SNull) -> ignore (L.build_call set_edge_w_int_func [| e; w'; L.const_int i1_t 0 |] "" builder)
         | (A.Int, _) -> ignore (L.build_call set_edge_w_int_func [| e; w'; L.const_int i1_t 1 |] "" builder)
         | (A.Bool, SNull) -> ignore (L.build_call set_edge_w_bool_func [| e; w'; L.const_int i1_t 0 |] "" builder)
         | (A.Bool, _) -> ignore (L.build_call set_edge_w_bool_func [| e; w'; L.const_int i1_t 1 |] "" builder)
         | (A.String, SNull) -> ignore (L.build_call set_edge_w_str_func [| e; w'; L.const_int i1_t 0 |] "" builder)
         | (A.String, _) -> ignore (L.build_call set_edge_w_str_func [| e; w'; L.const_int i1_t 1 |] "" builder)
         | _ -> raise A.Unsupported_constructor);
         e
      | SNodeExpr (l, d) ->
         let l' = expr fdecls vars builder l in
         let d' = expr fdecls vars builder d in
         let n = L.build_call create_node_func [||] "create_node" builder in
         (match l with
          | (A.Int, _) -> ignore (L.build_call set_node_label_int_func [| n; l' |] "" builder)
          | (A.Bool, _) -> ignore (L.build_call set_node_label_bool_func [| n; l' |] "" builder)
          | (A.String, _) -> ignore (L.build_call set_node_label_str_func [| n; l' |] "" builder)
          | _ -> raise A.Unsupported_constructor);
         (match d with
          | (A.Int, v) ->
            if v = SNull
            then ignore (L.build_call set_node_data_int_func [| n; L.const_int i32_t 0; L.const_int i1_t 0 |] "" builder)
            else ignore (L.build_call set_node_data_int_func [| n; d'; L.const_int i1_t 1 |] "" builder)
          | (A.Bool, v) ->
            if v = SNull
            then ignore (L.build_call set_node_data_bool_func [| n; L.const_int i1_t 0; L.const_int i1_t 0 |] "" builder)
            else ignore (L.build_call set_node_data_bool_func [| n; d'; L.const_int i1_t 1 |] "" builder)
          | (A.String, v) ->
            if v = SNull
            then ignore (L.build_call set_node_data_str_func [| n; L.const_null str_t; L.const_int i1_t 0 |] "" builder)
            else ignore (L.build_call set_node_data_str_func [| n; d'; L.const_int i1_t 1 |] "" builder)
          | _ -> raise A.Unsupported_constructor);
         n
      | SNull ->
         (match ty with
          | A.Int -> L.const_null i32_t
          | A.Bool -> L.const_null i1_t
          | A.String -> L.const_null str_t
          | _ -> L.const_null void_ptr_t)
      | SNoexpr ->
         L.undef (L.void_type context) (* placeholder *)
    
    and handle_mcall_expr fdecls vars builder ty e args = function
    | "set_node" ->
         (match args with
          | ((A.Node(_), _) as n) :: [] ->
            let g_ptr = expr fdecls vars builder e in
            let n_ptr = expr fdecls vars builder n in
            let n_ptr' = L.build_call clone_node_func [| n_ptr |] "clone_node" builder in
            L.build_call graph_set_node_func [| g_ptr; n_ptr' |] "tmp_data" builder        
          | _ -> raise A.Unsupported_constructor)
    | "remove_node" ->
         (match args with
          | ((l_typ, _) as l) :: [] ->
            let g_ptr = expr fdecls vars builder e in
            let l' = expr fdecls vars builder l in 
            (match l_typ with
             | A.Int -> L.build_call remove_node_int_func [| g_ptr; l' |] "tmp_data" builder
             | A.String -> L.build_call remove_node_str_func [| g_ptr; l' |] "tmp_data" builder
             | A.Bool -> L.build_call remove_node_bool_func [| g_ptr; l' |] "tmp_data" builder
             | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | "remove_edge" ->
         (match args with
          | ((src_typ, _) as src) :: dst :: [] ->
             let g_ptr = expr fdecls vars builder e in
             let src_ptr = expr fdecls vars builder src in
             let dst_ptr = expr fdecls vars builder dst in
             let e_ptr = (match src_typ with
               | A.Int -> L.build_call get_edge_by_src_and_dst_int_func [| g_ptr; src_ptr; dst_ptr |] "get_edge_by_src_and_dst_int" builder
               | A.Bool -> L.build_call get_edge_by_src_and_dst_bool_func [| g_ptr; src_ptr; dst_ptr |] "get_edge_by_src_and_dst_bool" builder
               | A.String -> L.build_call get_edge_by_src_and_dst_str_func [| g_ptr; src_ptr; dst_ptr |] "get_edge_by_src_and_dst_str" builder
               | _ -> raise A.Unsupported_constructor) in
             L.build_call remove_edge_func [| g_ptr; e_ptr |] "remove_edge" builder
          | _ -> raise A.Unsupported_constructor)
    | "get_node" ->
         (match e, args with
          | (A.Graph(lt, _, _), _), label :: [] ->
            let g_ptr = expr fdecls vars builder e in
            let label' = expr fdecls vars builder label in
            (match lt with
             | A.Int -> L.build_call get_node_by_label_int_opt_func [| g_ptr; label' |] "get_node_by_label" builder
             | A.Bool -> L.build_call get_node_by_label_bool_opt_func [| g_ptr; label' |] "get_node_by_label" builder
             | A.String -> L.build_call get_node_by_label_str_opt_func [| g_ptr; label' |] "get_node_by_label" builder
             | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | "get_weight" ->
         (match e, args with
          | (A.Graph(lt, _, wt), _), src :: dst :: [] ->
             let g_ptr = expr fdecls vars builder e in
             let src' = expr fdecls vars builder src in
             let dst' = expr fdecls vars builder dst in
             let e_ptr = (match lt with
               | A.Int -> L.build_call get_edge_by_src_and_dst_int_func [| g_ptr; src'; dst' |] "get_edge_by_src_and_dst_int" builder
               | A.Bool -> L.build_call get_edge_by_src_and_dst_bool_func [| g_ptr; src'; dst' |] "get_edge_by_src_and_dst_bool" builder
               | A.String -> L.build_call get_edge_by_src_and_dst_str_func [| g_ptr; src'; dst' |] "get_edge_by_src_and_dst_str" builder
               | _ -> raise A.Unsupported_constructor) in
             (match wt with
               | A.Int -> L.build_call get_edge_w_int_func [| e_ptr |] "get_edge_w" builder
               | A.Bool -> L.build_call get_edge_w_bool_func [| e_ptr |] "get_edge_w" builder
               | A.String -> L.build_call get_edge_w_str_func [| e_ptr |] "get_edge_w" builder
               | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | "get_name" ->
         let n_ptr = expr fdecls vars builder e in
         let ret = L.build_call get_node_label_func [| n_ptr |] "tmp_data" builder in
         (match ty with
         | A.String -> ret
         | A.Int -> L.build_load (L.build_bitcast ret i32_ptr_t "bitcast" builder) "deref" builder
         | A.Bool -> L.build_load (L.build_bitcast ret i32_ptr_t "bitcast" builder) "deref" builder
         | _ -> raise A.Unsupported_constructor)
    | "has_node" ->
         (match args with
          | ((n_typ, _) as n) :: [] ->
            let g_ptr = expr fdecls vars builder e in
            let n' = expr fdecls vars builder n in
            (match n_typ with
             | A.Int -> L.build_call graph_has_node_int_func [| g_ptr; n' |] "tmp_data" builder
             | A.String -> L.build_call graph_has_node_str_func [| g_ptr; n' |] "tmp_data" builder
             | A.Bool -> L.build_call graph_has_node_bool_func [| g_ptr; n' |] "tmp_data" builder
             | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | "get_data" ->
         let n_ptr = expr fdecls vars builder e in
         let ret = L.build_call get_node_data_func [| n_ptr |] "tmp_data" builder in
         (match ty with
         | A.String -> ret
         | A.Bool -> L.build_load (L.build_bitcast ret i32_ptr_t "bitcast" builder) "deref" builder
         | A.Int -> L.build_load (L.build_bitcast ret i32_ptr_t "bitcast" builder) "deref" builder
         | _ -> raise A.Unsupported_constructor)
    | "set_edge" ->
         (match args with
          | ((src_typ, _) as src) :: dst :: ((w_typ, _) as w) :: [] ->
             let g_ptr = expr fdecls vars builder e in
             let src' = expr fdecls vars builder src in
             let dst' = expr fdecls vars builder dst in
             let w' = expr fdecls vars builder w in
             (match (src_typ, w_typ) with
             | (A.Int, A.Bool) -> L.build_call graph_set_edge_int_bool_func [| g_ptr; src'; dst'; w' |] "tmp_data" builder
             | (A.Int, A.Int) -> L.build_call graph_set_edge_int_int_func [| g_ptr; src'; dst'; w' |] "tmp_data" builder
             | (A.Bool, A.Bool) -> L.build_call graph_set_edge_bool_bool_func [| g_ptr; src'; dst'; w' |] "tmp_data" builder
             | (A.Bool, A.Int) -> L.build_call graph_set_edge_bool_int_func [| g_ptr; src'; dst'; w' |] "tmp_data" builder
             | (A.String, A.Bool) -> L.build_call graph_set_edge_str_bool_func [| g_ptr; src'; dst'; w' |] "tmp_data" builder
             | (A.Bool, A.String) -> L.build_call graph_set_edge_bool_str_func [| g_ptr; src'; dst'; w' |] "tmp_data" builder
             | (A.String, A.Int) -> L.build_call graph_set_edge_str_int_func [| g_ptr; src'; dst'; w' |] "tmp_data" builder
             | (A.Int, A.String) -> L.build_call graph_set_edge_int_str_func [| g_ptr; src'; dst'; w' |] "tmp_data" builder
             | (A.String, A.String) -> L.build_call graph_set_edge_str_str_func [| g_ptr; src'; dst'; w' |] "tmp_data" builder
             | _ -> raise A.Unsupported_constructor)
          | ((src_typ, _) as src) :: dst :: [] ->
             let g_ptr = expr fdecls vars builder e in
             let src' = expr fdecls vars builder src in
             let dst' = expr fdecls vars builder dst in
             (match src_typ with
              | A.Int -> L.build_call graph_set_edge_int_func [| g_ptr; src'; dst' |] "tmp_data" builder
              | A.String -> L.build_call graph_set_edge_str_func [| g_ptr; src'; dst' |] "tmp_data" builder
              | A.Bool -> L.build_call graph_set_edge_bool_func [| g_ptr; src'; dst' |] "tmp_data" builder
              | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | "set_data" ->
         (match args with
          | ((dt, dv) as d) :: [] ->
             let n_ptr = expr fdecls vars builder e in
             let d_ptr = expr fdecls vars builder d in
             (match dt with
              | A.Int ->
                 if dv = SNull
                 then L.build_call set_node_data_int_func [| n_ptr; L.const_int i32_t 0; L.const_int i1_t 0 |] "" builder
                 else L.build_call set_node_data_int_func [| n_ptr; d_ptr; L.const_int i1_t 1 |] "" builder
              | A.Bool ->
                 if dv = SNull
                 then L.build_call set_node_data_bool_func [| n_ptr; L.const_int i1_t 0; L.const_int i1_t 0 |] "" builder
                 else L.build_call set_node_data_bool_func [| n_ptr; d_ptr; L.const_int i1_t 1 |] "" builder
              | A.String ->
                 if dv = SNull
                 then L.build_call set_node_data_str_func [| n_ptr; L.const_null str_t; L.const_int i1_t 0 |] "" builder
                 else L.build_call set_node_data_str_func [| n_ptr; d_ptr; L.const_int i1_t 1 |] "" builder
              | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | "are_neighbors" ->
         (match e, args with
          | (A.Graph(lt, _, _), _), src :: dst :: [] ->
             let g_ptr = expr fdecls vars builder e in
             let src' = expr fdecls vars builder src in
             let dst' = expr fdecls vars builder dst in
             (match lt with
              | A.Int ->
                L.build_call are_neighbors_int_func [| g_ptr; src'; dst' |] "are_neighbors" builder
              | A.Bool ->
                L.build_call are_neighbors_bool_func [| g_ptr; src'; dst' |] "are_neighbors" builder
              | A.String ->
                L.build_call are_neighbors_str_func [| g_ptr; src'; dst' |] "are_neighbors" builder
              | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | "is_empty" ->
         (match e with
          | (A.Graph(_), _) ->
             let g_ptr = expr fdecls vars builder e in
             L.build_call is_empty_func [| g_ptr |] "is_empty" builder
          | _ -> raise A.Unsupported_constructor)
    | "print" ->
         (match e with
          | (A.Graph(_), _) ->
             let g_ptr = expr fdecls vars builder e in
             L.build_call print_graph_func [| g_ptr |] "" builder
          | (A.Node(_), _) ->
             let n_ptr = expr fdecls vars builder e in
             L.build_call print_node_func [| n_ptr |] "" builder
          | _ -> raise A.Unsupported_constructor)
    | "neighbors" ->
         (match e, args with
          | (A.Graph(_), _), ((nlt, _) as nl) :: [] ->
             let g_ptr = expr fdecls vars builder e in
             let nl' = expr fdecls vars builder nl in 
             let n_ptr = (match nlt with
                         | A.Int | A.Bool -> L.build_call get_node_by_label_int_func [| g_ptr; nl' |] "get_node_by_label_int" builder
                         | A.String -> L.build_call get_node_by_label_str_func [| g_ptr; nl' |] "get_node_by_label_str" builder
                         | _ -> raise A.Unsupported_constructor) in
             L.build_call neighbors_one_arg_func [| n_ptr |] "neihghbors_one_arg" builder
          | (A.Graph(_), _), ((nlt, _) as nl) :: level :: include_current :: [] ->
             let g_ptr = expr fdecls vars builder e in
             let nl' = expr fdecls vars builder nl in
             let level' = expr fdecls vars builder level in
             let include_current' = expr fdecls vars builder include_current in
             let n_ptr = (match nlt with
                         | A.Int | A.Bool -> L.build_call get_node_by_label_int_func [| g_ptr; nl' |] "get_node_by_label_int" builder
                         | A.String -> L.build_call get_node_by_label_str_func [| g_ptr; nl' |] "get_node_by_label_str" builder
                         | _ -> raise A.Unsupported_constructor) in
             L.build_call neighbors_func [| n_ptr; level'; include_current' |] "neighbors" builder
          | _ -> raise A.Unsupported_constructor)
    | "find" ->
         (match e, args with
          | (A.Graph(_, dt, _), _), d :: [] ->
            let g_ptr = expr fdecls vars builder e in
            let d' = expr fdecls vars builder d in
            (match dt with
             | A.Int -> L.build_call find_data_int_func [| g_ptr; d' |] "find_data" builder
             | A.Bool -> L.build_call find_data_bool_func [| g_ptr; d' |] "find_data" builder
             | A.String -> L.build_call find_data_str_func [| g_ptr; d' |] "find_data" builder
             | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | "dfs" ->
         (match e, args with
          | (A.Graph(lt, _, _), _), l :: [] ->
            let g_ptr = expr fdecls vars builder e in
            let l' = expr fdecls vars builder l in
            (match lt with
             | A.Int -> L.build_call dfs_int_func [|g_ptr; l'|] "dfs_int" builder
             | A.Bool -> L.build_call dfs_int_func [|g_ptr; l'|] "dfs_int" builder
             | A.String -> L.build_call dfs_str_func [|g_ptr; l'|] "dfs_str" builder
             | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | "bfs" ->
         (match e, args with
          | (A.Graph(lt, _, _), _), l :: [] ->
            let g_ptr = expr fdecls vars builder e in
            let l' = expr fdecls vars builder l in
            (match lt with
             | A.Int -> L.build_call bfs_int_func [|g_ptr; l'|] "bfs_int" builder
             | A.Bool -> L.build_call bfs_int_func [|g_ptr; l'|] "bfs_int" builder
             | A.String -> L.build_call bfs_str_func [|g_ptr; l'|] "bfs_str" builder
             | _ -> raise A.Unsupported_constructor)
          | _ -> raise A.Unsupported_constructor)
    | _ -> raise A.Unsupported_constructor
    in
 
    let add_terminal builder instr =
      (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	    | Some _ -> ()
      | None -> ignore (instr builder)
    in

    let rec stmt (fdecls, vars, builder) = function
	    | SBlock sl ->
        List.fold_left stmt (fdecls, vars, builder) sl
      (* Generate code for this expression, return resulting builder *)
      | SExpr e ->
        let _ = expr fdecls vars builder e in (fdecls, vars, builder)
      (* fun f = ... (...) (...) *)
      | SVdecl (ty, s, e) ->
        (match e with
        | (A.Fun(_), SAsn(var_name, (A.Fun(_), SFunsig(t, bl, e')))) ->
          (* Make the function's signature*)
          let sfdecl = {styp = t; sfname = var_name; sargs = bl; sbody = [SReturn(e')]} in
          (* Get the function's llvalue*)
          let vars' = add_local_var builder vars (ty, s) in
          let ll_fun_val = expr fdecls vars' builder e in
          let fdecls' = StringMap.add var_name (ll_fun_val, sfdecl) fdecls in
          let builder' = L.builder_at_end context (L.entry_block ll_fun_val) in
          let new_locals = List.fold_left2 (add_arg builder') StringMap.empty sfdecl.sargs (Array.to_list (L.params ll_fun_val)) in 
          let (_, _, builder'') = stmt (fdecls', new_locals, builder') (SBlock sfdecl.sbody) in

          (add_terminal builder'' (match sfdecl.styp with
                            A.Void -> L.build_ret_void
                          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)));

          (fdecls', vars', builder)
        | _ -> 
          let vars' = add_local_var builder vars (ty, s) in
          let fdecls' = (match ty with A.Fun(_) -> add_local_fdecl vars' fdecls (ty, s) | _ -> fdecls) in
          let _ = expr fdecls vars' builder e in (fdecls', vars', builder))
        
      | SReturn e ->
        let _ = match sfdecl.styp with
                (* Special "return nothing" instr *)
                | A.Void -> L.build_ret_void builder 
                (* Build return statement *)
                | _ -> L.build_ret (expr fdecls vars builder e) builder 
        in (fdecls, vars, builder)
      | SIf (p, then_stmt, else_stmt) ->
        let bool_val = expr fdecls vars builder p in
        let merge_bb = L.append_block context "merge" the_function in

        let then_bb = L.append_block context "then" the_function in
        let _, _, builder' = stmt (fdecls, vars, L.builder_at_end context then_bb) then_stmt in
        add_terminal builder' (L.build_br merge_bb);

        let else_bb = L.append_block context "else" the_function in
        let _, _, builder' = stmt (fdecls, vars, L.builder_at_end context else_bb) else_stmt in
        add_terminal builder' (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        (fdecls, vars, L.builder_at_end context merge_bb)

      | SWhile (p, body) ->
        let p_bb = L.append_block context "while" the_function in
        ignore (L.build_br p_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        let _, _, builder' = stmt (fdecls, vars, L.builder_at_end context body_bb) body in
        add_terminal builder' (L.build_br p_bb);

        let p_builder = L.builder_at_end context p_bb in
        let bool_val = expr fdecls vars p_builder p in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb p_builder);
        (fdecls, vars, L.builder_at_end context merge_bb)

      | SFor (e1, p, e2, body) -> stmt (fdecls, vars, builder)
         (SBlock [SExpr e1 ; SWhile (p, SBlock [body ; SExpr e2]) ] )
      | SForNode (n, g, body) ->
        (match g with
         | (A.Graph(lt, dt, _), _) ->
           let graph_ptr = expr fdecls vars builder g in

           (* allocate space for n, add to symbol table, and initially set to head of node linked list *)
           let n_ptr = L.build_alloca (ltype_of_typ (A.Node(lt, dt))) n builder in
           let vars = StringMap.add n n_ptr vars in
           let hd_node = L.build_call graph_to_node_iterable_func [| graph_ptr |] "hd_node" builder in
           ignore(L.build_store hd_node n_ptr builder);

           (* create predicate block *)
           let p_bb = L.append_block context "while" the_function in
           ignore (L.build_br p_bb builder);

           (* while body block *)
           let body_bb = L.append_block context "while_body" the_function in
           let body_builder = L.builder_at_end context body_bb in
           let _, _, builder' = stmt (fdecls, vars, body_builder) body in
           (* change curr_node to be pointer to next node *)
           let curr_node = L.build_load n_ptr "curr_node" builder' in
           let next_node = L.build_call get_graph_next_node_func [| curr_node |] "next_node" builder' in
           ignore(L.build_store next_node n_ptr builder');
           add_terminal builder' (L.build_br p_bb);

           (* define predicate *)
           let p_builder = L.builder_at_end context p_bb in
           let n_val = L.build_load n_ptr "node_tmp" p_builder in
           let bool_val = L.build_is_not_null n_val "bool_val" p_builder in

           (* merge *)
           let merge_bb = L.append_block context "merge" the_function in
           ignore (L.build_cond_br bool_val body_bb merge_bb p_builder);
           (fdecls, vars, L.builder_at_end context merge_bb)
         | _ -> raise A.Unsupported_constructor)
      | SForEdge (src, dst, w, g, body) ->
        (match g with
         | (A.Graph(lt, dt, wt), _) ->
           let graph_ptr = expr fdecls vars builder g in

           (* allocate space for edge variables, add to symbol table, and initially set to head of edge linked list *)
           let edge_ptr = L.build_alloca void_ptr_t "edge" builder in
           let src_ptr = L.build_alloca (ltype_of_typ (A.Node(lt, dt))) "src" builder in
           let dst_ptr = L.build_alloca (ltype_of_typ (A.Node(lt, dt))) "dst" builder in
           let w_ptr = L.build_alloca (ltype_of_typ wt) "w" builder in
           let vars = StringMap.add src src_ptr (StringMap.add dst dst_ptr (StringMap.add w w_ptr vars)) in
           let hd_edge = L.build_call graph_to_edge_iterable_func [| graph_ptr |] "hd_edge" builder in
           let hd_edge_src = L.build_call get_edge_src_func [| hd_edge |] "hd_edge_src" builder in
           let hd_edge_dst = L.build_call get_edge_dst_func [| hd_edge |] "hd_edge_dst" builder in
           let hd_edge_w = (match wt with
            | A.Int -> L.build_call get_edge_w_int_func [| hd_edge |] "hd_edge_w" builder
            | A.Bool -> L.build_call get_edge_w_bool_func [| hd_edge |] "hd_edge_w" builder
            | A.String -> L.build_call get_edge_w_str_func [| hd_edge |] "hd_edge_w" builder
            | _ -> raise A.Unsupported_constructor) in
           ignore(L.build_store hd_edge edge_ptr builder);
           ignore(L.build_store hd_edge_src src_ptr builder);
           ignore(L.build_store hd_edge_dst dst_ptr builder);
           ignore(L.build_store hd_edge_w w_ptr builder);

           (* create predicate block *)
           let p_bb = L.append_block context "while" the_function in
           ignore (L.build_br p_bb builder);

           (* while body block *)
           let body_bb = L.append_block context "while_body" the_function in
           let body_builder = L.builder_at_end context body_bb in
           let _, _, builder' = stmt (fdecls, vars, body_builder) body in
           (* change curr_edge to be pointer to next edge *)
           let curr_edge = L.build_load edge_ptr "curr_edge" builder' in
           let next_edge = L.build_call get_graph_next_edge_func [| curr_edge |] "next_edge" builder' in
           let next_edge_src = L.build_call get_edge_src_func [| next_edge |] "next_edge_src" builder' in
           let next_edge_dst = L.build_call get_edge_dst_func [| next_edge |] "next_edge_dst" builder' in
           let next_edge_w = (match wt with
            | A.Int -> L.build_call get_edge_w_int_func [| next_edge |] "next_edge_w" builder'
            | A.Bool -> L.build_call get_edge_w_bool_func [| next_edge |] "next_edge_w" builder'
            | A.String -> L.build_call get_edge_w_str_func [| next_edge |] "next_edge_w" builder'
            | _ -> raise A.Unsupported_constructor) in
           ignore(L.build_store next_edge edge_ptr builder');
           ignore(L.build_store next_edge_src src_ptr builder');
           ignore(L.build_store next_edge_dst dst_ptr builder');
           ignore(L.build_store next_edge_w w_ptr builder');
           add_terminal builder' (L.build_br p_bb);

           (* define predicate *)
           let p_builder = L.builder_at_end context p_bb in
           let e_val = L.build_load edge_ptr "edge_tmp" p_builder in
           let bool_val = L.build_is_not_null e_val "bool_val" p_builder in

           (* merge *)
           let merge_bb = L.append_block context "merge" the_function in
           ignore (L.build_cond_br bool_val body_bb merge_bb p_builder);
           (fdecls, vars, L.builder_at_end context merge_bb)
         | _ -> raise A.Unsupported_constructor)
    in

    let (_, _, builder) = stmt (local_fdecls, local_vars, builder) (SBlock sfdecl.sbody) in

    add_terminal builder (match sfdecl.styp with
                            A.Void -> L.build_ret_void
                          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  List.iter (build_function_body StringMap.empty) functions;
  the_module
