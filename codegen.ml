module L = Llvm
module A = Ast
open Sast  

module StringMap = Map.Make(String)

let translate (globals, functions) =  
	let context =	L.global_context () in
	let the_module = L.create_module context "Hippograph" in 

	let void_t 	= 	L.void_type context
  and i32_t  = L.i32_type  context
	and i8_t 	= 	L.i8_type context
  and i1_t   = L.i1_type   context
	and str_t	=	L.pointer_type (L.i8_type context) in


	let ltype_of_typ = function
    | A.Void    -> void_t
    | A.Int     -> i32_t
    | A.Bool  	-> i1_t
    | A.String 	-> str_t
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

	let print_t : L.lltype = 
    	L.var_arg_function_type void_t [| L.pointer_type i8_t |] in
	let print_func : L.llvalue = 
  	L.declare_function "printf" print_t the_module in

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
    let local_vars =
      let add_arg m (t, n) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m in

      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m in
      
      let args = List.fold_left2 add_arg StringMap.empty sfdecl.sargs
          (Array.to_list (L.params the_function)) in
      
      args in
      (* List.fold_left add_local formals fdecl.A.locals in *)

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let rec expr builder ((_,e) : sexpr) = match e with
    | SStringlit s -> L.build_global_stringptr s "str" builder
    | SIntlit i -> L.const_int i32_t i
    | SBoollit b -> L.const_int i1_t (if b then 1 else 0)
    | SVar s -> L.build_load (lookup s) s builder
    | SUnop(op, e) ->
        let e' = expr builder e in
        (match op with
          A.Neg     -> L.build_neg
        | A.Not     -> L.build_not) e' "tmp" builder
    | SBinop (e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
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
       L.build_call print_func [| str_format_str ; ( expr builder e ) |] "" builder
    | SFCall ("print_int", [e]) ->
       L.build_call print_func [| int_format_str ; ( expr builder e ) |] "" builder
    | SFCall ("print_bool", [e]) ->
  		 L.build_call print_func [| int_format_str ; ( expr builder e ) |] "" builder
    | SFCall (f, act) ->
       let (fdef, sfdecl) = StringMap.find f function_decls in
       let actuals = List.rev (List.map (expr builder) (List.rev act)) in
       let result = (match sfdecl.styp with A.Void -> "" | _ -> f ^ "_result") in
       L.build_call fdef (Array.of_list actuals) result builder
    | SAsn (s, e) ->
        let e' = expr builder e in
        ignore (L.build_store e' (lookup s) builder); e'
  	in 

    let add_terminal builder instr =
      (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	    | Some _ -> ()
      | None -> ignore (instr builder)
    in

    let rec stmt builder = function
	    | SBlock sl ->
        List.fold_left stmt builder sl
      (* Generate code for this expression, return resulting builder *)
      | SExpr e ->
        let _ = expr builder e in builder 
      | SReturn e ->
        let _ = match sfdecl.styp with
                (* Special "return nothing" instr *)
                | A.Void -> L.build_ret_void builder 
                (* Build return statement *)
                | _ -> L.build_ret (expr builder e) builder 
        in builder
    in

    let builder = stmt builder (SBlock sfdecl.sbody) in

    add_terminal builder (match sfdecl.styp with
                            A.Void -> L.build_ret_void
                          | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
