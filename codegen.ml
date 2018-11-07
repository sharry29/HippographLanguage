module L = Llvm
open Ast
(* open Sast  *)

module StringMap = Map.Make(String)

let translate (globals, functions) =  
	let context =	L.global_context () in
	let the_module = L.create_module context "Hippograph" in 

	let void_t 	= 	L.void_type context
	and i8_t 	= 	L.i8_type context
	and str_t	=	L.pointer_type (L.i8_type context) in


	let ltype_of_typ = function
    | Void  	-> void_t
    | String 	-> str_t
  	in

  	let print_t : L.lltype = 
      	L.var_arg_function_type void_t [| L.pointer_type i8_t |] in
  	let print_func : L.llvalue = 
    	L.declare_function "printf" print_t the_module in

    let function_decls : (L.llvalue * fdecl) StringMap.t =
	    let function_decl m (fdecl : fdecl) =
	      let name = fdecl.fname
	      and formal_types = 
			Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.args)
	      in let ftype = L.function_type (ltype_of_typ fdecl.typ) formal_types in
	      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
	    List.fold_left function_decl StringMap.empty functions in

	let build_function_body (fdecl : fdecl) =
	    let (the_function, _) = StringMap.find fdecl.fname function_decls in
	    let builder = L.builder_at_end context (L.entry_block the_function) in

	    let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

	    let rec expr builder (e : expr) = match e with
	    | Stringlit s -> L.build_global_stringptr s "str" builder
	    | FCall ("print", [e]) ->
	  		 L.build_call print_func [| str_format_str ; ( expr builder e ) |] "printf" builder
	  	in 
	let add_terminal builder instr =
      (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	  |	Some _ -> ()
      | None -> ignore (instr builder)
    in

    let rec stmt builder = function
	  | Block sl -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
      | Expr e -> let _ = expr builder e in builder 
      | Return e -> let _ = match fdecl.typ with
                              (* Special "return nothing" instr *)
                              Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder 
                     		in builder
    in 
    let builder = stmt builder (Block fdecl.body) in
    add_terminal builder (match fdecl.typ with
        Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  	in

  List.iter build_function_body functions;
  the_module