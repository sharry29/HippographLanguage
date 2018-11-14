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
	and str_t	=	L.pointer_type (L.i8_type context) in


	let ltype_of_typ = function
    | A.Void    -> void_t
    | A.Int  	  -> i32_t
    | A.String 	-> str_t
  	in

  	let print_t : L.lltype = 
      	L.var_arg_function_type void_t [| L.pointer_type i8_t |] in
  	let print_func : L.llvalue = 
    	L.declare_function "printf" print_t the_module in

    let function_decls : (L.llvalue * sfdecl) StringMap.t =
	    let function_decl m (sfdecl : sfdecl) =
	      let name = sfdecl.sfname
	      and formal_types = 
			Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) sfdecl.sargs)
	      in let ftype = L.function_type (ltype_of_typ sfdecl.styp) formal_types in
	      StringMap.add name (L.define_function name ftype the_module, sfdecl) m in
	    List.fold_left function_decl StringMap.empty functions in

	let build_function_body (sfdecl : sfdecl) =
	    let (the_function, _) = StringMap.find sfdecl.sfname function_decls in
	    let builder = L.builder_at_end context (L.entry_block the_function) in

	    let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

	    let rec expr builder ((_,e) : sexpr) = match e with
	    | SStringlit s -> L.build_global_stringptr s "str" builder
      | SIntlit i -> L.const_int i32_t i
	    | SFCall ("print", [e]) ->
	  		 L.build_call print_func [| str_format_str ; ( expr builder e ) |] "" builder
	  	in 
	let add_terminal builder instr =
      (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	  |	Some _ -> ()
      | None -> ignore (instr builder)
    in

    let rec stmt builder = function
	  | SBlock sl -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
      | SExpr e -> let _ = expr builder e in builder 
      | SReturn e -> let _ = match sfdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
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
