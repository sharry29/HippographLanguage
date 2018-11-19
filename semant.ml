open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, funcs) =
  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : binding list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)
  check_binds "global" globals;

  (**** Check functions ****)
  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      args = [(ty, "x")];
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print_int", Int); ("print_bool", Bool); ("print", String) ]
  in
  (* Add function name to symbol table *)
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

  (* Collect all function names into one symbol table *)
  let fdecls = List.fold_left add_func built_in_decls funcs
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s fdecls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (*Ensure "main" is defined*)

  let check_function func =
    (* Make sure no args are void or duplicates *)
    check_binds "args" func.args;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_asn lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build global and local symbol table of variables for this function *)

    let global_vars = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                                    StringMap.empty (globals)
    in

    let local_vars = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                                  StringMap.empty (func.args)
    in

    (* Return a variable from our local symbol table *)
    let type_of_variable vars s =
      try StringMap.find s vars
      with Not_found -> 
        try StringMap.find s global_vars
        with Not_found ->
          raise (Failure ("undeclared variable " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr vars = function
        Intlit l  -> (Int, SIntlit l)
      | Floatlit l  -> (Float, SFloatlit l)
      | Boollit l   -> (Bool, SBoollit l)
      | Charlit l   -> (Char, SCharlit l)
      | Stringlit l -> (String, SStringlit l)
      (*| Funsig      -> (* to be completed *)*)
      | Noexpr      -> (Void, SNoexpr)
      | Var s       -> (type_of_variable vars s, SVar s)
      | Asn(var, e) as ex -> 
        let lt = type_of_variable vars var
        and (rt, e') = expr vars e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
          string_of_typ rt ^ " in " ^ string_of_expr ex
        in (check_asn lt rt err, SAsn(var, (rt, e')))
      | Unop(op, e) as ex -> 
        let (t, e') = expr vars e in
        let ty = 
          match op with
          | Not when t = Bool -> Bool
          | Neg when t = Int -> Int
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
        in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr vars e1 
          and (t2, e2') = expr vars e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
                   | Add | Sub | Mul | Div when same && t1 = Int -> Int
                   | Add | Sub | Mul | Div when same && t1 = Float -> Float
                   | Eq  | Neq when same -> Bool
                   | Lt | Leq | Gt | Geq when same && (t1 = Int || t1 = Float) -> Bool
                   | And | Or when same && t1 = Bool -> Bool
                   | _ -> 
                      raise (Failure ("illegal binary operator " ^
                                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      (* | MCall(e, mname, args) as mcall ->  
      	  let fd = find_func mname in
          let param_length = List.length fd.args in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr mcall))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_asn ft et err, e')
          in 
          let args' = List.map2 check_call fd.args args
          in (fd.typ, SMCall(expr e, mname, args')) *)
      | FCall(fname, args) as fcall -> 
         let fd = find_func fname in
         let param_length = List.length fd.args in
         if List.length args != param_length then
           raise (Failure ("expecting " ^ string_of_int param_length ^ 
                           " arguments in " ^ string_of_expr fcall))
         else let check_call (ft, _) e = 
           let (et, e') = expr vars e in 
           let err = "illegal argument found " ^ string_of_typ et ^
             " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
           in (check_asn ft et err, e')
         in 
         let args' = List.map2 check_call fd.args args
         in (fd.typ, SFCall(fname, args'))
    in

    let check_bool_expr vars e = 
      let (t', e') = expr vars e
      and err = "expected Boolean expression in " (*^ string_of_expr e*)
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt vars = function
        Expr e -> (vars, SExpr (expr vars e))
      (* TODO: Start *)
      | For (e1, e2, e3, st) -> 
          let (_, st') = check_stmt vars st in
          (vars, SFor (expr vars e1, check_bool_expr vars e2, expr vars e3, st'))
      | ForNode (s1, s2, st) ->
          let (_, st') = check_stmt vars st in
          (vars, SForNode (s1, s2, st'))
      | ForEdge (s1, s2, st) -> 
          let (_, st') = check_stmt vars st in
          (vars, SForEdge (s1, s2, st'))
      | While (p, st) ->
          let (_, st') = check_stmt vars st in
          (vars, SWhile (check_bool_expr vars p, st'))
      | If (p, b1, b2) ->
          let (_, b1') = check_stmt vars b1 in
          let (_, b2') = check_stmt vars b2 in
          (vars, SIf (check_bool_expr vars p, b1', b2'))
      | Break -> (vars, SBreak)
      | Continue -> (vars, SContinue)
      (* TODO: End *)
      | Vdecl (ty, s, e) ->
          let vars' = StringMap.add s ty vars in
          (vars', SVdecl (ty, s, expr vars' e))
      | Return e -> 
         let (t, e') = expr vars e in
         if t = func.typ then (vars, SReturn (t, e'))
         else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                              string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list vars = function
              [Return _ as s] -> 
                let (_, s') = check_stmt vars s in [s']
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list vars (sl @ ss) (* Flatten blocks *)
            | s :: ss         ->
                let (vars', s') = check_stmt vars s in
                s' :: check_stmt_list vars' ss
            | []              -> []
          in (vars, SBlock(check_stmt_list vars sl))

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sargs = func.args;
      sbody = let (_, st) = check_stmt local_vars (Block func.body)
              in match st with
	             SBlock(sl) -> sl
              | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function funcs)
