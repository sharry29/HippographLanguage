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

  (**** Check functions and methods ****)

  let built_in_fdecls =
    let add_bind map (name, (ty, args)) = 
      StringMap.add name
                    { typ = ty; fname = name; args = args; body = [] }
                    map in
    let mappings = [
      "print_int", (Void, [(Int, "x")]);
      "print_bool", (Void, [(Bool, "x")]);
      "print", (Void, [(String, "x")])
    ] in
    List.fold_left add_bind StringMap.empty mappings
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_fdecls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let fdecls = List.fold_left add_func built_in_fdecls funcs
  in
  
  (* Return a function from our symbol table *)
  let find_func local_fdecls s = 
    try StringMap.find s fdecls
    with Not_found -> 
      try StringMap.find s local_fdecls
      with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* Return a method from our symbol table *)
  let find_method libtyp s =
    try match libtyp with
        | Edge(wt) ->
            (match s with
            | "weight" ->
              if wt = Void
              then raise (Failure ("edge " ^ s ^ " cannot be applied to void weights"))
              else { typ = wt; fname = s; args = []; body = [] }
            | _ ->
              raise Not_found)
        | Node(lt, dt) ->
           (match s with
            | "get_data" ->
               if dt = Void
               then raise (Failure ("node data type is void"))
               else { typ = dt; fname = s; args = []; body = [] }
            | "data" ->
               if dt = Void
               then raise (Failure ("node data type is void"))
               else { typ = Void; fname = s; args = [(Node(lt, dt), "x")]; body = [] }
            | _ ->
               raise Not_found)
        | Graph(lt, dt, _) ->
          (match s with
          | "set_node" ->
              { typ = Bool; fname = s; args = [(Node(lt, dt), "x")]; body = [] }
          | _ -> raise Not_found)
        | _ -> raise Not_found
    with Not_found -> raise (Failure ("unrecognized method " ^ string_of_typ libtyp ^ "." ^ s))
  in

  let _ = find_func fdecls "main" in (*Ensure "main" is defined*)

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
    let rec expr fdecls vars = function
        Intlit l  -> (Int, SIntlit l)
      | Floatlit l  -> (Float, SFloatlit l)
      | Boollit l   -> (Bool, SBoollit l)
      | Charlit l   -> (Char, SCharlit l)
      | Stringlit l -> (String, SStringlit l)
      | Null -> (Bool, SNull)
      | Funsig (t, bl, e) ->
          check_anon_func_expr fdecls vars t bl e
      | Noexpr      -> (Void, SNoexpr)
      | Var s       -> (type_of_variable vars s, SVar s)
      | NodeExpr (l, d)  ->
        let (lt, _) as l' = expr fdecls vars l in
        let (dt, _) as d' = expr fdecls vars d in
        (Node(lt, dt), SNodeExpr(l', d'))
      | EdgeExpr (src, dst, w)  ->
        let (wt, _) as w' = expr fdecls vars w in
        (Edge(wt), SEdgeExpr(expr fdecls vars src, expr fdecls vars dst, w'))
      | Asn(var, e) -> 
          check_asn_expr fdecls vars var e
      | Unop(op, e) -> 
          check_unop_expr fdecls vars op e
      | Binop(e1, op, e2) -> 
          check_binop_expr fdecls vars e1 op e2
      | FCall(fname, args) ->
         check_fcall_expr fdecls vars fname args
      | MCall(instance, mname, args) ->
         check_mcall_expr fdecls vars instance mname args
      | GraphExpr(node_list, edge_list) ->
         check_graph_expr fdecls vars node_list edge_list

    and coerce_null_to_typ new_typ e =
      match e with
      | (Bool, SNull) -> (new_typ, SNull)
      | _ -> e

    and check_asn_expr fdecls vars var e =
      let lvt = type_of_variable vars var in
      let (rvt, e') = coerce_null_to_typ lvt (expr fdecls vars e) in
      let err = "illegal assignment " ^ string_of_typ lvt ^ " = " ^
        string_of_typ rvt ^ " in " ^ string_of_expr (Asn(var, e))
      in

      match lvt, rvt, e' with
      | Node(_), Node(_), _ ->
         (* If left expression is a node with bool type data, wrap right expression in a SNodeExpr *)
         (check_asn lvt rvt err, SAsn(var, (rvt, e')))
      | Node(nlt, Bool), _, _ ->
         (check_asn nlt rvt err, SAsn(var, (lvt, SNodeExpr((rvt, e'), (Bool, SNull)))))
      | Graph(llt, ldt, lwt), Graph(_, _, _), SGraphExpr(nl, el) ->
         (* Coerce (Bool, SNull) to correct type then check type equality *)
         let nl' = List.map (fun (_, e) -> match e with
                                           | SNodeExpr((lt, le), d) ->
                                              let (dt, de) = coerce_null_to_typ ldt d in
                                              let lt = check_asn llt lt err in
                                              let dt = check_asn ldt dt err in
                                              (Node(lt, dt), SNodeExpr((lt, le), (dt, de)))
                                           | _ -> raise Unsupported_constructor) nl in
         let el' = List.map (fun (_, e) -> match e with
                                           | SEdgeExpr(src, dst, w) ->
                                              let (wt, we) = coerce_null_to_typ lwt w in
                                              let wt = check_asn lwt wt err in
                                              (Edge(lwt), SEdgeExpr(src, dst, (wt, we)))
                                           | _ -> raise Unsupported_constructor) el in

         let t = Graph(llt, ldt, lwt) in
         (t, SAsn(var, (t, SGraphExpr(nl', el'))))
      | Fun, Fun, SFunsig(ty, bl, (_, fn_body)) ->
          let (_, new_expr) = expr fdecls vars e in
(*             (if StringMap.mem "f" fdecls(* '  *)then print_string("Here") else print_string("not here"));
 *)          (check_asn lvt rvt err, SAsn(var, (rvt, new_expr)))
          (*If we are storing an anonymous function in a variable, we need to add that*)
     
      | _ ->
         (check_asn lvt rvt err, SAsn(var, (rvt, e')))

    and check_unop_expr fdecls vars op e =
      let (t, e') = expr fdecls vars e in
      let ty = 
        match op with
        | Not when t = Bool -> Bool
        | Neg when t = Int -> Int
        | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^ 
                               string_of_typ t ^ " in " ^ string_of_expr (Unop(op, e))))
      in (ty, SUnop(op, (t, e')))

    and check_binop_expr fdecls vars e1 op e2 =
      let (t1, e1') = expr fdecls vars e1 
      and (t2, e2') = expr fdecls vars e2 in
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
                                   string_of_typ t2 ^ " in " ^ string_of_expr (Binop(e1, op, e2))))
      in (ty, SBinop((t1, e1'), op, (t2, e2')))

    and check_fcall_expr fdecls vars fname args =
      let fd = find_func fdecls fname in
      let param_length = List.length fd.args in
      if List.length args != param_length
      then raise (Failure ("expecting " ^ string_of_int param_length ^
                           " arguments in " ^ string_of_expr (FCall(fname, args))))
      else let check_call (ft, _) e = 
             let (et, e') = expr fdecls vars e in 
             let err = "illegal argument found " ^ string_of_typ et ^
                       " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e in
             (check_asn ft et err, e')
           in 
           let args' = List.map2 check_call fd.args args in
           (fd.typ, SFCall(fname, args'))

    and check_mcall_expr fdecls vars instance mname args =
      let (instance_typ, _) as instance' = expr fdecls vars instance in
      let md = find_method instance_typ mname in
      let param_length = List.length md.args in
      if List.length args != param_length
      then raise (Failure ("expecting " ^ string_of_int param_length ^
                           " arguments in " ^ string_of_expr (MCall(instance, mname, args))))
      else let check_call (ft, _) e =
             let (et, e') = expr fdecls vars e in
             let err = "illegal argument found " ^ string_of_typ et ^
                       " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e in
             (check_asn ft et err, e')
           in
           let args' = List.map2 check_call md.args args in
           (md.typ, SMCall(instance', mname, args'))

    and check_graph_expr fdecls vars node_list edge_list =
     (* infer node label/data types from first nodes in list if any,
        and check that all items have the same type *)
      let node_label_typ, node_data_typ, s_node_list =
        if node_list = [] 
        then (Bool, Bool, []) (* bool type, for now *)
        else let err = "type mismatch in graph nodes" in
          let check_node_typ (lt_opt, dt_opt) n =
            match n with
            | (Node(lt, dt), SNodeExpr(_, d)) ->
               (* check matching node label *)
               let lt_opt = (match lt_opt with
               | None -> Some(lt)
               | Some(lt') -> if lt = lt'
                              then lt_opt
                              else raise (Failure err)) in
               (* check matching node data *)
               let dt_opt = (match d with
               | (Bool, SNull) -> dt_opt
               | _ -> match dt_opt with
                      | None -> Some(dt)
                      | Some(dt') -> if dt = dt'
                                     then dt_opt
                                     else raise (Failure err))
               in (lt_opt, dt_opt)
            | _ -> raise Unsupported_constructor
          in
          let node_list' = List.map (expr fdecls vars) node_list in
          match List.fold_left check_node_typ (None, None) node_list' with
          | None, _ -> raise (Failure "graph node names are required")
          | Some(lt), None -> (lt, Bool, node_list')
          | Some(lt), Some(dt) -> (lt, dt, node_list')
      in
      (* infer edge weight types from first edge in list if any,
        and check that all items have the same type *)
      let edge_typ, s_edge_list =
        if edge_list = []
        then (Bool, []) (* bool type, for now *)
        else let err = "type mismatch in graph edges" in
          let check_edge_typ wt_opt e =
            match e with
            | (Edge(wt), SEdgeExpr(_, _, w)) ->
              (match w with
               | (Bool, SNull) -> wt_opt
               | _ -> (match wt_opt with
                       | None -> Some(wt)
                       | Some(wt') -> if wt = wt'
                                      then wt_opt
                                      else raise (Failure err)))
            | _ -> raise Unsupported_constructor
          in
          let edge_list' = List.map (expr fdecls vars) edge_list in
          match List.fold_left check_edge_typ None edge_list' with
          | None -> (Bool, edge_list')
          | Some(wt) -> (wt, edge_list')
     in
     (Graph(node_label_typ, node_data_typ, edge_typ), SGraphExpr(s_node_list, s_edge_list))


    and check_anon_func_expr fdecls vars typ b_list ex = 
      let vars' = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                                    vars (b_list) in 
      let (ty, sx) = expr fdecls vars' ex in
      let err = "type mismatch in result of anonymous function" in
      let checked_type = check_asn typ ty err in 
      (Fun, SFunsig(checked_type, b_list, (ty, sx)))
    in

    let check_bool_expr fdecls vars e = 
      let (t', e') = expr fdecls vars e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt fdecls vars = function
        Expr e -> (* 
          (match e with
              Asn(s, Funsig(ty, bl, body)) ->   
                    let new_fdecl = {typ = ty; fname = s; args = bl; body = [Expr(body)]} in
                    let fdecls' = add_func fdecls new_fdecl in
                    (fdecls', vars, SExpr (expr fdecls' vars e))
            | _ -> 
               *)(fdecls, vars, SExpr (expr fdecls vars e))
      (* TODO: Start *)
      | For (e1, e2, e3, st) -> 
          let (_, _, st') = check_stmt fdecls vars st in
          (fdecls, vars, SFor (expr fdecls vars e1, check_bool_expr fdecls vars e2, expr fdecls vars e3, st'))
      | ForNode (n, g, st) ->
          (match expr fdecls vars g with
           | (Graph(lt, dt, _), _) as ge ->
              let vars' = StringMap.add n (Node (lt, dt)) vars in
              let (_, _, st') = check_stmt fdecls vars' st in
              (fdecls, vars', SForNode (n, ge, st'))
           | (ty, _) -> raise (Failure ("illegal argument found: expected graph, got " ^ string_of_typ ty)))
      | ForEdge (src, dst, w, g, st) ->
          (match expr fdecls vars g with
           | (Graph(lt, dt, wt), _) as ge ->
              let nt = Node(lt, dt) in
              let vars' = StringMap.add src nt (StringMap.add dst nt (StringMap.add w wt vars)) in
              let (_, _, st') = check_stmt fdecls vars' st in
              (fdecls, vars', SForEdge (src, dst, w, ge, st'))
           | (ty, _) -> raise (Failure ("illegal argument found: expected graph, got " ^ string_of_typ ty)))
      | While (p, st) ->
          let (_, _, st') = check_stmt fdecls vars st in
          (fdecls, vars, SWhile (check_bool_expr fdecls  vars p, st'))
      | If (p, b1, b2) ->
          let (_, _, b1') = check_stmt fdecls vars b1 in
          let (_, _, b2') = check_stmt fdecls vars b2 in
          (fdecls, vars, SIf (check_bool_expr fdecls vars p, b1', b2'))
      | Break -> (fdecls, vars, SBreak)
      | Continue -> (fdecls, vars, SContinue)
      (* TODO: End *)
      | Vdecl (ty, s, e) ->
        if ty = Void
        then raise (Failure ("variable '" ^ s ^ "' declared void"))
        else let vars' = StringMap.add s ty vars in
          (match ty, e with
          | Fun, Asn(_, Var(var_name)) -> 
              (print_string "here\n");
              let new_fun = {(find_func fdecls var_name) with fname = s} in
              let fdecls' = add_func fdecls new_fun in 
              (fdecls', vars', SVdecl (ty, s, expr fdecls vars' e))
          | _ ->
              (fdecls, vars', SVdecl (ty, s, expr fdecls vars' e)))
      | Return e -> 
         let (t, e') = coerce_null_to_typ func.typ (expr fdecls vars e) in
         if t = func.typ then (fdecls, vars, SReturn (t, e'))
         else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                              string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list fdecls vars = function
              [Return _ as s] -> 
                let (_, _, s') = check_stmt fdecls vars s in [s']
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list fdecls vars (sl @ ss) (* Flatten blocks *)
            | s :: ss         ->
                (match s with
                  | Vdecl(_, var_name, Asn(_, Funsig(ty, bl, body))) ->
                    let new_fdecl = {typ = ty; fname = var_name; args = bl; body = [Expr(body)]} in
                    let fdecls' = add_func fdecls new_fdecl in
                    let (fdecls'', vars', s') = check_stmt fdecls' vars s in
                    s' :: check_stmt_list fdecls'' vars' ss
                  | _ ->
                    let (fdecls', vars', s') = check_stmt fdecls vars s in
                    s' :: check_stmt_list fdecls' vars' ss)
            | []              -> []
          in (fdecls, vars, SBlock(check_stmt_list fdecls vars sl))

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sargs = func.args;
      sbody = let (_, _, st) = check_stmt StringMap.empty local_vars (Block func.body)
              in match st with
	             SBlock(sl) -> sl
              | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map (check_function) funcs)
