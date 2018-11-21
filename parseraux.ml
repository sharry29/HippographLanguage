open Ast;;

let construct_node_expr expr =
  (* if already a NodeExpr, keep it that way; otherwise create a NodeExpr with Null data *)
  match expr with
  | NodeExpr(_, _) -> expr
  | _ -> NodeExpr(expr, Null)
;;

let node_list_append_opt n_expr n_list =
  (* add to list if key doesn't exist; ignore otherwise *)
  let label, _ = unwrap_node_expr n_expr in
  if List.exists (fun n -> let l, _ = unwrap_node_expr n in l = label) n_list
  then n_list
  else n_expr :: n_list
;;

let edge_list_append_opt e_expr e_list =
  (* add to list if src-dst pair doesn't exist; ignore otherwise regardless of weight *)
  let src, dst, _ = unwrap_edge_expr e_expr in
  if List.exists (fun e -> let s, d, _ = unwrap_edge_expr e in s = src && d = dst) e_list
  then e_list
  else e_expr :: e_list
;;

let update_node_edge_list_with_edge (n_list, e_list) weight n_expr =
  let prev_n_label, _ = unwrap_node_expr (List.hd n_list) in
  let n_label, _ = unwrap_node_expr n_expr in
  let n_list' = node_list_append_opt n_expr n_list in
  let e_list' = edge_list_append_opt (EdgeExpr(n_label, prev_n_label, weight))
                                     (edge_list_append_opt (EdgeExpr(prev_n_label, n_label, weight))
                                                           e_list)
  in
  (n_list', e_list')
;;

let update_node_edge_list_with_redge (n_list, e_list) weight n_expr =
  let prev_n_label, _ = unwrap_node_expr (List.hd n_list) in
  let n_label, _ = unwrap_node_expr n_expr in
  let n_list' = node_list_append_opt n_expr n_list in
  let e_list' = edge_list_append_opt (EdgeExpr(prev_n_label, n_label, weight)) e_list in
  (n_list', e_list')
;;

let update_node_edge_list_with_ledge (n_list, e_list) weight n_expr =
  let prev_n_label, _ = unwrap_node_expr (List.hd n_list) in
  let n_label, _ = unwrap_node_expr n_expr in
  let n_list' = node_list_append_opt n_expr n_list in
  let e_list' = edge_list_append_opt (EdgeExpr(n_label, prev_n_label, weight)) e_list in
  (n_list', e_list')
;;

let merge_node_edge_lists (n_list1, e_list1) (n_list2, e_list2) =
  let n_list' = List.fold_right node_list_append_opt n_list2 n_list1 in
  let e_list' = List.fold_right edge_list_append_opt e_list2 e_list1 in
  (n_list', e_list')
;;