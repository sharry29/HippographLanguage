let node_list_append_opt ((key, _) as n_expr) n_list =
  (* add to list if key doesn't exist; ignore otherwise *)
  if List.exists (fun (k, _) -> k = key) n_list
  then n_list
  else n_expr :: n_list
;;

let edge_list_append_opt ((src, dst, _) as e_expr) e_list =
  (* add to list if src-dst pair doesn't exist; ignore otherwise regardless of weight *)
  if List.exists (fun (s, d, _) -> s = src && d = dst) e_list
  then e_list
  else e_expr :: e_list
;;

let update_node_edge_list_with_edge (n_list, e_list) weight ((n_key, n_val) as n_expr) =
  let (prev_n_key, _) = List.hd n_list in
  let n_list' = node_list_append_opt n_expr n_list in
  let e_list' = edge_list_append_opt (n_key, prev_n_key, weight) (edge_list_append_opt (prev_n_key, n_key, weight) e_list) in
  (n_list', e_list')
;;

let update_node_edge_list_with_redge (n_list, e_list) weight ((n_key, n_val) as n_expr) =
  let (prev_n_key, _) = List.hd n_list in
  let n_list' = node_list_append_opt n_expr n_list in
  let e_list' = edge_list_append_opt (prev_n_key, n_key, weight) e_list in
  (n_list', e_list')
;;

let update_node_edge_list_with_ledge (n_list, e_list) weight ((n_key, n_val) as n_expr) =
  let (prev_n_key, _) = List.hd n_list in
  let n_list' = node_list_append_opt n_expr n_list in
  let e_list' = edge_list_append_opt (n_key, prev_n_key, weight) e_list in
  (n_list', e_list')
;;

let merge_node_edge_lists (n_list1, e_list1) (n_list2, e_list2) =
  let n_list' = List.fold_right node_list_append_opt n_list2 n_list1 in
  let e_list' = List.fold_right edge_list_append_opt e_list2 e_list1 in
  (n_list', e_list')
;;