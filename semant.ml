(* Semantic checking for Hippograph compiler *)

open Ast

module StringMap = Map.Make(String)

let check (globals, functions) = 

	(*Check for duplicates*)
	let check_dups exceptf list = 
		let rec helper = function
		a :: b :: _ when a = b -> raise (Failure (exceptf a))
		| _ :: tail -> helper tail
		| [] -> ()
		in helper (List.sort compare list)
	in

	(*Check for bindings to void*)



	(****  Global Vars Check ****)

	List.iter (check_not_void (fun n -> "illegal void global variable" ^ n)) globals;

	check_dups (fun n -> "duplicate global variable" ^ n) (List.map snd globals);

	(**** Functions Check ****)

