print_string "Hello world!\n";;

let x = 4 in
	print_int (x + 5);;
	print_endline;;

let x = 4;;

begin match x with
	|1 -> print_int 4
	|_ -> ()
end;;