let _ =
    let lexbuf = Lexing.from_channel stdin in
     let result = Parser.program Scanner.token lexbuf in
        print_string "Hello, World!\n"; flush stdout