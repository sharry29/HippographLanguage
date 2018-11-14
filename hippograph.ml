let _ =
let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in
    print_string (Ast.string_of_program ast);
    let sast = Semant.check ast in
    print_string (Sast.string_of_sprogram sast)
(*    let m = Codegen.translate sast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)*) 
