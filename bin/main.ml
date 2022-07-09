let () = 
  assert (Array.length Sys.argv > 1) ;
  let file = Sys.argv.(1) in print_endline file ;
  print_endline (Sys.getcwd ());
  let token_list =   Parser.lex_string (Parser.read_whole_file file) in
  let ast = Parser.parse_token_list token_list in 
  print_endline (Parser.string_of_ast ast);

