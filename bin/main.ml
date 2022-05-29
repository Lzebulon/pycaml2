let () = 
  assert (Array.length Sys.argv > 1) ;
  let file = Sys.argv.(1) in print_endline file ;
  print_endline (Sys.getcwd ());
  Parser.lex_string (Parser.read_whole_file file);


