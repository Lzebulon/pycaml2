let () = 
  assert (Array.length Sys.argv > 1) ;
  let file = Sys.argv.(1) in print_endline file ;
  print_endline (Sys.getcwd ());
  Parser.onTest;

  let ic = open_in file in
  try 
    let rec parse_file acc= match input_char ic with
    | ' ' -> print_endline acc; if true then  parse_file "" else ()
    | '\n' -> print_endline acc; parse_file ""
    | ';' -> print_endline acc; parse_file ";"
    | a -> parse_file (acc ^ (String.make 1 a) )
    in
    parse_file "";
    flush stdout;
    close_in ic  

  with End_of_file -> print_endline "fini" 
  | e -> close_in_noerr ic; raise e


