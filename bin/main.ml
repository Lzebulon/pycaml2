let () = 
  assert (Array.length Sys.argv > 1) ;
  let file = Sys.argv.(1) in print_endline file ;
  print_endline (Sys.getcwd ());

  let ic = open_in file in
  let parsed = Parser.parse ic in
  List.iter (Printf.printf "%s ") parsed;;


