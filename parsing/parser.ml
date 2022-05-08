type valid_type = INT | STRING | FLOAT;;
type arbre = Definition of arbre list * arbre | Variable of string * valid_type ;;



(* let is_valid_token acc pile= match acc with
| "let" -> Definition(recup_args, recup_corps)
| _ -> pile;; *)

let parse ic = 
  let rec parse_file acc = try 
      match input_char ic with
      | ' ' -> print_endline acc; if true then  acc::(parse_file "") else []
      | '\n' -> print_endline acc;print_endline "hop"; acc::(parse_file "")
      | '\t' -> print_endline acc; print_endline "tabulation" ; acc::(parse_file "")
      | ';' -> print_endline acc; acc::(parse_file ";") 
      | '(' -> print_endline acc; acc::(parse_file "")
      | ')' -> print_endline acc; acc::(parse_file "") 
      | a -> parse_file (acc ^ (String.make 1 a) ) 
    
    with End_of_file -> flush stdout; close_in ic ;print_endline "fini"; [acc]
    | e -> close_in_noerr ic; raise e
  in
    parse_file "" ;;

let rec parcours_liste l = match l with
| "let"::q -> let rec get_args q = match q with 
                                  [] -> failwith "Il manque le =" 
                                  |"="::_ -> [] 
                                  | t::a -> Variable(t, STRING) :: (get_args a) in Definition(get_args q, Variable("a",STRING))
| _::q-> parcours_liste q
| [] -> Variable("a",FLOAT);;