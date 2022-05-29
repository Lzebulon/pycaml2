 type token_type = TOKEN_ID | TOKEN_EQUALS | TOKEN_LPAREN | TOKEN_RPAREN | TOKEN_LBRACE 
| TOKEN_RBRACE | TOKEN_COLON | TOKEN_COMMA | TOKEN_LT | TOKEN_GT | TOKEN_INT | TOKEN_RIGHT_ARROW
| TOKEN_SEMI | TOKEN_DOUBLE_COLON | TOKEN_EOF;;

type token_struct = {value :string; token_type : token_type;loc : int};;

type lexer = {src : string; mutable actual_char : char ;src_size : int ;mutable loc : int; mutable line : int; mutable column : int} ;;

exception LexerError of {message: string; line : int; column : int};;


let init_token value tokenType= (value, tokenType) ;;

let read_whole_file filename =
	let ch = open_in filename in
	let s = really_input_string ch (in_channel_length ch) in
	close_in ch;
	s;;

let string_of_token_type token = match token with
| TOKEN_ID -> "TOKEN_ID"
| TOKEN_EQUALS -> "TOKEN_EQUALS"
| TOKEN_LPAREN -> "TOKEN_LPAREN"
| TOKEN_RPAREN -> "TOKEN_RPAREN"
| TOKEN_LBRACE -> "TOKEN_LBRACE"
| TOKEN_RBRACE -> "TOKEN_RBRACE"
| TOKEN_COLON -> "TOKEN_COLON"
| TOKEN_COMMA -> "TOKEN_COMMA"
| TOKEN_LT -> "TOKEN_LT"
| TOKEN_GT -> "TOKEN_GT"
| TOKEN_INT -> "TOKEN_INT"
| TOKEN_RIGHT_ARROW-> "TOKEN_RIGHT_ARROW"
| TOKEN_SEMI -> "TOKEN_SEMI"
| TOKEN_DOUBLE_COLON -> "TOKEN_DOUBLE_COLON"
| TOKEN_EOF-> "TOKEN_EOF";;

let string_of_token token = "< type = " ^ (string_of_token_type token.token_type) ^ " ; value = " ^ ( token.value) ^ " ; position approximative = " ^ (string_of_int token.loc) ^ " >";;

let isalnum c = match c with '0'..'9'| 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false;;
let isdigit c = match c with '0'..'9' -> true | _ -> false;;


let lexer_advance lexer=
  if lexer.loc <= lexer.src_size then begin
    lexer.loc <- lexer.loc + 1 ;
    lexer.column <- lexer.column + 1;
    lexer.actual_char <- lexer.src.[lexer.loc];
    if lexer.actual_char = '\n' then
      (lexer.line <- lexer.line + 1;
      lexer.column <- 0; )
  end;;

let lexer_advance_with lexer token =
  lexer_advance lexer; token;;


let lexer_advance_current lexer token_type =
  let value =Char.escaped lexer.actual_char in 
  let tok = {value = value; token_type = token_type; loc = lexer.loc}
in lexer_advance lexer;

tok
;;




let lexer_parse_id lexer =
  let value = ref "" in
  while isalnum lexer.actual_char do
    value := !value ^(Char.escaped lexer.actual_char);
    lexer_advance lexer;
  done;

  {value = !value;token_type = TOKEN_ID; loc = lexer.loc} ;;

let lexer_parse_number lexer =
    let value = ref "" in
    while isdigit lexer.actual_char || lexer.actual_char = '.' do
      value := !value ^(Char.escaped lexer.actual_char);
      lexer_advance lexer;
    done;
  
    {value = !value;token_type = TOKEN_INT; loc = lexer.loc};;

let lexer_skip_whitespace lexer =
  while match lexer.actual_char with ' '|'\t'| '\n' -> true | _ -> false do
    lexer_advance lexer
  done;;


let lexer_delete_comment lexer = 
  try 
    while not (lexer.actual_char = '*' && lexer.src.[lexer.loc + 1] = ')')  do
      lexer_advance lexer
    done;
    lexer_advance lexer;lexer_advance lexer
  with _ -> failwith "veuillez fermez les commentaires";;

let rec lexer_next_token lexer =
  if lexer.loc + 1<> lexer.src_size then begin
    lexer_skip_whitespace lexer;
    if isdigit lexer.actual_char then
      lexer_parse_number lexer
    else
    if isalnum lexer.actual_char then
      lexer_parse_id lexer
    else match lexer.actual_char with
    | '=' -> lexer_advance_current lexer TOKEN_EQUALS
    | '(' -> if lexer.src.[lexer.loc + 1] = '*' then (lexer_delete_comment lexer; lexer_next_token lexer)
                                                else lexer_advance_current lexer TOKEN_LPAREN
    | ')'-> lexer_advance_current lexer TOKEN_RPAREN 
    | '{'-> lexer_advance_current lexer TOKEN_LBRACE 
    | '}'-> lexer_advance_current lexer TOKEN_RBRACE 
    | ','-> lexer_advance_current lexer TOKEN_COMMA 
    | '<'-> lexer_advance_current lexer TOKEN_LT 
    | '>'-> lexer_advance_current lexer TOKEN_GT 
    | '-' -> lexer_advance_current lexer TOKEN_RIGHT_ARROW
    | ';'-> if lexer.src.[lexer.loc + 1] = ';' then lexer_advance_with lexer {value = ";;"; token_type = TOKEN_DOUBLE_COLON ; loc = lexer.loc}
              else lexer_advance_with lexer {value = ";"; token_type =  TOKEN_COLON; loc = lexer.loc}
    
    | _ -> raise (LexerError{message = "Unexpected Caracter : '"^Char.escaped lexer.actual_char^"'"; line = lexer.line; column = lexer.column}) end
  
  else {value = ""; token_type = TOKEN_EOF; loc = lexer.loc};;


let lex_string doc =
  let file = "filetest.pycaml" in
  try 
    let lexer = {src = doc; actual_char = doc.[0] ; src_size = String.length doc ;loc = 0; line = 1; column = 1} in
    let token = ref (lexer_next_token lexer) in
    while (!token).token_type <> TOKEN_EOF do
      print_endline (string_of_token !token);
      token := lexer_next_token lexer
    done;
    print_endline (string_of_token !token);
  with 
    | LexerError e ->  print_endline ("File \""^file^"\", line "^string_of_int e.line^", character "^string_of_int e.column^": \n\t"^e.message) 
  ;;


(*Parser*)


(*
integer-lietral ::= [-] (0…9) { 0…9 ∣ _ }
*)

(*
string-literal	::=	" { string-character } "   
*)
(*
float-literal	::=	[-] (0…9) { 0…9 ∣ _ } [. { 0…9 ∣ _ }]   
*)
(*
   constant ::= integer-literal | float-literal | string-literal | false | true | () | begin end
*)



(*
expr	::=	value-path
 	∣	 constant
 	∣	 ( expr )
 	∣	 begin expr end
 	∣	 expr { , expr }+
 	∣	 - expr
 	∣	 -. expr
 	∣	 expr infix-op expr     (maybe)
 	∣	 if expr then expr [ else expr ]
 	∣	 while expr do expr done
 	∣	 for value-name = expr ( to ∣ downto ) expr do expr done
 	∣	 expr ; expr
 	∣	 let [rec] let-binding { and let-binding } in expr  
*)










type ast = AST_SEQUENCE of ast list | AST_DECLARATION of string * ast list * ast list | CALL of string * string list | NONE ;;