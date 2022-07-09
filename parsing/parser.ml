 type token_type = TOKEN_ID | TOKEN_KEYWORD | TOKEN_EQUALS | TOKEN_LPAREN | TOKEN_RPAREN | TOKEN_LBRACE 
| TOKEN_RBRACE | TOKEN_COLON | TOKEN_COMMA | TOKEN_LT | TOKEN_GT | TOKEN_INT | TOKEN_RIGHT_ARROW
| TOKEN_SEMI | TOKEN_DOUBLE_COLON | TOKEN_EOF | NO_TOKEN;;

type debug = { mutable line : int; mutable column : int};;

type token_struct = {value :string; token_type : token_type;loc : int; debug : debug};;
type lexer = {src : string; mutable actual_char : char ;src_size : int ;mutable loc : int; mutable debug : debug} ;;

exception LexerError of {message: string; debug : debug};;


let init_token value tokenType= (value, tokenType) ;;

let read_whole_file filename =
	let ch = open_in filename in
	let s = really_input_string ch (in_channel_length ch) in
	close_in ch;
	s;;

let string_of_token_type token = match token with
| TOKEN_ID -> "TOKEN_ID"
| TOKEN_KEYWORD -> "TOKEN_KEYWORD"
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
| TOKEN_EOF-> "TOKEN_EOF"
| NO_TOKEN -> "NO_TOKEN";;

let string_of_debug debug = "line "^string_of_int debug.line^", character "^string_of_int debug.column;;

let string_of_token token = "< token_type = " ^ (string_of_token_type token.token_type) ^ " ; value = " ^ ( token.value) ^ " ; position approximative = " ^ (string_of_int token.loc) ^ 
" ; debug = { "^string_of_debug token.debug^" }" ^" >";;

let isalnum c = match c with '0'..'9'| 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false;;
let isdigit c = match c with '0'..'9' | '_' -> true | _ -> false;;

let keywords_list = ["and";"as";"assert"   ;   "asr"      ;   "begin"    ;   "class";
"constraint" ; "do"      ;    "done"    ;    "downto"   ;   "else"    ;    "end";
"exception"  ; "external"   ; "false"    ;   "for"      ;   "fun"       ;  "function";
"functor"  ;   "if"       ;   "in"      ;    "include"  ;   "inherit" ;   " initializer";
"land"     ;   "lazy "    ;   "let"      ;   "lor"      ;   "lsl"    ;     "lsr";
"lxor"    ;   " match"   ;    "method"  ;    "mod"    ;     "module"   ;   "mutable";
"new"      ;   "nonrec"    ;  "object"   ;   "of"   ;       "open"      ;  "or";
"private"  ;   "rec"     ;    "sig      ;   struct"   ;   "then"   ;     "to";
"true"     ;   "try"      ;   "type"    ;    "val"    ;     "virtual"    ; "when";
"while"    ;   "with"];;

let operator_list = ["+";"-";"*";"/"];;

let iskeyword s = List.exists (fun x -> x = s) keywords_list;;

let isoperator s = List.exists (fun x -> x = s) operator_list;;

let lexer_advance lexer=
  if lexer.loc <= lexer.src_size then begin
    lexer.loc <- lexer.loc + 1 ;
    lexer.debug.column <- lexer.debug.column + 1;
    lexer.actual_char <- lexer.src.[lexer.loc];
    if lexer.actual_char = '\n' then
      (lexer.debug.line <- lexer.debug.line + 1;
      lexer.debug.column <- 0; )
  end;;

let lexer_advance_with lexer token =
  lexer_advance lexer; token;;


let lexer_advance_current lexer token_type =
  let value =Char.escaped lexer.actual_char in 
  let tok = {value = value; token_type = token_type; loc = lexer.loc; debug = lexer.debug}
in lexer_advance lexer;

tok
;;




let lexer_parse_id lexer =
  let value = ref "" in
  while isalnum lexer.actual_char do
    value := !value ^(Char.escaped lexer.actual_char);
    lexer_advance lexer;
  done;
  if iskeyword !value then {value = !value;token_type = TOKEN_KEYWORD; loc = lexer.loc; debug = lexer.debug}
  else
  {value = !value;token_type = TOKEN_ID; loc = lexer.loc; debug = lexer.debug} ;;

let lexer_parse_number lexer =
    let value = ref "" in
    while isdigit lexer.actual_char || lexer.actual_char = '.' do
      value := !value ^(Char.escaped lexer.actual_char);
      lexer_advance lexer;
    done;
  
    {value = !value;token_type = TOKEN_INT; loc = lexer.loc; debug = lexer.debug};;

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
  if lexer.loc + 1< lexer.src_size then begin
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
    | '-' -> if lexer.src.[lexer.loc + 1] = '>' then lexer_advance_with lexer {value = "->"; token_type = TOKEN_RIGHT_ARROW ; loc = lexer.loc; debug = lexer.debug}
    else lexer_advance_with lexer {value = "-"; token_type =  TOKEN_ID; loc = lexer.loc; debug = lexer.debug}
    | '*' -> lexer_advance_with lexer {value = "*"; token_type =  TOKEN_ID; loc = lexer.loc; debug = lexer.debug}
    | '+' -> lexer_advance_with lexer {value = "+"; token_type =  TOKEN_ID; loc = lexer.loc; debug = lexer.debug}
    | ';'-> if lexer.src.[lexer.loc + 1] = ';' then (  lexer_advance_with lexer {value = ";;"; token_type = TOKEN_DOUBLE_COLON ; loc = lexer.loc; debug = lexer.debug})
              else lexer_advance_with lexer {value = ";"; token_type =  TOKEN_COLON; loc = lexer.loc; debug = lexer.debug}
    
    | _ -> raise (LexerError{message = "Unexpected Caracter : '"^Char.escaped lexer.actual_char^"'"; debug = lexer.debug}) end
  
  else {value = ""; token_type = TOKEN_EOF; loc = lexer.loc; debug = lexer.debug};;


let lex_string doc =
  let file = "filetest.pycaml" in
  try 
    let lexer = {src = doc; actual_char = doc.[0] ; src_size = String.length doc ;loc = 0; debug = {line = 1; column = 1}} in
    let rec tokenize token = if (token).token_type <> TOKEN_EOF then begin
      print_endline (string_of_token token);
      token :: (tokenize (lexer_next_token lexer))
    end else (print_endline (string_of_token token);[token]) in
    tokenize (lexer_next_token lexer)
  with 
    | LexerError e ->  failwith ("File \""^file^"\", line "^string_of_int e.debug.line^", character "^string_of_int e.debug.column^": \n\t"^e.message) 
  ;;


(*Parser*)

(*
ident	::=	 (letter ∣ _) { letter ∣ 0…9 ∣ _ ∣ ' } 
 
capitalized-ident	::=	 (A…Z) { letter ∣ 0…9 ∣ _ ∣ ' } 
 
lowercase-ident	::=	(a…z ∣ _) { letter ∣ 0…9 ∣ _ ∣ ' } 
 
letter	::=	 A…Z ∣ a…z
   
label-name	::=	 lowercase-ident
*)

(*
infix-symbol	::=	(core-operator-char ∣ % ∣ <) { operator-char }

operator-char	::=	~ ∣ ! ∣ ? ∣ core-operator-char ∣ % ∣ < ∣ : ∣ .

core-operator-char	::=	$ ∣ & ∣ * ∣ + ∣ - ∣ / ∣ = ∣ > ∣ @ ∣ ^ ∣ |
*)


(*Keywords 
   
      and         "as"          "assert"      "asr"         begin       "class"
      "constraint"  do          done        downto      else        end
      exception   "external"    false       for         fun         function
      "functor"     if          in          "include"     "inherit"    " initializer"
      "land        lazy "       let         "lor         lsl         lsr"
      "lxor"       " match"       method      mod         module      mutable
      "new         nonrec      object      of          open        or"
      "private"     rec         "sig         struct"      then        to
      true        try         type        val         "virtual"     when
      while       with


    !=    #     &     &&    '     (     )     *     +     ,     -
    -.    ->    .     ..    .~    :     ::    :=    :>    ;     ;;
    <     <-    =     >     >]    >}    ?     [     [<    [>    [|
    ]     _     `     {     {<    |     |]    ||    }     ~



*)


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

(*Names
   
value-name	::=	lowercase-ident
 	            ∣	 ( operator-name )
 
operator-name	::=	prefix-symbol ∣ infix-op
 
infix-op	::=	infix-symbol
 	∣	 * ∣ + ∣ - ∣ -. ∣ = ∣ != ∣ < ∣ > ∣ or ∣ || ∣ & ∣ && ∣ :=
 	∣	 mod ∣ land ∣ lor ∣ lxor ∣ lsl ∣ lsr ∣ asr
 
constr-name	::=	capitalized-ident
 
tag-name	::=	capitalized-ident
 
typeconstr-name	::=	lowercase-ident
 
field-name	::=	lowercase-ident
 
module-name	::=	capitalized-ident
 
modtype-name	::=	ident
 
class-name	::=	lowercase-ident
 
inst-var-name	::=	lowercase-ident
 
method-name	::=	lowercase-ident


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










type ast = AST_SEQUENCE of ast list 
          | AST_FUNCTION_DECLARATION of string * string list * ast * ast
          | AST_CONSTANT_DECLARATION of string * ast * ast
          | AST_CALL of string * ast list 
          | AST_CONSTANT of {value : string; constant_type : token_type} 
          |NONE ;;

let rec string_list_to_string list = match list with
| [] -> ""
| t::[] -> " "^t
| t::q -> " "^t^","^string_list_to_string q;;

let rec string_of_ast_sequence list = match list with
| [] -> ""
| t::[] -> string_of_ast t^" ;"
| t::q -> (string_of_ast t)^";\n"^string_of_ast_sequence q
and string_of_ast_call list = match list with
| [] -> ""
| t::[] -> string_of_ast t^" "
| t::q -> (string_of_ast t)^", "^string_of_ast_call q
and 
string_of_ast ast = match ast with
| AST_FUNCTION_DECLARATION (a,b,c,d) ->  a^"("^string_list_to_string b^"){\n"^string_of_ast c ^"\n} in "^string_of_ast d
| AST_CONSTANT_DECLARATION (a,c,d) ->  a^" = "^string_of_ast c ^" in "^string_of_ast d
| AST_CONSTANT a -> a.value
| AST_SEQUENCE a -> string_of_ast_sequence a
| AST_CALL (a,b) -> a^"( "^string_of_ast_call b^" )"
| NONE -> "AST_NONE";;


type parser = {mutable token_list : token_struct list; mutable actual_token : token_struct};;

let parser_get_next_token parser =
  match parser.token_list with
  | [] -> ()
  | t::q -> parser.token_list <- q; 
  parser.actual_token <- t;;

let rec parse_patern parser =
  parser_get_next_token parser;
  if parser.actual_token.token_type = TOKEN_ID then let actualtok = parser.actual_token in actualtok.value :: parse_patern parser
    else [];;

let rec string_of_token_list token_list = match token_list with
| [] -> ""
| t::q -> string_of_token t ^ string_of_token_list q;;



let rec parse_expr parser = 

  let rec get_sequence_to list_stop parser= 
    let rec aux () = 
      parser_get_next_token parser;
      if List.exists (fun x -> x = parser.actual_token.token_type) list_stop then 
      [] else  (let actual_ast = parse_expr parser in actual_ast::aux())
    in AST_SEQUENCE(aux ())

  in

  let rec get_argument parser = 
    parser_get_next_token parser;
    match parser.actual_token.token_type with
    | TOKEN_ID -> let actual_token = parser.actual_token in AST_CALL(actual_token.value, [])::get_argument parser
    | TOKEN_KEYWORD when parser.actual_token.value = "in" -> []
    | TOKEN_COLON -> []
    | TOKEN_DOUBLE_COLON -> []
    | _ -> let actual_ast = parse_expr parser in actual_ast::get_argument parser


  in

  match parser.actual_token.token_type with  
  | TOKEN_KEYWORD -> (match parser.actual_token.value with
                | "let" -> begin 
                  parser_get_next_token parser;
                  let token_declaration = parser.actual_token in 
                  let parameter = parse_patern parser in 
                  if parser.actual_token.token_type = TOKEN_EQUALS then parser_get_next_token parser
                  else failwith "add =";
                  let expression = parse_expr parser in
                  let inexpr = 
                    parser_get_next_token parser ; 
                    if parser.actual_token.value = "in" then parse_expr parser
                    else NONE 
                  in
                  if parameter = [] then AST_CONSTANT_DECLARATION (token_declaration.value,expression,inexpr)
                  else AST_FUNCTION_DECLARATION (token_declaration.value,parameter,expression,inexpr)
                end
                | "in" -> parser_get_next_token parser ; parse_expr parser
                | _ -> NONE)
  | TOKEN_ID -> (let call = parser.actual_token in AST_CALL(call.value,get_argument parser ))
  | TOKEN_EQUALS -> NONE
  | TOKEN_LPAREN -> get_sequence_to [TOKEN_LPAREN] parser
  | TOKEN_RPAREN -> NONE
  | TOKEN_LBRACE -> NONE
  | TOKEN_RBRACE -> NONE
  | TOKEN_COLON -> NONE
  | TOKEN_COMMA -> NONE
  | TOKEN_LT -> NONE
  | TOKEN_GT -> NONE
  | TOKEN_INT -> AST_CONSTANT {value = parser.actual_token.value; constant_type = parser.actual_token.token_type}
  | TOKEN_RIGHT_ARROW -> NONE
  | TOKEN_SEMI -> NONE
  | TOKEN_DOUBLE_COLON -> NONE 
  | TOKEN_EOF -> print_endline "call TOKEN_EOF"; NONE
  | NO_TOKEN -> parser_get_next_token parser; parse_expr parser;;


let parse_token_list token_list =
  let parser = {token_list = token_list; actual_token = {value =""; token_type = NO_TOKEN;loc = 0; debug = {line = 0;column =0}}} in 
  let rec parse_aux parser =
    match parser.token_list with
    | [] -> []
    | [t] when t.token_type = TOKEN_EOF -> []
    | _ -> let expression = parse_expr parser in parser_get_next_token parser; expression ::parse_aux parser
  in AST_SEQUENCE(parse_aux parser);;







