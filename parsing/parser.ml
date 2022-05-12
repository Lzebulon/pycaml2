type token = UT | UKNOW_type of string | 
            POINT | LBRACKET | RBRACKET | STAR | START_COMMENT | END_COMMENT |
            SEMI_COLUM | END_FUNCTION | 
            LINE_RETURN | INT of string | TOKEN_FLOAT of string | SPACE of int | TABULATION;;
let keywords = ["let";"if";"then"; "else"; "in"; "while"; "for";
                 "do"; "done"; "begin"];;

let symboles = ['=';'/';'!';',';';';':';'-';'+';'*'];;

let is_symbole x = match List.find_opt (fun a -> a = x) symboles with Some _ -> true | _ -> false;;
let lexer ic = 
  let rec lex_file token_type = try 
      let char_read = input_char ic in
      match char_read with
      | ' ' -> (match token_type with
                | UT -> lex_file (SPACE(1))
                | SPACE(x) -> if x+1 = 4 then TABULATION::(lex_file UT) else lex_file (SPACE(x+1))
                | _ -> token_type::lex_file (SPACE(1)))
      | '\t' -> (match token_type with
                | UT -> TABULATION::lex_file UT
                | SPACE(x) -> TABULATION::(lex_file (SPACE(x))) 
                | _ -> token_type::TABULATION::lex_file UT)
      | '\n' -> (match token_type with
                | UT | SPACE _ -> LINE_RETURN::lex_file UT
                | _ ->token_type::LINE_RETURN::lex_file UT)
      | '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> 
                  (match token_type with 
                  | INT(x) -> lex_file (INT(x ^ (Char.escaped char_read)))
                  | TOKEN_FLOAT(x) -> lex_file (TOKEN_FLOAT(x ^ (Char.escaped char_read)))
                  | UKNOW_type(x) -> lex_file (UKNOW_type(x ^ (Char.escaped char_read)))
                  | _ -> lex_file (INT(Char.escaped char_read))) 
      | '.' -> ( match token_type with
              | INT x -> lex_file (TOKEN_FLOAT(x ^ (Char.escaped char_read)))
              | TOKEN_FLOAT(x) -> lex_file (TOKEN_FLOAT(x ^ (Char.escaped char_read)))
              | UKNOW_type(x) -> lex_file (UKNOW_type(x ^ (Char.escaped char_read)))
              | _ -> token_type::lex_file (UKNOW_type(".")))
      | '(' -> ( match token_type with
                | UT | SPACE _ -> lex_file LBRACKET
                | _ -> token_type :: lex_file LBRACKET)
      | '*' -> (match token_type with
                | LBRACKET -> START_COMMENT::lex_file UT
                | SPACE _ -> lex_file STAR
                | _ -> token_type::lex_file STAR )
      | ')' -> (match token_type with
                | STAR -> END_COMMENT :: lex_file UT
                | SPACE _ -> RBRACKET::lex_file UT
                | _ -> token_type::RBRACKET::lex_file UT)
      | ';' -> (match token_type with
              | SEMI_COLUM -> END_FUNCTION::lex_file UT
              | UT -> lex_file SEMI_COLUM
              | _ -> token_type::lex_file SEMI_COLUM)
      | '/'  -> (match token_type with
                | SEMI_COLUM -> END_FUNCTION::lex_file UT
                | UT -> lex_file SEMI_COLUM
                | _ -> token_type::lex_file SEMI_COLUM)
      | _ -> match token_type with
              | UKNOW_type x -> lex_file (UKNOW_type (x ^ (Char.escaped char_read) ))
              | TOKEN_FLOAT _ | INT _-> failwith "Not implemented or unknow syntaxe"
              | SPACE _ | UT -> lex_file (UKNOW_type(Char.escaped char_read))
              | _ -> token_type::lex_file (UKNOW_type(Char.escaped char_read))
              
    with End_of_file -> flush stdout; close_in ic ;print_endline "fini"; ( match token_type with
                | UT | SPACE _ -> []
                | _ -> [token_type] )
    | e -> close_in_noerr ic; raise e
  in
    lex_file UT;;
