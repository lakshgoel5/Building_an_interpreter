open Parser
open Calc  (* Import the tokenizer *)

let to_string = function
| COMM_START -> "COMM_START"
| COMM_END -> "COMM_END"
| INPUT -> "INPUT"
| PRINT -> "PRINT"
| FILENAME x -> "FILENAME(" ^ x ^ ")"
| BOOL -> "BOOL"
| INT -> "INT"
| FLOAT -> "FLOAT"

| MATRIX -> "MATRIX"
| VECTOR -> "VECTOR"

| VEC_INT (x,l)-> "VEC_INT(" ^ string_of_int x ^ ",\"" ^ l ^ "\")"
| VEC_FLOAT (x,l) -> "VEC_FLOAT(" ^ string_of_int x ^ ",\"" ^ l ^ "\")"
| VEC_GEN (x,l)-> "VEC_GEN(" ^ string_of_int x ^ ",\"" ^ l ^ "\")"
| MAT_INT (d1,d2,l)-> "MAT_INT(" ^ string_of_int d1 ^ "," ^ string_of_int d2 ^ ",\"" ^ l ^ "\")"
| MAT_FLOAT (d1,d2,l)-> "MAT_FLOAT("^ string_of_int d1 ^"," ^ string_of_int d2 ^ ",\"" ^ l ^ "\")"
| MAT_GEN (d1,d2,l)-> "MAT_GEN(" ^ string_of_int d1 ^ "," ^ string_of_int d2 ^ ",\"" ^ l ^ "\")"

| ABS -> "ABS"
| EQUALS -> "EQUALS"
| PLUS -> "PLUS"
| MINUS -> "MINUS"
| TIMES -> "TIMES"
| DIVIDE -> "DIVIDE"
| LPAREN -> "LPAREN"
| RPAREN -> "RPAREN"
| LBRACE -> "LBRACE"
| RBRACE -> "RBRACE"
| VAR s -> "VAR(" ^ s ^ ")"
| CONST_INT i -> "CONST_INT(" ^ string_of_int i ^ ")"
| CONST_FLOAT f -> "CONST_FLOAT(" ^ string_of_float f ^ ")"
| CONST_BOOL b -> "CONST_BOOL(" ^ string_of_bool b ^ ")"
| CONJUNCTION -> "CONJUNCTION"
| DISJUNCTION -> "DISJUNCTION"
| NEGATION -> "NEGATION"
| LESS -> "LESS"
| LESSEQ -> "LESSEQ"
| GREATER -> "GREATER"
| GREATEREQ -> "GREATEREQ"
| EQUAL -> "EQUAL"
| NOTEQUAL -> "NOTEQUAL"
| REM -> "REM"
| DOT_PROD -> "DOT_PROD"
| ANGLE -> "ANGLE"
| MAG -> "MAG"
| DIM -> "DIM"
| TRANSPOSE -> "TRANSPOSE"
| DET -> "DET"
| FOR -> "FOR"
| WHILE -> "WHILE"
| SEMICOLON -> "SEMICOLON"
| COMMA -> "COMMA"
| IF -> "IF"
| THEN -> "THEN"
| ELSE -> "ELSE"
| LBRACKET -> "LBRACKET"
| RBRACKET -> "RBRACKET"
| EOF -> "EOF"

let () =
  let filename = "testcases/input1.txt" in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let rec loop () =
    match Calc.tokenize lexbuf with
    | EOF -> Printf.printf "EOF\n"; close_in chan  (* End of file reached *)
    | token ->
      Printf.printf "%s\n" (to_string token); (* Print each token *)
      loop () (* Continue reading tokens *)
  in
  loop ()
