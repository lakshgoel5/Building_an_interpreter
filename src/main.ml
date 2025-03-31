open Calc
open Parser
open Interpreter

let initialize_env () = Environment.StringMap.empty;;

(* let run_file filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let ast = Parser.top Calc.tokenize lexbuf in
  let env = initialize_env () in
  try
    Utils.run_type_checker ast; 
    Interpreter.eval env ast
  with
  | e -> Printf.printf "Error Occurred in Main: %s\n" (Printexc.to_string e);
  Const_Int 0


let () =
  if Array.length Sys.argv = 2 then
    (* Run the file provided as command line argument *)
    run_file Sys.argv.(1)
  else
    run_file "testcases/input1.txt"
;; *)

let () =
let filename = 
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)  (* Use the first command-line argument *)
  else
    "testcases/input1.txt"  (* Default if no argument provided *)
  in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let env = initialize_env () in
  try
    let ast = Parser.top Calc.tokenize lexbuf in
    Utils.run_type_checker ast;
    let _ = Interpreter.eval env ast in
    close_in chan;  (* Close the channel after reading *)
    ()
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | Parsing.Parse_error -> 
    let pos = Lexing.lexeme_start lexbuf in
    let error_char = Lexing.lexeme lexbuf in
    Printf.printf "Syntax error at position %d: Unexpected '%s'\n" pos error_char
  | _ -> Printf.printf "Unexpected error occurred.\n"
