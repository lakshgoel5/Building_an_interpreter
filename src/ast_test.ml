open Ast
open Parser

let rec pretty_print_ast e indent =
  let indent_str = String.make indent ' ' in
  let new_indent = indent + 2 in
  let print_indented s = Printf.printf "%s%s" indent_str s in
  
  match e with
  | Line (e1, e2) -> 
      print_indented "Line(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Const_Int i -> 
      print_indented (Printf.sprintf "Const_Int(%d)" i)
  | Const_Float f -> 
      print_indented (Printf.sprintf "Const_Float(%f)" f)
  | Const_Bool b -> 
      print_indented (Printf.sprintf "Const_Bool(%b)" b)
  | Add (e1, e2) -> 
      print_indented "Add(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Multiply (e1, e2) -> 
      print_indented "Multiply(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Divide (e1, e2) -> 
      print_indented "Divide(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Sub (e1, e2) -> 
      print_indented "Sub(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Const_Vector_int (i, l) -> 
      print_indented (Printf.sprintf "Const_Vector_int(%d, [\n" i);
      List.iteri (fun idx e -> 
        pretty_print_ast e new_indent;
        if idx < List.length l - 1 then Printf.printf ",\n"
      ) l;
      Printf.printf "\n%s])" indent_str
  | Const_Vector_float (i, l) -> 
    print_indented (Printf.sprintf "Const_Vector_float(%d, [\n" i);
    List.iteri (fun idx e -> 
      pretty_print_ast e new_indent;
      if idx < List.length l - 1 then Printf.printf ",\n"
    ) l;
    Printf.printf "\n%s])" indent_str
  | Const_Matrix_int (i, j, l) -> 
      print_indented (Printf.sprintf "Const_Matrix_int(%d, %d, [\n" i j);
      List.iteri (fun idx row -> 
        Printf.printf "%s  [\n" indent_str;
        List.iteri (fun jdx e -> 
          pretty_print_ast e (new_indent + 2);
          if jdx < List.length row - 1 then Printf.printf ",\n"
        ) row;
        Printf.printf "\n%s  ]" indent_str;
        if idx < List.length l - 1 then Printf.printf ",\n"
      ) l;
      Printf.printf "\n%s])" indent_str
  | Const_Matrix_float (i, j, l) -> 
    print_indented (Printf.sprintf "Const_Matrix_float(%d, %d, [\n" i j);
    List.iteri (fun idx row -> 
      Printf.printf "%s  [\n" indent_str;
      List.iteri (fun jdx e -> 
        pretty_print_ast e (new_indent + 2);
        if jdx < List.length row - 1 then Printf.printf ",\n"
      ) row;
      Printf.printf "\n%s  ]" indent_str;
      if idx < List.length l - 1 then Printf.printf ",\n"
    ) l;
    Printf.printf "\n%s])" indent_str
  | Variable s -> 
      print_indented (Printf.sprintf "Variable(%s)" s)
  | Equal (e1, e2) -> 
      print_indented "Equal(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | NotEqual (e1, e2) -> 
      print_indented "NotEqual(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Less (e1, e2) -> 
      print_indented "Less(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | LessEq (e1, e2) -> 
      print_indented "LessEq(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Greater (e1, e2) -> 
      print_indented "Greater(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | GreaterEq (e1, e2) -> 
      print_indented "GreaterEq(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Remainder (e1, e2) -> 
      print_indented "Remainder(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Conjunction (b1, b2) -> 
      print_indented "Conjunction(\n";
      pretty_print_ast b1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast b2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Disjunction (b1, b2) -> 
      print_indented "Disjunction(\n";
      pretty_print_ast b1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast b2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Negation b -> 
      print_indented "Negation(\n";
      pretty_print_ast b new_indent;
      Printf.printf "\n%s)" indent_str
  | Assign (t, e) -> 
      print_indented "Assign(\n";
      pretty_print_ast t new_indent;
      Printf.printf ",\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | Int s -> 
      print_indented (Printf.sprintf "Int(%s)" s)
  | Float s -> 
      print_indented (Printf.sprintf "Float(%s)" s)
  | Bool s -> 
      print_indented (Printf.sprintf "Bool(%s)" s)
  | Vectorf (d,s) -> 
      print_indented (Printf.sprintf "Vectorf(%d,%s)" d s)
  | Vectori (d,s) -> 
    print_indented (Printf.sprintf "Vectori(%d,%s)" d s)
  | Matrixi (r,c,s) -> 
      print_indented (Printf.sprintf "Matrixi(%d,%d,%s)" r c s)
  | Matrixf (r,c,s) -> 
    print_indented (Printf.sprintf "Matrixf(%d,%d,%s)" r c s)
  | For (e1, e2, e3, e4) -> 
      print_indented "For(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e3 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e4 new_indent;
      Printf.printf "\n%s)" indent_str
  | While (e1, e2) -> 
      print_indented "While(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | If (e1, e2, e3) -> 
      print_indented "If(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e3 new_indent;
      Printf.printf "\n%s)" indent_str
  | Print e -> 
      print_indented "Print(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | Inputf s -> 
      print_indented "Inputf(\n";
      pretty_print_ast s new_indent;
      Printf.printf "\n%s)" indent_str
  | Input ->
      print_indented "Input"
  | Filename s -> 
      print_indented (Printf.sprintf "Filename(%s)" s)
  | Abs e -> 
      print_indented "Abs(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | Sqrt e -> 
      print_indented "Sqrt(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | Pow (e1, e2) -> 
      print_indented "Pow(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Log (e1, e2) -> 
      print_indented "Log(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Mag e -> 
      print_indented "Mag(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | Transpose e -> 
      print_indented "Transpose(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | Determinant e -> 
      print_indented "Determinant(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | Inverse e -> 
      print_indented "Inverse(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | CreateEmpty (e1, e2) ->
      print_indented "CreateEmpty(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Minor (a1, e1, e2) ->
      print_indented "Minor(\n";
      pretty_print_ast a1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Angle (e1, e2) -> 
      print_indented "Angle(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Dimension e -> 
      print_indented "Dimension(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | Row e -> 
      print_indented "Row(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | Cols e ->
      print_indented "Cols(\n";
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | DotProd (e1, e2) -> 
      print_indented "DotProd(\n";
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | AccessVector (s, e) -> 
      print_indented (Printf.sprintf "AccessVector(%s,\n" s);
      pretty_print_ast e new_indent;
      Printf.printf "\n%s)" indent_str
  | AccessMatrix (s, e1, e2) -> 
      print_indented (Printf.sprintf "AccessMatrix(%s,\n" s);
      pretty_print_ast e1 new_indent;
      Printf.printf ",\n";
      pretty_print_ast e2 new_indent;
      Printf.printf "\n%s)" indent_str
  | Raise s -> 
      print_indented (Printf.sprintf "Raise(%s)" s)

(* Wrapper function to start the pretty printing *)
let pretty_print e =
  pretty_print_ast e 0;
  Printf.printf "\n"

let () =
let filename = 
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)  (* Use the first command-line argument *)
  else
    "testcases/input7.txt"  (* Default if no argument provided *)
  in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = Parser.top Calc.tokenize lexbuf in (*tokenize creates tokens of lexbuf and top in entry point of parser*)
    Printf.printf "AST Produced:\n";
    pretty_print ast;
    Printf.printf "\n";
    Printf.printf "\nPerforming type checking...\n";
    Utils.run_type_checker ast;
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | Parsing.Parse_error -> 
    let pos = Lexing.lexeme_start lexbuf in
    let error_char = Lexing.lexeme lexbuf in
    Printf.printf "Syntax error at position %d: Unexpected '%s'\n" pos error_char
  | _ -> Printf.printf "Unexpected error occurred.\n"
