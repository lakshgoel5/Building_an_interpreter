open Ast
open Typechecker

let parse_exp_list s =
s
|> String.trim                (* Trim spaces around the string *)
|> (fun str ->                (* Remove brackets if present *)
	if String.get str 0 = '[' && String.get str (String.length str - 1) = ']'
	then String.sub str 1 (String.length str - 2)
	else str)
|> String.split_on_char ','   (* Split by commas *)
|> List.map String.trim       (* Trim each element *)
|> List.map (fun x ->
	try Ast.Const_Int (int_of_string x)
	with Failure _ ->
	try Ast.Const_Float (float_of_string x)
	with Failure _ -> failwith "Invalid vector/matrix element"
)

let parse_exp_list_list s =
s
|> String.trim
|> (fun str ->
	if String.get str 0 = '[' && String.get str (String.length str - 1) = ']'
	then String.sub str 1 (String.length str - 2)
	else str)
|> String.split_on_char ';'         (* Split outer structure into rows *)
|> List.map (fun row ->
	row
	|> parse_exp_list             (* Use `parse_exp_list` for elements in each row *)
    )

    
let rec negative_nos = function
	| Const_Int x -> Const_Int (-x)
	| Const_Float x -> Const_Float (-.x)
	| Const_Vector_int (n, lst) -> Const_Vector_int (n, List.map negative_nos lst)
	| Const_Vector_float (n, lst) -> Const_Vector_float (n, List.map negative_nos lst)
	| Const_Matrix_int (r, c, mat) -> Const_Matrix_int (r, c, List.map (fun row -> List.map negative_nos row) mat)
	| Const_Matrix_float (r, c, mat) -> Const_Matrix_float (r, c, List.map (fun row -> List.map negative_nos row) mat)
	| _ -> failwith "Negation is only defined for numeric types"
    

let thd (_, _, z) = z
let second (_, y, _) = y
let first (x, _, _) = x



let string_of_data_type = function
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TVectorf dim -> Printf.sprintf "Vector(%d)" dim
  | TVectori dim -> Printf.sprintf "Vector(%d)" dim
  | TMatrixf (r, c) -> Printf.sprintf "Matrixf(%d,%d)" r c
  | TMatrixi (r, c) -> Printf.sprintf "Matrixi(%d,%d)" r c
  | TStatement -> "Statement"
  | TCreate -> "Create"
  | TInput -> "Input"

let symbol_table = ref (SymbolTable.empty)

(* Recursively traverses the AST and applies the provided function to each node *)
let rec traverse_ast process_node ast =
  (* First process this node *)
  process_node ast;
  
  (* Then recursively process child nodes based on the AST structure *)
  match ast with
  | Line(stmt1, stmt2) ->
      traverse_ast process_node stmt1;
      traverse_ast process_node stmt2
  | For(init, cond, incr, body) ->
      traverse_ast process_node init;
      traverse_ast process_node cond;
      traverse_ast process_node incr;
      traverse_ast process_node body
  | If(cond, then_branch, else_branch) ->
      traverse_ast process_node cond;
      traverse_ast process_node then_branch;
      traverse_ast process_node else_branch
  | While(cond, body) ->
      traverse_ast process_node cond;
      traverse_ast process_node body
  | Print(expr) ->
      traverse_ast process_node expr
  | Assign(e1, e2) | Add(e1, e2) | Sub(e1, e2) | Multiply(e1, e2) | Divide(e1, e2)
  | Equal(e1, e2) | NotEqual(e1, e2) | Less(e1, e2) | LessEq(e1, e2)
  | Greater(e1, e2) | GreaterEq(e1, e2) | DotProd(e1, e2)
  | Remainder(e1, e2) | Angle(e1, e2) | Conjunction(e1, e2) | Disjunction(e1, e2) ->
      traverse_ast process_node e1;
      traverse_ast process_node e2
  | Negation(e) | Abs(e) | Sqrt(e) | Mag(e) | Dimension(e)
  | Row(e) | Cols(e) | Transpose(e) | Determinant(e) | Inverse(e) ->
      traverse_ast process_node e
  | Pow(e1, e2) | Log(e1, e2) | CreateEmpty(e1, e2) ->
      traverse_ast process_node e1;
      traverse_ast process_node e2
  | Minor(e1, e2, e3) ->
      traverse_ast process_node e1;
      traverse_ast process_node e2;
      traverse_ast process_node e3
  | AccessMatrix(_, e1, e2) ->
      traverse_ast process_node e1;
      traverse_ast process_node e2
  | AccessVector(_, e) ->
      traverse_ast process_node e
  (* Base Case *)
  | Int(_) | Float(_) | Bool(_) | Vectori(_, _) | Vectorf(_, _)
  | Matrixi(_, _, _) | Matrixf(_, _, _) | Const_Int(_) | Const_Float(_)
  | Const_Bool(_) | Variable(_) | Filename(_) | Input | Inputf(_)
  | Const_Vector_int(_, _) | Const_Vector_float(_, _)
  | Const_Matrix_int(_, _, _) | Const_Matrix_float(_, _, _) 
  | Raise(_) -> ()

(* In a separate module or function *)
let populate_symbol_table ast =
  let rec process_node = function
    | Int(var) -> 
        symbol_table := SymbolTable.add var TInt !symbol_table
    | Float(var) -> 
        symbol_table := SymbolTable.add var TFloat !symbol_table
    | Bool(var) -> 
        symbol_table := SymbolTable.add var TBool !symbol_table
    | Vectorf(size, var) -> 
        symbol_table := SymbolTable.add var (TVectorf size) !symbol_table
    | Vectori(size, var) -> 
        symbol_table := SymbolTable.add var (TVectori size) !symbol_table
    | Matrixf(rows, cols, var) ->
        symbol_table := SymbolTable.add var (TMatrixf (rows, cols)) !symbol_table
    | Matrixi(rows, cols, var) ->
        symbol_table := SymbolTable.add var (TMatrixi (rows, cols)) !symbol_table
    | _ -> ()
  in
  (* Traverse the entire AST *)
  traverse_ast process_node ast

let run_type_checker (ast : Ast.exp) =
  try
    (* Step 1: Perform type checking *)
    let _ = populate_symbol_table ast in
    let _ = type_check ast !symbol_table in
    
    (* Step 2: If we get here, type checking passed *)
    Printf.printf "Type checking passed successfully!\n"
  
  with
  | TypeError error ->
      begin match error with
      | TypeMismatch (op, expected, actual) ->
          Printf.printf "Type error in %s: Expected %s, got %s\n"
            op
            (string_of_data_type expected)
            (string_of_data_type actual)
      | UndefinedVariable var ->
          Printf.printf "Type error: Undefined variable '%s'\n" var
      | IncompatibleDimensions (op, type1, type2) ->
          Printf.printf "Type error in %s: Incompatible dimensions %s and %s\n"
            op (string_of_data_type type1) (string_of_data_type type2)
      | InvalidOperation msg ->
          Printf.printf "Type error: Invalid operation - %s\n" msg
      | InvalidZeroDimension ->
          Printf.printf "Type Error: Vector/Matrix of dimension zero"
      | OtherError msg ->
          Printf.printf "Type error: %s\n" msg
      end
  | e ->
      Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
