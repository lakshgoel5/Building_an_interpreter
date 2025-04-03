open Ast
open Environment
open Operations

(*env is gamma*)
(**)

let rec eval_with_env env ast =
  match ast with
  | Line (e1, e2) ->
    let (new_env, _) = eval_with_env env e1 in
    eval_with_env new_env e2

  | Const_Int i -> (env, Const_Int i)
  | Const_Float f -> (env, Const_Float f)
  | Const_Bool b -> (env, Const_Bool b)
  | Const_Vector_int (d,v) -> (env, Const_Vector_int (d,v))
  | Const_Vector_float (d,v) -> (env, Const_Vector_float (d,v))
  | Const_Matrix_float (r, c, m) -> (env, Const_Matrix_float (r, c, m))
  | Const_Matrix_int (r, c, m) -> (env, Const_Matrix_int (r, c, m))
  
  (*Un initialized variables set to 0*)
  | Int name -> let new_env = Environment.extend env name (VInt 0) in (new_env, Const_Int 0)
  | Float name -> let new_env = Environment.extend env name (VFloat 0.0) in (new_env, Const_Float 0.0)
  | Bool name -> let new_env = Environment.extend env name (VBool false) in (new_env, Const_Bool false)
  | Vectori (d, name) -> let new_env = Environment.extend env name (VVectori (d, List.init d (fun _ -> 0))) in (new_env, Const_Vector_int (d, List.init d (fun _ -> Const_Int 0)))
  | Vectorf (d, name) -> let new_env = Environment.extend env name (VVectorf (d, List.init d (fun _ -> 0.0))) in (new_env, Const_Vector_float (d, List.init d (fun _ -> Const_Float 0.0)))
  | Matrixi (r, c, name) -> let new_env = Environment.extend env name (VMatrixi (r, c, List.init r (fun _ -> List.init c (fun _ -> 0)))) in (new_env, Const_Matrix_int (r, c, List.init r (fun _ -> List.init c (fun _ -> Const_Int 0))))
  | Matrixf (r, c, name) -> let new_env = Environment.extend env name (VMatrixf (r, c, List.init r (fun _ -> List.init c (fun _ -> 0.0)))) in (new_env, Const_Matrix_float (r, c, List.init r (fun _ -> List.init c (fun _ -> Const_Float 0.0))))
  | Variable name ->
    (match Environment.lookup env name with
      | Some (VInt i) -> (env, Const_Int i)
      | Some (VFloat f) -> (env, Const_Float f)
      | Some (VBool b) -> (env, Const_Bool b)
      | Some (VVectori (d, l)) -> (env, Const_Vector_int (d, List.map (fun i -> Const_Int i) l))
      | Some (VVectorf (d, l)) -> (env, Const_Vector_float (d, List.map (fun f -> Const_Float f) l))
      | Some (VMatrixi (r, c, m)) -> (env, Const_Matrix_int (r, c, List.map (List.map (fun i -> Const_Int i)) m))
      | Some (VMatrixf (r, c, m)) -> (env, Const_Matrix_float (r, c, List.map (List.map (fun f -> Const_Float f)) m))
      | None -> raise (Failure ("Variable not found: " ^ name)))
    
  | Assign(Int(e1), e2) ->
    let v2 = eval env e2 in
    (match v2 with
     | Const_Int i ->
         let new_env = Environment.extend env e1 (VInt i) in
         (new_env, v2)
     | _ -> raise (Failure "Type mismatch: expected int"))
     
  | Assign(Float(e1), e2) ->
    let v2 = eval env e2 in
    (match v2 with
     | Const_Float f ->
         let new_env = Environment.extend env e1 (VFloat f) in
         (new_env, v2)
     | _ -> raise (Failure "Type mismatch: expected float"))
     
  | Assign(Bool(e1), e2) ->
    let v2 = eval env e2 in
    (match v2 with
     | Const_Bool b ->
         let new_env = Environment.extend env e1 (VBool b) in
         (new_env, v2)
     | _ -> raise (Failure "Type mismatch: expected bool"))
     
  | Assign(Vectori(d,e1), e2) ->
    let v2 = eval env e2 in
    (match v2 with
     | Const_Vector_int (d1, l) when d1 = d ->
         let vec_values = List.map (fun x -> match x with Const_Int f -> f | _ -> raise (Failure "Invalid vector element")) l in
         let new_env = Environment.extend env e1 (VVectori (d, vec_values)) in
         (new_env, v2)
     | _ -> raise (Failure "Type mismatch: expected vector int"))
  
  | Assign(Vectorf(d,e1), e2) ->
    let v2 = eval env e2 in
    (match v2 with
      | Const_Vector_float (d1, l) when d1 = d ->
        let vec_values = List.map (fun x -> match x with Const_Float f -> f | _ -> raise (Failure "Invalid vector element")) l in
        let new_env = Environment.extend env e1 (VVectorf (d, vec_values)) in
        (new_env, v2)
      | _ -> raise (Failure "Type mismatch: expected vector float"))

  | Assign(Matrixi(d1,d2,e1), e2) ->
    let v2 = eval env e2 in
    (match v2 with
      | Const_Matrix_int (dm1, dm2, m) -> (* No need to check dimension, already done *)
        (* Explicit pattern matching inside List.map to avoid partial match warning *)
        let matrix_values = List.map (fun row -> 
          List.map (fun x -> match x with
            | Const_Int i -> i
            | _ -> failwith "Type error: Expected only int values in matrix"
          ) row
        ) m in
        let new_env = Environment.extend env e1 (VMatrixi (d1, d2, matrix_values)) in
        (new_env, v2)
      | _ -> raise (Failure "Type mismatch: expected matrix int")
    )
    
  | Assign(Matrixf(d1,d2,e1), e2) ->
    let v2 = eval env e2 in
    (match v2 with
      | Const_Matrix_float (dm1, dm2, m) -> (* No need to check dimension, already done *)
        (* Explicit pattern matching inside List.map to avoid partial match warning *)
        let matrix_values = List.map (fun row -> 
          List.map (fun x -> match x with
            | Const_Float i -> i
            | _ -> failwith "Type error: Expected only float values in matrix"
          ) row
        ) m in
        let new_env = Environment.extend env e1 (VMatrixf (d1, d2, matrix_values)) in
        (new_env, v2)
      | _ -> raise (Failure "Type mismatch: expected matrix float")
    )
  
  | Assign(Variable name, e2) ->
    let v2 = eval env e2 in
    let valu = Environment.lookup env name in
    (match (v2, valu) with
      | (Const_Int i, Some (VInt _)) ->
        let new_env = Environment.extend env name (VInt i) in
        (new_env, v2)
      | (Const_Float f, Some (VFloat _)) ->
        let new_env = Environment.extend env name (VFloat f) in
        (new_env, v2)
      | (Const_Bool b, Some (VBool _)) ->
        let new_env = Environment.extend env name (VBool b) in
        (new_env, v2)
      | (Const_Vector_int (d, l), Some (VVectori (d1, _))) when d = d1 ->
        let vec_values = List.map (fun x -> match x with Const_Int f -> f | _ -> raise (Failure "Invalid vector element")) l in
        let new_env = Environment.extend env name (VVectori (d, vec_values)) in
        (new_env, v2)
      | (Const_Vector_float (d, l), Some (VVectorf (d1, _))) when d = d1 ->
        let vec_values = List.map (fun x -> match x with Const_Float f -> f | _ -> raise (Failure "Invalid vector element")) l in
        let new_env = Environment.extend env name (VVectorf (d, vec_values)) in
        (new_env, v2)
      | (Const_Matrix_int (r, c, m), Some (VMatrixi (r1, c1, _))) when r = r1 && c = c1 ->
        let mat_values = List.map (fun row -> List.map (fun x -> match x with Const_Int i -> i | _ -> raise (Failure "Invalid matrix element")) row) m in
        let new_env = Environment.extend env name (VMatrixi (r, c, mat_values)) in
        (new_env, v2)
      | (Const_Matrix_float (r, c, m), Some (VMatrixf (r1, c1, _))) when r = r1 && c = c1 ->
        let mat_values = List.map (fun row -> List.map (fun x -> match x with Const_Float f -> f | _ -> raise (Failure "Invalid matrix element")) row) m in
        let new_env = Environment.extend env name (VMatrixf (r, c, mat_values)) in
        (new_env, v2)
      | _ -> raise (Failure "Type mismatch: variable type does not match"))

  | Assign (_, _) -> raise (Failure "Invalid Assignment")
  
  | Print(e) ->
    let v = eval env e in
    (match v with
      | Const_Int i -> Printf.printf "%d\n" i; (env, v)
      | Const_Float f -> Printf.printf "%f\n" f; (env, v)
      | Const_Bool b -> Printf.printf "%b\n" b; (env, v)
      | Const_Vector_int (d, l) -> 
          let str_values = String.concat ", " (List.map (fun x -> match x with Const_Int i -> string_of_int i | _ -> "") l) in
          Printf.printf "Vector of dimension %d: [%s]\n" d str_values; (env, v)
      | Const_Vector_float (d, l) -> 
          let str_values = String.concat ", " (List.map (fun x -> match x with Const_Float f -> string_of_float f | _ -> "") l) in
          Printf.printf "Vector of dimension %d: [%s]\n" d str_values; (env, v)
      | Const_Matrix_int (d1, d2, m) ->
        let row_strings = List.map (fun row -> 
          String.concat ", " 
            (List.map (fun x -> match x with  (*matching necessary, else it gives pattern matchng warning*)
                | Const_Int i -> string_of_int i 
                | _ -> failwith "Type error: Expected only int values in matrix")
            row)
        ) m in
        Printf.printf "Matrix of dimension %d %d:[\n" d1 d2;
        List.iter (fun row -> Printf.printf "[%s]\n" row) row_strings;
        Printf.printf "]\n"; (env, v)
        
      | Const_Matrix_float (d1, d2, m) ->
        let row_strings = List.map (fun row -> 
          String.concat ", " 
            (List.map (fun x -> match x with 
                | Const_Float f -> string_of_float f 
                | _ -> failwith "Type error: Expected only float values in matrix")
            row)
        ) m in
        Printf.printf "Matrix of dimension %d %d:[\n" d1 d2;
        List.iter (fun row -> Printf.printf "[%s]\n" row) row_strings;
        Printf.printf "]\n"; (env, v)
    
      | _ -> raise (Failure "Unsupported type for printing"))
  
  | Add (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Int (i1 + i2))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Float (f1 +. f2))
    | (Const_Vector_int (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int f1, Const_Int f2) -> Const_Int (f1 + f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_int (d, res))    
    | (Const_Vector_float (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Float f2) -> Const_Float (f1 +. f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Vector_int (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int i1, Const_Float f2) -> Const_Float ((float_of_int i1) +. f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res)) 
    | (Const_Vector_float (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Int i2) -> Const_Float (f1 +. (float_of_int i2))
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res)) 
    | (Const_Matrix_int (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Int i2) -> Const_Int (i1 + i2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_int (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Float f2) -> Const_Float (f1 +. f2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_int (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Float f2) -> Const_Float ((float_of_int i1) +. f2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Int i2) -> Const_Float (f1 +. (float_of_int i2))
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | _ -> raise (Failure "Type mismatch in addition operation"))

  | Sub (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Int (i1 - i2))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Float (f1 -. f2))
    | (Const_Vector_int (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int f1, Const_Int f2) -> Const_Int (f1 - f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_int (d, res))
    | (Const_Vector_float (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Float f2) -> Const_Float (f1 -. f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Vector_int (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int i1, Const_Float f2) -> Const_Float ((float_of_int i1) -. f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Vector_float (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Int i2) -> Const_Float (f1 -. (float_of_int i2))
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Matrix_int (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Int i2) -> Const_Int (i1 - i2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_int (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Float f2) -> Const_Float (f1 -. f2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_int (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Float f2) -> Const_Float ((float_of_int i1) -. f2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Int i2) -> Const_Float (f1 -. (float_of_int i2))
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | _ -> raise (Failure "Type mismatch in subtraction operation"))
  | Multiply(e1, e2) -> (*Matrix Scaler Mult not included*)
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Int (i1 * i2))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Float (f1 *. f2))
    | (Const_Vector_int (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int f1, Const_Int f2) -> Const_Int (f1 * f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_int (d, res))
    | (Const_Vector_float (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Float f2) -> Const_Float (f1 *. f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Vector_int (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int i1, Const_Float f2) -> Const_Float ((float_of_int i1) *. f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Vector_float (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Int i2) -> Const_Float (f1 *. (float_of_int i2))
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Matrix_int (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Int i2) -> Const_Int (i1 * i2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_int (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Float f2) -> Const_Float (f1 *. f2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_int (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Float f2) -> Const_Float ((float_of_int i1) *. f2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Int i2) -> Const_Float (f1 *. (float_of_int i2))
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Vector_int (d, s1), Const_Int i2) ->
      let res = List.map (fun x -> match x with Const_Int f1 -> Const_Int (f1 * i2) | _ -> raise (Failure "Invalid vector element")) s1 in
      (env, Const_Vector_int (d, res))
    | (Const_Vector_float (d, s1), Const_Float f2) ->
      let res = List.map (fun x -> match x with Const_Float f1 -> Const_Float (f1 *. f2) | _ -> raise (Failure "Invalid vector element")) s1 in
      (env, Const_Vector_float (d, res))
    | (Const_Int i1, Const_Vector_int (d, s2)) ->
      let res = List.map (fun x -> match x with Const_Int f2 -> Const_Int (i1 * f2) | _ -> raise (Failure "Invalid vector element")) s2 in
      (env, Const_Vector_int (d, res))
    | (Const_Float f1, Const_Vector_float (d, s2)) ->
      let res = List.map (fun x -> match x with Const_Float f2 -> Const_Float (f1 *. f2) | _ -> raise (Failure "Invalid vector element")) s2 in
      (env, Const_Vector_float (d, res))
    | _ -> raise (Failure "Type mismatch in multiplication operation"))

  | Divide (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> if i2 <> 0 then (env, Const_Int (i1 / i2)) else raise (Failure "Division by zero in integer division")
    | (Const_Float f1, Const_Float f2) -> if f2 <> 0.0 then (env, Const_Float (f1 /. f2)) else raise (Failure "Division by zero in float division")
    | (Const_Vector_int (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int f1, Const_Int f2) -> if f2 <> 0 then Const_Int (f1 / f2) else raise (Failure "Division by zero in vector int division")
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_int (d, res))
    | (Const_Vector_float (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Float f2) -> if f2 <> 0.0 then Const_Float (f1 /. f2) else raise (Failure "Division by zero in vector float division")
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Vector_int (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int i1, Const_Float f2) ->  if f2 <> 0.0 then Const_Float ((float_of_int i1) /. f2) else raise (Failure "Division by zero in vector int to float division")
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Vector_float (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Int i2)  -> if i2 <> 0 then Const_Float (f1 /. (float_of_int i2)) else raise (Failure "Division by zero in vector float to int division")
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Matrix_int (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Int i2) -> if i2 <> 0 then Const_Int (i1 / i2) else raise (Failure "Division by zero in matrix int division")
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_int (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Float f2) -> if f2 <> 0.0 then Const_Float (f1 /. f2) else raise (Failure "Division by zero in matrix float division")
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_int (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Float f2) -> if f2 <> 0.0 then Const_Float ((float_of_int i1) /. f2) else raise (Failure "Division by zero in matrix int to float division")
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Int i2) ->if i2 <> 0 then Const_Float (f1 /. (float_of_int i2)) else raise (Failure "Division by zero in matrix float to int division")
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | _ -> raise (Failure "Type mismatch in division operation"))

  | Equal(e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Bool (i1 = i2))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Bool (f1 = f2))
    | (Const_Vector_int (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.for_all2 (fun x y -> match (x, y) with
        | (Const_Int f1, Const_Int f2) -> f1 = f2
        | _ -> false) s1 s2 in
      (env, Const_Bool res)
    | (Const_Vector_float (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.for_all2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Float f2) -> f1 = f2
        | _ -> false) s1 s2 in
      (env, Const_Bool res)
    | (Const_Matrix_float (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.for_all2 (fun row1 row2 -> List.for_all2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Float f2) -> f1 = f2
        | _ -> false) row1 row2) m1 m2 in
      (env, Const_Bool res)
    | (Const_Matrix_int (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.for_all2 (fun row1 row2 -> List.for_all2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Int i2) -> i1 = i2
        | _ -> false) row1 row2) m1 m2 in
      (env, Const_Bool res)
    | _ -> raise (Failure "Type mismatch in equality operation"))
  | Less (e1, e2) -> (*Vectors and Matrices not added*)
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Bool (i1 < i2))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Bool (f1 < f2))
    | _ -> raise (Failure "Type mismatch in less than operation"))

  | LessEq (e1, e2) -> (*Vectors and Matrices not added*)
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Bool (i1 <= i2))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Bool (f1 <= f2))
    | _ -> raise (Failure "Type mismatch in less than or equal to operation"))

  | Greater (e1, e2) -> (*Vectors and Matrices not added*)
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Bool (i1 > i2))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Bool (f1 > f2))
    | _ -> raise (Failure "Type mismatch in greater than operation"))

  | GreaterEq (e1, e2) -> (*Vectors and Matrices not added*)
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Bool (i1 >= i2))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Bool (f1 >= f2))
    | _ -> raise (Failure "Type mismatch in greater than or equal to operation"))

  | NotEqual (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Bool (i1 <> i2))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Bool (f1 <> f2))
    | (Const_Vector_int (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.exists2 (fun x y -> match (x, y) with
        | (Const_Int f1, Const_Int f2) -> f1 <> f2
        | _ -> false) s1 s2 in
      (env, Const_Bool res)
    | (Const_Vector_float (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.exists2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Float f2) -> f1 <> f2
        | _ -> false) s1 s2 in
      (env, Const_Bool res)
    | (Const_Matrix_float (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.exists2 (fun row1 row2 -> List.exists2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Float f2) -> f1 <> f2
        | _ -> false) row1 row2) m1 m2 in
      (env, Const_Bool res)
    | (Const_Matrix_int (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.exists2 (fun row1 row2 -> List.exists2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Int i2) -> i1 <> i2
        | _ -> false) row1 row2) m1 m2 in
      (env, Const_Bool res)
    | _ -> raise (Failure "Type mismatch in not equal to operation"))

  | Remainder (e1, e2) ->
    (match (eval env e1, eval env e2) with
    | (Const_Int v1, Const_Int v2) -> if v2 <> 0 then (env, Const_Int (v1 mod v2)) else raise (Failure "Division by zero in finding remainder")
    | _ -> failwith "Type error: Expected integer expressions for remainder operation")
  
  | Conjunction (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Bool b1, Const_Bool b2) -> (env, Const_Bool (b1 && b2))
    | _ -> raise (Failure "Type mismatch in conjunction operation"))
  | Disjunction (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Bool b1, Const_Bool b2) -> (env, Const_Bool (b1 || b2))
    | _ -> raise (Failure "Type mismatch in disjunction operation"))
  | Negation e1 ->
    let v1 = eval env e1 in
    (match v1 with
    | Const_Bool b1 -> (env, Const_Bool (not b1))
    | _ -> raise (Failure "Type mismatch in negation operation"))
    
  | Raise s -> raise (Failure s)
  | If (e1, e2, e3) ->
    (match eval env e1 with
    | Const_Bool v1 ->
        if v1 then
          let v2 = eval env e2 in
          (env, v2)
        else
          let v3 = eval env e3 in
          (env, v3)
    | _ -> failwith "Type error: Expected a boolean expression in if condition")

  | While (e1, e2) ->
    let rec loop env =
      (match eval_with_env env e1 with
      | (new_env, Const_Bool v1) ->
          if v1 then
            let (nenv, v2) = eval_with_env new_env e2 in
            loop nenv
          else
            (env, Const_Bool true)
      | _ -> failwith "Type error: Expected a boolean expression in while condition")
    in
    loop env

  | For (e1, e2, e3, e4) ->
    let (init_env,v1) = eval_with_env env e1 in
    let rec loop env =
      (match eval_with_env env e2 with
        | (new_env, Const_Bool v2) ->
          if v2 then
            let (nenv, v4) = eval_with_env new_env e4 in
            let (menv, v3) = eval_with_env nenv e3 in
            loop menv
          else
            (new_env, Const_Bool true)
        | _ -> failwith "Type error: Expected a boolean 2nd expression in for loop")
    in loop init_env

  | Abs e1 ->
    let v1 = eval env e1 in
    (match v1 with
    | Const_Int i -> (env, Const_Int (abs i))
    | Const_Float f -> (env, Const_Float (abs_float f))
    | _ -> raise (Failure "Abs can be applied only on Ints and Floats"))

  | Sqrt e1 ->
    let v1 = eval env e1 in
    (match v1 with
    | Const_Int i -> if i < 0 then raise (Failure "Sqrt of negative number") else (env, Const_Float (sqrt (float_of_int i)))
    | Const_Float f -> if f < 0.0 then raise (Failure "Sqrt of negative number") else (env, Const_Float (sqrt f))
    | _ -> raise (Failure "Sqrt can be applied only on Ints and Floats"))

  | Pow (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> (env, Const_Int (int_of_float ((float_of_int i1) ** (float_of_int i2))))
    | (Const_Float f1, Const_Float f2) -> (env, Const_Float (f1 ** f2))
    | (Const_Int i1, Const_Float f2) -> (env, Const_Float ((float_of_int i1) ** f2))
    | (Const_Float f1, Const_Int i2) -> (env, Const_Float (f1 ** (float_of_int i2)))
    | _ -> raise (Failure "Pow can be applied only on Ints and Floats"))

  | Log (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Int i1, Const_Int i2) -> if i1 <= 0 || i2 <= 1 then raise (Failure "Log base and value must be > 0 and base != 1") else (env, Const_Float (log (float_of_int i1) /. log (float_of_int i2)))
    | (Const_Float f1, Const_Float f2) -> if f1 <= 0.0 || f2 <= 1.0 then raise (Failure "Log base and value must be > 0 and base != 1") else (env, Const_Float (log f1 /. log f2))
    | (Const_Int i1, Const_Float f2) -> if i1 <= 0 || f2 <= 1.0 then raise (Failure "Log base and value must be > 0 and base != 1") else (env, Const_Float (log (float_of_int i1) /. log f2))
    | (Const_Float f1, Const_Int i2) -> if f1 <= 0.0 || i2 <= 1 then raise (Failure "Log base and value must be > 0 and base != 1") else (env, Const_Float (log f1 /. log (float_of_int i2)))
    | _ -> raise (Failure "Log can be applied only on Ints and Floats"))

  | Row e1 -> 
    let v1 = eval env e1 in
    (match v1 with
    | Const_Matrix_int (d1, d2, m) -> (env, Const_Int (d1))
    | Const_Matrix_float (d1, d2, m) -> (env, Const_Int (d1))
    | _ -> raise (Failure "Row can be applied only on Matrices"))

  | Cols e1 -> 
    let v1 = eval env e1 in
    (match v1 with
    | Const_Matrix_int (d1, d2, m) -> (env, Const_Int (d2))
    | Const_Matrix_float (d1, d2, m) -> (env, Const_Int (d2))
    | _ -> raise (Failure "Cols can be applied only on Matrices"))

  | Dimension e1 -> 
    let v1 = eval env e1 in
    (match v1 with
    | Const_Vector_int (d, s) -> (env, Const_Int (d))
    | Const_Vector_float (d, s) -> (env, Const_Int (d))
    | _ -> raise (Failure "Dimension can be applied only on Vectors"))
  
  | CreateEmpty (d1, d2) -> (*Only support floats*)
    (match (d1, d2) with
    | (Const_Int i1, Const_Int i2) when i1 > 0 && i2 > 0 -> (env, Const_Matrix_float (i1, i2, List.init i1 (fun _ -> List.init i2 (fun _ -> Const_Float 0.0))))
    | _ -> raise (Failure "Dimensions can only be positive Ints"))

  | DotProd (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Vector_int (d, s1), Const_Vector_int (d1, s2)) -> let res = Operations.dot_product_int (Operations.exp_list_to_int_list s1) (Operations.exp_list_to_int_list s2) in (env, Const_Int res)
    | (Const_Vector_float (d, s1), Const_Vector_float (d1, s2)) -> let res = Operations.dot_product_float (Operations.exp_list_to_float_list s1) (Operations.exp_list_to_float_list s2) in (env, Const_Float res)
    | _ -> raise (Failure "Type mismatch in dot product operation"))

  | Angle (e1, e2) -> (*Always return float*)
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | (Const_Vector_int (d, s1), Const_Vector_int (d1, s2)) -> if Operations.length (Operations.exp_list_to_float_list s1) <> 0. && Operations.length (Operations.exp_list_to_float_list s2) <> 0.  then let res = Operations.angle (Operations.exp_list_to_float_list s1) (Operations.exp_list_to_float_list s2) in (env, Const_Float res) else raise (Failure "Vector should not be NULL/zero")
    | (Const_Vector_float (d, s1), Const_Vector_float (d1, s2)) -> if Operations.length (Operations.exp_list_to_float_list s1) <> 0. && Operations.length (Operations.exp_list_to_float_list s2) <> 0.  then let res = Operations.angle (Operations.exp_list_to_float_list s1) (Operations.exp_list_to_float_list s2) in (env, Const_Float res) else raise (Failure "Vector should not be NULL/zero")
    | _ -> raise (Failure "Type mismatch in angle operation"))

  | Transpose e1 ->
    let v1 = eval env e1 in
    (match v1 with
    | Const_Matrix_int (r, c, m) -> let res = Operations.transpose m in (env, Const_Matrix_int (c, r, res))
    | Const_Matrix_float (r, c, m) -> let res = Operations.transpose m in (env, Const_Matrix_float (c, r, res))
    | _ -> raise (Failure "Transpose can be applied only on Matrices"))

  | Mag e1 -> (*Always return float*)
    let v1 = eval env e1 in
    (match v1 with
    | Const_Vector_int (d, s) -> let res = Operations.magnitude (Operations.exp_list_to_float_list s) in (env, Const_Float res)
    | Const_Vector_float (d, s) -> let res = Operations.magnitude (Operations.exp_list_to_float_list s) in (env, Const_Float res)
    | _ -> raise (Failure "Magnitude can be applied only on Vectors"))

  | Determinant e1 ->
    let v1 = eval env e1 in
    (match v1 with
    | Const_Matrix_int (r, c, m) -> let res = Operations.determinant_int (Operations.exp_matrix_to_int_matrix m) in (env, Const_Int res)
    | Const_Matrix_float (r, c, m) -> let res = Operations.determinant_float (Operations.exp_matrix_to_float_matrix m) in (env, Const_Float res)
    | _ -> raise (Failure "Determinant can be applied only on Matrices"))

  | Minor (e1, e2, e3) -> (*Matrix, index1, index2*)
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    let v3 = eval env e3 in
    (match (v1,v2,v3) with
    | (Const_Matrix_int (r, c, m), Const_Int i1, Const_Int i2) -> if (i1>=0 && i1<r) && (i2>=0 && i2<c) then let res = (Operations.minor m i1 i2) in (env, Const_Matrix_int (r-1, c-1, res)) else raise (Failure "Index out of bounds in Minor")
    | (Const_Matrix_float (r, c, m), Const_Int i1, Const_Int i2) -> if (i1>=0 && i1<r) && (i2>=0 && i2<c) then let res = (Operations.minor m i1 i2) in (env, Const_Matrix_float (r-1, c-1, res)) else raise (Failure "Index out of bounds in Minor")
    | _ -> raise (Failure "Minor can be applied only on Matrices"))

  | Inverse e1 ->
    let v1 = eval env e1 in
    (match v1 with
    | Const_Matrix_int (r, c, m) -> let res = Operations.float_matrix_to_exp_matrix (Operations.inverse (Operations.exp_matrix_to_float_matrix m)) in (env, Const_Matrix_float (r, c, res))
    | Const_Matrix_float (r, c, m) -> let res = Operations.float_matrix_to_exp_matrix (Operations.inverse (Operations.exp_matrix_to_float_matrix m)) in (env, Const_Matrix_float (r, c, res))
    | _ -> raise (Failure "Inverse can be applied only on Matrices"))

  | Input ->
    let input = read_line () in
    let lexbuf = Lexing.from_string input in
    let ast = Parser.top Calc.tokenize lexbuf in
    let result = eval env ast in
    (env, result)

  | Inputf(Filename (s)) -> (*s is a string*)
    let chan = open_in s in
    let lexbuf = Lexing.from_channel chan in
    let ast = Parser.top Calc.tokenize lexbuf in
    let result = eval env ast in
    (env, result)
  
  | Inputf (_) -> raise (Failure "Not a valid input format")
  | Filename (_) -> (env, Const_Bool true)
  | AccessMatrix(s, e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match (v1,v2) with
    | Const_Int r, Const_Int c ->
      (match Environment.lookup env s with
      | Some (VMatrixi (rows, cols, m)) when r >= 0 && r < rows && c >= 0 && c < cols ->
        (env, Const_Int (List.nth (List.nth m r) c))
      | Some (VMatrixf (rows, cols, m)) when r >= 0 && r < rows && c >= 0 && c < cols ->
        (env, Const_Float (List.nth (List.nth m r) c))
      | _ -> raise (Failure "Invalid access or out of bounds"))
    | _, _ -> raise (Failure "Only Intigers allowed in access"))

  | AccessVector(s, e) -> 
    let v = eval env e in
    (match v with
    | Const_Int i ->
      (match Environment.lookup env s with
      | Some (VVectori (dim, vec)) when i >= 0 && i < dim ->
        (env, Const_Int (List.nth vec i))
      | Some (VVectorf (dim, vec)) when i >= 0 && i < dim ->
        (env, Const_Float (List.nth vec i))
      | _ -> raise (Failure "Invalid access or out of bounds"))
    | _ -> raise (Failure "Only integers allowed in access"))
  
and eval env ast =
  let (_, result) = eval_with_env env ast in
  result
