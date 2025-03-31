open Ast
open Environment

(*env is gamma*)
(**)

let rec eval_with_env env ast =
  match ast with
  | Line (e1, e2) ->
    let (new_env, _) = eval_with_env env e1 in
    eval_with_env new_env e2

  | Const_Int i -> (env, Const_Int i)
  | Const_Float f -> (env, Const_Float f)
  | Const_Vector_int (d,v) -> (env, Const_Vector_int (d,v))
  | Const_Vector_float (d,v) -> (env, Const_Vector_float (d,v))
  | Const_Bool b -> (env, Const_Bool b)
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
  
  | Print(e) ->
    let v = eval env e in
    (match v with
     | Const_Int i -> Printf.printf "%d\n" i; (env, v)
     | Const_Float f -> Printf.printf "%f\n" f; (env, v)
     | Const_Bool b -> Printf.printf "%b\n" b; (env, v)
     | Const_Vector_int (d, l) -> 
         let str_values = String.concat ", " (List.map (fun x -> match x with Const_Int i -> string_of_int i | _ -> "") l) in
         Printf.printf "Vector of dimension %d: %s\n" d str_values; (env, v)
     | Const_Vector_float (d, l) -> 
         let str_values = String.concat ", " (List.map (fun x -> match x with Const_Float f -> string_of_float f | _ -> "") l) in
         Printf.printf "Vector of dimension %d: %s\n" d str_values; (env, v)
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
    | (Const_Int i1, Const_Int i2) when i2 <> 0 -> (env, Const_Int (i1 / i2))
    | (Const_Float f1, Const_Float f2) when f2 <> 0.0 -> (env, Const_Float (f1 /. f2))
    | (Const_Vector_int (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int f1, Const_Int f2) when f2 <> 0 -> Const_Int (f1 / f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_int (d, res))
    | (Const_Vector_float (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Float f2) when f2 <> 0.0 -> Const_Float (f1 /. f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Vector_int (d, s1), Const_Vector_float (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Int i1, Const_Float f2) when f2 <> 0.0 -> Const_Float ((float_of_int i1) /. f2)
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Vector_float (d, s1), Const_Vector_int (d1, s2)) when d = d1 ->
      let res = List.map2 (fun x y -> match (x, y) with
        | (Const_Float f1, Const_Int i2) when i2 <> 0 -> Const_Float (f1 /. (float_of_int i2))
        | _ -> raise (Failure "Invalid vector element")) s1 s2 in
      (env, Const_Vector_float (d, res))
    | (Const_Matrix_int (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Int i2) when i2 <> 0 -> Const_Int (i1 / i2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_int (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Float f2) when f2 <> 0.0 -> Const_Float (f1 /. f2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_int (r, c, m1), Const_Matrix_float (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Int i1, Const_Float f2) when f2 <> 0.0 -> Const_Float ((float_of_int i1) /. f2)
        | _ -> raise (Failure "Invalid matrix element")) row1 row2) m1 m2 in
      (env, Const_Matrix_float (r, c, res))
    | (Const_Matrix_float (r, c, m1), Const_Matrix_int (r1, c1, m2)) when r = r1 && c = c1 ->
      let res = List.map2 (fun row1 row2 -> List.map2 (fun x y -> match (x,y) with
        | (Const_Float f1, Const_Int i2) when i2 <> 0 -> Const_Float (f1 /. (float_of_int i2))
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
    
  | _ -> raise (Failure "Unsupported operation")

and eval env ast =
  let (_, result) = eval_with_env env ast in
  result
