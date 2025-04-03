open Ast
open Environment

exception DimensionError;;
exception DivisionBy0;;
exception NonInvertible;;

let rec exp_list_to_int_list exps =
  match exps with
  | [] -> []
  | Const_Int n :: rest -> n :: (exp_list_to_int_list rest)
  | _ -> raise (Failure "Expected list of Const_Int values")

let rec exp_list_to_float_list exps =
  match exps with
  | [] -> []
  | Const_Float f :: rest -> f :: (exp_list_to_float_list rest)
  | Const_Int i :: rest -> float_of_int i :: (exp_list_to_float_list rest)
  | _ -> raise (Failure "Expected list of Const_Float values")


let rec float_list_to_exp_list l =
  match l with
  | [] -> []
  | f :: rest -> Const_Float f :: (float_list_to_exp_list rest)

let rec int_list_to_exp_list l =
  match l with
  | [] -> []
  | i :: rest -> Const_Int i :: (int_list_to_exp_list rest)

let rec exp_matrix_to_int_matrix exps =
  match exps with
  | [] -> []
  | row :: rest -> (exp_list_to_int_list row) :: (exp_matrix_to_int_matrix rest)

let rec exp_matrix_to_float_matrix exps =
  match exps with
  | [] -> []
  | row :: rest -> (exp_list_to_float_list row) :: (exp_matrix_to_float_matrix rest)

let rec float_matrix_to_exp_matrix m =
  match m with
  | [] -> []
  | row :: rest -> (float_list_to_exp_list row) :: (float_matrix_to_exp_matrix rest)

let rec int_matrix_to_exp_matrix m =
  match m with
  | [] -> []
  | row :: rest -> (List.map (fun x -> Const_Int x) row) :: (int_matrix_to_exp_matrix rest)


let rec dot_product_int list1 list2 = 
  match list1, list2 with
  | [], [] -> 0
  | x::xs, y::ys -> (x * y) + dot_product_int xs ys
  | _, _ -> raise DimensionError
;;

let rec dot_product_float list1 list2 = 
  match list1, list2 with
  | [], [] -> 0.0
  | x::xs, y::ys -> (x *. y) +. dot_product_float xs ys
  | _, _ -> raise DimensionError
;;

let length list = Float.sqrt (dot_product_float list list);;

let angle v1 v2 =  (*angle in rads*)
  if (length v1 = 0. || length v2 = 0.) then 
    raise DivisionBy0
  else
    let cos_angle = (dot_product_float v1 v2) /. (length v1 *. length v2) in
    let cos_angle = 
      if cos_angle < -1.0 then -1.0
      else if cos_angle > 1.0 then 1.0
      else cos_angle
    in
    Float.acos cos_angle
;;

let magnitude v = length v;;

let transpose m = 
  let cols = List.length (List.hd m) in
  let rec transpose_helper m acc =
    if List.length acc = cols then List.rev acc
    else
      let new_row = List.map (fun row -> List.hd row) m in (*Pick all first elements of each row, and make a new list*)
      let new_m = List.map (fun row -> List.tl row) m in (*Pick all elements of list except first one to create new matrix*)
      transpose_helper new_m (new_row :: acc)
  in
  transpose_helper m []

let matrix_multiply_int m1 m2 =
  if m1 = [] || m2 = [] then raise DimensionError
  else
    let m2_t = transpose m2 in
    let rec multiply_helper m1 acc =
      match m1 with
      | [] -> List.rev acc
      | row1 :: rest1 ->
        let new_row = List.map (fun col -> dot_product_int row1 col) m2_t in
        multiply_helper rest1 (new_row :: acc)
    in
    multiply_helper m1 []

let matrix_multiply_float m1 m2 =
  if m1 = [] || m2 = [] then raise DimensionError
  else
    let m2_t = transpose m2 in
    let rec multiply_helper m1 acc =
      match m1 with
      | [] -> List.rev acc
      | row1 :: rest1 ->
        let new_row = List.map (fun col -> dot_product_float row1 col) m2_t in
        multiply_helper rest1 (new_row :: acc)
    in
    multiply_helper m1 []
  

let removing_element_from_row row j =
  let rec remove_helper row acc i =
    match row with
    | [] -> List.rev acc
    | x::rest -> if i = j then remove_helper rest acc (i + 1) (*skip this element*)
    else remove_helper rest (x :: acc) (i + 1)
  in
  remove_helper row [] 0

let minor m i j =
  let rec remove_row_and_col m i j acc row_num =
    match m with
    | [] -> List.rev acc
    | row::rest ->
      if row_num = i then remove_row_and_col rest i j acc (row_num + 1) (*that row not added to new matrix*)
      else
        let new_row = removing_element_from_row row j in
        remove_row_and_col rest i j (new_row :: acc) (row_num + 1)
  in
  remove_row_and_col m i j [] 0

let rec determinant_int m =
  if List.length m = 1 then
    List.hd (List.hd m)
  else
    let rec det m acc i j =
      if List.length m = 0 then raise DimensionError
      else if List.length (List.hd m) = j then acc
      else
        let sign = if (i + j) mod 2 = 0 then 1 else -1 in
        let element = List.nth (List.hd m) j in
        let minor_m = minor m i j in
        let minor_det = determinant_int minor_m in
        let new_acc = acc + sign * element * minor_det in
        det m new_acc i (j + 1)
    in
    det m 0 0 0

let rec determinant_float m =
  if List.length m = 1 then
    List.hd (List.hd m)
  else
    let rec det m acc i j =
      if List.length m = 0 then raise DimensionError
      else if List.length (List.hd m) = j then acc
      else
        let sign = if (i + j) mod 2 = 0 then 1. else -1. in
        let element = List.nth (List.hd m) j in
        let minor_m = minor m i j in
        let minor_det = determinant_float minor_m in
        let new_acc = acc +. sign *. element *. minor_det in
        det m new_acc i (j + 1)
    in
    det m 0.0 0 0

let list_of_cofactors m i =
  let rec row_cofactors m acci j = 
    if j = List.length (List.hd m) then List.rev acci
    else 
      let sign = if (i + j) mod 2 = 0 then 1.0 else -1.0 in
      let cofactor = sign *. determinant_float (minor m i j) in
      row_cofactors m (cofactor :: acci) (j+1)
  in
    row_cofactors m [] 0

let cofactor_matrix m =
  let rec build_co m acc i =
    if i = List.length m then List.rev acc
    else
      let new_row = list_of_cofactors m i in
      build_co m (new_row :: acc) (i + 1)
  in
  build_co m [] 0
  

let inverse m = 
  let det = determinant_float m in
    if det = 0.0 then
      raise NonInvertible
    else
      let adj = transpose (cofactor_matrix m) in 
      let scale = 1.0 /. det in 
      List.map (fun row -> List.map (fun x -> scale *. x) row ) adj


let update_matrix matrix i j value =
  match matrix with
  | Const_Matrix_int (r, c, rows) ->
      let updated_rows = 
        List.mapi (fun row_idx row ->
          if row_idx = i then
            List.mapi (fun col_idx elem ->
              if col_idx = j then value else elem
            ) row
          else row
        ) rows
      in
      Const_Matrix_int (r, c, updated_rows)
  | Const_Matrix_float (r, c, rows) ->
      let updated_rows = 
        List.mapi (fun row_idx row ->
          if row_idx = i then
            List.mapi (fun col_idx elem ->
              if col_idx = j then value else elem
            ) row
          else row
        ) rows
      in
      Const_Matrix_float (r, c, updated_rows)
  | _ -> raise (Failure "Invalid matrix type in update_matrix")

let update_vector vector i value =
  match vector with
  | Const_Vector_int (r, v) ->
      let updated_vector = 
        List.mapi (fun idx elem ->
          if idx = i then value else elem
        ) v
      in
      Const_Vector_int (r, updated_vector)
  | Const_Vector_float (r, v) ->
      let updated_vector = 
        List.mapi (fun idx elem ->
          if idx = i then value else elem
        ) v
      in
      Const_Vector_float (r, updated_vector)
  | _ -> raise (Failure "Invalid vector type in update_vector")

let update_for_env m =
  match m with
  | Const_Matrix_int (r, c, m) ->
      let matrix_values = List.map (fun row -> 
        List.map (fun x -> match x with
          | Const_Int i -> i
          | _ -> failwith "Type error: Expected only int values in matrix"
        ) row
      ) m in
    VMatrixi (r, c, matrix_values)
  | Const_Matrix_float (r, c, m) ->
      let matrix_values = List.map (fun row -> 
        List.map (fun x -> match x with
          | Const_Float f -> f
          | _ -> failwith "Type error: Expected only float values in matrix"
        ) row
      ) m in
    VMatrixf (r, c, matrix_values)
  | Const_Vector_int (r, v) ->
      let vector_values = List.map (fun x -> match x with
        | Const_Int i -> i
        | _ -> failwith "Type error: Expected only int values in vector"
      ) v in
    VVectori (r, vector_values)
  | Const_Vector_float (r, v) ->
      let vector_values = List.map (fun x -> match x with
        | Const_Float f -> f
        | _ -> failwith "Type error: Expected only float values in vector"
      ) v in
    VVectorf (r, vector_values)
  | _ -> raise (Failure "Invalid matrix/vector type in update_for_env")