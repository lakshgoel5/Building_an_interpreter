type types =  Bool    (* boolean *)
  | Scalar   (* a scalar — any float value *)
  | Vector of int   (* n-dimensional with elements of type float*)
;; 

type vector = float list 
type values = B of bool |  S of float | V of vector 

type expr =  
    T | F   (* Boolean constants *)
  | ConstS of float    (* Scalar constants *)
  | ConstV of float list    (* Vector constants *)
  | Add of expr * expr   (* overloaded — disjunction of two booleans or sum of  two scalars or sum of two vectors of the same dimension *)
  | Inv of expr     (* overloaded — negation of a boolean or additive inverse of  a scalar or additive inverse of a vector *)
  | ScalProd of expr * expr   (* overloaded — conjunction of two booleans or product of a scalar with another scalar or product of a scalar and a vector *)
  | DotProd of expr * expr  (* dot product of two vectors of the same dimension *)
  | Mag of expr   (* overloaded: absolute value of a scalar or magnitude of a vector *)
  | Angle of expr * expr  (* in radians, the angle between two vectors *)
  | IsZero of expr (* overloaded: checks if a boolean expression evaluates to F,  or if a given scalar is within epsilon of 0.0 or is the vector close — within epsilon on each coordinate —  to the zero vector *)
  | Cond of expr * expr * expr  (* "if_then_else" --  if the first expr evaluates to T then evaluate the second expr, else the third expr *)
;; 

exception Wrong of expr;;
exception DimensionError;;
exception DivisionBy0;;

let size v =
  let rec aux v acc = match v with
    | [] -> acc  (* Base case: return accumulated count *)
    | _::xs -> aux xs (acc + 1)  (* Increment accumulator *)
  in
  aux v 0  (* Start recursion with accumulator 0 *);;

let ebsilon = 1e-6;;
let negebsilon = -1e-6 

let rec addv (v1) (v2) = 
  match v1, v2 with
    [], [] -> raise DimensionError
  | [x], [y] -> [x +. y]
  | x::xs, y::ys -> (x +. y) :: addv xs ys
  | _, _ -> raise DimensionError
;;

let rec scale c (v:vector) : vector = match v with
  | [] -> raise DimensionError
  | [x] -> [c *. x]
  | x :: xs -> (c *. x) :: scale c xs
;;

let rec dot_prod (v1:vector) (v2: vector) : float = 
  match v1, v2 with
    [], _::_ -> raise DimensionError
  | _::_, [] -> raise DimensionError
  | [], [] -> raise DimensionError
  | [x], [y] -> x *. y
  | x::xs, y::ys -> (x *. y) +. dot_prod xs ys
;;

let length (v: vector) : float = 
  Float.sqrt (dot_prod v v)
;;

let angle (v1 : vector) (v2 : vector) : float = 
  if (length v1=0. || length v2=0.) then 
    raise DivisionBy0
  else
    let cos_angle = (dot_prod v1 v2) /. (length v1 *. length v2) in
    let cos_angle = 
      if cos_angle < -1.0 then -1.0
      else if cos_angle > 1.0 then 1.0
      else cos_angle
    in
    Float.acos cos_angle

let inv (v:vector) : vector = scale (-1.) v
    
let rec is_zero (v: vector) = match v with
    [x] -> 
      if x<ebsilon && x>negebsilon then
        true
      else
        false
  | x::xs -> 
      if x<ebsilon && x>negebsilon then
        true && is_zero xs
      else
        false
  | [] -> raise DimensionError
            
            





let rec type_of (e : expr) : types = 
  match e with
  | T -> Bool
  | F -> Bool
  | ConstS _ -> Scalar  (* Matches any float *)
  | ConstV v -> if ((size v)!=0) then (Vector (size v)) else raise (Wrong e)  (* Vector length determines its type *)
                                   
  | Add (e1, e2) -> 
      let t1 = type_of e1 in
      let t2 = type_of e2 in
      (match t1, t2 with
       | Bool, Bool -> Bool
       | Scalar, Scalar -> Scalar
       | Vector n1, Vector n2 -> if n1 = n2 && n1!=0 then Vector n1 else raise (Wrong e)
       | _ -> raise (Wrong e))
      
  | Inv e1 ->
      (match type_of e1 with
       | Bool -> Bool
       | Scalar -> Scalar
       | Vector n -> if n!=0 then Vector n
           else raise (Wrong e))

  | ScalProd (e1, e2) ->
      (match type_of e1, type_of e2 with
       | Bool, Bool -> Bool
       | Scalar, Scalar -> Scalar
       | Scalar, Vector n -> if n !=0 then Vector n else raise (Wrong e)
       | Vector n, Scalar -> if n !=0 then Vector n else raise (Wrong e)
       | _ -> raise (Wrong e))

  | DotProd (e1, e2) ->
      (match type_of e1, type_of e2 with
       | Vector n1, Vector n2 -> if n1 = n2 && n1!=0 then Scalar else raise (Wrong e)
       | _ -> raise (Wrong e))

  | Mag e1 ->
      (match type_of e1 with
       | Scalar -> Scalar
       | Vector n -> if n!=0 then Scalar else raise (Wrong e)
       | _ -> raise (Wrong e))

  | Angle (e1, e2) ->
      (match type_of e1, type_of e2 with
       | Vector n1, Vector n2 -> if n1 = n2 && n1!=0 then Scalar else raise (Wrong e)
       | _ -> raise (Wrong e))

  | IsZero e1 ->
      (match type_of e1 with
       | Bool -> Bool
       | Scalar -> Bool
       | Vector n -> if n!=0 then Bool else raise (Wrong e))

  | Cond (e1, e2, e3) ->
      (match type_of e1, type_of e2, type_of e3 with
       | Bool, Scalar, Scalar -> Scalar
       | Bool, Bool, Bool -> Bool
       | Bool, Vector n1, Vector n2 -> if n1 = n2 && n1!=0 then Vector n1 else raise (Wrong e)
       | _ -> raise (Wrong e) )
;;








let rec eval (e : expr) : values = match e with
    T -> B true
  | F -> B false
  | ConstS s -> S s
  | ConstV v -> V v 
                  
  | Add (e1, e2) ->
      (match eval e1, eval e2 with
       | B n1, B n2 -> B (n1 || n2)
       | S s1, S s2 -> S (s1 +. s2)
       | V v1, V v2 -> 
           if size v1 = size v2 && size v1 !=0 then V (addv v1 v2)
           else raise (Wrong e)
       | _ -> raise (Wrong e))
          
  | Inv (e1) ->
      (match eval e1 with
       | B n1 -> B (not n1)
       | S s1 -> S (-1. *. s1)
       | V v1 -> if (size v1) !=0 then V (inv v1) else raise (Wrong (e))
      )
      
  | ScalProd (e1, e2) ->
      (match eval e1, eval e2 with
       | B n1, B n2 -> B (n1 && n2)
       | S s1, S s2 -> S (s1 *. s2)
       | S s1, V v2 -> 
           if ((size v2) !=0) then V (scale s1 v2)
           else raise (Wrong e)
       | V v1, S s2 -> 
           if ((size v1) !=0) then V (scale s2 v1)
           else raise (Wrong e)
       | _ -> raise (Wrong e)
      )
      
  | DotProd (e1, e2) ->
      (match eval e1, eval e2 with
       | V v1, V v2 -> 
           if size v1 = size v2 && size v1 !=0 then S (dot_prod v1 v2)
           else raise (Wrong e)
       | _ -> raise (Wrong e)
      )
      
  | Mag e1 ->
      (match eval e1 with
       | S s1 -> 
           if s1 < 0. then S (-1. *. s1)
           else S s1
       | V v1 -> if (size v1)!=0 then S (length v1) else raise (Wrong(Mag e1))
       | _ -> raise (Wrong e)
      )
      
  | Angle (e1, e2) ->
      (match eval e1, eval e2 with
       | V v1, V v2 -> 
           if (size v1 = size v2) && (size v1 !=0) && (length v1 !=0.) && (length v2!=0.) then S (angle v1 v2)
           else raise (Wrong e)
       | _ -> raise (Wrong e)
      )
      
  | IsZero e1 ->
      (match eval e1 with
       | B n1 -> B (n1 = false)
       | S s1 -> B (s1 < ebsilon && s1 > negebsilon)
       | V v1 -> B (is_zero v1)
      )
      
  | Cond (e1, e2, e3) ->
      (match eval e1, eval e2, eval e3 with
       | B n1, S s2, S s3 -> if n1 then S s2 else S s3
       | B n1, B n2, B n3 -> if n1 then B n2 else B n3
       | B n1, V v2, V v3 -> if n1 then
        if (size v3) = (size v2) && (size v3)!=0 then V v2 
        else raise (Wrong e)
      else 
        if (size v2) = (size v2) && (size v3)!=0 then V v3
        else raise (Wrong e)
       | _ -> raise (Wrong e)
      )
;;
