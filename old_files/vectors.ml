(* Code *)

exception DimensionError;;
exception DivisionBy0;;

type vector = float list

module Vector = struct
  let rec create n x : vector = match n with
      1 -> [x]
    | _ -> 
        if n>0 then 
          x :: create (n-1) x
        else 
          raise DimensionError


  let dim (v: vector) : int = 
    if v = [] then
      raise DimensionError
    else
      let rec dim1 v=
        match v with
          [] -> 0
        | x::xs -> 1 + dim1 xs
      in dim1 v

  let ebsilon = 1e-6
  let negebsilon = -1e-6
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


  let unit n j : vector= 
    if j>n || n<1 || j<1 then
      raise DimensionError
    else
      let rec unit1 n j=
        match n with
          1 -> 
            if j=1 then 
              [1.0]
            else
              [0.0]
        | _ -> 
            if n=j then 
              unit1 (n-1) j @ [1.0] (* why bracket necessary in n-1?? *)
            else 
              unit1 (n-1) j @ [0.0] 
      in
      unit1 n j


  let rec scale c (v:vector) : vector = match v with
    | [] -> raise DimensionError
    | [x] -> [c *. x]
    | x :: xs -> (c *. x) :: scale c xs


  let rec addv (v1: vector)  (v2: vector) : vector = 
    match v1, v2 with
      [], _::_ -> raise DimensionError
    | _::_, [] -> raise DimensionError
    | [], [] -> raise DimensionError
    | [x], [y] -> [x +. y]
    | x::xs, y::ys -> (x +. y) :: addv xs ys


  let rec dot_prod (v1:vector) (v2: vector) : float = 
    match v1, v2 with
      [], _::_ -> raise DimensionError
    | _::_, [] -> raise DimensionError
    | [], [] -> raise DimensionError
    | [x], [y] -> x *. y
    | x::xs, y::ys -> (x *. y) +. dot_prod xs ys

  let length (v: vector) : float = 
    Float.sqrt (dot_prod v v)

  let inv (v:vector) : vector = scale (-1.) v



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

end;;