type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VVectori of int * int list
  | VVectorf of int * float list
  | VMatrixi of int * int * int list list
  | VMatrixf of int * int * float list list
;;

module StringMap = Map.Make(String)
type env = value StringMap.t

let lookup env name = StringMap.find_opt name env (*Can return None, else returns Some(...)*)

let extend env name value = StringMap.add name value env
  