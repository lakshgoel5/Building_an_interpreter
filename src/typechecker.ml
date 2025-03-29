(*Vector operations on same dimension*)
(*Acted on AST's builded on some (well-formed) input*)
(*need a symbol table for variables to match their type and you should also propagate types.*)
(*For multiplication and addition, you should check the dimensions if they can be multiplied or added. If they are compatible, you need not check anything wrt the variable being assigned the result to - this is because you should assume the declaration to be of the form mat E := B+C - so you infer the dimension of E according to B+C*)
(*we can use maps and hashtables inbuilt in Ocaml for the symbol tables*)

open Ast

type data_type = TInt | TFloat | TBool | TVectorf of int | TVectori of int | TMatrixf of int * int | TMatrixi of int * int | TStatement | TCreate | TInput

module SymbolTable = Map.Make(String)

(*Map.Make expects a module with a type t and a comparison function called compare.
The String module satisfies this requirement — it defines a type t = string and a compare function for comparing strings.*)

(*
SymbolTable is now a module that behaves like a dictionary (or hash table) where:
Keys are strings (due to Map.Make(String)).
Values can be of any type you specify when using SymbolTable.
*)

type type_error =
  TypeMismatch of string * data_type * data_type
  | UndefinedVariable of string
  | IncompatibleDimensions of string * data_type * data_type
  | InvalidOperation of string
	| InvalidZeroDimension
  | OtherError of string

exception TypeError of type_error


let rec type_check (exp : Ast.exp) (symbol_table : data_type SymbolTable.t) = match exp with
	| Const_Int _ -> TInt
	| Const_Float _ -> TFloat
	| Const_Bool _ -> TBool
	| Const_Matrix_int (row,col,_) -> TMatrixi(row, col)
	| Const_Matrix_float (row,col,_) -> TMatrixf(row, col)
	| Const_Vector_int (dim,_) -> TVectori(dim)
	| Const_Vector_float (dim,_) -> TVectorf(dim)
	| Variable var -> begin match SymbolTable.find_opt var symbol_table with
		| Some t -> t
		| None -> raise (TypeError(UndefinedVariable var))
	end

	| Int _ -> TInt
	| Float _ -> TFloat
	| Bool _ -> TBool
	| Matrixf (row,col,_) -> TMatrixf(row, col)
	| Matrixi (row,col,_) -> TMatrixi(row, col)
	| Vectorf (dim,_) -> TVectorf(dim)
	| Vectori (dim,_) -> TVectori(dim)
  | Filename _ -> TStatement

	| Inputf (Variable var) -> begin match SymbolTable.find_opt var symbol_table with
		| Some t -> t
		| None -> raise (TypeError(UndefinedVariable var))
	end
  | Inputf (Filename var) -> TInput
  | Input -> TInput
	| Inputf _ -> raise (TypeError(InvalidOperation "Input can only be assigned to a variable"))

	| Print (Variable var) -> begin match SymbolTable.find_opt var symbol_table with
    | Some t -> t
    | None -> raise (TypeError(UndefinedVariable var))
	end
	| Print _ -> raise (TypeError(InvalidOperation "Print can only be called on a variable"))

  | Assign (Int var, e2) ->
		let t2 = type_check e2 symbol_table in
		if (t2 <> TInt && t2 <> TInput) then
			raise (TypeError (TypeMismatch ("assignment to int", TInt, t2)))
		else TInt

	| Assign (Float var, e2) ->
		let t2 = type_check e2 symbol_table in
		if (t2 <> TFloat && t2 <> TInput) then
			raise (TypeError (TypeMismatch ("assignment to float", TFloat, t2)))
		else TFloat

	| Assign (Bool var, e2) ->
		let t2 = type_check e2 symbol_table in
		if (t2 <> TBool && t2 <> TInput) then
			raise (TypeError (TypeMismatch ("assignment to bool", TBool, t2)))
		else TBool

	| Assign (Vectori (dim,var), e2) ->
		let t2 = type_check e2 symbol_table in
		begin match t2 with
		| TInput -> TVectori dim
		| TVectori d -> if(d = dim) then TVectori dim else raise (TypeError (IncompatibleDimensions ("vector assignment", TVectori dim, TVectori d)))
		| _ -> raise (TypeError (TypeMismatch ("assignment to vector", TVectori dim, t2)))
		end

	| Assign (Vectorf (dim,var), e2) ->
		let t2 = type_check e2 symbol_table in
		begin match t2 with
		| TInput -> TVectorf dim
		| TVectorf d -> if(d = dim) then TVectorf dim else raise (TypeError (IncompatibleDimensions ("vector assignment", TVectorf dim, TVectorf d)))
		| _ -> raise (TypeError (TypeMismatch ("assignment to vector", TVectorf dim, t2)))
		end

	| Assign (Matrixi (row,col,var), e2) ->
		let t2 = type_check e2 symbol_table in
		begin match t2 with
		| TInput -> TMatrixi (row, col)
		| TCreate -> TMatrixi (row, col)
		| TMatrixi (r, c) -> 
			if r = row then 
				if c = col then 
					TMatrixi (row, col)
				else raise (TypeError (IncompatibleDimensions ("Matrixi assignment", TMatrixi (row,col), TMatrixi(r,c))))
			else raise (TypeError (IncompatibleDimensions ("Matrixi assignment", TMatrixi (row,col), TMatrixi(r,c))))
		| _ -> raise (TypeError (TypeMismatch ("assignment to Matrixi", TMatrixi (row, col), t2)))
		end

	| Assign (Matrixf (row,col,var), e2) ->
		let t2 = type_check e2 symbol_table in
		begin match t2 with
		| TInput -> TMatrixf (row, col)
		| TCreate -> TMatrixf (row, col)
		| TMatrixf (r, c) -> 
			if r = row then 
				if c = col then 
					TMatrixf (row, col)
				else raise (TypeError (IncompatibleDimensions ("Matrixf assignment", TMatrixf (row,col), TMatrixf(r,c))))
			else raise (TypeError (IncompatibleDimensions ("Matrixf assignment", TMatrixf (row,col), TMatrixf(r,c))))
		| _ -> raise (TypeError (TypeMismatch ("assignment to Matrixf", TMatrixf (row, col), t2)))
		end

  | Assign (AccessVector (var, idx), e2) ->
    let idx_t = type_check idx symbol_table in
    let t2 = type_check e2 symbol_table in
    begin match SymbolTable.find_opt var symbol_table with
    | Some t1 -> 
        begin match (t1, idx_t, t2) with
        | (TVectori d, TInt, TInt) -> 
            if d > 0 then TInt
            else raise (TypeError (InvalidZeroDimension))
        | (TVectorf d, TInt, TFloat) ->
            if d > 0 then TFloat
            else raise (TypeError (InvalidZeroDimension))
				| (TVectori d, TInt, TInput) ->
						if d > 0 then TInt
						else raise (TypeError (InvalidZeroDimension))
				| (TVectorf d, TInt, TInput) ->
						if d > 0 then TFloat
						else raise (TypeError (InvalidZeroDimension))
        | (_, _, _) -> raise (TypeError (TypeMismatch ("vector element assignment", 
                              (if t1 = TVectori 0 then TInt else TFloat), t2)))
        end
    | None -> raise (TypeError (UndefinedVariable var))
    end
	
	| Assign (AccessMatrix (var, row_idx, col_idx), e2) ->
		let row_t = type_check row_idx symbol_table in
		let col_t = type_check col_idx symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match SymbolTable.find_opt var symbol_table with
		| Some t1 -> 
				begin match (t1, row_t, col_t, t2) with
				| (TMatrixi (r, c), TInt, TInt, TInt) ->
						if r > 0 && c > 0 then TInt
						else raise (TypeError (InvalidZeroDimension))
				| (TMatrixf (r, c), TInt, TInt, TFloat) ->
						if r > 0 && c > 0 then TFloat
						else raise (TypeError (InvalidZeroDimension))
				| (TMatrixi (r, c), TInt, TInt, TInput) ->
						if r > 0 && c > 0 then TInt
						else raise (TypeError (InvalidZeroDimension))
				| (TMatrixf (r, c), TInt, TInt, TInput) ->
						if r > 0 && c > 0 then TFloat
						else raise (TypeError (InvalidZeroDimension))
				| (_, _, _, _) -> raise (TypeError (TypeMismatch ("matrix element assignment", 
																(if t1 = TMatrixi (0, 0) then TInt else TFloat), t2)))
				end
		| None -> raise (TypeError (UndefinedVariable var))
		end

	| Assign (Variable var, e2) ->
		let t2 = type_check e2 symbol_table in
		begin match SymbolTable.find_opt var symbol_table with
		| Some t1 -> 
			begin match (t1, t2) with
			| (d, TInput) -> d
			| (TMatrixf (r1, c1), TCreate) -> TMatrixf (r1, c1)
			| (TMatrixi (r1, c1), TCreate) -> TMatrixi (r1, c1)
			| (TInt, TInt) -> TInt
			| (TFloat, TFloat) -> TFloat
			| (TBool, TBool) -> TBool
			| (TVectori dim1, TVectori dim2) ->
				if dim1 = dim2 then
					if dim1 <> 0 then TVectori dim2
					else raise (TypeError (IncompatibleDimensions ("Vectori assignment", TVectori dim1,TVectori dim2)))
				else raise (TypeError (InvalidZeroDimension))
			| (TVectorf dim1, TVectorf dim2) ->
				if dim1 = dim2 then
					if dim1 <> 0 then TVectorf dim2
					else raise (TypeError (IncompatibleDimensions ("Vectorf assignment", TVectorf dim1,TVectorf dim2)))
				else raise (TypeError (InvalidZeroDimension))
			| (TMatrixf (r1, c1), TMatrixf (r2, c2)) ->
				if (r1 = r2 && c1 = c2) then 
					if (r1 <> 0 && c1 <> 0) then TMatrixf (r2, c2)
					else raise (TypeError (InvalidZeroDimension))
				else raise (TypeError (IncompatibleDimensions ("Matrixf assignment", TMatrixf (r1 ,c1), TMatrixf (r2 , c2))))
			| (TMatrixi (r1, c1), TMatrixi (r2, c2)) ->
				if (r1 = r2 && c1 = c2) then 
					if (r1 <> 0 && c1 <> 0) then TMatrixi (r2, c2)
					else raise (TypeError (InvalidZeroDimension))
				else raise (TypeError (IncompatibleDimensions ("Matrixi assignment", TMatrixi (r1 ,c1), TMatrixi (r2 , c2))))
			| _ -> raise (TypeError (TypeMismatch ("assignment", t1, t2)))
			end
		| None -> raise (TypeError (UndefinedVariable var))
		end

	| Assign ( _, _) -> raise (TypeError(OtherError("Invalid Assignment")))
	
	| Add (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) -> TInt
		| (TFloat, TFloat) -> TFloat
		| (TVectori d1, TVectori d2) -> if d1 = d2 then TVectori d1 else raise (TypeError (IncompatibleDimensions ("addition", TVectori d1, TVectori d2)))
		| (TVectorf d1, TVectorf d2) -> if d1 = d2 then TVectorf d1 else raise (TypeError (IncompatibleDimensions ("addition", TVectorf d1, TVectorf d2)))
		| (TMatrixf (r1, c1), TMatrixf (r2, c2)) -> if r1 = r2 && c1 = c2 then TMatrixf (r1, c1) else raise (TypeError (IncompatibleDimensions ("addition", TMatrixf(r1, c1), TMatrixf(r2, c2))))
		| (TMatrixi (r1, c1), TMatrixi (r2, c2)) -> if r1 = r2 && c1 = c2 then TMatrixi (r1, c1) else raise (TypeError (IncompatibleDimensions ("addition", TMatrixi(r1, c1), TMatrixi(r2, c2))))
		| _ -> raise (TypeError (TypeMismatch ("addition", t1, t2)))
		end
	
	| Sub (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) -> TInt
		| (TFloat, TFloat) -> TFloat
		| (TVectorf d1, TVectorf d2) -> if d1 = d2 then TVectorf d1 else raise (TypeError (IncompatibleDimensions ("subtraction", TVectorf d1, TVectorf d2)))
		| (TVectori d1, TVectori d2) -> if d1 = d2 then TVectori d1 else raise (TypeError (IncompatibleDimensions ("subtraction", TVectori d1, TVectori d2)))
		| (TMatrixf (r1, c1), TMatrixf (r2, c2)) -> if r1 = r2 && c1 = c2 then TMatrixf (r1, c1) else raise (TypeError (IncompatibleDimensions ("subtraction", TMatrixf(r1, c1), TMatrixf(r2, c2))))
		| (TMatrixi (r1, c1), TMatrixi (r2, c2)) -> if r1 = r2 && c1 = c2 then TMatrixi (r1, c1) else raise (TypeError (IncompatibleDimensions ("subtraction", TMatrixi(r1, c1), TMatrixi(r2, c2))))
		| _ -> raise (TypeError (TypeMismatch ("subtraction", t1, t2)))
		end
	
	| Multiply (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) -> TInt
		| (TFloat, TFloat) -> TFloat
		| (TInt, TFloat) -> TFloat
		| (TFloat, TInt) -> TFloat

		| (TInt, TVectori d1) -> TVectori d1
		| (TVectori d1, TInt) -> TVectori d1
		| (TFloat, TVectori d1) -> TVectori d1
		| (TVectori d1, TFloat) -> TVectori d1

		| (TInt, TVectorf d1) -> TVectorf d1
		| (TVectorf d1, TInt) -> TVectorf d1
		| (TFloat, TVectorf d1) -> TVectorf d1
		| (TVectorf d1, TFloat) -> TVectorf d1

		| (TInt, TMatrixi (r,c)) -> TMatrixi (r,c)
		| (TMatrixi (r,c), TInt) -> TMatrixi (r,c)
		| (TFloat, TMatrixi (r,c)) -> TMatrixi (r,c)
		| (TMatrixi (r,c), TFloat) -> TMatrixi (r,c)

		| (TInt, TMatrixf (r,c)) -> TMatrixf (r,c)
		| (TMatrixf (r,c), TInt) -> TMatrixf (r,c)
		| (TFloat, TMatrixf (r,c)) -> TMatrixf (r,c)
		| (TMatrixf (r,c), TFloat) -> TMatrixf (r,c)

		| (TVectori d1, TVectori d2) -> if d1 = d2 then TVectori d1 else raise (TypeError (IncompatibleDimensions ("multiplication", TVectori d1, TVectori d2)))
		| (TVectorf d1, TVectorf d2) -> if d1 = d2 then TVectorf d1 else raise (TypeError (IncompatibleDimensions ("multiplication", TVectorf d1, TVectorf d2)))

		| (TMatrixi (r1, c1), TMatrixi (r2, c2)) -> if c1 = r2 then TMatrixi (r1, c2) else raise (TypeError (IncompatibleDimensions ("multiplication", TMatrixi(r1,c1), TMatrixi(r2,c2))))
		| (TMatrixf (r1, c1), TMatrixf (r2, c2)) -> if c1 = r2 then TMatrixf (r1, c2) else raise (TypeError (IncompatibleDimensions ("multiplication", TMatrixf(r1,c1), TMatrixf(r2,c2))))
		| (TMatrixi (r1, c1), TMatrixf (r2, c2)) -> if c1 = r2 then TMatrixf (r1, c2) else raise (TypeError (IncompatibleDimensions ("multiplication", TMatrixf(r1,c1), TMatrixf(r2,c2))))
		| (TMatrixf (r1, c1), TMatrixi (r2, c2)) -> if c1 = r2 then TMatrixf (r1, c2) else raise (TypeError (IncompatibleDimensions ("multiplication", TMatrixf(r1,c1), TMatrixf(r2,c2))))

		| _ -> raise (TypeError (TypeMismatch ("multiplication", t1, t2)))
		end
	
	| Divide (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) -> TInt
		| (TFloat, TFloat) -> TFloat
		| (TInt, TFloat) -> TFloat
		| (TFloat, TInt) -> TFloat
		| _ -> raise (TypeError (TypeMismatch ("division", t1, t2)))
		end

	| Equal (e1, e2) | NotEqual (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) | (TFloat, TFloat) | (TBool, TBool) -> TBool
		| (TVectori d1, TVectori d2) -> if (d1 = d2 && d1 <> 0) then TBool else raise (TypeError (IncompatibleDimensions ("equality comparison", TVectori d1, TVectori d2)))
		| (TVectorf d1, TVectorf d2) -> if (d1 = d2 && d1 <> 0) then TBool else raise (TypeError (IncompatibleDimensions ("equality comparison", TVectorf d1, TVectorf d2)))
		| (TMatrixi (r1,c1), TMatrixi (r2,c2))-> if (((r1 = r2 && c1 = c2) && r1 <> 0) && c1 <> 0) then TBool else raise (TypeError (IncompatibleDimensions ("equality comparison", TMatrixi(r1, c1), TMatrixi(r2, c2))))
		| (TMatrixf (r1,c1), TMatrixf (r2,c2))-> if (((r1 = r2 && c1 = c2) && r1 <> 0) && c1 <> 0) then TBool else raise (TypeError (IncompatibleDimensions ("equality comparison", TMatrixf(r1, c1), TMatrixf(r2, c2))))
		| _ -> raise (TypeError (TypeMismatch ("equality comparison", t1, t2)))
		end
  
  | Less (e1, e2) | LessEq (e1, e2) | Greater (e1, e2) | GreaterEq (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) | (TFloat, TFloat) -> TBool
		| (TVectori d1, TVectori d2) -> if (d1 = d2 && d1 <> 0) then TBool else raise (TypeError (IncompatibleDimensions ("equality comparison", TVectori d1, TVectori d2)))
		| (TVectorf d1, TVectorf d2) -> if (d1 = d2 && d1 <> 0) then TBool else raise (TypeError (IncompatibleDimensions ("equality comparison", TVectorf d1, TVectorf d2)))
		| (TMatrixi (r1,c1), TMatrixi (r2,c2))-> if (((r1 = r2 && c1 = c2) && r1 <> 0) && c1 <> 0) then TBool else raise (TypeError (IncompatibleDimensions ("equality comparison", TMatrixi(r1, c1), TMatrixi(r2, c2))))
		| (TMatrixf (r1,c1), TMatrixf (r2,c2))-> if (((r1 = r2 && c1 = c2) && r1 <> 0) && c1 <> 0) then TBool else raise (TypeError (IncompatibleDimensions ("equality comparison", TMatrixf(r1, c1), TMatrixf(r2, c2))))
		(* | (TInt, TFloat) | (TFloat, TInt) -> TBool *)
		| _ -> raise (TypeError (TypeMismatch ("comparison", t1, t2)))
		end
  
  | Remainder (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) -> TInt
		| _ -> raise (TypeError (TypeMismatch ("remainder", t1, t2)))
		end
  
  | Conjunction (e1, e2) | Disjunction (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		if t1 <> TBool || t2 <> TBool then
			raise (TypeError (TypeMismatch ("logical operation", TBool, t2)))
		else TBool
  
  | Abs e ->
		let t = type_check e symbol_table in
		begin match t with
		| TInt -> TInt
		| TFloat -> TFloat
		| _ -> raise (TypeError (TypeMismatch ("abs", TInt, t)))
		end

	| Sqrt e ->
		let t = type_check e symbol_table in
		begin match t with
		| TInt -> TFloat
		| TFloat -> TFloat
		| _ -> raise (TypeError (TypeMismatch ("sqrt", TFloat, t)))
		end
	
	| Pow (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) -> TInt
		| (TFloat, TFloat) -> TFloat
		| (TFloat, TInt) -> TFloat
		| (TInt, TFloat) -> TFloat
		| _ -> raise (TypeError (TypeMismatch ("pow", t1, t2)))
		end

	| Log (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) -> TFloat
		| (TFloat, TFloat) -> TFloat
		| (TFloat, TInt) -> TFloat
		| (TInt, TFloat) -> TFloat
		| _ -> raise (TypeError (TypeMismatch ("log", TFloat, t1)))
		end
  
  | Mag e ->
		let t = type_check e symbol_table in
		begin match t with
		| TVectorf d -> if d <> 0 then TFloat  (* Magnitude of a vector is a float *)
		else raise (TypeError (InvalidZeroDimension))
		| TVectori d -> if d <> 0 then TFloat  (* Magnitude of a vector is a float *)
			else raise (TypeError (InvalidZeroDimension))
		| _ -> raise (TypeError (TypeMismatch ("magnitude", TVectorf 0, t)))
		end
  
  | Transpose e ->
		let t = type_check e symbol_table in
		begin match t with
		| TMatrixf (r, c) -> TMatrixf (c, r)  (* Transpose switches dimensions *)
		| TMatrixi (r, c) -> TMatrixi (c, r)  (* Transpose switches dimensions *)
		| _ -> raise (TypeError (TypeMismatch ("transpose", TMatrixf (0, 0), t)))
		end

	| CreateEmpty (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TInt, TInt) -> TCreate
		| (TInt, _) -> raise (TypeError (TypeMismatch ("create_empty", TCreate, t2)))
		| (_, TInt) -> raise (TypeError (TypeMismatch ("create_empty", TCreate, t1)))
		| (_,_) -> raise (TypeError (TypeMismatch ("create_empty", t1, t2)))
	end

	| Minor (a1, e1, e2) ->
		let t1 = type_check a1 symbol_table in
		let t2 = type_check e1 symbol_table in
		let t3 = type_check e2 symbol_table in
		begin match (t1, t2, t3) with
		| (TMatrixi (r, c), TInt, TInt) ->
				if r > 1 && c > 1 then TMatrixi (r - 1, c - 1)
				else raise (TypeError (InvalidOperation "Matrixi dimensions must be greater than 1 for minor operation"))
		| (TMatrixi _, _, _) -> raise (TypeError (TypeMismatch ("minor operation", TInt, t2)))

		| (TMatrixf (r, c), TInt, TInt) ->
			if r > 1 && c > 1 then TMatrixf (r - 1, c - 1)
			else raise (TypeError (InvalidOperation "Matrixf dimensions must be greater than 1 for minor operation"))
		| (TMatrixf _, _, _) -> raise (TypeError (TypeMismatch ("minor operation", TInt, t2)))

		| (_, _, _) -> raise (TypeError (TypeMismatch ("minor operation", TMatrixf (0, 0), t1)))
		end


  | Determinant e ->
		let t = type_check e symbol_table in
		begin match t with
		| TMatrixf (r, c) ->
				if r <> c then
					raise (TypeError (InvalidOperation "Determinant requires a square matrix"))
				else TFloat
		| TMatrixi (r, c) ->
			if r <> c then
				raise (TypeError (InvalidOperation "Determinant requires a square matrix"))
			else TFloat
		| _ -> raise (TypeError (TypeMismatch ("determinant", TMatrixf (0, 0), t)))
		end

	| Inverse e ->
		let t = type_check e symbol_table in
		begin match t with
		| TMatrixf (r, c) ->
				if r <> c then
					raise (TypeError (InvalidOperation "Inverse requires a square matrix"))
				else TMatrixf (r, c)
		| TMatrixi (r, c) ->
			if r <> c then
				raise (TypeError (InvalidOperation "Inverse requires a square matrix"))
			else TMatrixf (r, c)
		| _ -> raise (TypeError (TypeMismatch ("inverse", TMatrixf (0, 0), t)))
		end
  
  | Angle (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TVectorf dim1, TVectorf dim2) -> if (dim1 = dim2 && dim1 <> 0) then TFloat else raise (TypeError (IncompatibleDimensions ("angle", TVectorf dim1, TVectorf dim2)))
		| (TVectori dim1, TVectori dim2) -> if (dim1 = dim2 && dim1 <> 0) then TFloat else raise (TypeError (IncompatibleDimensions ("angle", TVectori dim1, TVectori dim2)))
		| _ -> raise (TypeError (TypeMismatch ("angle", t1, t2)))
		end
  
  | Dimension e ->
		let t = type_check e symbol_table in
		begin match t with
		| TVectori d -> if d <> 0 then TInt  (* Dimension of a vector is an integer *)
		else raise (TypeError (InvalidZeroDimension))
		| TVectorf d -> if d <> 0 then TInt  (* Dimension of a vector is an integer *)
			else raise (TypeError (InvalidZeroDimension))
		(* | TMatrix _ -> TInt  Could return a tuple or pair of integers *)
		| _ -> raise (TypeError (TypeMismatch ("dimension", TVectorf 0, t)))
		end

	| Row e ->
		let t = type_check e symbol_table in
		begin match t with
		| TMatrixi (r, _) -> if r <> 0 then TInt  (* Number of rows in a matrix is an integer *)
		else raise (TypeError (InvalidZeroDimension))
		| TMatrixf (r, _) -> if r <> 0 then TInt  (* Number of rows in a matrix is an integer *)
			else raise (TypeError (InvalidZeroDimension))
		| _ -> raise (TypeError (TypeMismatch ("row", TMatrixf (0, 0), t)))
		end

	| Cols e -> 
		let t = type_check e symbol_table in
		begin match t with
		| TMatrixi (_, c) -> if c <> 0 then TInt  (* Number of columns in a matrix is an integer *)
		else raise (TypeError (InvalidZeroDimension))
		| TMatrixf (_, c) -> if c <> 0 then TInt  (* Number of columns in a matrix is an integer *)
			else raise (TypeError (InvalidZeroDimension))
		| _ -> raise (TypeError (TypeMismatch ("cols", TMatrixf (0, 0), t)))
		end
  
  | DotProd (e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match (t1, t2) with
		| (TVectorf dim1, TVectorf dim2) -> if (dim1 = dim2 && dim1 <> 0) then TFloat else raise (TypeError (IncompatibleDimensions ("dotprod", TVectorf dim1, TVectorf dim2)))  (* Dot product of two vectors is a scalar (float) *)
		| (TVectori dim1, TVectori dim2) -> if (dim1 = dim2 && dim1 <> 0) then TFloat else raise (TypeError (IncompatibleDimensions ("dotprod", TVectori dim1, TVectori dim2)))  (* Dot product of two vectors is a scalar (float) *)
		| _ -> raise (TypeError (TypeMismatch ("dot product", t1, t2)))
		end
  
	| For (init, cond, incr, body) ->
		(* Check that init is an assignment expression *)
		(match init with
		| Assign (_, _) -> 
			let _ = type_check init symbol_table in
			(* Check condition is a boolean expression *)
			let t_cond = type_check cond symbol_table in
			if t_cond <> TBool then
				raise (TypeError (TypeMismatch ("for condition", TBool, t_cond)))
			else
				(* Check that incr is an assignment expression *)
				(match incr with
				| Assign (_, _) -> 
					let _ = type_check incr symbol_table in
					let _ = type_check body symbol_table in
					TStatement (* For loops return a sequence type *)
				| _ -> raise (TypeError (InvalidOperation "For loop increment must be an assignment")))
		| _ -> raise (TypeError (InvalidOperation "For loop initialization must be an assignment")))
	
	| While (cond, body) ->
		(* Check condition is a boolean expression *)
		let t_cond = type_check cond symbol_table in
		if t_cond <> TBool then
			raise (TypeError (TypeMismatch ("while condition", TBool, t_cond)))
		else
			let _ = type_check body symbol_table in
			TStatement (* While loops return a sequence type *)
	
	| If (cond, then_expr, else_expr) ->
		(* Check condition is a boolean expression *)
		let t_cond = type_check cond symbol_table in
		if t_cond <> TBool then
			raise (TypeError (TypeMismatch ("if condition", TBool, t_cond)))
		else
			let _ = type_check then_expr symbol_table in
			let _ = type_check else_expr symbol_table in
			TStatement (* If statement returns a sequence type *)

	| AccessVector (var, e) ->
		let t = type_check e symbol_table in
		begin match SymbolTable.find_opt var symbol_table with
		| Some t1 -> 
			begin match (t1, t) with
			| (TVectori d1, TInt) -> if d1 > 0 then TInt else raise (TypeError (InvalidZeroDimension))
			| (TVectorf d1, TInt) -> if d1 > 0 then TFloat else raise (TypeError (InvalidZeroDimension))
			| (TMatrixi (d1, d2), TInt) -> if (d1 > 0 && d2>0) then TVectori d2 else raise (TypeError (InvalidZeroDimension))
			| (TMatrixf (d1, d2), TInt) -> if (d1 > 0 && d2>0) then TVectorf d2 else raise (TypeError (InvalidZeroDimension))
			| _ -> raise (TypeError (TypeMismatch ("vector access", TVectorf 0, t1)))
			end
		| None -> raise (TypeError (UndefinedVariable var))
		end

	| AccessMatrix (var, e1, e2) ->
		let t1 = type_check e1 symbol_table in
		let t2 = type_check e2 symbol_table in
		begin match SymbolTable.find_opt var symbol_table with
		| Some t -> 
			begin match (t, t1, t2) with
			| (TMatrixi (r, c), TInt, TInt) -> if r > 0 && c > 0 then TInt else raise (TypeError (InvalidZeroDimension))
			| (TMatrixf (r, c), TInt, TInt) -> if r > 0 && c > 0 then TFloat else raise (TypeError (InvalidZeroDimension))
			| _ -> raise (TypeError (TypeMismatch ("matrix access", TMatrixf (0, 0), t)))
			end
		| None -> raise (TypeError (UndefinedVariable var))
		end
	
	| Negation e ->
		let t = type_check e symbol_table in
		begin match t with
		| TBool -> TBool
		| _ -> raise (TypeError (TypeMismatch ("negation", TBool, t)))
		end
	
	| Raise e -> TStatement
	
	| Line (e1, e2) ->
		let _ = type_check e1 symbol_table in
		type_check e2 symbol_table (* Line returns the type of its last expression *)