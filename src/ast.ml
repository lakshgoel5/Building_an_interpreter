type exp =
| Const_Int of int
| Const_Float of float
| Const_Bool of bool
| Const_Vector_int of int * exp list
| Const_Vector_float of int * exp list
| Const_Matrix_float of int * int * exp list list
| Const_Matrix_int of int * int * exp list list
| Variable of string
| Filename of string
| AccessMatrix of string * exp * exp
| AccessVector of string * exp

| Inputf of exp
| Input
| Print of exp


| Int of string
| Float of string
| Bool of string
| Vectorf of int * string
| Vectori of int * string
| Matrixf of int * int * string
| Matrixi of int * int * string

| Add of exp * exp
| Sub of exp * exp
| Multiply of exp * exp
| Divide of exp * exp

| Assign of exp * exp

| Equal of exp * exp
| Less of exp * exp
| LessEq of exp * exp
| Greater of exp * exp
| GreaterEq of exp * exp
| NotEqual of exp * exp

| Remainder of exp * exp

| Conjunction of exp * exp
| Disjunction of exp * exp
| Negation of exp

| Abs of exp
| Sqrt of exp
| Pow of exp * exp
| Log of exp * exp

| Mag of exp
| Transpose of exp
| Determinant of exp
| Row of exp
| Cols of exp
| Angle of exp * exp
| Dimension of exp
| DotProd of exp * exp
| CreateEmpty of exp * exp
| Minor of exp * exp * exp
| Inverse of exp

| Raise of string

| For of exp * exp * exp * exp
| While of exp * exp
| If of exp * exp * exp

| Line of exp * exp

(*
My tokens are my Terminals, and exp are non-terminals.

*)