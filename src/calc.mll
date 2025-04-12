{
  open Parser
}

let alphanum = ['a'-'z''A'-'Z''0'-'9']
let digit = ['0'-'9']
let whitespace = [' ' '\t' '\n']+
let variables = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']* (*Variables start with a letter or underscore, but can contain nunmbers and primes*)
let integer = ('0' | ['1' - '9']['0'-'9']*) (*Integers are a sequence of digits*)
let float = (((['0'-'9']+'.')|(['0'-'9']+'.'['0'-'9']+)|('.'['0'-'9']+))|((['0'-'9']+'.')|(['0'-'9']+'.'['0'-'9']+)|('.'['0'-'9']+)|integer)'e'['+''-']?(integer)) (*Floats are a sequence of (digits, a period) or (a period, and a sequence of digit) or (both)*)
let filename_char = ['a'-'z''A'-'Z''0'-'9''_']['a'-'z''A'-'Z''0'-'9''_'''']*'.'['a'-'z''A'-'Z']+ (*Filenames start with letters, number or underscore and can contain letters, numbers, underscores, periods, and slashes*)
let element = (float  | integer)

let int_list = ((whitespace)*['['](whitespace)*'-'?integer(whitespace)*([','](whitespace)* '-'? integer)*(whitespace)*[']'])
let float_list = ((whitespace)*['['](whitespace)* '-'? float(whitespace)* ([','](whitespace)* '-'? float)* (whitespace)*[']'])
let gen_list = ((whitespace)*['['](whitespace)* element (whitespace)*([','](whitespace)* element)* (whitespace)*[']'])
let vector_int = (['1'-'9'] ['0'-'9']* as x )(whitespace)*(int_list as l)  (* 5[3,5,23,2,10] *)
let vector_float = (['1'-'9'] ['0'-'9']* as x )(whitespace)*(float_list as l)  (* 2[3.4 ,38.2] *)
let vector_general = (['1'-'9'] ['0'-'9']* as x )(whitespace)*(gen_list as l)  (* 2[3.4 ,38.2] *)
let matrix_int = (integer as d1)(whitespace)*[','](whitespace)*(integer as d2) (whitespace)* (((whitespace)*['['](whitespace)*(int_list)((whitespace)*[';'](whitespace)* int_list)*(whitespace)*[']']) as l)
let matrix_float = (integer as d1)(whitespace)*[','](whitespace)*(integer as d2) (whitespace)* (((whitespace)*['['](whitespace)*(float_list)((whitespace)*[';'](whitespace)*float_list)*(whitespace)*[']']) as l)
let matrix_general =  (integer as d1)(whitespace)*[','](whitespace)*(integer as d2) (whitespace)* (((whitespace)*['['](whitespace)*(gen_list)((whitespace)*[';'](whitespace)*gen_list)*(whitespace)*[']']) as l)
let comment = ([^'*'] | ('*' [^')']))*

let bool = "true" | "false"


rule tokenize = parse
  | whitespace { tokenize lexbuf }  (* Ignore spaces *)
  | "(*" comment "*)" { tokenize lexbuf } (* Ignore comments *)
  | "Input" { INPUT }
  | "Print" { PRINT }

  | "vectorf" { VECTORF }
  | "vectori" { VECTORI }
  | "matrixf" { MATRIXF }
  | "matrixi" { MATRIXI }
  | "bool" { BOOL }
  | "int" { INT }
  | "float" { FLOAT }

(*variables and constants*)
  | vector_int { VEC_INT (int_of_string(x),l)}  (* if input is 2[3,5,23,2,10] then output is [LBRACKET][CONST_INT 3][COMMA][CONST_INT 5][COMMA][CONST_INT 23][COMMA][CONST_INT 2][COMMA][CONST_INT 10][RBRACKET]*)
  | vector_float { VEC_FLOAT (int_of_string(x),l)}  (* if input is 2[2.45; 3.29] then output is [LBRACKET][CONST_FLOAT 2.45][SEMICOLON][CONST_FLOAT 3.29][RBRACKET]*)
  | vector_general { VEC_GEN (int_of_string(x),l)}  (* if input is 2[3,5,23,2,10] then output is [LBRACKET][CONST_INT 3][COMMA][CONST_INT 5][COMMA][CONST_INT 23][COMMA][CONST_INT 2][COMMA][CONST_INT 10][RBRACKET]*)
  | matrix_int { MAT_INT(int_of_string(d1),int_of_string(d2),l) }  
  | matrix_float { MAT_FLOAT(int_of_string(d1),int_of_string(d2),l) }  
  | matrix_general { MAT_GEN(int_of_string(d1),int_of_string(d2),l) }  
  | integer { CONST_INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { CONST_FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool { CONST_BOOL (bool_of_string (Lexing.lexeme lexbuf)) }

(*control constructs*)
  | "for" { FOR }
  | "while" { WHILE }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }

(*operators*)
  | "abs" { ABS }
  | "sqrt" { SQRT }
  | "pow" { POW }
  | "log" { LOG }
  | "/" { DIVIDE }
  | "*" { TIMES } (*scaler mult of vector, matrix, int, float*)
  | "+" { PLUS } (*addition of vectors, floats, ints*)
  | "-" { MINUS }
  | ":=" { EQUALS }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "(" { LPAREN }
  | ")" { RPAREN }
  (*absolute value*)

(*bool*)
  | "&&" { CONJUNCTION }
  | "||" { DISJUNCTION }
  | "!" { NEGATION }

(*comparison for ints*)
  | "%" { REM }
  | "!=" { NOTEQUAL }
  | "==" { EQUAL }
  | "<" { LESS }
  | "<=" { LESSEQ }
  | ">" { GREATER }
  | ">=" { GREATEREQ }

(*vectors*)
  | "mag" { MAG }
  | "dotprod" { DOT_PROD }
  | "angle" { ANGLE }
  | "dim" { DIM }
  | "create_empty" {CREATE_EMPTY}
  | "minor" {MINOR}

(*matrix*)
  | "transpose" { TRANSPOSE }
  | "det" { DET }
  | "row" { ROW }
  | "cols" { COLS }
  | "inv" { INVERSE }

  | "raise" { RAISE }

  | filename_char { FILENAME (Lexing.lexeme lexbuf) }
  | variables { VAR (Lexing.lexeme lexbuf) }
  | eof { EOF } (*CTRL + C*)
  | _ { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
