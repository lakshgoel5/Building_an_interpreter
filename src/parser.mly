%{
    open Ast
    open Utils
    open Typechecker
%}
/*G = <T,N,P,S>*/

/*T*/
%token <int> CONST_INT
%token <float> CONST_FLOAT
%token <bool> CONST_BOOL

%token <int*string> VEC_INT
%token <int*string> VEC_FLOAT
%token <int*string> VEC_GEN

%token <int*int*string> MAT_INT
%token <int*int*string> MAT_FLOAT
%token <int*int*string> MAT_GEN

%token < string > FILENAME
%token < string > VAR

%token INPUT PRINT
%token BOOL INT FLOAT VECTORI VECTORF MATRIXI MATRIXF ABS
%token EQUALS PLUS MINUS TIMES DIVIDE
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token CONJUNCTION DISJUNCTION NEGATION
%token LESS LESSEQ GREATER GREATEREQ EQUAL NOTEQUAL
%token REM SQRT POW LOG
%token DOT_PROD ANGLE MAG DIM ROW COLS TRANSPOSE DET
%token FOR WHILE IF THEN ELSE COMMA SEMICOLON
%token EOF
%token RAISE INVERSE
%token CREATE_EMPTY MINOR
/*Compare these snippets with tokens*/


/*S*/
%start top
%type <Ast.exp> top

/*N and P*/
%%
top: /*Top level*/
    newline EOF { $1 }

newline:
  | newline SEMICOLON statement {Line($1, $3)}
  | newline SEMICOLON { $1 }
  | statement { $1 }

statement:
  | FOR LPAREN assignment SEMICOLON assignment SEMICOLON assignment RPAREN LBRACE newline RBRACE { For($3, $5, $7, $10) }
  | IF LPAREN assignment RPAREN THEN LBRACE newline RBRACE ELSE LBRACE newline RBRACE { If($3, $7, $11) }
  | WHILE LPAREN assignment RPAREN LBRACE newline RBRACE { While($3, $6) }
  | PRINT LPAREN lowest RPAREN { Print($3) } /* Print is a statement. It can contain more functionality*/
  | assignment { $1 }

assignment:
  | assignment EQUALS con_dis { Assign($1, $3) }
  | types { $1 }

types:
  | INT VAR { Int($2) }
  | FLOAT VAR { Float($2) }
  | BOOL VAR { Bool($2) }
  | VECTORF CONST_INT VAR { Vectorf($2,$3) }
  | VECTORI CONST_INT VAR { Vectori($2,$3) }
  | MATRIXF CONST_INT CONST_INT VAR { Matrixf($2,$3,$4) }
  | MATRIXI CONST_INT CONST_INT VAR { Matrixi($2,$3,$4) }
  | con_dis { $1 }

con_dis:
  | con_dis CONJUNCTION gen_comparison { Conjunction($1, $3) }
  | con_dis DISJUNCTION gen_comparison { Disjunction($1, $3) }
  | gen_comparison { $1 }

gen_comparison:  /* Highest level for comparisons */
  | gen_comparison EQUAL int_comparison { Equal($1, $3) }
  | gen_comparison NOTEQUAL int_comparison { NotEqual($1, $3) }
  | int_comparison { $1 }

int_comparison:
  | int_comparison LESS lev2 { Less($1, $3) }
  | int_comparison LESSEQ lev2 { LessEq($1, $3) }
  | int_comparison GREATER lev2 { Greater($1, $3) }
  | int_comparison GREATEREQ lev2 { GreaterEq($1, $3) }
  | lev2 { $1 }

lev2:
  | lev2 PLUS lev1 { Add($1, $3) }
  | lev2 MINUS lev1 { Sub($1, $3) }
  | lev1 { $1 }        /* lev2 acts as the base for recursion */

lev1:
  | lev1 DIVIDE functions { Divide($1, $3) }
  | lev1 TIMES functions { Multiply($1, $3) }
  | lev1 DOT_PROD functions { DotProd($1, $3) }
  | functions { $1 }

functions:
  | INPUT LPAREN RPAREN { Input }
  | INPUT LPAREN lowest RPAREN { Inputf($3) }
  | functions REM negation { Remainder($1, $3) }
  | ABS LPAREN negation RPAREN { Abs($3) }
  | SQRT LPAREN negation RPAREN{ Sqrt($3) }
  | POW LPAREN negation COMMA negation RPAREN { Pow($3, $5) }
  | LOG LPAREN negation COMMA negation RPAREN { Log($3, $5) }
  | RAISE LPAREN VAR RPAREN { Raise($3) }
  | functions ANGLE negation { Angle($1, $3) }
  | MAG LPAREN negation RPAREN { Mag($3) }
  | DIM LPAREN negation RPAREN { Dimension($3) }
  | ROW LPAREN negation RPAREN { Row($3) }
  | COLS LPAREN negation RPAREN { Cols($3) }
  | TRANSPOSE LPAREN negation RPAREN { Transpose($3) } /*Here access may be required */
  | CREATE_EMPTY LPAREN negation COMMA negation RPAREN {CreateEmpty ($3, $5)}
  | MINOR LPAREN negation COMMA negation COMMA negation RPAREN {Minor ($3, $5, $7)}
  | DET LPAREN negation RPAREN { Determinant($3) }
  | INVERSE LPAREN negation RPAREN { Inverse($3) }
  | negation { $1 }

negation:
  | NEGATION neg_nos { Negation($2) } /*Here also access may be required*/
  | neg_nos { $1 }

neg_nos:
  | MINUS lowest { Utils.negative_nos $2 }
  | access { $1 }

access: /*for matrix or vector access like mat[2,3], vec[3+5]*/
  | VAR LBRACKET lev2 COMMA lev2 RBRACKET { AccessMatrix($1, $3, $5) }
  | VAR LBRACKET lev2 RBRACKET { AccessVector($1, $3) }
  | lowest { $1 }

lowest: /*Lowest Level*/
  | CONST_BOOL { Const_Bool($1) }
  | CONST_INT { Const_Int($1) }
  | CONST_FLOAT { Const_Float($1) }
  | VEC_INT { Const_Vector_int(fst $1, Utils.parse_exp_list (snd $1)) }
  | VEC_FLOAT { Const_Vector_float(fst $1, Utils.parse_exp_list (snd $1)) }
  | MAT_INT { Const_Matrix_int(Utils.first $1, Utils.second $1, Utils.parse_exp_list_list (Utils.thd $1)) }
  | MAT_FLOAT { Const_Matrix_float(Utils.first $1, Utils.second $1, Utils.parse_exp_list_list (Utils.thd $1)) }
  | FILENAME { Filename($1) }
  | VAR { Variable($1) }
  | LPAREN con_dis RPAREN { $2 }  /* Non-bool equations can have comparisons only once */
