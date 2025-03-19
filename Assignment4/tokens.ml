(* tokens.ml *)
type token =
  | VAR of string
  | INPUT of string
  | PRINT of string
  | INT of int
  | BOOL of bool
  | FLOAT of float
  | TYPE_INT
  | TYPE_BOOL
  | TYPE_FLOAT
  | TYPE_VECTOR
  | TYPE_MATRIX
  | INTVECTOR of int * string
  | FLOATVECTOR of int * string
  | INTMATRIX of int * int * string
  | FLOATMATRIX of int * int * string
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | ABS
  | ANGLE
  | DIMENSION
  | SCALE
  | ADDV
  | DOTPROD
  | LEN
  | INV
  | TRANSPOSE
  | DETERMINANT
  | LSQ
  | RSQ
  | LBR
  | RBR
  | LPAREN
  | RPAREN 
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | ASSIGN
  | COMMA
  | SEMICOLON
  | AND
  | OR
  | NOT
  | IF
  | THEN
  | ELSE
  | FOR
  | TO
  | DO
  | WHILE
  | EOF
