%{
open Ast
open Lexing

let parse_error msg = 
  Printf.eprintf "Parse error: %s\n" msg;
  raise (Failure "Parse error")
%}

%token <string> VAR
%token <string> INPUT PRINT
%token <string> RAISE
%token <int> INT
%token <bool> BOOL
%token <float> FLOAT
%token TYPE_INT TYPE_BOOL TYPE_FLOAT TYPE_VECTOR TYPE_MATRIX
%token <int * string> INTVECTOR
%token <int * string> FLOATVECTOR
%token <int * int * string> INTMATRIX
%token <int * int * string> FLOATMATRIX
%token PLUS MINUS TIMES DIV ABS SQRT
%token ANGLE DIMENSION SCALE ADDV DOTPROD LEN 
%token TRANSPOSE DETERMINANT INV MINOR
%token LSQ RSQ LBR RBR LPAREN RPAREN 
%token EQ NEQ LT GT LEQ GEQ ASSIGN COMMA SEMICOLON
%token AND OR NOT
%token IF THEN ELSE
%token FOR TO DO
%token WHILE
%token EOF

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV
%right NOT ABS SQRT LEN DIMENSION ANGLE
%right SCALE ADDV DOTPROD
%left TRANSPOSE DETERMINANT INV MINOR
%left LSQ

%start program
%type <Ast.expr> program

%%

program:
  | seq_stmt { $1 }

seq_stmt:
  | stmt { $1 }
  | seq_stmt stmt { Seq($1, $2) }

stmt:
  | expr SEMICOLON { $1 }
  | expr ASSIGN expr SEMICOLON { AssignExpr ($1, $3) }
  | expr ASSIGN INPUT SEMICOLON { AssignExpr ($1, Input $3) }
  | PRINT SEMICOLON { Print $1 }
  | IF LPAREN expr RPAREN THEN LBR seq_stmt RBR ELSE LBR seq_stmt RBR SEMICOLON { If ($3, $7, $11) }  (* Required ELSE *)
  | FOR LPAREN VAR ASSIGN expr TO expr RPAREN DO LBR seq_stmt RBR SEMICOLON { For ($3, $5, $7, $11) }
  | WHILE LPAREN expr RPAREN DO LBR seq_stmt RBR SEMICOLON { While ($3, $7) }
  | TYPE_INT VAR SEMICOLON { VarType ("int", $2, None, None) }
  | TYPE_BOOL VAR SEMICOLON { VarType ("bool", $2, None, None) }
  | TYPE_FLOAT VAR SEMICOLON { VarType ("float", $2, None, None) }
  | TYPE_INT TYPE_VECTOR INT VAR SEMICOLON { VarType ("vector", $4, Some ($3, None), Some ("int")) }
  | TYPE_INT TYPE_MATRIX INT COMMA INT VAR SEMICOLON { VarType ("matrix", $6, Some ($3, Some $5), Some("int")) }
  | TYPE_FLOAT TYPE_VECTOR INT VAR SEMICOLON { VarType ("vector", $4, Some ($3, None), Some("float")) }
  | TYPE_FLOAT TYPE_MATRIX INT COMMA INT VAR SEMICOLON { VarType ("matrix", $6, Some ($3, Some $5), Some("float")) }
  | TYPE_INT VAR ASSIGN expr SEMICOLON { Seq (VarType ("int", $2, None, None), AssignExpr (Var $2, $4)) }
  | TYPE_BOOL VAR ASSIGN expr SEMICOLON { Seq (VarType ("bool", $2, None, None), AssignExpr (Var $2, $4)) }
  | TYPE_FLOAT VAR ASSIGN expr SEMICOLON { Seq (VarType ("float", $2, None, None), AssignExpr (Var $2, $4)) }
  | TYPE_INT TYPE_VECTOR INT VAR ASSIGN expr SEMICOLON { Seq (VarType ("vector", $4, Some ($3, None), Some("int")), AssignExpr (Var $4, $6)) }
  | TYPE_INT TYPE_MATRIX INT COMMA INT VAR ASSIGN expr SEMICOLON { Seq (VarType ("matrix", $6, Some ($3, Some $5), Some("int")), AssignExpr (Var $6, $8)) }
  | TYPE_FLOAT TYPE_VECTOR INT VAR ASSIGN expr SEMICOLON { Seq (VarType ("vector", $4, Some ($3, None), Some("float")), AssignExpr (Var $4, $6)) }
  | TYPE_FLOAT TYPE_MATRIX INT COMMA INT VAR ASSIGN expr SEMICOLON { Seq (VarType ("matrix", $6, Some ($3, Some $5), Some("float")), AssignExpr (Var $6, $8)) }
  | TYPE_INT VAR ASSIGN INPUT SEMICOLON { Seq (VarType ("int", $2, None, None), AssignExpr (Var $2, Input $4)) }
  | TYPE_BOOL VAR ASSIGN INPUT SEMICOLON { Seq (VarType ("bool", $2, None, None), AssignExpr (Var $2, Input $4)) }
  | TYPE_FLOAT VAR ASSIGN INPUT SEMICOLON { Seq (VarType ("float", $2, None, None), AssignExpr (Var $2, Input $4)) }
  | TYPE_INT TYPE_VECTOR INT VAR ASSIGN INPUT SEMICOLON { Seq (VarType ("vector", $4, Some ($3, None), Some ("int")), AssignExpr (Var $4, Input $6)) }
  | TYPE_INT TYPE_MATRIX INT COMMA INT VAR ASSIGN INPUT SEMICOLON { Seq (VarType ("matrix", $6, Some ($3, Some $5), Some ("int")), AssignExpr (Var $6, Input $8)) }
  | TYPE_FLOAT TYPE_VECTOR INT VAR ASSIGN INPUT SEMICOLON { Seq (VarType ("vector", $4, Some ($3, None), Some ("float")), AssignExpr (Var $4, Input $6)) }
  | TYPE_FLOAT TYPE_MATRIX INT COMMA INT VAR ASSIGN INPUT SEMICOLON { Seq (VarType ("matrix", $6, Some ($3, Some $5), Some ("float")), AssignExpr (Var $6, Input $8)) }


expr:
  | INT { IntLit $1 }
  | FLOAT { FloatLit $1 }
  | BOOL { BoolLit $1 }
  | INTVECTOR { let (dim, lit) = $1 in VecLit (IntVec (dim, lit)) }
  | FLOATVECTOR { let (dim, lit) = $1 in VecLit (FloatVec (dim, lit)) }
  | INTMATRIX { let (rows, cols, lit) = $1 in MatLit (IntMat (rows, cols, lit)) }
  | FLOATMATRIX { let (rows, cols, lit) = $1 in MatLit (FloatMat (rows, cols, lit)) }
  | VAR { Var $1 }
  | RAISE {Raise $1}
  | LPAREN expr RPAREN { $2 }
  | VAR LSQ expr RSQ { Index (Var $1, $3) }
  | VAR LSQ expr COMMA expr RSQ { IndexMat (Var $1, $3, $5) }
  | expr PLUS expr { Plus ($1, $3) }
  | expr MINUS expr { Minus ($1, $3) }
  | LPAREN MINUS expr RPAREN { Uminus ($3) }
  | expr TIMES expr { Times ($1, $3) }
  | expr DIV expr { Div ($1, $3) }
  | expr AND expr { And ($1, $3) }
  | expr OR expr { Or ($1, $3) }
  | expr EQ expr { Eq ($1, $3) }
  | expr NEQ expr { Neq ($1, $3) }
  | expr LT expr { Lt ($1, $3) }
  | expr GT expr { Gt ($1, $3) }
  | expr LEQ expr { Leq ($1, $3) }
  | expr GEQ expr { Geq ($1, $3) }
  | ABS LPAREN expr RPAREN { Abs $3 }
  | NOT LPAREN expr RPAREN { Not $3 }
  | SQRT LPAREN expr RPAREN { Sqrt $3 }
  | SCALE LPAREN expr COMMA expr RPAREN { Scale ($3, $5) }
  | ADDV LPAREN expr COMMA expr RPAREN { AddV ($3, $5) }
  | DOTPROD LPAREN expr COMMA expr RPAREN { DotProd ($3, $5) }
  | ANGLE LPAREN expr COMMA expr RPAREN { Angle ($3, $5) }
  | LEN LPAREN expr RPAREN { Len $3 }
  | DIMENSION LPAREN expr RPAREN { Dimension $3 }
  | TRANSPOSE LPAREN expr RPAREN { Transpose $3 }
  | DETERMINANT LPAREN expr RPAREN { Determinant $3 }
  | INV LPAREN expr RPAREN { Inv $3 }
  | MINOR LPAREN expr COMMA expr COMMA expr RPAREN{ Minor ($3, $5, $7) }
%%