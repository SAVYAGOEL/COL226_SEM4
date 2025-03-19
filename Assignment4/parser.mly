%{
open Ast
open Lexing

let parse_error msg = 
  Printf.eprintf "Parse error: %s\n" msg;
  raise (Failure "Parse error")
%}

%token <string> VAR
%token <string> INPUT PRINT
%token <int> INT
%token <bool> BOOL
%token <float> FLOAT
%token TYPE_INT TYPE_BOOL TYPE_FLOAT TYPE_VECTOR TYPE_MATRIX
%token <int * string> INTVECTOR
%token <int * string> FLOATVECTOR
%token <int * int * string> INTMATRIX
%token <int * int * string> FLOATMATRIX
%token PLUS MINUS TIMES DIV ABS
%token ANGLE DIMENSION SCALE ADDV DOTPROD LEN INV
%token TRANSPOSE DETERMINANT
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
%right NOT ABS LEN DIMENSION ANGLE
%right SCALE ADDV DOTPROD
%left TRANSPOSE DETERMINANT
%left LSQ

%start program
%type <Ast.expr> program

%%

program:
  | seq_stmt { let _ = Ast.typecheck_expr Ast.empty $1 in $1 }

seq_stmt:
  | stmt { $1 }
  | seq_stmt stmt { Seq($1, $2) }

stmt:
  | expr SEMICOLON { $1 }
  | expr ASSIGN expr SEMICOLON { AssignExpr ($1, $3) }
  | INPUT SEMICOLON { Input $1 }
  | PRINT SEMICOLON { Print $1 }
  | IF LPAREN expr RPAREN THEN LPAREN seq_stmt RPAREN ELSE LPAREN seq_stmt RPAREN SEMICOLON { If ($3, $7, $11) }  (* Required ELSE *)
  | FOR LPAREN VAR ASSIGN expr TO expr RPAREN DO LPAREN seq_stmt RPAREN SEMICOLON { For ($3, $5, $7, $11) }
  | WHILE LPAREN expr RPAREN DO LPAREN seq_stmt RPAREN SEMICOLON { While ($3, $7) }
  | TYPE_INT VAR SEMICOLON { VarType ("int", $2, None) }
  | TYPE_BOOL VAR SEMICOLON { VarType ("bool", $2, None) }
  | TYPE_FLOAT VAR SEMICOLON { VarType ("float", $2, None) }
  | TYPE_VECTOR INT VAR SEMICOLON { VarType ("vector", $3, Some ($2, None)) }
  | TYPE_MATRIX INT COMMA INT VAR SEMICOLON { VarType ("matrix", $5, Some ($2, Some $4)) }
  | TYPE_INT VAR ASSIGN expr SEMICOLON { Seq (VarType ("int", $2, None), AssignExpr (Var $2, $4)) }
  | TYPE_BOOL VAR ASSIGN expr SEMICOLON { Seq (VarType ("bool", $2, None), AssignExpr (Var $2, $4)) }
  | TYPE_FLOAT VAR ASSIGN expr SEMICOLON { Seq (VarType ("float", $2, None), AssignExpr (Var $2, $4)) }
  | TYPE_VECTOR INT VAR ASSIGN expr SEMICOLON { Seq (VarType ("vector", $3, Some ($2, None)), AssignExpr (Var $3, $5)) }
  | TYPE_MATRIX INT COMMA INT VAR ASSIGN expr SEMICOLON { Seq (VarType ("matrix", $5, Some ($2, Some $4)), AssignExpr (Var $5, $7)) }

expr:
  | INT { IntLit $1 }
  | FLOAT { FloatLit $1 }
  | BOOL { BoolLit $1 }
  | INTVECTOR { let (dim, lit) = $1 in VecLit (IntVec (dim, lit)) }
  | FLOATVECTOR { let (dim, lit) = $1 in VecLit (FloatVec (dim, lit)) }
  | INTMATRIX { let (rows, cols, lit) = $1 in MatLit (IntMat (rows, cols, lit)) }
  | FLOATMATRIX { let (rows, cols, lit) = $1 in MatLit (FloatMat (rows, cols, lit)) }
  | VAR { Var $1 }
  | LPAREN expr RPAREN { $2 }
  | expr LSQ expr RSQ { Index ($1, $3) }
  | expr LSQ expr COMMA expr RSQ { IndexMat ($1, $3, $5) }
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
  | ABS expr { Abs $2 }
  | NOT expr { Not $2 }
  | SCALE expr expr { Scale ($2, $3) }
  | ADDV expr expr { AddV ($2, $3) }
  | DOTPROD expr expr { DotProd ($2, $3) }
  | ANGLE expr expr { Angle ($2, $3) }
  | LEN expr { Len $2 }
  | DIMENSION expr { Dimension $2 }
  | TRANSPOSE expr { Transpose $2 }
  | DETERMINANT expr { Determinant $2 }
%%