let rec print_tokens lexbuf =
  match Lexer.tokenize lexbuf with
  | Lexer.EOF -> print_endline "EOF"
  | t -> print_endline (string_of_token t); print_tokens lexbuf

and string_of_token = function
  | Lexer.VAR s -> "VAR(" ^ s ^ ")"
  | Lexer.INPUT s -> "INPUT(" ^ s ^ ")"
  | Lexer.PRINT s -> "PRINT(" ^ s ^ ")"
  | Lexer.INT n -> "INT(" ^ string_of_int n ^ ")"
  | Lexer.BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | Lexer.FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | Lexer.TYPE_INT -> "TYPE_INT"
  | Lexer.TYPE_BOOL -> "TYPE_BOOL"
  | Lexer.TYPE_FLOAT -> "TYPE_FLOAT"
  | Lexer.TYPE_VECTOR -> "TYPE_VECTOR"
  | Lexer.TYPE_MATRIX -> "TYPE_MATRIX"
  | Lexer.INTVECTOR (dim, lit) -> "INTVECTOR(" ^ string_of_int dim ^ ",\"" ^ lit ^ "\")"
  | Lexer.FLOATVECTOR (dim, lit) -> "FLOATVECTOR(" ^ string_of_int dim ^ ",\"" ^ lit ^ "\")"
  | Lexer.INTMATRIX (rows, cols, lit) -> "INTMATRIX(" ^ string_of_int rows ^ "," ^ string_of_int cols ^ ",\"" ^ lit ^ "\")"
  | Lexer.FLOATMATRIX (rows, cols, lit) -> "FLOATMATRIX(" ^ string_of_int rows ^ "," ^ string_of_int cols ^ ",\"" ^ lit ^ "\")"
  | Lexer.PLUS -> "PLUS" | Lexer.MINUS -> "MINUS" | Lexer.TIMES -> "TIMES"
  | Lexer.DIV -> "DIV" | Lexer.ABS -> "ABS"
  | Lexer.ANGLE -> "ANGLE" | Lexer.DIMENSION -> "DIMENSION" | Lexer.SCALE -> "SCALE"
  | Lexer.ADDV -> "ADDV" | Lexer.DOTPROD -> "DOTPROD" | Lexer.LEN -> "LEN"
  | Lexer.INV -> "INV" | Lexer.TRANSPOSE -> "TRANSPOSE" | Lexer.DETERMINANT -> "DETERMINANT"
  | Lexer.LSQ -> "LSQ" | Lexer.RSQ -> "RSQ" | Lexer.LBR -> "LBR" | Lexer.RBR -> "RBR"
  | Lexer.LPAREN -> "LPAREN" | Lexer.RPAREN -> "RPAREN"
  | Lexer.EQ -> "EQ" | Lexer.NEQ -> "NEQ" | Lexer.LT -> "LT" | Lexer.GT -> "GT"
  | Lexer.LEQ -> "LEQ" | Lexer.GEQ -> "GEQ" | Lexer.ASSIGN -> "ASSIGN"
  | Lexer.DOT -> "DOT" | Lexer.COMMA -> "COMMA" | Lexer.SEMICOLON -> "SEMICOLON"
  | Lexer.AND -> "AND" | Lexer.OR -> "OR" | Lexer.NOT -> "NOT"
  | Lexer.IF -> "IF" | Lexer.THEN -> "THEN" | Lexer.ELSE -> "ELSE"
  | Lexer.FOR -> "FOR" | Lexer.TO -> "TO" | Lexer.DO -> "DO"
  | Lexer.WHILE -> "WHILE" | Lexer.BREAK -> "BREAK" | Lexer.CONTINUE -> "CONTINUE"
  | Lexer.EOF -> "EOF"

let () =
  let lexbuf = Lexing.from_channel stdin in
  try print_tokens lexbuf
  with exn -> Printf.eprintf "Error: %s\n" (Printexc.to_string exn); exit 1