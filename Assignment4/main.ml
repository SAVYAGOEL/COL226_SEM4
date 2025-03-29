open Ast
open Lexing
open Parser
open Lexer

(* Pretty-printer for types *)
let rec string_of_typ = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TVector (n, TInt) -> Printf.sprintf "vector<%d, int>" n
  | TVector (n, TFloat) -> Printf.sprintf "vector<%d, float>" n
  | TMatrix (r, c, TInt) -> Printf.sprintf "matrix<%d, %d, int>" r c
  | TMatrix (r, c, TFloat) -> Printf.sprintf "matrix<%d, %d, float>" r c
  | TUnit -> "unit"
  | _ -> Printf.sprintf "check kr bhai error hai"

(* Pretty-printer for AST *)
let rec expr_to_string = function
  | IntLit i -> Printf.sprintf "IntLit(%d)" i
  | FloatLit f -> Printf.sprintf "FloatLit(%f)" f
  | BoolLit b -> Printf.sprintf "BoolLit(%b)" b
  | VecLit (IntVec (n, v)) -> Printf.sprintf "VecLit(IntVec(%d, %s))" n v
  | VecLit (FloatVec (n, v)) -> Printf.sprintf "VecLit(FloatVec(%d, %s))" n v
  | MatLit (IntMat (r, c, v)) -> Printf.sprintf "MatLit(IntMat(%d, %d, %s))" r c v
  | MatLit (FloatMat (r, c, v)) -> Printf.sprintf "MatLit(FloatMat(%d, %d, %s))" r c v
  | Var v -> Printf.sprintf "Var(%s)" v
  | Plus (e1, e2) -> Printf.sprintf "Plus(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Minus (e1, e2) -> Printf.sprintf "Minus(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Uminus (e) -> Printf.sprintf "Uminus(%s)" (expr_to_string e)
  | Times (e1, e2) -> Printf.sprintf "Times(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Div (e1, e2) -> Printf.sprintf "Div(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | And (e1, e2) -> Printf.sprintf "And(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Or (e1, e2) -> Printf.sprintf "Or(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Eq (e1, e2) -> Printf.sprintf "Eq(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Neq (e1, e2) -> Printf.sprintf "Neq(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Lt (e1, e2) -> Printf.sprintf "Lt(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Gt (e1, e2) -> Printf.sprintf "Gt(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Leq (e1, e2) -> Printf.sprintf "Leq(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Geq (e1, e2) -> Printf.sprintf "Geq(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Not e -> Printf.sprintf "Not(%s)" (expr_to_string e)
  | Abs e -> Printf.sprintf "Abs(%s)" (expr_to_string e)
  | Scale (e1, e2) -> Printf.sprintf "Scale(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | AddV (e1, e2) -> Printf.sprintf "AddV(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | DotProd (e1, e2) -> Printf.sprintf "DotProd(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Angle (e1, e2) -> Printf.sprintf "Angle(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Len e -> Printf.sprintf "Len(%s)" (expr_to_string e)
  | Dimension e -> Printf.sprintf "Dimension(%s)" (expr_to_string e)
  | Transpose e -> Printf.sprintf "Transpose(%s)" (expr_to_string e)
  | Determinant e -> Printf.sprintf "Determinant(%s)" (expr_to_string e)
  | Inv e -> Printf.sprintf "Inv(%s)" (expr_to_string e)
  | Minor (e1, e2, e3) -> Printf.sprintf "Minor(%s, %s, %s)" (expr_to_string e1) (expr_to_string e2) (expr_to_string e3)
  | Sqrt e -> Printf.sprintf "Sqrt(%s)" (expr_to_string e)
  | Index (e1, e2) -> Printf.sprintf "Index(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | IndexMat (e1, e2, e3) -> Printf.sprintf "IndexMat(%s, %s, %s)" (expr_to_string e1) (expr_to_string e2) (expr_to_string e3)
  | AssignExpr (e1, e2) -> Printf.sprintf "AssignExpr(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Seq (e1, e2) -> Printf.sprintf "Seq(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | If (c, t, e) -> Printf.sprintf "If(%s, %s, %s)" (expr_to_string c) (expr_to_string t) (expr_to_string e)
  | For (v, s, e, b) -> Printf.sprintf "For(%s, %s, %s, %s)" v (expr_to_string s) (expr_to_string e) (expr_to_string b)
  | While (c, b) -> Printf.sprintf "While(%s, %s)" (expr_to_string c) (expr_to_string b)
  | Print s -> Printf.sprintf "Print(\"%s\")" s
  | Input s -> Printf.sprintf "Input(\"%s\")" s
  | Raise s -> Printf.sprintf "Raise(\"%s\")" s
  | VarType (t, v, None, None) -> Printf.sprintf "VarType(%s, %s, None)" t v
  | VarType (t, v, Some (n, None), Some s) -> Printf.sprintf "VarType(%s, %s, Some(%d, None), Some %s)" t v n s
  | VarType (t, v, Some (r, Some c), Some s) -> Printf.sprintf "VarType(%s, %s, Some(%d, Some %d), Some %s)" t v r c s
  | _ -> Printf.sprintf "Unknown expression"

(* Type checking function *)
(* Print position for error reporting *)
let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* Test runner *)
let run_test num input =
  Printf.printf "\nTest %d:\nInput: %s\n" num input;
  let lexbuf = Lexing.from_string input in
  try
    let ast = Parser.program Lexer.tokenize lexbuf in
    Printf.printf "AST: %s\n" (expr_to_string ast);
    typecheck_program ast;
    Printf.printf "Type Check: Passed\n"
  with
  | Parsing.Parse_error ->
      Printf.printf "Type Check: Failed - Syntax error at %s\n" (print_position lexbuf)
  | TypeError msg ->
      Printf.printf "Type Check: Failed - %s\n" msg
  | e ->
      Printf.printf "Type Check: Failed - Unexpected error at %s: %s\n" (print_position lexbuf) (Printexc.to_string e)

(* Main function with updated and new test cases *)
let main () =
  let tests = [
    (* Instructor's Adapted Test Cases *)
    (1, "int matrix 2,2 A := 2,2 [[1, 2], [3, 4]]; int matrix 2,2 B := 2,2 [[5, 6], [7, 8]]; int matrix 2,2 add_matrices := (A + B); Print(add_matrices);"); (* Matrix Sum *)
    (2, "int matrix 2,2 A := 2,2 [[1, 2], [3, 4]]; int matrix 2,2 transpose_matrix := transpose(A); Print(transpose_matrix);"); (* Matrix Transpose *)
    (3, "int matrix 2,2 A := 2,2 [[1, 2], [3, 4]]; float determinant_of_matrix := det(A); Print(determinant_of_matrix);"); (* Matrix Determinant *)
    (4, "int matrix 2,2 A := 2,2 [[1, 2], [3, 4]]; if ((det(A) != 0)) then { int matrix 2,2 cofactor_matrix := 2,2 [[4, -3], [-2, 1]]; int matrix 2,2 adjoint_of_matrix := transpose(cofactor_matrix); int matrix 2,2 inverse_of_matrix := scale((1 / det(A)), adjoint_of_matrix); Print(inverse_of_matrix); } else { Print(error); };");
    (5, "int matrix 2,3 A := 2,3 [[1, 2, 3], [4, 5, 6]]; int matrix 3,2 B := 3,2 [[1, 2], [3, 4], [5, 6]]; int matrix 2,2 multiply_matrices := (A * B); Print(multiply_matrices);"); (* Matrix Multiplication *)
    (6, "int matrix 2,2 A := 2,2 [[1, 2], [3, 4]]; int vector 2 x := 2 [5, 6]; int matrix 2,1 multiply_vector_matrix := (A * x); Print(multiply_vector_matrix);"); (* Matrix-Vector Product *)
    (7, "int matrix 2,2 A := 2,2 [[1, 2], [3, 4]]; int vector 2 b := 2 [5, 11]; if ((det(A) != 0)) then { int vector 2 x := scale((1 / det(A)), (A * b)); Print(x); } else { Print(error); };"); (* Gaussian Elimination *)
    (8, "int matrix 2,2 A := 2,2 [[1, 2], [3, 4]]; float trace := (1.0 + 4.0); float determinant := det(A); float D := ((trace * trace) - (4.0 * determinant)); if ((D >= 0)) then { float eigenvalue1 := (((trace + D) / 2)); float eigenvalue2 := (((trace - D) / 2)); Print(eigenvalue1); Print(eigenvalue2); } else { Print(error); };"); (* Eigenvalues *)
    (9, "int matrix 5,3 V := 5,3 [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]]; int vector 3 vector_sum := 3 [0, 0, 0]; for (i := 0 to 4) do { vector_sum := addv(vector_sum, minor(V, i, 0)); }; Print(vector_sum);"); (* For Loop *)
    (10, "int matrix 2,2 A := 2,2 [[1, 2], [3, 4]]; int sum_of_squares := 0; for (i := 0 to 1) do { for (j := 0 to 1) do { sum_of_squares := (sum_of_squares + (A[i,j] * A[i,j])); }; }; Print(sum_of_squares);"); (* Magnitude *)
    (11, "int matrix 2,2 A := 2,2 [[1, 2], [3, 4]]; float threshold := 0.000001; float norm_diff := 10.0; while ((norm_diff > threshold)) do { A := (A * A); norm_diff := (norm_diff - threshold); }; Print(A);"); (* While Loop *)

    (* Type Compatibility Tests *)
    (12, "int x := 5; float y := 3.14; float z := (x + y); Print(z);"); (* Mixed scalar arithmetic *)
    (13, "int vector 3 v1 := 3 [1, 2, 3]; int vector 3 v2 := 3 [4, 5, 6]; int vector 3 v3 := addv(v1, v2); Print(v3);"); (* Vector addition *)
    (14, "int matrix 2,2 m1 := 2,2 [[1, 2], [3, 4]]; int matrix 2,2 m2 := 2,2 [[5, 6], [7, 8]]; int matrix 2,2 m3 := (m1 * m2); Print(m3);"); (* Square matrix multiplication *)
    (15, "int x := 5; int vector 3 v := 3 [1, 2, 3]; v := scale(x, v); Print(v);"); (* Scalar-vector scaling *)

    (* Type Error Tests *)
    (16, "int x := 5; bool y := true; int z := (x + y); Print(z);"); (* Incompatible arithmetic *)
    (17, "int vector 3 v := 3 [1, 2, 3]; int vector 2 w := 2 [4, 5]; int vector 3 u := addv(v, w); Print(u);"); (* Vector dimension mismatch *)
    (18, "int matrix 2,3 m1 := 2,3 [[1, 2, 3], [4, 5, 6]]; int matrix 2,3 m2 := 2,3 [[7, 8, 9], [10, 11, 12]]; int matrix 2,3 m3 := (m1 * m2); Print(m3);"); (* Invalid matrix multiplication *)
    (19, "float x := 3.14; float vector 3 v := 3 [1, 2, 3]; v := scale(x, v); Print(v);"); (* Invalid scalar type for scaling *)
    (20, "int matrix 2,2 m := 2,2 [[1, 2], [3, 4]]; m[true, 1] := 5; Print(m);"); (* Invalid index type *)

    (* Parse Error Tests *)
    (21, "int x := (5 + ); Print(x);"); (* Missing operand *)
    (22, "int matrix 2,2 m := 2,2 [[1, 2] [3, 4]]; Print(m);"); (* Missing comma in matrix *)
    (23, "if (true then { Print(x); } else { Print(y); };"); (* Missing parenthesis *)
    (24, "for (i := 0 to 2 do { Print(i); };"); (* Missing closing parenthesis *)
    (25, "int x := 5 Print(x);"); (* Missing semicolon *)

    (* Complex Inputs *)
    (26, "int x := 0; for (i := 0 to 3) do { if ((i > 1)) then { for (j := 0 to 2) do { x := (x + (i * j)); Print(x); }; } else { x := (x - i); Print(x); }; };"); (* Nested loops and conditionals *)
    (27, "int matrix 3,3 m := 3,3 [[1, 2, 3], [4, 5, 6], [7, 8, 9]]; for (i := 0 to 2) do { for (j := 0 to 2) do { m[i,j] := (m[i,j] * 2); }; }; Print(m);"); (* Matrix indexing in nested loops *)
    (28, "int x := 5; while ((x > 0)) do { if ((x > 2)) then { x := (x - 1); Print(x); } else { x := (x - 2); Print(x); }; };"); (* While with nested if *)
    (29, "int matrix 2,2 m1 := 2,2 [[1, 2], [3, 4]]; int matrix 2,2 m2 := (m1 * m1); int matrix 2,2 m3 := (m2 * m1); m3 := scale(2, m3); Print(m3);"); (* Chained matrix operations *)

    (* Edge Cases *)
    (30, "int matrix 1,1 m := 1,1 [[42]]; Print(m);"); (* 1x1 matrix *)
    (31, "int vector 1 v := 1 [5]; Print(v);"); (* 1D vector *)
    (32, "int x := 2147483647; x := (x + 1); Print(x);"); (* Integer overflow check *)
    (33, "float x := 0.0000001; float y := (x / 1000000); Print(y);"); (* Very small float *)
    (34, "int matrix 2,2 m; Input(m); Print(m);"); (* Input without initialization *)

    (* Additional Complex Tests *)
    (35, "int x := 0; for (i := 0 to 5) do { for (j := 0 to 5) do { if (((i * j) > 10)) then { x := (x + (i * j)); Print(x); } else { x := (x - 1); Print(x); }; }; };"); (* Deeply nested with arithmetic *)
    (36, "int matrix 3,2 m1 := 3,2 [[1, 2], [3, 4], [5, 6]]; int matrix 2,3 m2 := 2,3 [[1, 2, 3], [4, 5, 6]]; int matrix 3,3 m3 := (m1 * m2); int matrix 3,2 m4 := (m3 * m1); Print(m4);"); (* Multiple matrix multiplications *)
    (37, "int vector 3 v := 3 [1, 2, 3]; int x := dot_prod(v, v); float y := len(v); Print(x); Print(y);"); (* Vector operations *)
    (38, "int matrix 2,2 m := 2,2 [[1, 2], [3, 4]]; float d := det(m); int matrix 2,2 t := transpose(m); if ((d != 0)) then { m := scale((1 / d), t); Print(m); } else { Print(error); };"); (* Combined operations with control flow *)
    (39, "int x := 10; while ((x > 0)) do { for (i := 0 to x) do { x := (x - 1); Print(x); }; };"); (* Nested while and for *)
    (40, "int matrix 4,4 m := 4,4 [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]; for (i := 0 to 3) do { m[i,i] := 0; }; Print(m);"); (* Large matrix with diagonal modification *)

    (* New Test Cases for Added Functionalities *)
    (41, "int matrix 3,3 m := 3,3 [[1, 2, 3], [4, 5, 6], [7, 8, 9]]; int matrix 2,2 minor_m := minor(m, 1, 1); Print(minor_m);"); (* Minor operation *)
    (42, "int vector 3 v := 3 [1, 2, 3]; float angle_result := angle(v, v); Print(angle_result);"); (* Angle between vectors *)
    (43, "int vector 3 v := 3 [3, 4, 0]; float length := len(v); Print(length);"); (* Vector length *)
    (44, "int vector 4 v := 4 [1, 2, 3, 4]; int dimen := dim(v); Print(dimen);"); (* Vector dimension *)
    (45, "int x := 16; float sqrt_result := sqrt(x); Print(sqrt_result);"); (* Square root *)
    (46, "int x := (-5); int abs_result := abs(x); Print(abs_result);"); (* Absolute value *)
    (47, "float matrix 2,2 m := 2,2 [[1.0, 2.0], [3.0, 4.0]]; m[1,0] := 5.2; Print(m);"); (* Matrix indexing with float *)
    (48, "int matrix 2,2 m := 2,2 [[1, 2], [3, 4]]; int x := m[0,1]; Print(x);"); (* Matrix indexing *)
    (49, "float vector 3 v := 3 [1.1, 2.0, 3.0]; v[1] := 5.2; Print(v);"); (* Vector indexing with float *)
    (50, "int vector 3 v := 3 [1, 2, 3]; int x := v[0]; Print(x);"); (* Vector indexing *)
    (51, "int x := 5;");
    (52, "int x := Input();");
    (53, "int vector 3 v := 3 [1, 2, 3]; v[0] := Input();");
    (54, "int x; x := Input();");
    (55, "int x; x := 5;");
    (56, "bool x := (5 = 3.2);")
  ] in
  List.iter (fun (num, input) -> run_test num input) tests

let () = main ()