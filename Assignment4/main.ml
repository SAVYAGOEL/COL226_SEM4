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

(* Pretty-printer for AST - adapted from your first main.ml *)
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
  | Index (e1, e2) -> Printf.sprintf "Index(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | IndexMat (e1, e2, e3) -> Printf.sprintf "IndexMat(%s, %s, %s)" (expr_to_string e1) (expr_to_string e2) (expr_to_string e3)
  | AssignExpr (e1, e2) -> Printf.sprintf "AssignExpr(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | Seq (e1, e2) -> Printf.sprintf "Seq(%s, %s)" (expr_to_string e1) (expr_to_string e2)
  | If (c, t, e) -> Printf.sprintf "If(%s, %s, %s)" (expr_to_string c) (expr_to_string t) (expr_to_string e)
  | For (v, s, e, b) -> Printf.sprintf "For(%s, %s, %s, %s)" v (expr_to_string s) (expr_to_string e) (expr_to_string b)
  | While (c, b) -> Printf.sprintf "While(%s, %s)" (expr_to_string c) (expr_to_string b)
  | Print s -> Printf.sprintf "Print(\"%s\")" s
  | Input s -> Printf.sprintf "Input(\"%s\")" s
  | VarType (t, v, None) -> Printf.sprintf "VarType(%s, %s, None)" t v
  | VarType (t, v, Some (n, None)) -> Printf.sprintf "VarType(%s, %s, Some(%d, None))" t v n
  | VarType (t, v, Some (r, Some c)) -> Printf.sprintf "VarType(%s, %s, Some(%d, Some %d))" t v r c

(* Print position for error reporting *)
let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* Test runner *)
let run_test num input =
  Printf.printf "\nTest %d:\nInput: %s\n" num input;
  let lexbuf = Lexing.from_string input in
  try
    let ast = Parser.program Lexer.tokenize lexbuf in  (* Use your parser *)
    Printf.printf "AST: %s\n" (expr_to_string ast);
    typecheck_program ast;  (* Your type checker *)
    Printf.printf "Type Check: Passed\n"
  with
  | Parsing.Parse_error ->
      Printf.printf "Type Check: Failed - Syntax error at %s\n" (print_position lexbuf)
  | TypeError msg ->
      Printf.printf "Type Check: Failed - %s\n" msg
  | e ->
      Printf.printf "Type Check: Failed - Unexpected error at %s: %s\n" (print_position lexbuf) (Printexc.to_string e)

(* Main function with test cases *)
let main () =
  let tests = [
    (* 1. Basic Declaration and Assignment *)
    (1, "int x := 42; Print(x);");

    (* 2. Float Arithmetic *)
    (2, "float x := 5.5 + 3.14; Print(x);");

    (* 3. Type Mismatch *)
    (3, "int x := 3.14; Print(x);");

    (* 4. Vector Declaration and Print *)
    (4, "vector 3 v := 3 [1, 2, 3]; Print(v);");

    (* 5. Vector Type Mismatch *)
    (5, "vector 3 v := 3 [1.5, 2.5, 3.5]; Print(v);");

    (* 6. If with Scoping *)
    (6, "int x := 5; if (x > 0) then (int y := 10; Print(y);) else (Print(x););");

    (* 7. Variable Shadowing *)
    (7, "int x := 5; int x := 10; Print(x);");

    (* 8. Scoping Error *)
    (8, "if (true) then (int x := 5;) else (Print(y);); x := x - 1;");

    (* 9. Invalid For Loop Bounds *)
    (9, "for (x := 1 to 3.14) do (Print(x););");

    (* 10. Nested For with Shadowing *)
    (10, "int x := 0; for (i := 1 to 3) do (int x := i; Print(x);); Print(x);");

    (* 11. Invalid If Condition *)
    (11, "if (5) then (Print(x);) else (Print(y););");

    (* 12. Matrix Dimension Mismatch *)
    (12, "matrix 2,2 m := 2,3 [[1, 2, 3], [4, 5, 6]]; Print(m);");

    (* 13. Input with Declaration *)
    (13, "int x; Input(Enter); Print(x);");

    (* 14. Standalone Input *)
    (14, "Input(x);");

    (* 15. Mixed Type Arithmetic *)
    (15, "float x := 5 + 3.14; Print(x);");

    (* 16. While with Scoping *)
    (16, "int x := 3; while (x > 0) do (int y := x; Print(y); x := x - 1;);");

    (* 17. Invalid While Condition *)
    (17, "while (5) do (Print(x););");

    (* 18. Vector Index Type Error *)
    (18, "vector 2 v := 2 [1, 2]; v[true] := 3;");

    (* 19. Multiple Declarations *)
    (19, "int x := 5; float x := 3.14; Print(x);");

    (* 20. Scale Operation *)
    (20, "vector 3 v := 3 [1, 2, 3]; v := scale 5 v; Print(v);");

    (* 21. Invalid Scale *)
    (21, "vector 3 v := 3 [1, 2, 3]; v := scale 2.5 v; Print(v);");

    (* 22. Vector Addition *)
    (22, "vector 3 v1 := 3 [1, 2, 3]; vector 3 v2 := 3 [4, 5, 6]; vector 3 v3 := addv v1 v2; Print(v3);");

    (* 23. Dot Product *)
    (23, "vector 3 v1 := 3 [1, 2, 3]; vector 3 v2 := 3 [4, 5, 6]; int d := dot_prod v1 v2; Print(d);");

    (* 24. Matrix Operations *)
    (24, "matrix 2,2 m := 2,2 [[1, 2], [3, 4]]; matrix 2,2 t := transpose m; Print(t);");

    (* 25. Determinant *)
    (25, "matrix 2,2 m := 2,2 [[1, 2], [3, 4]]; float d := det m; Print(d);");

    (* 26. Nested Control Flow *)
    (26, "int x := 0; for (i := 0 to 2) do (if (x < 1) then (x := x + 1; Print(x);) else (Print(done);););");

    (* 27. Syntax Error *)
    (27, "int x := 5 + ; Print(x);");

    (* 28. Matrix Indexing *)
    (28, "matrix 2,2 m := 2,2 [[1, 2], [3, 4]]; m[0,1] := 5; Print(m);");

    (* 29. Unary Operator *)
    (29, "int x := abs (-5); Print(x);");

    (* 30. Complex Scope and Operations *)
    (30, "int x := 10; if (x > 5) then (vector 3 v := 3 [1, 2, 3]; v := scale 2 v; Print(v);) else (Print(x););");

    (* 29. Unary Operator *)
    (29, "int x := abs (-5); Print(x);");

    (* 30. Complex Scope and Operations *)
    (30, "int x := 10; if (x > 5) then (vector 3 v := 3 [1, 2, 3]; v := scale 2 v; Print(v);) else (Print(x););");

    (* 31. Valid Matrix Multiplication *)
    (31, "matrix 2,3 m1 := 2,3 [[1, 2, 3], [4, 5, 6]]; matrix 3,2 m2 := 3,2 [[1, 2], [3, 4], [5, 6]]; matrix 2,2 m3 := m1 * m2; Print(m3);");

    (* 32. Invalid Matrix Multiplication *)
    (32, "matrix 2,3 m1 := 2,3 [[1, 2, 3], [4, 5, 6]]; matrix 2,3 m2 := 2,3 [[7, 8, 9], [10, 11, 12]]; matrix 2,3 m3 := m1 * m2; Print(m3);");

    (* 33. Scalar Multiplication *)
    (33, "int x := 5 * 3; Print(x);");

    (* 34. Mixed Scalar and Matrix Multiplication *)
    (34, "matrix 2,2 m := 2,2 [[1, 2], [3, 4]]; int x := 5; m := m * m; Print(m);");

  ] in
  List.iter (fun (num, input) -> run_test num input) tests

(* Execute main *)
let () = main ()