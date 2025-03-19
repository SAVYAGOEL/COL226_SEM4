(* main.ml *)

open Ast

(* Convert AST to string for printing *)
let rec string_of_expr = function
  | IntLit n -> Printf.sprintf "IntLit(%d)" n
  | FloatLit f -> Printf.sprintf "FloatLit(%f)" f
  | BoolLit b -> Printf.sprintf "BoolLit(%b)" b
  | VecLit (IntVec (n, s)) -> Printf.sprintf "VecLit(IntVec(%d, %s))" n s
  | VecLit (FloatVec (n, s)) -> Printf.sprintf "VecLit(FloatVec(%d, %s))" n s
  | MatLit (IntMat (r, c, s)) -> Printf.sprintf "MatLit(IntMat(%d, %d, %s))" r c s
  | MatLit (FloatMat (r, c, s)) -> Printf.sprintf "MatLit(FloatMat(%d, %d, %s))" r c s
  | Var v -> Printf.sprintf "Var(%s)" v
  | Plus (e1, e2) -> Printf.sprintf "Plus(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Minus (e1, e2) -> Printf.sprintf "Minus(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Gt (e1, e2) -> Printf.sprintf "Gt(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Lt (e1, e2) -> Printf.sprintf "Lt(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Assign (v, e) -> Printf.sprintf "Assign(%s, %s)" v (string_of_expr e)
  | Seq (e1, e2) -> Printf.sprintf "Seq(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | VarType (t, v, None) -> Printf.sprintf "VarType(%s, %s, None)" t v
  | VarType (t, v, Some (n, None)) -> Printf.sprintf "VarType(%s, %s, %d)" t v n
  | VarType (t, v, Some (r, Some c)) -> Printf.sprintf "VarType(%s, %s, %d, %d)" t v r c
  | Print v -> Printf.sprintf "Print(%s)" v
  | Input v -> Printf.sprintf "Input(%s)" v
  | If (cond, thn, els) -> Printf.sprintf "If(%s, %s, %s)" (string_of_expr cond) (string_of_expr thn) (string_of_expr els)
  | For (v, s, e, b) -> Printf.sprintf "For(%s, %s, %s, %s)" v (string_of_expr s) (string_of_expr e) (string_of_expr b)
  | While (c, b) -> Printf.sprintf "While(%s, %s)" (string_of_expr c) (string_of_expr b)
  | Index (v, i) -> Printf.sprintf "Index(%s, %s)" (string_of_expr v) (string_of_expr i)
  | _ -> "ComplexExpr" (* Simplified for brevity *)

(* Helper to print test results without stopping *)
let run_test test_num input ast =
  Printf.printf "Test %d:\nInput: %s\nAST: %s\n" test_num input (string_of_expr ast);
  try
    typecheck_program ast;
    Printf.printf "Type Check: Passed\n\n"
  with
  | TypeError msg -> Printf.printf "Type Check: Failed - %s\n\n" msg
  | _ -> Printf.printf "Type Check: Failed - Unexpected error\n\n"

let main () =
  (* Test 1: Basic valid scoping *)
  run_test 1 "int x := 42; Print(x);"
    (Seq (VarType ("int", "x", None), Seq (Assign ("x", IntLit 42), Print "x")));

  (* Test 2: Undefined variable *)
  run_test 2 "Print(x);"
    (Print "x");

  (* Test 3: Type mismatch in assignment *)
  run_test 3 "int x := 3.14;"
    (Seq (VarType ("int", "x", None), Assign ("x", FloatLit 3.14)));

  (* Test 4: Valid vector assignment *)
  run_test 4 "vector 3 v := 3 [1, 2, 3]; Print(v);"
    (Seq (VarType ("vector", "v", Some (3, None)), Seq (Assign ("v", VecLit (IntVec (3, "[1, 2, 3]"))), Print "v")));

  (* Test 5: Vector type mismatch *)
  run_test 5 "vector 3 v := 3 [1.5, 2.5, 3.5];"
    (Seq (VarType ("vector", "v", Some (3, None)), Assign ("v", VecLit (FloatVec (3, "[1.5, 2.5, 3.5]")))));

  (* Test 6: Scoping in if *)
  run_test 6 "int x := 5; if (x > 0) then (int y := 10; Print(y);) else (Print(x););"
    (Seq (VarType ("int", "x", None),
          Seq (Assign ("x", IntLit 5),
               If (Gt (Var "x", IntLit 0),
                   Seq (VarType ("int", "y", None), Seq (Assign ("y", IntLit 10), Print "y")),
                   Print "x"))));

  (* Test 7: Variable shadowing *)
  run_test 7 "int x := 5; int x := 10; Print(x);"
    (Seq (VarType ("int", "x", None),
          Seq (Assign ("x", IntLit 5),
               Seq (VarType ("int", "x", None), Seq (Assign ("x", IntLit 10), Print "x")))));

  (* Test 8: Scoping leak *)
  run_test 8 "if (true) then (int x := 5;); Print(x);"
    (Seq (If (BoolLit true, Seq (VarType ("int", "x", None), Assign ("x", IntLit 5)), IntLit 0), Print "x"));

  (* Test 9: Invalid for loop variable *)
  run_test 9 "for (x := 1 to 3.14) do (Print(x););"
    (For ("x", IntLit 1, FloatLit 3.14, Print "x"));

  (* Test 10: Valid nested scoping *)
  run_test 10 "int x := 0; for (i := 1 to 3) do (int x := i; Print(x);); Print(x);"
    (Seq (VarType ("int", "x", None),
          Seq (Assign ("x", IntLit 0),
               Seq (For ("i", IntLit 1, IntLit 3,
                         Seq (VarType ("int", "x", None), Seq (Assign ("x", Var "i"), Print "x"))),
                    Print "x"))));

  (* Test 11: Type mismatch in condition *)
  run_test 11 "if (5) then (Print(x);) else (Print(y););"
    (If (IntLit 5, Print "x", Print "y"));

  (* Test 12: Matrix dimension mismatch *)
  run_test 12 "matrix 2,2 m := 2,3 [[1, 2, 3], [4, 5, 6]];"
    (Seq (VarType ("matrix", "m", Some (2, Some 2)),
          Assign ("m", MatLit (IntMat (2, 3, "[[1, 2, 3], [4, 5, 6]]")))));

  (* Test 13: Valid input *)
  run_test 13 "int x; Input(x); Print(x);"
    (Seq (VarType ("int", "x", None), Seq (Input "x", Print "x")));

  (* Test 14: Input undefined variable *)
  run_test 14 "Input(x);"
    (Input "x");

  (* Test 15: Mixed type arithmetic *)
  run_test 15 "int x := 5 + 3.14;"
    (Seq (VarType ("int", "x", None), Assign ("x", Plus (IntLit 5, FloatLit 3.14))));

  (* Test 16: Valid while with scoping *)
  run_test 16 "int x := 3; while (x > 0) do (int y := x; Print(y); x := x - 1;);"
    (Seq (VarType ("int", "x", None),
          Seq (Assign ("x", IntLit 3),
               While (Gt (Var "x", IntLit 0),
                      Seq (VarType ("int", "y", None),
                           Seq (Assign ("y", Var "x"),
                                Seq (Print "y", Assign ("x", Minus (Var "x", IntLit 1)))))))));

  (* Test 17: Invalid while condition *)
  run_test 17 "while (5) do (Print(x););"
    (While (IntLit 5, Print "x"));

  (* Test 18: Vector index type mismatch *)
  run_test 18 "vector 2 v := 2 [1, 2]; int x := v[true];"
    (Seq (VarType ("vector", "v", Some (2, None)),
          Seq (Assign ("v", VecLit (IntVec (2, "[1, 2]"))),
               Assign ("x", Index (Var "v", BoolLit true)))));

  (* Test 19: Multiple declarations *)
  run_test 19 "int x := 5; float x := 3.14; Print(x);"
    (Seq (VarType ("int", "x", None),
          Seq (Assign ("x", IntLit 5),
               Seq (VarType ("float", "x", None), Seq (Assign ("x", FloatLit 3.14), Print "x")))));

  (* Test 20: Valid type coercion *)
  run_test 20 "float x := 5 + 2.5; Print(x);"
    (Seq (VarType ("float", "x", None), Seq (Assign ("x", Plus (IntLit 5, FloatLit 2.5)), Print "x")));

  (* Test 21: Invalid AST - non-unit program *)
  run_test 21 "int x := 5;"
    (Seq (VarType ("int", "x", None), Assign ("x", IntLit 5)));

  (* Test 22: Deep nesting *)
  run_test 22 "int x := 0; if (true) then (for (i := 1 to 2) do (while (x < 1) do (x := x + 1;););); Print(x);"
    (Seq (VarType ("int", "x", None),
          Seq (Assign ("x", IntLit 0),
               Seq (If (BoolLit true,
                        For ("i", IntLit 1, IntLit 2,
                             While (Lt (Var "x", IntLit 1),
                                    Assign ("x", Plus (Var "x", IntLit 1)))),
                        IntLit 0),
                    Print "x"))))

let () = main ()