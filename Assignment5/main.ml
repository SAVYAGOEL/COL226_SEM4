open Ast
open Lexer
open Parser
open Evaluator

let print_section_header title =
  Printf.printf "\n=== %s ===\n" title

let print_test_case name result expected =
  Printf.printf "%-50s: %-25s (Expected: %s)\n" name (string_of_value result) expected

let print_error_case name error =
  Printf.printf "%-50s: ERROR: %s\n" name error

let run_test program name expected =
  try
    let lexbuf = Lexing.from_string program in
    let ast = Parser.program Lexer.tokenize lexbuf in
    let _ = typecheck_program ast in (* Ensure type safety *)
    let (result, _) = eval Env.empty ast in (* Extract value, discard env *)
    print_test_case name result expected
  with
  | Eval_error msg -> print_error_case name ("Eval_error: " ^ msg)
  | TypeError msg -> print_error_case name ("TypeError: " ^ msg)
  | Failure _ -> print_error_case name "Parse error"
  | _ -> print_error_case name "Unexpected error"

let () =
  (* Section 1: Invalid Programs - Parsing and Lexing Errors *)
  print_section_header "Invalid Programs - Parsing and Lexing Errors";
  run_test "int x := 5 + ; Print(x);" "Missing operand" "Parse error";
  run_test "int x := 5 + 3 Print(x);" "Missing semicolon" "Parse error";
  run_test "int x = 5;" "Missing colon" "Parse error";
  run_test "if (true) then { x := 1 };" "Missing else" "Parse error"; (* Your grammar requires ELSE *)
  run_test "for (i := 0 to 3 { x := x + i; }" "Unclosed for loop" "Parse error";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4;" "Unclosed matrix" "Parse error";

  (* Section 2: Invalid Programs - Type Errors *)
  print_section_header "Invalid Programs - Type Errors";
  run_test "int x := 5 + true; Print(x);" "Int plus bool" "TypeError: Incompatible types for arithmetic operation";
  run_test "bool x := 5; Print(x);" "Assign int to bool" "TypeError: Type mismatch in assignment to variable x";
  run_test "int x := 3; float y := x + true; Print(y);" "Mixed type error" "TypeError: Incompatible types for arithmetic operation";
  run_test "int vector 3 v := 3 [1, 2, 3]; int x := v + 5; Print(x);" "Vector plus scalar" "TypeError: Incompatible types for arithmetic operation";
  run_test "int matrix 2, 2 m := 2, 2 [[1, 2], [3, 4]]; bool b := m; Print(b);" "Matrix to bool" "TypeError: Type mismatch in assignment to variable b";
  run_test "int x := 5; if (x) then { Print(Hi); } else { Print(Hey); };" "Non-boolean condition" "TypeError: If condition must be boolean";
  run_test "for (i := true to 3) do { Print(i); };" "Non-int for bound" "TypeError: For loop bounds must be integers";

  (* Section 3: Invalid Programs - Runtime Errors *)
  print_section_header "Invalid Programs - Runtime Errors";
  run_test "int x := 6 / 0; Print(x);" "Division by zero" "Eval_error: Division by zero";
  run_test "int vector 3 v := 3 [1, 2]; Print(v);" "Vector dimension mismatch" "Eval_error: Vector dimension mismatch: expected 3, got 2";
  run_test "int matrix 2, 2 m := 2, 2 [[1, 2], [3]]; Print(m);" "Matrix dimension mismatch" "Eval_error: Matrix dimension mismatch: expected 2x2, got 2x1";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; int matrix 2, 3 B := 2, 3 [[1, 2, 3], [4, 5, 6]]; int matrix 2, 2 C := A * B; Print(C);" "Matrix mult dimension mismatch" "Eval_error: Matrix multiplication dimension mismatch";
  run_test "int vector 2 v := Input(error.txt); Print(v);" "Invalid file input (assume error.txt contains 'bool x := true;')" "Eval_error: Input: invalid syntax in 'bool x := true;'";
  run_test "float matrix 2, 2 A := 2, 2 [[1.0, 0.0], [0.0, 0.0]]; float matrix 2, 2 invA := inv(A); Print(invA);" "Singular matrix inverse" "Eval_error: Inv: matrix is singular";
  run_test "int vector 3 v := 3 [1, 2, 3]; int x := v[5]; Print(x);" "Vector index out of bounds" "Eval_error: Index: out of bounds 5";
  run_test "int matrix 2, 2 m := 2, 2 [[1, 2], [3, 4]]; int x := m[2, 0]; Print(x);" "Matrix row index out of bounds" "Eval_error: IndexMat: row index 2 out of bounds";

  (* Section 4: Valid Programs *)
  print_section_header "Valid Programs - Basic Operations";
  run_test "int x := 5 + 3; Print(x);" "Integer Addition" "unit (prints 8)";
  run_test "float x := 5.5 + 3.2; Print(x);" "Float Addition" "unit (prints 8.7)";
  run_test "int x := 5 - 2; Print(x);" "Integer Subtraction" "unit (prints 3)";
  run_test "float x := (-5.5); Print(x);" "Unary Minus Float" "unit (prints -5.5)";
  run_test "int x := 4 * 2; Print(x);" "Integer Multiplication" "unit (prints 8)";
  run_test "float x := 4.0 * 2.5; Print(x);" "Float Multiplication" "unit (prints 10)";
  run_test "int x := exp(2, 3); Print(x);" "Exponentiation Integer" "unit (prints 8)";
  run_test "float x := exp(2.0, 3.0); Print(x);" "Exponentiation Float" "unit (prints 8)";
  run_test "bool x := true and false; Print(x);" "Logical And" "unit (prints false)";
  run_test "bool x := true or false; Print(x);" "Logical Or" "unit (prints true)";
  run_test "bool x := not(true); Print(x);" "Logical Not" "unit (prints false)";
  run_test "int x := abs((-5)); Print(x);" "Absolute Value Integer" "unit (prints 5)";
  run_test "float x := sqrt(16.0); Print(x);" "Square Root Float" "unit (prints 4)";
  run_test "int vector 3 v := 3 [1, 2, 3]; int vector 3 w := addv(v, v); Print(w);" "Vector Addition" "unit (prints [2, 4, 6])";
  run_test "int vector 3 v := 3 [1, 2, 3]; int x := dot_prod(v, v); Print(x);" "Dot Product Integer" "unit (prints 14)";
  run_test "int vector 3 v := 3 [3, 4, 5]; float x := len(v); Print(x);" "Vector Length" "unit (prints 7.071068)";
  run_test "int vector 2 v1 := 2 [1, 0]; int vector 2 v2 := 2 [0, 1]; float x := angle(v1, v2); Print(x);" "Vector Angle" "unit (prints 1.570796)";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; int matrix 2, 2 B := 2, 2 [[5, 6], [7, 8]]; int matrix 2, 2 sum := A + B; Print(sum);" "Matrix Addition" "unit (prints [[6, 8], [10, 12]])";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; int matrix 2, 2 prod := A * A; Print(prod);" "Matrix Multiplication" "unit (prints [[7, 10], [15, 22]])";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; int vector 2 x := 2 [5, 6]; int vector 2 result := A * x; Print(result);" "Matrix-Vector Multiplication" "unit (prints [17, 39])";

  print_section_header "Valid Programs - Control Flow";
  run_test "int x := 0; if (true) then { x := 1; } else { x := 2; }; Print(x);" "If True" "unit (prints 1)";
  run_test "int x := 0; if (false) then { x := 1; } else { x := 2; }; Print(x);" "If False" "unit (prints 2)";
  run_test "int x := 0; for (i := 0 to 3) do { x := x + i; }; Print(x);" "For Loop Ascending" "unit (prints 6)";
  run_test "int x := 0; for (i := 3 to 0) do { x := x + i; }; Print(x);" "For Loop Descending" "unit (prints 6)";
  run_test "int x := 0; while (x < 3) do { x := x + 1; }; Print(x);" "While Loop" "unit (prints 3)";
  run_test "int x := 0; if (true) then { for (i := 0 to 1) do { x := x + i; }; } else { x := 5; }; Print(x);" "Nested If-For" "unit (prints 1)";

  print_section_header "Valid Programs - Input and Print";
  run_test "int vector 2 x := Input(data.txt); Print(x);" "Input from file (data.txt: '2 [3, 4];')" "unit (prints [3, 4])";
  run_test "int x := 5; Print(x);" "Print variable" "unit (prints 5)";
  run_test "Print(hello);" "Print string (hello)" "unit (prints hello)";
  run_test "float matrix 2, 2 A := Input(matrix.txt); Print(A);" "Input matrix (matrix.txt: '2, 2 [[1.0, 2.0], [3.0, 4.0]];')" "unit (prints [[1, 2], [3, 4]])";

  print_section_header "Valid Programs - Complex Nested Programs";
  run_test "int x := 0; for (i := 0 to 2) do { int y := i; if (y > 0) then { x := x + y; } else { x := x + 1; }; }; Print(x);" "Nested For-If" "unit (prints 4)";
  run_test "int matrix 2, 2 m := 2, 2 [[1, 2], [3, 4]]; int s := 0; for (i := 0 to 1) do { for (j := 0 to 1) do { s := s + m[i, j]; }; }; Print(s);" "Nested Loops with Matrix" "unit (prints 10)";
  run_test "int x := 0; while (x < 5) do { if (x < 3) then { x := x + 1; } else { x := x + 2; }; }; Print(x);" "Nested While-If" "unit (prints 5)";

  print_section_header "Valid Programs - Proper Programs";
  run_test "float matrix 2, 2 A := 2, 2 [[1.0, 2.0], [3.0, 4.0]]; float a := A[0, 0]; float b := A[0, 1]; float c := A[1, 0]; float d := A[1, 1];
    float trace := a + d; float determinant := det(A); float D := trace * trace - 4.0 * determinant;
    if (D >= 0.0) then { float eigenvalue1 := (trace + exp(D, 0.5)) / 2.0; float eigenvalue2 := (trace - exp(D, 0.5)) / 2.0; Print(eigenvalue1); Print(eigenvalue2); } else { Print(trace); };"
    "Eigenvalues of 2x2 Matrix" "unit (prints 5.37228132327, -0.372281323269)";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; int matrix 2, 2 B := 2, 2 [[5, 6], [7, 8]]; int matrix 2, 2 sum := A + B; Print(sum);" "Matrix Sum" "unit (prints [[6, 8], [10, 12]])";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; int matrix 2, 2 t := transpose(A); Print(t);" "Matrix Transpose" "unit (prints [[1, 3], [2, 4]])";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; int d := det(A); Print(d);" "Matrix Determinant" "unit (prints -2)";
  run_test "float matrix 2, 2 A := 2, 2 [[1.0, 2.0], [3.0, 4.0]]; float d := det(A); if (d != 0.0) then { float matrix 2, 2 invA := inv(A); Print(invA); } else { Print(d); };"
    "Matrix Inverse" "unit (prints [[-2, 1], [1.5, -0.5]])";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; int matrix 2, 2 B := 2, 2 [[5, 6], [7, 8]]; int matrix 2, 2 prod := A * B; Print(prod);" "Matrix Multiplication" "unit (prints [[19, 22], [43, 50]])";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; int vector 2 x := 2 [5, 6]; int vector 2 result := A * x; Print(result);" "Matrix-Vector Product" "unit (prints [17, 39])";
  run_test "float matrix 2, 2 A := 2, 2 [[1.0, 2.0], [3.0, 4.0]]; float vector 2 b := 2 [5.0, 11.0]; float d := det(A); if (d != 0.0) then { float matrix 2, 2 invA := inv(A); float vector 2 x := invA * b; Print(x); } else { Print(d); };"
    "Gaussian Elimination" "unit (prints [1, 2])";
  run_test "int matrix 5, 3 V := 5, 3 [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]]; int vector 3 sum := 3 [0, 0, 0]; for (i := 0 to 4) do { sum := sum + V[i]; }; Print(sum);"
    "Vector Sum in For Loop" "unit (prints [35, 40, 45])";
  run_test "int matrix 2, 2 A := 2, 2 [[1, 2], [3, 4]]; float sum_sq := 0.0; for (i := 0 to 1) do { for (j := 0 to 1) do { sum_sq := sum_sq + A[i, j] * A[i, j]; }; }; float mag := exp(sum_sq, 0.5); Print(mag);"
    "Matrix Magnitude" "unit (prints 5.477226)";
  (* run_test "float matrix 2, 2 A := 2, 2 [[1.0, 2.0], [3.0, 4.0]]; float threshold := 0.000001; float norm_diff := exp(30.0, 0.5); while (norm_diff > threshold) do { A := A * A; float sum_sq := 0.0; for (i := 0 to 1) do { for (j := 0 to 1) do { sum_sq := sum_sq + A[i, j] * A[i, j]; }; }; norm_diff := exp(sum_sq, 0.5) - threshold; }; Print(A);"
    "While Loop Matrix Power" "unit (prints large matrix)"; *)
  (* Additional Proper Programs *)
  run_test "int matrix 3, 3 M := 3, 3 [[1, 2, 3], [4, 5, 6], [7, 8, 9]]; int d := det(M); Print(d);" "3x3 Matrix Determinant" "unit (prints 0)";
  run_test "float matrix 2, 2 A := 2, 2 [[1.0, 2.0], [3.0, 4.0]]; float vector 2 v := 2 [1.0, 1.0]; float vector 2 Av := A * v; float mag := sqrt(dot_prod(Av, Av)); Print(mag);" "Matrix-Vector Magnitude" "unit (prints 7.61577310586)";

  (* New Demo Test Cases Section *)
  print_section_header "Demo Test Cases";
  run_test "float matrix 6, 6 A := Input(t1_A.txt); 
            float vector 6 b := Input(t1_b.txt); 
            float matrix 6, 6 A_T := transpose(A); 
            float matrix 6, 6 A_TA := A_T * A; 
            float d := det(A_TA); 
            if (d != 0.0) then { 
              float matrix 6, 6 A_TA_inv := inv(A_TA); 
              float vector 6 A_Tb := A_T * b; 
              float vector 6 theta := A_TA_inv * A_Tb; 
              Print(theta); 
            } else { 
              Print(Noinverse); 
            };" 
           "Demo t1 - Least Squares Solution" "unit (prints theta or 'Noinverse')";
  run_test "float matrix 4, 5 A := Input(t2_A.txt); 
            float matrix 4, 5 B := Input(t2_B.txt); 
            float matrix 5, 4 D := Input(t2_D.txt); 
            float vector 4 u := Input(t2_u.txt); 
            float matrix 4, 5 C := A + B; 
            float matrix 4, 4 E := C * D; 
            float d := det(E); 
            if (d != 0.0) then { 
              float matrix 4, 4 E_inverse := inv(E); 
              float vector 4 x := E_inverse * u; 
              Print(x); 
            } 
            else { Print(Noinverse); };" 
           "Demo t2 - Matrix Equation Solution" "unit (prints x or 'Noinverse')";
  run_test "float vector 4 v := Input(t3_v.txt); 
            float sum_result := 0.0; 
            for (i := 0 to 3) do {
              sum_result := sum_result + v[i]; 
            }; 
            float ans := 2.5 * sum_result; 
            Print(ans);" 
           "Demo t3 - Vector Sum Scaling" "unit (prints scaled sum)";
  run_test "float matrix 4, 5 A := Input(t4_A.txt); 
            float threshold := 0.001; 
            float sum_of_squares := 0.0; 
            for (i := 0 to 3) do { 
              for (j := 0 to 4) do { 
                sum_of_squares := sum_of_squares + A[i, j] * A[i, j]; 
              }; 
            }; 
            float norm := sqrt(sum_of_squares); 
            while (norm > threshold) do { 
              for (i := 0 to 3) do { 
                for (j := 0 to 4) do { 
                  A[i, j] := A[i, j] * 0.5; 
                }; 
              }; 
              sum_of_squares := 0.0; 
              for (i := 0 to 3) do {
                for (j := 0 to 4) do { 
                  sum_of_squares := sum_of_squares + A[i, j] * A[i, j]; 
                }; 
              }; 
              norm := sqrt(sum_of_squares); 
            }; 
            Print(A);" 
           "Demo t4 - Matrix Normalization" "unit (prints normalized matrix)";
  run_test "float matrix 3, 2 A := Input(t5_A.txt); 
            float matrix 2, 3 B := Input(t5_B.txt); 
            float matrix 3, 2 C := A + B; 
            Print(C);" 
           "Demo t5 - Matrix Addition" "unit (prints 3x2 matrix)";
  run_test "float matrix 3, 4 A := Input(t6_A.txt); 
            float matrix 5, 2 B := Input(t6_B.txt); 
            float matrix 3, 2 C := A * B; 
            Print(C);" 
           "Demo t6 - Matrix Multiplication" "unit (prints 3x2 matrix)";
  run_test "float matrix 3, 3 A := Input(t7_A.txt); 
            float vector 3 v := Input(t7_v.txt); 
            float matrix 3, 3 C := A + v; 
            Print(C);" 
           "Demo t7 - Matrix-Vector Addition" "unit (prints 3x3 matrix)";
  run_test "float matrix 2, 2 A := Input(t8_A.txt); 
            float d := det(A); 
            if (d != 0.0) then { 
              float matrix 2, 2 A_inv := inv(A); 
              Print(A_inv); 
            } 
            else { Print(Noinverse); };" 
           "Demo t8 - Matrix Inverse (Singular)" "unit (prints 'Noinverse')";
  run_test "float matrix 3, 3 A := 3, 3 [[1.0, 4.0, 3.0], [7.0, 45.0, 87.0], [45.0, 21.0, 4.0]];
            float matrix 3, 3 B := inv(A);
            float matrix 3, 3 C := B * A;
            Print(C);"
            "Sample Matrix Inverse Test" "unit (prints identity matrix)";

  Printf.printf "\nTesting completed.\n"