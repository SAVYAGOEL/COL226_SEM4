open Vector;;
(* Test cases for the vector module. *)

(* Test the create function. *)

let () =
  Printf.printf "Testing create...\n";
  let v1 = Vector.create 3 1.5 in
  let v2 = Vector.create 5 0. in
  let v3 = Vector.create 1 (-2.3) in
  assert (v1 = [1.5; 1.5; 1.5]);
  assert (v2 = [0.; 0.; 0.; 0.; 0.]);
  assert (v3 = [-2.3]);
  let test_dimension_error n x =
    try
      let _ = Vector.create n x in
      Printf.printf "FAILED: create(%d, %f) should raiseDimensionError\n" n x
    with
    | DimensionError _ -> Printf.printf "PASSED: create(%d, %f) raisedDimensionError\n" n x  (* Here assumed DimensionError of string, can be 0-ary also*)
    | _ -> Printf.printf "FAILED: create(%d, %f) raised the wrong exception\n" n x
  in
  test_dimension_error 0 1.0;
  test_dimension_error (-2) 3.5;

  Printf.printf "Testing dim...\n";
  assert (Vector.dim v1 = 3);
  assert (Vector.dim v2 = 5);
  assert (Vector.dim v3 = 1);
  Printf.printf "dim passed!\n";

  Printf.printf "Testing is_zero...\n";
  assert (Vector.is_zero [0.; 0.; 0.]);
  assert (not (Vector.is_zero [0.; 1.; 0.]));
  assert (not (Vector.is_zero [1.0]));
  Printf.printf "is_zero passed!\n";

  Printf.printf "Testing unit...\n";
  assert (Vector.unit 3 2 = [0.; 1.; 0.]);
  assert (Vector.unit 4 1 = [1.; 0.; 0.; 0.]);
  assert (Vector.unit 5 5 = [0.; 0.; 0.; 0.; 1.]);

  Printf.printf "unit passed for valid inputs!\n";
  let test_unit_error n j =
    try
      let _ = Vector.unit n j in
      Printf.printf "FAILED: unit(%d, %d) should raiseDimensionError\n" n j
    with
    | DimensionError _ -> Printf.printf "PASSED: unit(%d, %d) raisedDimensionError\n" n j (* Here assumed DimensionError of string, can be 0-ary also*)
    | _ -> Printf.printf "FAILED: unit(%d, %d) raised the wrong exception\n" n j
  in

  test_unit_error 3 0;   (* j < 1 *)
  test_unit_error 3 4;   (* j > n *)
  test_unit_error 0 1;   (* n < 1 *)
  test_unit_error (-2) 1; (* n < 1 *)
  Printf.printf "unit passed!\n";

  Printf.printf "Testing addv...\n";
  assert (Vector.addv [1.; 2.; 3.] [4.; 5.; 6.] = [5.; 7.; 9.]);
  assert (Vector.addv [0.; 0.] [0.; 0.] = [0.; 0.]);
  Printf.printf "addv passed!\n";

  let test_addv_error v1 v2 =
    try
      let _ = Vector.addv v1 v2 in
      Printf.printf "FAILED: addv %s + %s should raiseDimensionError\n"
        (String.concat ", " (List.map string_of_float v1))
        (String.concat ", " (List.map string_of_float v2))
    with
    | DimensionError _ -> Printf.printf "PASSED: addv with mismatched dimensions raisedDimensionError\n"
    | _ -> Printf.printf "FAILED: addv raised the wrong exception\n"
  in
  test_addv_error [1.; 2.] [1.; 2.; 3.];

  Printf.printf "Testing dot_prod...\n";
assert (Vector.dot_prod [1.; 2.; 3.] [4.; 5.; 6.] = 32.);
assert (Vector.dot_prod [1.; 0.; -1.] [0.; 1.; 0.] = 0.);
Printf.printf "dot_prod passed!\n";

(* Test dot_prod exception cases *)
let test_dot_prod_error v1 v2 =
  try
    let _ = Vector.dot_prod v1 v2 in
    Printf.printf  "FAILED: dot_prod %s . %s should raiseDimensionError\n"
      (String.concat ", " (List.map string_of_float v1))
      (String.concat ", " (List.map string_of_float v2))
  with
  | DimensionError _ -> Printf.printf  "PASSED: dot_prod with mismatched dimensions raisedDimensionError\n"
  | _ -> Printf.printf  "FAILED: dot_prod raised the wrong exception\n"
in
test_dot_prod_error [1.; 2.] [1.; 2.; 3.];

Printf.printf "Testing inv...\n";
assert (Vector.inv [1.; -2.; 3.] = [-1.; 2.; -3.]);
assert (Vector.inv [0.; 0.; 0.] = [0.; 0.; 0.]);
Printf.printf "inv passed!\n";

Printf.printf "Testing length...\n";
assert (Vector.length [3.; 4.] = 5.);
assert (Vector.length [0.; 0.; 0.] = 0.);
Printf.printf "length passed!\n";

Printf.printf "Testing angle...\n";
assert (abs_float (Vector.angle [1.; 0.] [0.; 1.] -. (Float.pi /. 2.)) < 1e-6);
assert (abs_float (Vector.angle [1.; 1.] [1.; 1.] -. 0.) < 1e-6);
assert (abs_float (Vector.angle [1.; 0.; 0.] [0.; 1.; 0.] -. (Float.pi /. 2.)) < 1e-6);
assert (abs_float (Vector.angle [1.; 2.; 3.] [-1.; -2.; -3.] -. Float.pi) < 1e-6);
assert (abs_float (Vector.angle [1.; 2.; 2.] [2.; 1.; 2.] -. 0.4758822496) < 1e-6);
Printf.printf "angle passed!\n";

let test_angle_error v1 v2 =
  try
    let _ = Vector.angle v1 v2 in
    Printf.printf "FAILED: angle %s , %s should raiseDimensionError\n"
      (String.concat ", " (List.map string_of_float v1))
      (String.concat ", " (List.map string_of_float v2))
  with
  | DimensionError _ -> Printf.printf  "PASSED: angle with mismatched dimensions raisedDimensionError\n"
  | _ -> Printf.printf "FAILED: angle raised the wrong exception\n"
in
test_angle_error [1.; 2.] [1.; 2.; 3.];

Printf.printf "Testing add+dotprod...\n";

let v1 = [1.; 2.; 3.] in
let v2 = [4.; -1.; 0.] in

let v3 = Vector.addv v1 v2 in
assert (v3 = [5.; 1.; 3.]);

let dp1 = Vector.dot_prod v3 v1 in
assert (dp1 = (5. *. 1.) +. (1. *. 2.) +. (3. *. 3.));

let dp2 = Vector.dot_prod v1 v2 in
let scaled_v1 = List.map (fun x -> x *. dp2) v1 in
assert (scaled_v1 = [2.; 4.; 6.]);

Printf.printf "add+dotprod tests passed!\n";
Printf.printf "All tests passed!\n";
