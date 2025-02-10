open A2

(* ------------------TEST CASES (CORNER AND EDGE CASES)(type checker)------------------- *)

(* Test case 1 : Testing raise Wrong cases*)

let%test "type_of (Add (ConstS 2.5, ConstV [3.5; 4.5])) raises Wrong (Add (ConstS 2.5, ConstV [3.5; 4.5]))" =
  (try
    let _ = type_of (Add (ConstS 2.5, ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Add (ConstS 2.5, ConstV [3.5; 4.5])
  | _ -> false)

let%test "type_of (Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5])) raises Wrong (Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5]))" =
  (try
    let _ = type_of (Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5])) in false
  with
  | Wrong e -> e = Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5])
  | _ -> false)

let%test "type_of (Add (T, ConstS 2.5)) raises Wrong (Add (T, ConstS 2.5))" =
  (try
    let _ = type_of (Add (T, ConstS 2.5)) in false
  with
  | Wrong e -> e = Add (T, ConstS 2.5)
  | _ -> false)

let%test "type_of (ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5])) raises Wrong (ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5]))" =
  (try
    let _ = type_of (ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5])
  | _ -> false)

let%test "type_of (DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) raises Wrong (DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5]))" =
  (try
    let _ = type_of (DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])
  | _ -> false)

let%test "type_of (Mag T) raises Wrong (Mag T)" =
  (try
    let _ = type_of (Mag T) in false
  with
  | Wrong e -> e = Mag T
  | _ -> false)

let%test "type_of (Angle (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) raises Wrong (Angle (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5]))" =
  (try
    let _ = type_of (Angle (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Angle (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])
  | _ -> false)

let%test "type_of (Cond (T, ConstS 2.5, ConstV [3.5; 4.5])) raises Wrong (Cond (T, ConstS 2.5, ConstV [3.5; 4.5]))" =
  (try
    let _ = type_of (Cond (T, ConstS 2.5, ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Cond (T, ConstS 2.5, ConstV [3.5; 4.5])
  | _ -> false)

let%test "type_of (Sub (ConstS 2.5, ConstV [3.5; 4.5])) raises Wrong (Sub (ConstS 2.5, ConstV [3.5; 4.5]))" =
  (try
    let _ = type_of (Sub (ConstS 2.5, ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Sub (ConstS 2.5, ConstV [3.5; 4.5])
  | _ -> false)

let%test "type_of (Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) raises Wrong (Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5]))" =
  (try
    let _ = type_of (Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])
  | _ -> false)

let%test "type_of (Add (ConstV [], ConstV [])) raises Wrong (Add (ConstV [], ConstV []))" =
  (try
    let _ = type_of (Add (ConstV [], ConstV [])) in false
  with
  | Wrong e -> e = Add (ConstV [], ConstV [])
  | _ -> false)

(* Test case 2 : Testing Nested expressions (3-4 levels deeep)*)

(* Nested Cond inside Add *)
let%test "type_of (Add (Cond (T, ConstS 1.0, ConstS 2.0), Cond (F, ConstS 3.0, ConstS 4.0)))" = 
  (type_of (Add (Cond (T, ConstS 1.0, ConstS 2.0), Cond (F, ConstS 3.0, ConstS 4.0))) = Scalar)

(* Nested ScalProd inside Angle *)
let%test "type_of (Angle (ScalProd (ConstS 2.0, ConstV [1.0; 0.0]), ScalProd (ConstS 3.0, ConstV [0.0; 1.0])))" = 
  (type_of (Angle (ScalProd (ConstS 2.0, ConstV [1.0; 0.0]), ScalProd (ConstS 3.0, ConstV [0.0; 1.0]))) = Scalar)

(* Deeply nested Add/Inv/Mag *)
let%test "type_of (Mag (Add (Inv (ConstV [3.0; 4.0]), ConstV [1.0; 1.0])))" = 
  (type_of (Mag (Add (Inv (ConstV [3.0; 4.0]), ConstV [1.0; 1.0]))) = Scalar)

(* Test case 3 : Testing type of scalars and vectors close to zero*)

(* IsZero on a scalar near zero *)
let%test "type_of (IsZero (Add (ConstS 1e-10, Inv (ConstS 1e-10))))" = 
  (type_of (IsZero (Add (ConstS 1e-10, Inv (ConstS 1e-10)))) = Bool)

(* IsZero on a vector near zero *)
let%test "type_of (IsZero (Add (ConstV [1e-10; 1e-10], Inv (ConstV [1e-10; 1e-10])))" = 
  (type_of (IsZero (Add (ConstV [1e-10; 1e-10], Inv (ConstV [1e-10; 1e-10])))) = Bool)

(* Test case 4 : Testing complex mixed operations*)

(* Nested Cond + DotProd + ScalProd *)
let%test "type_of (Cond (IsZero (ConstV [0.0; 0.0]), DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), DotProd (ConstV [2.0; 0.0], ConstV [5.0; 6.0])))" = 
  (type_of (Cond (IsZero (ConstV [0.0; 0.0]), DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), DotProd (ConstV [2.0; 0.0], ConstV [5.0; 6.0]))) = Scalar)


    
(* ------------------TEST CASES (CORNER CASES AND EDGE CASES) (evaluator)------------------- *)

(* Test case 1 : Testing raise Wrong cases*)

let%test "eval (Add (ConstS 2.5, ConstV [3.5; 4.5])) raises Wrong (Add (ConstS 2.5, ConstV [3.5; 4.5]))" =
  (try
    let _ = eval (Add (ConstS 2.5, ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Add (ConstS 2.5, ConstV [3.5; 4.5])
  | _ -> false)

let%test "eval (Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5])) raises Wrong (Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5]))" =
  (try
    let _ = eval (Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5])) in false
  with
  | Wrong e -> e = Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5])
  | _ -> false)

let%test "eval (Add (T, ConstS 2.5)) raises Wrong (Add (T, ConstS 2.5))" =
  (try
    let _ = eval (Add (T, ConstS 2.5)) in false
  with
  | Wrong e -> e = Add (T, ConstS 2.5)
  | _ -> false)

let%test "eval (ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5])) raises Wrong (ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5]))" =
  (try
    let _ = eval (ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5])
  | _ -> false)

let%test "eval (DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) raises Wrong (DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5]))" =
  (try
    let _ = eval (DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])
  | _ -> false)

let%test "eval (Mag T) raises Wrong (Mag T)" =
  (try
    let _ = eval (Mag T) in false
  with
  | Wrong e -> e = Mag T
  | _ -> false)

let%test "eval (Angle (ConstS 2.5, ConstV [3.5; 4.5])) raises Wrong (Angle (ConstS 2.5, ConstV [3.5; 4.5]))" =
  (try
    let _ = eval (Angle (ConstS 2.5, ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Angle (ConstS 2.5, ConstV [3.5; 4.5])
  | _ -> false)

let%test "eval (Cond (T, ConstS 2.5, ConstV [3.5; 4.5])) raises Wrong (Cond (T, ConstS 2.5, ConstV [3.5; 4.5]))" =
  (try
    let _ = eval (Cond (T, ConstS 2.5, ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Cond (T, ConstS 2.5, ConstV [3.5; 4.5])
  | _ -> false)

let%test "eval (Sub (ConstS 2.5, ConstV [3.5; 4.5])) raises Wrong (Sub (ConstS 2.5, ConstV [3.5; 4.5]))" =
  (try
    let _ = eval (Sub (ConstS 2.5, ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Sub (ConstS 2.5, ConstV [3.5; 4.5])
  | _ -> false)

let%test "eval (Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) raises Wrong (Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5]))" =
  (try
    let _ = eval (Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) in false
  with
  | Wrong e -> e = Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])
  | _ -> false)

let%test "eval (Add (ConstV [], ConstV [])) raises Wrong (Add (ConstV [], ConstV []))" =
  (try
    let _ = eval (Add (ConstV [], ConstV [])) in false
  with
  | Wrong e -> e = Add (ConstV [], ConstV [])
  | _ -> false)

(* Test case 2 : Testing Nested expressions (3-4 levels deeep)*)

(* Nested Cond inside Add *)
let%test "eval (Add (Cond (T, ConstS 1.0, ConstS 2.0), Cond (F, ConstS 3.0, ConstS 4.0)))" = 
  (eval (Add (Cond (T, ConstS 1.0, ConstS 2.0), Cond (F, ConstS 3.0, ConstS 4.0))) = S 5.0)

(* Nested ScalProd inside Angle *)
let%test "eval (Angle (ScalProd (ConstS 2.0, ConstV [1.0; 0.0]), ScalProd (ConstS 3.0, ConstV [1.0; 0.0])))" = 
  (eval (Angle (ScalProd (ConstS 2.0, ConstV [1.0; 0.0]), ScalProd (ConstS 3.0, ConstV [1.0; 0.0]))) = S 0.0)

(* Deeply nested Add/Inv/Mag *)
let%test "eval (Mag (Add (Inv (ConstV [4.0; 5.0]), ConstV [1.0; 1.0])))" = 
  (eval (Mag (Add (Inv (ConstV [4.0; 5.0]), ConstV [1.0; 1.0]))) = S 5.0)

(* Test case 3 : Testing type of scalars and vectors close to zero*)

(* IsZero on a scalar near zero *)
let%test "eval (IsZero (Add (ConstS 1e-7, Inv (ConstS 1e-7))))" = (eval (IsZero (Add (ConstS 1e-7, Inv (ConstS 1e-7)))) = B true)
let%test "eval (IsZero (ConstS 1e-7))" = (eval (IsZero (ConstS 1e-7)) = B true)

(* IsZero on a vector near zero *)
let%test "eval (IsZero (Add (ConstV [1e-7; 1e-7], Inv (ConstV [1e-7; 1e-7])))" = (eval (IsZero (Add (ConstV [1e-7; 1e-7], Inv (ConstV [1e-7; 1e-7])))) = B true)
let%test "eval (IsZero (ConstV [1e-7; 1e-7]))" = (eval (IsZero (ConstV [1e-7; 1e-7])) = B true)  

(* Test case 4 : Testing complex mixed operations*)

(* Nested Cond + DotProd + ScalProd *)
let%test "eval (Cond (IsZero (ConstV [1e-7; 1e-7]), DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), DotProd (ConstV [2.0; 0.0], ConstV [5.0; 6.0])))" = 
  (eval (Cond (IsZero (ConstV [1e-7; 1e-7]), DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), DotProd (ConstV [2.0; 0.0], ConstV [5.0; 6.0]))) = S 11.0)
  
  
  