open Assignment2.A2

(*Testing*)

(*
Test Case format:

open A2
let%test "test1_name" = (type_of <expr> = <expected output>)
let%test "test2_name" = (eval <expr> = <expected output>)

(* For Example *)
let%test "type_of T" = (type_of T = Bool)
let%test "type_of (Inv (ConstS (-2.5)))" = (type_of (Inv (ConstS (-2.5))) = Scalar)
let%test "eval (Inv T)" = (eval (Inv T) = B false)
let%test "eval (Mag (ConstV [3.0; 4.0]))" = (eval (Mag (ConstV [3.0; 4.0])) = S 5.0)

If you want to add testcases which give exceptions then you can do so in the following manner:

let%test "test_name" = 
(try
  let _ = eval <expr> in false
 with
 | Wrong e -> e =<expr>
 | _ -> false

(* Example *)
let%test "eval (DotProd (ConstV [1.0; 2.0], ConstV [1.0])) raises Wrong (DotProd (ConstV [1.0; 2.0], ConstV [1.0]))" =
  (try
    let _ = eval (DotProd (ConstV [1.0; 2.0], ConstV [1.0])) in false
  with
  | Wrong e -> e = DotProd (ConstV [1.0; 2.0], ConstV [1.0])
  | _ -> false)


*)

(*I have to make 4-5 test cases for each contructor. It must contain all edge cases, and must also contain cases that raise exceptions. I have to make them both for eval and for type_of*)

(*Test Cases for type_of and eval*)

let%test "type_of T" = type_of T = Bool
let%test "type_of F" = type_of F = Bool
let%test "type_of (GenUnit (3, 2))" = type_of (GenUnit (3, 2)) = Vector 3
let%test "type_of (GenZero 3)" = type_of (GenZero 3) = Vector 3

let%test "type_of (Sub (ConstS 3.0, ConstS 4.0))" =
  type_of (Sub (ConstS 3.0, ConstS 4.0)) = Scalar

let%test "type_of (Sub (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  type_of (Sub (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = Vector 2

let%test "type_of (Add (T, F))" = type_of (Add (T, F)) = Bool

let%test "type_of (Add (ConstS 3.0, ConstS 4.0))" =
  type_of (Add (ConstS 3.0, ConstS 4.0)) = Scalar

let%test "type_of (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  type_of (Add (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = Vector 2

let%test "type_of (Inv T)" = type_of (Inv T) = Bool
let%test "type_of (Inv (ConstS 3.0))" = type_of (Inv (ConstS 3.0)) = Scalar

let%test "type_of (Inv (ConstV [1.0; 2.0]))" =
  type_of (Inv (ConstV [ 1.0; 2.0 ])) = Vector 2

let%test "type_of (ScalProd (T, F))" = type_of (ScalProd (T, F)) = Bool

let%test "type_of (ScalProd (ConstS 3.0, ConstS 4.0))" =
  type_of (ScalProd (ConstS 3.0, ConstS 4.0)) = Scalar

let%test "type_of (ScalProd (ConstS 3.0, ConstV [1.0; 2.0]))" =
  type_of (ScalProd (ConstS 3.0, ConstV [ 1.0; 2.0 ])) = Vector 2

let%test "type_of (ScalProd (ConstV [1.0; 2.0], ConstS 3.0))" =
  type_of (ScalProd (ConstV [ 1.0; 2.0 ], ConstS 3.0)) = Vector 2

let%test "type_of (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  type_of (DotProd (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = Scalar

let%test "type_of (Mag (ConstS (-3.0)))" =
  type_of (Mag (ConstS (-3.0))) = Scalar

let%test "type_of (Mag (ConstV [3.0; 4.0]))" =
  type_of (Mag (ConstV [ 3.0; 4.0 ])) = Scalar

let%test "type_of (Angle (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  type_of (Angle (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = Scalar

let%test "type_of (IsZero (ConstS 0.0))" = type_of (IsZero (ConstS 0.0)) = Bool

let%test "type_of (IsZero (ConstV [0.0; 0.0]))" =
  type_of (IsZero (ConstV [ 0.0; 0.0 ])) = Bool

let%test "type_of (IsZero (ConstV [0.0; 0.0; 0.0]))" =
  type_of (IsZero (ConstV [ 0.0; 0.0; 0.0 ])) = Bool

let%test "type_of (Cond (T, ConstS 3.0, ConstS 4.0))" =
  type_of (Cond (T, ConstS 3.0, ConstS 4.0)) = Scalar

let%test "type_of (Cond (F, ConstS 3.0, ConstS 4.0))" =
  type_of (Cond (F, ConstS 3.0, ConstS 4.0)) = Scalar

let%test "type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  type_of (Cond (T, ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = Vector 2

let%test "type_of (Cond (F, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  type_of (Cond (F, ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = Vector 2

let%test "eval T" = eval T = B true
let%test "eval F" = eval F = B false
let%test "eval (GenUnit (3, 2))" = eval (GenUnit (3, 2)) = V [ 0.0; 1.0; 0.0 ]
let%test "eval (GenZero 3)" = eval (GenZero 3) = V [ 0.0; 0.0; 0.0 ]

let%test "eval (Sub (ConstS 3.0, ConstS 4.0))" =
  eval (Sub (ConstS 3.0, ConstS 4.0)) = S (-1.0)

let%test "eval (Sub (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  eval (Sub (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = V [ -2.0; -2.0 ]

let%test "eval (Add (T, F))" = eval (Add (T, F)) = B true

let%test "eval (Add (ConstS 3.0, ConstS 4.0))" =
  eval (Add (ConstS 3.0, ConstS 4.0)) = S 7.0

let%test "eval (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  eval (Add (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = V [ 4.0; 6.0 ]

let%test "eval (Inv T)" = eval (Inv T) = B false
let%test "eval (Inv (ConstS 3.0))" = eval (Inv (ConstS 3.0)) = S (-3.0)

let%test "eval (Inv (ConstV [1.0; 2.0]))" =
  eval (Inv (ConstV [ 1.0; 2.0 ])) = V [ -1.0; -2.0 ]

let%test "eval (ScalProd (T, F))" = eval (ScalProd (T, F)) = B false

let%test "eval (ScalProd (ConstS 3.0, ConstS 4.0))" =
  eval (ScalProd (ConstS 3.0, ConstS 4.0)) = S 12.0

let%test "eval (ScalProd (ConstS 3.0, ConstV [1.0; 2.0]))" =
  eval (ScalProd (ConstS 3.0, ConstV [ 1.0; 2.0 ])) = V [ 3.0; 6.0 ]

let%test "eval (ScalProd (ConstV [1.0; 2.0], ConstS 3.0))" =
  eval (ScalProd (ConstV [ 1.0; 2.0 ], ConstS 3.0)) = V [ 3.0; 6.0 ]

let%test "eval (DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  eval (DotProd (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = S 11.0

let%test "eval (Mag (ConstS (-3.0)))" = eval (Mag (ConstS (-3.0))) = S 3.0

let%test "eval (Mag (ConstV [3.0; 4.0]))" =
  eval (Mag (ConstV [ 3.0; 4.0 ])) = S 5.0

let%test "eval (Angle (ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  eval (Angle (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ]))
  = S (acos (11.0 /. (sqrt 5.0 *. sqrt 25.0)))

let%test "eval (IsZero (ConstS 0.0))" = eval (IsZero (ConstS 0.0)) = B true

let%test "eval (IsZero (ConstV [0.0; 0.0]))" =
  eval (IsZero (ConstV [ 0.0; 0.0 ])) = B true

let%test "eval (IsZero (ConstV [0.0; 0.0; 0.0]))" =
  eval (IsZero (ConstV [ 0.0; 0.0; 0.0 ])) = B true

let%test "eval (Cond (T, ConstS 3.0, ConstS 4.0))" =
  eval (Cond (T, ConstS 3.0, ConstS 4.0)) = S 3.0

let%test "eval (Cond (F, ConstS 3.0, ConstS 4.0))" =
  eval (Cond (F, ConstS 3.0, ConstS 4.0)) = S 4.0

let%test "eval (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  eval (Cond (T, ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = V [ 1.0; 2.0 ]

let%test "eval (Cond (F, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" =
  eval (Cond (F, ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])) = V [ 3.0; 4.0 ]

(*Nested TestCases*)

(*eval*)
let%test "eval (Add (ConstS 3.0, Inv (Add (ConstS 4.0, ConstS 5.0))))" =
  eval (Add (ConstS 3.0, Inv (Add (ConstS 4.0, ConstS 5.0)))) = S (-6.0)

let%test
    "eval (Add (ScalProd (ConstS 2.0, ConstV [1.0; 2.0]), Inv (ConstV [3.0; \
     4.0])))" =
  eval
    (Add (ScalProd (ConstS 2.0, ConstV [ 1.0; 2.0 ]), Inv (ConstV [ 3.0; 4.0 ])))
  = V [ -1.0; 0.0 ]

let%test
    "eval (ScalProd (ConstS 2.0, Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0])))" =
  eval (ScalProd (ConstS 2.0, Add (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])))
  = V [ 8.0; 12.0 ]

let%test
    "eval (ScalProd (ConstS 2.0, DotProd (ConstV [1.0; 2.0], ConstV [3.0; \
     4.0])))" =
  eval
    (ScalProd (ConstS 2.0, DotProd (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])))
  = S 22.0

let%test
    "eval (Mag (Angle (ConstV [1.0; 2.0], Add (ConstV [3.0; 4.0], ConstV [1.0; \
     1.0]))))" =
  eval
    (Mag
       (Angle
          (ConstV [ 1.0; 2.0 ], Add (ConstV [ 3.0; 4.0 ], ConstV [ 1.0; 1.0 ]))))
  = S (acos (14.0 /. (sqrt 5.0 *. sqrt 41.0)))

let%test
    "eval (DotProd (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), ScalProd \
     (ConstS 2.0, ConstV [5.0; 6.0])))" =
  eval
    (DotProd
       ( Add (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ]),
         ScalProd (ConstS 2.0, ConstV [ 5.0; 6.0 ]) ))
  = S 112.0

let%test
    "eval (Mag (Angle (Add (ConstV [1.0; 1.0], ConstV [2.0; 2.0]), Inv (ConstV \
     [3.0; 3.0]))))" =
  eval
    (Mag
       (Angle
          ( Add (ConstV [ 1.0; 1.0 ], ConstV [ 2.0; 2.0 ]),
            Inv (ConstV [ 3.0; 3.0 ]) )))
  = S Float.pi

(*type_of*)

let%test "type_of (Add (ConstS 3.0, Inv (Add (ConstS 4.0, ConstS 5.0))))" =
  type_of (Add (ConstS 3.0, Inv (Add (ConstS 4.0, ConstS 5.0)))) = Scalar

let%test
    "type_of (Add (ScalProd (ConstS 2.0, ConstV [1.0; 2.0]), Inv (ConstV [3.0; \
     4.0])))" =
  type_of
    (Add (ScalProd (ConstS 2.0, ConstV [ 1.0; 2.0 ]), Inv (ConstV [ 3.0; 4.0 ])))
  = Vector 2

let%test
    "type_of (ScalProd (ConstS 2.0, Add (ConstV [1.0; 2.0], ConstV [3.0; \
     4.0])))" =
  type_of
    (ScalProd (ConstS 2.0, Add (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])))
  = Vector 2

let%test
    "type_of (ScalProd (ConstS 2.0, DotProd (ConstV [1.0; 2.0], ConstV [3.0; \
     4.0])))" =
  type_of
    (ScalProd (ConstS 2.0, DotProd (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ])))
  = Scalar

let%test
    "type_of (Mag (Angle (ConstV [1.0; 2.0], Add (ConstV [3.0; 4.0], ConstV \
     [1.0; 1.0]))))" =
  type_of
    (Mag
       (Angle
          (ConstV [ 1.0; 2.0 ], Add (ConstV [ 3.0; 4.0 ], ConstV [ 1.0; 1.0 ]))))
  = Scalar

let%test
    "type_of (DotProd (Add (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), ScalProd \
     (ConstS 2.0, ConstV [5.0; 6.0])))" =
  type_of
    (DotProd
       ( Add (ConstV [ 1.0; 2.0 ], ConstV [ 3.0; 4.0 ]),
         ScalProd (ConstS 2.0, ConstV [ 5.0; 6.0 ]) ))
  = Scalar

let%test
    "type_of (Mag (Angle (Add (ConstV [1.0; 1.0], ConstV [2.0; 2.0]), Inv \
     (ConstV [3.0; 3.0]))))" =
  type_of
    (Mag
       (Angle
          ( Add (ConstV [ 1.0; 1.0 ], ConstV [ 2.0; 2.0 ]),
            Inv (ConstV [ 3.0; 3.0 ]) )))
  = Scalar

(*Exception TestCases*) 

(* 1. Type error: Expressions are not well-typed *)
let%test "type_of (Add (ConstS 3.0, ConstV [1.0; 2.0])) raises Wrong (Add ...)" =
  (try
     let _ = type_of (Add (ConstS 3.0, ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = Add (ConstS 3.0, ConstV [1.0; 2.0])
   | _ -> false)

let%test "type_of (DotProd (ConstS 3.0, ConstV [1.0; 2.0])) raises Wrong (DotProd ...)" =
  (try
     let _ = type_of (DotProd (ConstS 3.0, ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = DotProd (ConstS 3.0, ConstV [1.0; 2.0])
   | _ -> false)

(* 2. Vector with dimension 0 *)
let%test "eval (Add (ConstV [], ConstV [1.0; 2.0])) raises Wrong (Add ...)" =
  (try
     let _ = eval (Add (ConstV [], ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = Add (ConstV [], ConstV [1.0; 2.0])
   | _ -> false)

let%test "eval (DotProd (ConstV [], ConstV [1.0; 2.0])) raises Wrong (DotProd ...)" =
  (try
     let _ = eval (DotProd (ConstV [], ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = DotProd (ConstV [], ConstV [1.0; 2.0])
   | _ -> false)

(* 3. Undefined angle: Vector with magnitude 0 *)
let%test "eval (Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0])) raises Wrong (Angle ...)" =
  (try
     let _ = eval (Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0])) in false
   with
   | Wrong e -> e = Angle (ConstV [0.0; 0.0], ConstV [1.0; 2.0])
   | _ -> false)

let%test "eval (Angle (ConstV [1.0; 2.0], ConstV [0.0; 0.0])) raises Wrong (Angle ...)" =
  (try
     let _ = eval (Angle (ConstV [1.0; 2.0], ConstV [0.0; 0.0])) in false
   with
   | Wrong e -> e = Angle (ConstV [1.0; 2.0], ConstV [0.0; 0.0])
   | _ -> false)

let%test "eval (Angle (ConstV [0.0; 0.0], ConstV [0.0; 0.0])) raises Wrong (Angle ...)" =
  (try
     let _ = eval (Angle (ConstV [0.0; 0.0], ConstV [0.0; 0.0])) in false
   with
   | Wrong e -> e = Angle (ConstV [0.0; 0.0], ConstV [0.0; 0.0])
   | _ -> false)

(* 4. Operating Vectors with different dimensions*)

(* Test for Dot Product with Different Dimensions *)
let%test "type_of (DotProd (ConstV [1.0; 2.0], ConstV [1.0])) raises Wrong (DotProd ...)" =
  (try
     let _ = type_of (DotProd (ConstV [1.0; 2.0], ConstV [1.0])) in false
   with
   | Wrong e -> e = DotProd (ConstV [1.0; 2.0], ConstV [1.0])
   | _ -> false)

(* Test for Addition with Different Dimensions *)
let%test "type_of (Add (ConstV [1.0; 2.0], ConstV [3.0])) raises Wrong (Add ...)" =
  (try
     let _ = type_of (Add (ConstV [1.0; 2.0], ConstV [3.0])) in false
   with
   | Wrong e -> e = Add (ConstV [1.0; 2.0], ConstV [3.0])
   | _ -> false)

(* Test for Angle Between Vectors of Different Dimensions *)
let%test "type_of (Angle (ConstV [1.0; 2.0], ConstV [3.0])) raises Wrong (Angle ...)" =
  (try
     let _ = type_of (Angle (ConstV [1.0; 2.0], ConstV [3.0])) in false
   with
   | Wrong e -> e = Angle (ConstV [1.0; 2.0], ConstV [3.0])
   | _ -> false)