
(* ADT vector from assignment 1 : *)

type vector = float list
   exception DimensionError of string
   exception ZeroVectorError of string
               
               
   let create n x : vector =
     if n < 1 then raise (DimensionError "Dimension must be an integer >= 1")
     else
       let rec create2 n x acc =
         if n = 0 then acc  
         else create2 (n - 1) x (x :: acc)  
       in
       create2 n x []
   ;;            
               
   let dim (v:vector) =                       
     let rec dim2 v acc = match v with 
         [] -> acc
       | _::xs -> dim2 xs (acc+1)
     in 
     dim2 v 0 
   ;;        
               
   let rec is_zero (v:vector) = match v with
       [] -> true
     | x::xs -> if (x < 0.000001 && x >= 0.0) || (x <= 0.0 && x > -0.000001) then is_zero xs
         else false 
   ;;
   
   let unit n j : vector =
     if (n < 1) || (j<1) || (j>n) then raise (DimensionError "j must be between 1 and n inclusive")
     else 
       let rec unit2 n j acc v =
         if acc > n then v 
         else if acc == (n-j+1) then unit2 n j (acc + 1) (1.0 :: v)  
         else unit2 n j (acc + 1) (0.0 :: v)  
       in
       unit2 n j 1 []
   ;;    
           
   let rec scale c (v:vector) : vector = match v with 
       [] -> []
     | x::xs -> (c *. x)::(scale c xs)
   ;;                                 
              
   let rec addv (v1:vector) (v2:vector) : vector = 
     if (dim v1) == (dim v2 ) then match v1 with 
         [] -> []
       | x::xs -> (x +. (List.hd v2))::(addv xs (List.tl v2))
     else raise (DimensionError "Dimensions must match")
   ;;                   
               
   let dot_prod (v1:vector) (v2:vector) =
     if (dim v1) == (dim v2 ) then 
       let rec dot2 v1 v2 acc = match v1 with
           [] -> acc
         | x::xs -> dot2 xs (List.tl v2) (acc +. x *. List.hd v2) 
       in
       dot2 v1 v2 0. 
     else raise (DimensionError "Dimensions must match")
   ;;              
         
   let inv (v:vector) : vector =
     scale (-1.0) v
   ;;                                       
               
   let length (v:vector) =
     let temp = dot_prod v v
     in
     Float.sqrt(temp)
   ;;              
              
   let angle (v1:vector) (v2:vector) = 
     if (is_zero v1) || (is_zero v2) then raise (ZeroVectorError "Angle can not be calculated with zero vector")
     else
       let l1 = length v1
       in
       let l2 = length v2
       in 
       let dot = dot_prod v1 v2
       in
       if (dim v1 > 0) && (dim v2 > 0) then
         if ((dot /. l1) /. l2 > 1.0) then acos (1.0)
         else if ((dot /. l1) /. l2 < -1.0) then acos (-1.0)
         else acos ((dot /. l1) /. l2)
       else raise (DimensionError "Dimension of vector must be > 0")
   ;;   


(* ADT from assignment 1 ends here, assignment 2 starts here : *)   

(* Abstract syntax : *)

type types =  Bool    (* boolean *)
            | Scalar   (* a scalar — any float value *)
            | Vector of int   (* n-dimensional with elements of type float*)
;;

type expr =  
T | F   (* Boolean constants *)
| ConstS of float    (* Scalar constants *)
| ConstV of float list    (* Vector constants *)
| Add of expr * expr   (* overloaded — disjunction of two booleans or sum of  two scalars or sum of two vectors of the same dimension *)
| Inv of expr     (* overloaded — negation of a boolean or additive inverse of  a scalar or additive inverse of a vector *)
| ScalProd of expr * expr   (* overloaded — conjunction of two booleans or product of a scalar with another scalar or product of a scalar and a vector *)
| DotProd of expr * expr  (* dot product of two vectors of the same dimension *)
| Mag of expr   (* overloaded: absolute value of a scalar or magnitude of a vector *)
| Angle of expr * expr  (* in radians, the angle between two vectors *)
| IsZero of expr (* overloaded: checks if a boolean expression evaluates to F,  or if a given scalar is within epsilon of 0.0 or is the vector close — within epsilon on each coordinate —  to the zero vector *)
| Cond of expr * expr * expr  (* "if_then_else" --  if the first expr evaluates to T then evaluate the second expr, else the third expr *)
| Sub of expr * expr (* overloaded — difference of two scalars or difference of two vectors of the same dimension *)


;;

exception Wrong of expr;;

(* Type checker : *)

let rec type_of (e:expr) : types = 
  try
    (match e with
      T -> Bool
    | F -> Bool
    | ConstS _ -> Scalar
    | ConstV v -> if (dim v) > 0 then Vector (dim v) else raise (Wrong e)
    | Add (e1, e2) -> ( match (type_of e1) with
                      Scalar -> if (type_of e2) = Scalar then Scalar else raise (Wrong e)
                      | Vector n1 -> (match (type_of e2) with
                                        | Vector n2 -> if (n1 = n2) then Vector n1 else raise (Wrong e)
                                        | _ -> raise (Wrong e))
                      | Bool -> if (type_of e2) = Bool then Bool else raise (Wrong e) )
    | Inv e1 -> ( match (type_of e1) with
                  Scalar -> Scalar
                | Vector n -> Vector n
                | Bool -> Bool )
    | ScalProd (e1, e2) -> ( match (type_of e1) with
                            Scalar -> (match (type_of e2) with
                                        Scalar -> Scalar
                                      | Vector n -> Vector n
                                      | Bool -> raise (Wrong e))
                            | Vector n1 -> (match (type_of e2) with
                                            Scalar -> Vector n1
                                          | _ -> raise (Wrong e))
                            | Bool -> if (type_of e2) = Bool then Bool else raise (Wrong e) )
    | DotProd (e1, e2) -> ( match (type_of e1) with
                            Vector n1 -> (match (type_of e2) with
                                          | Vector n2 -> if n1 = n2 then Scalar else raise (Wrong e)
                                          | _ -> raise (Wrong e))
                            | _ -> raise (Wrong e) )
    | Mag e1 -> ( match (type_of e1) with
                  Scalar -> Scalar
                | Vector _ -> Scalar
                | Bool -> raise (Wrong e) )
    | Angle (e1, e2) -> ( match (type_of e1) with
                          Vector n1 -> (match (type_of e2) with
                                        | Vector n2 -> if (n1 = n2) then Scalar else raise (Wrong e)
                                        | _ -> raise (Wrong e))
                          | _ -> raise (Wrong e) )
    | IsZero e1 -> ( match (type_of e1) with
                    Bool -> Bool
                  | Scalar -> Bool
                  | Vector _ -> Bool)
    | Cond (e1, e2, e3) -> ( match (type_of e1) with
                            Bool -> (match (type_of e2) with
                                      | Scalar -> if (type_of e3) = Scalar then Scalar else raise (Wrong e)
                                      | Vector n1 -> (match (type_of e3) with
                                                      | Vector n2 -> if n1 = n2 then Vector n1 else raise (Wrong e)
                                                      | _ -> raise (Wrong e))
                                      | Bool -> if (type_of e3) = Bool then Bool else raise (Wrong e))
                            | _ -> raise (Wrong e1) )
    | Sub (e1, e2) -> ( match (type_of e1) with
                        Scalar -> if (type_of e2) = Scalar then Scalar else raise (Wrong e)
                      | Vector n1 -> (match (type_of e2) with
                                      | Vector n2 -> if n1 = n2 then Vector n1 else raise (Wrong e)
                                      | _ -> raise (Wrong e))
                      | Bool -> raise (Wrong e) ) )
  with
  | Wrong _ -> raise (Wrong e)
;;


(* Specifying the definitional interpreter :

val eval : expr -> values

eval[[T]] = true
eval[[F]] = false

eval[[ConstS s]] = s

eval[[Constv v]] = v

eval[[Add(e1, e2)]] = eval[[e1]] + eval[[e2]]     if eval[[e1]] and eval[[e2]] are of type Scalar
                    = eval[[e1]] + eval[[e2]]     if eval[[e1]] and eval[[e2]] are of type Vector n
                    = eval[[e1]] || eval[[e2]]     if eval[[e1]] and eval[[e2]] are of type Bool

eval[[Inv(e1)]] = -eval[[e1]]     if eval[[e1]] is of type Scalar
                = -eval[[e1]]     if eval[[e1]] is of type Vector n  (additive inverse of vector)
                = not eval[[e1]]     if eval[[e1]] is of type Bool  

eval[[ScalProd(e1, e2)]] = eval[[e1]] * eval[[e2]]     if eval[[e1]] and eval[[e2]] are of type Scalar
                        = eval[[e1]] * eval[[e2]]     if eval[[e1]] is of type Scalar and eval[[e2]] is of type Vector n
                        = eval[[e1]] * eval[[e2]]     if eval[[e1]] is of type Vector n and eval[[e2]] is of type Scalar
                        = eval[[e1]] && eval[[e2]]     if eval[[e1]] and eval[[e2]] are of type Bool

eval[[DotProd(e1, e2)]] = eval[[e1]] . eval[[e2]]     if eval[[e1]] and eval[[e2]] are of type Vector n

eval[[Mag(e1)]] = |eval[[e1]]|     if eval[[e1]] is of type Scalar  (here |s| is the absolute value of scalar s)
                = ||eval[[e1]]||     if eval[[e1]] is of type Vector n  (here ||v|| is the magnitude/length of vector v)

eval[[Angle(e1, e2)]] = angle(eval[[e1]], eval[[e2]])     if eval[[e1]] and eval[[e2]] are of type Vector n

eval[[IsZero(e1)]] = is_zero(eval[[e1]])     if eval[[e1]] is of type Vector n
                  = eval[[e1]] < 0.000001     if eval[[e1]] is of type Scalar
                  = eval[[e1]] == F     if eval[[e1]] is of type Bool

eval[[Cond(e1, e2, e3)]] = eval[[e2]]     if eval[[e1]] == T
                         = eval[[e3]]     if eval[[e1]] == F

eval[[Sub(e1, e2)]] = eval[[e1]] - eval[[e2]]     if eval[[e1]] and eval[[e2]] are of type Scalar
                    = eval[[e1]] - eval[[e2]]     if eval[[e1]] and eval[[e2]] are of type Vector n  (here - denotes vector subtraction)

*)

(* Bases on the above specifications for all the cases, below is the code for eval function: *)

type values = B of bool |  S of float | V of vector

let rec eval (e:expr) : values = 
  try 
    (match e with
      T -> B true
    | F -> B false
    | ConstS s -> S s
    | ConstV v -> if (dim v) > 0 then V v else raise (Wrong e)
    | Add (e1, e2) -> (match (eval e1) with
                        S s1 -> (match (eval e2) with     
                                  S s2 -> S (s1 +. s2)    (*case of both scalars*)
                                | _ -> raise (Wrong e))
                      | V v1 -> (match (eval e2) with
                                  V v2 -> if (((dim v1) == (dim v2)) && (dim v1 > 0)) then V (addv v1 v2) else raise (Wrong e)  (*case of both vectors*)
                                | _ -> raise (Wrong e))
                      | B b1 -> (match (eval e2) with   
                                  B b2 -> B (b1 || b2)    (*case of both booleans*)
                                | _ -> raise (Wrong e)) )
    | Inv e1 -> (match (eval e1) with
                  S s -> S (-1.0 *. s)      (*case of scalar*)
                | V v -> if (dim v) > 0 then V (inv v) else raise (Wrong e)          (*case of vector*)
                | B b -> B (not b) )        (*case of boolean*)
    | ScalProd (e1, e2) -> (match (eval e1) with
                            S s1 -> (match (eval e2) with
                                      S s2 -> S (s1 *. s2)      (*case of both scalars*)
                                    | V v -> if (dim v) > 0 then V (scale s1 v) else raise (Wrong e)    (*case of scalar and vector*)
                                    | _ -> raise (Wrong e)) 
                            | V v1 -> (match (eval e2) with       
                                        S s -> if (dim v1) > 0 then V (scale s v1) else raise (Wrong e)    (*case of vector and scalar*)
                                      | _ -> raise (Wrong e))
                            | B b1 -> (match (eval e2) with         
                                        B b2 -> B (b1 && b2)     (*case of both booleans*)
                                      | _ -> raise (Wrong e)) )
    | DotProd (e1, e2) -> (match (eval e1) with
                            V v1 -> (match (eval e2) with
                                      V v2 -> if ((dim v1) == (dim v2)) then S (dot_prod v1 v2) else raise (Wrong e)      (*case of both vectors*)
                                    | _ -> raise (Wrong e))
                            | _ -> raise (Wrong e) )
    | Mag e1 -> (match (eval e1) with
                  S s -> S (if s > 0.0 then s else (-1.0 *. s))     (*case of scalar*)
                | V v -> if (dim v) > 0 then S (length v) else raise (Wrong e)    (*case of vector*)
                | _ -> raise (Wrong e) )    
    | Angle (e1, e2) -> (match (eval e1) with
                        V v1 -> (match (eval e2) with
                                  V v2 -> if ((dim v1) == (dim v2) && (not (is_zero v1) && not (is_zero v2))) then S (angle v1 v2) else raise (Wrong e)        (*case of both vectors*)
                                | _ -> raise (Wrong e))
                        | _ -> raise (Wrong e) )
    | IsZero e1 -> (match (eval e1) with
                    V v -> if (dim v) > 0 then B (is_zero v) else raise (Wrong e)           (*case of vector*)
                  | S s -> B (s < 0.000001)         (*case of scalar*)
                  | B b -> B (b == false) )             (*case of boolean*)
    | Cond (e1, e2, e3) -> (match (eval e1) with
                            B b -> if (type_of e2 == type_of e3) then
                                      if (b == true) then (eval e2) else (eval e3)    (*case of boolean*)
                                  else raise (Wrong e)
                          | _ -> raise (Wrong e) )
    | Sub (e1, e2) -> (match (eval e1) with
                      S s1 -> (match (eval e2) with
                                S s2 -> S (s1 -. s2)              (*case of both scalars*)
                              | _ -> raise (Wrong e))
                      | V v1 -> (match (eval e2) with
                                V v2 -> if ((dim v1) == (dim v2)) then V (addv v1 (scale (-1.0) v2)) else raise (Wrong e)      (*case of both vectors*)
                              | _ -> raise (Wrong e))
                      | _ -> raise (Wrong e) )
  )
  with
  | Wrong _ -> raise (Wrong e)
;;


(* Testing for self reference : *)


(* ------------------TEST CASES (CORNER AND EDGE CASES)(type checker)------------------- *)

(* assert (type_of (Add (ConstS 2.5, ConstV [3.5; 4.5])) = Scalar);;
assert (type_of (Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5])) = Vector 2);;
assert (type_of (Add (T, ConstS 2.5)) = Bool);;
assert (type_of (ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5])) = Vector 2);;
assert (type_of (DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) = Scalar);;
assert (type_of (Mag T) = Scalar);;
assert (type_of (Angle (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) = Scalar);;
assert (type_of (Cond (T, ConstS 2.5, ConstV [3.5; 4.5])) = Vector 2);;
assert (type_of (Sub (ConstS 2.5, ConstV [3.5; 4.5])) = Scalar);;
assert (type_of (Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) = Vector 2);;
assert (type_of (Add (ConstV [], ConstV [])) = Vector 0);;

assert (type_of (Add (Cond (T, ConstS 1.0, ConstS 2.0), Cond (F, ConstS 3.0, ConstS 4.0))) = Scalar);;
assert (type_of (Angle (ScalProd (ConstS 2.0, ConstV [1.0; 0.0]), ScalProd (ConstS 3.0, ConstV [0.0; 1.0]))) = Scalar);;
assert (type_of (Mag (Add (Inv (ConstV [3.0; 4.0]), ConstV [1.0; 1.0]))) = Scalar);;
assert (type_of (IsZero (Add (ConstS 1e-7, Inv (ConstS 1e-7)))) = Bool);;
assert (type_of (IsZero (Add (ConstV [1e-7; 1e-7], Inv (ConstV [1e-7; 1e-7])))) = Bool);;
assert (type_of (Cond (IsZero (ConstV [0.0; 0.0]), DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), DotProd (ConstV [2.0; 0.0], ConstV [5.0; 6.0]))) = Scalar);;


(* ------------------TEST CASES (CORNER CASES AND EDGE CASES) (evaluator)------------------- *)

assert (eval (Add (ConstS 2.5, ConstV [3.5; 4.5])) = S 2.5);;
assert (eval (Add (ConstV [2.5; 3.5], ConstV [3.5; 4.5; 5.5])) = V [6.0; 7.0]);;
assert (eval (Add (T, ConstS 2.5)) = S 2.5);;
assert (eval (ScalProd (ConstV [2.5; 3.5], ConstV [3.5; 4.5])) = V [8.75; 15.75]);;
assert (eval (DotProd (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) = S 29.75);;
assert (eval (Mag T) = S 1.0);;
assert (eval (Angle (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) = S 0.0);;
assert (eval (Cond (T, ConstS 2.5, ConstV [3.5; 4.5])) = S 2.5);;
assert (eval (Sub (ConstS 2.5, ConstV [3.5; 4.5])) = S (-1.0));;
assert (eval (Sub (ConstV [2.5; 3.5; 4.5], ConstV [3.5; 4.5])) = V [-1.0; -1.0]);;
assert (eval (Add (ConstV [], ConstV [])) = V []);;

assert (eval (Add (Cond (T, ConstS 1.0, ConstS 2.0), Cond (F, ConstS 3.0, ConstS 4.0))) = S 5.0);;
assert (eval (Angle (ScalProd (ConstS 2.0, ConstV [1.0; 0.0]), ScalProd (ConstS 3.0, ConstV [1.0; 0.0]))) = S 0.0);;
assert (eval (Mag (Add (Inv (ConstV [4.0; 5.0]), ConstV [1.0; 1.0]))) = S 5.0);;
assert (eval (IsZero (Add (ConstS 1e-7, Inv (ConstS 1e-7)))) = B true);;
assert (eval (IsZero (Add (ConstV [1e-7; 1e-7], Inv (ConstV [1e-7; 1e-7])))) = B true);;
assert (eval (Cond (IsZero (ConstV [1e-7; 1e-7]), DotProd (ConstV [1.0; 2.0], ConstV [3.0; 4.0]), DotProd (ConstV [2.0; 0.0], ConstV [5.0; 6.0]))) = S 11.0);; *)
