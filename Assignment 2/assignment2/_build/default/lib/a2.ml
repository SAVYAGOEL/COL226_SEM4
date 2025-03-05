exception DimensionError

type vector = float list

let epsilon = 1e-6

module Vector = struct
  let _rev (v : vector) : vector =
    let rec __rev acc v =
      match v with [] -> acc | x :: v0 -> __rev (x :: acc) v0
    in
    __rev [] v

  let create n x : vector =
    if n < 1 then raise DimensionError
    else
      let rec _create acc a b =
        match a with 0 -> acc | _ -> _create (b :: acc) (a - 1) b
      in
      _create [] n x

  let dim (v : vector) =
    if v = [] then raise DimensionError
    else
      let rec _dim acc v =
        match v with [] -> acc | _ :: v0 -> _dim (acc + 1) v0
      in
      _dim 0 v

  let is_zero (v : vector) =
    if v = [] then raise DimensionError
    else
      let rec _is_zero v =
        match v with
        | [] -> true
        | x :: v0 -> if abs_float x >= epsilon then false else _is_zero v0
      in
      _is_zero v

  let unit n j : vector =
    if j < 1 || j > n || n < 1 then raise DimensionError
    else
      let rec _unit acc n j =
        match n with
        | 0 -> acc
        | _ ->
            if n = j then _unit (1.0 :: acc) (n - 1) j
            else _unit (0.0 :: acc) (n - 1) j
      in
      _unit [] n j

  let scale c (v : vector) : vector =
    if v = [] then raise DimensionError else List.map (fun x -> c *. x) v

  let addv (v1 : vector) (v2 : vector) : vector =
    if v1 = [] || v2 = [] then raise DimensionError
    else
      let rec _addv acc v1 v2 =
        match (v1, v2) with
        | [], [] -> acc
        | v1_hd :: v1_tl, v2_hd :: v2_tl ->
            _addv ((v1_hd +. v2_hd) :: acc) v1_tl v2_tl
        | _ -> raise DimensionError
      in
      _rev (_addv [] v1 v2)

  let dot_prod (v1 : vector) (v2 : vector) =
    if v1 = [] || v2 = [] then raise DimensionError
    else
      let rec _dot acc v1 v2 =
        match (v1, v2) with
        | [], [] -> acc
        | v1_hd :: v1_tl, v2_hd :: v2_tl ->
            _dot (acc +. (v1_hd *. v2_hd)) v1_tl v2_tl
        | _ -> raise DimensionError
      in
      _dot 0.0 v1 v2

  let inv (v : vector) : vector =
    if v = [] then raise DimensionError
    else
      let rec _inv acc v =
        match v with
        | [] -> acc
        | x :: v0 ->
            if abs_float x < epsilon then _inv (0.0 :: acc) v0
            else _inv (-.x :: acc) v0
      in
      _rev (_inv [] v)

  let length (v : vector) =
    if v = [] then raise DimensionError
    else
      let rec _length acc v =
        match v with [] -> sqrt acc | x :: v0 -> _length ((x *. x) +. acc) v0
      in
      _length 0.0 v

  let angle (v1 : vector) (v2 : vector) =
    if v1 = [] || v2 = [] then raise DimensionError
    else if Float.abs (length v1) < epsilon || Float.abs (length v2) < epsilon
    then
      raise
        (Failure
           "Cannot calculate angle between 2 vectors if either is a zero vector")
    else
      let temp = dot_prod v1 v2 /. (length v1 *. length v2) in
      if temp > 1.0 then acos 1.0
      else if temp < -1.0 then acos (-1.0)
      else acos temp
end

(* You may also enrich your language by introducing other operations such as 

Done: generating unit vectors 
Done: generating the zero vector
projecting a vector onto a hyperplane defined by a subset of axes (dimensions)
Done: subtracting scalars or subtracting vectors
transforming a vector (e.g., rotating it, etc.)  *)

(*Type checker*)
type types =
  | Bool (* boolean *)
  | Scalar (* a scalar — any float value *)
  | Vector of int (* n-dimensional with elements of type float *)

type expr =
  | T
  | F (* Boolean constants *)
  | GenUnit of int * int
    (* generates a unit vector of dimension n with 1 at position j and 0 elsewhere *)
  | GenZero of int (* generates a zero vector of dimension n *)
  | Sub of expr * expr
    (* overloaded — subtraction of two scalars or subtraction of two vectors of the same dimension *)
  | ConstS of float (* Scalar constants *)
  | ConstV of float list (* Vector constants *)
  | Add of expr * expr
    (* overloaded — disjunction of two booleans or sum of  two scalars or sum of two vectors of the same dimension *)
  | Inv of expr
    (* overloaded — negation of a boolean or additive inverse of  a scalar or additive inverse of a vector *)
  | ScalProd of expr * expr
    (* overloaded — conjunction of two booleans or product of a scalar with another scalar or product of a scalar and a vector *)
  | DotProd of
      expr * expr (* dot product of two vectors of the same dimension *)
  | Mag of
      expr (* overloaded: absolute value of a scalar or magnitude of a vector *)
  | Angle of expr * expr (* in radians, the angle between two vectors *)
  | IsZero of expr
    (* overloaded: checks if a boolean expression evaluates to F,  or if a given scalar is within epsilon of 0.0 or is the vector close — within epsilon on each coordinate —  to the zero vector *)
  | Cond of expr * expr * expr

(* "if_then_else" --  if the first expr evaluates to T then evaluate the second expr, else the third expr *)

exception Wrong of expr

let rec type_of e =
  try
    match e with
    | T -> Bool
    | F -> Bool
    | GenUnit (n, _) -> Vector n
    | GenZero n -> Vector n
    | Sub (e1, e2) -> (
        let t_e1 = type_of e1 and t_e2 = type_of e2 in
        match (t_e1, t_e2) with
        | Scalar, Scalar -> Scalar
        | Vector size1, Vector size2 ->
            if size1 = size2 then Vector size1 else raise (Wrong e)
        | _, _ -> raise (Wrong e))
    | ConstS _ -> Scalar
    | ConstV flst ->
        let n = Vector.dim flst in
        if n > 0 then Vector n else raise (Wrong e)
    | Add (e1, e2) -> (
        let t_e1 = type_of e1 and t_e2 = type_of e2 in
        match (t_e1, t_e2) with
        | Bool, Bool -> Bool
        | Scalar, Scalar -> Scalar
        | Vector size1, Vector size2 ->
            if size1 = size2 then Vector size1 else raise (Wrong e)
        | _, _ -> raise (Wrong e))
    | Inv e1 -> (
        let t_e1 = type_of e1 in
        match t_e1 with Bool -> Bool | Scalar -> Scalar | Vector n -> Vector n)
    | ScalProd (e1, e2) -> (
        let t_e1 = type_of e1 and t_e2 = type_of e2 in
        match (t_e1, t_e2) with
        | Bool, Bool -> Bool
        | Scalar, Scalar -> Scalar
        | Scalar, Vector n -> Vector n
        | Vector n, Scalar -> Vector n
        | _, _ -> raise (Wrong e))
    | DotProd (e1, e2) -> (
        let t_e1 = type_of e1 and t_e2 = type_of e2 in
        match (t_e1, t_e2) with
        | Vector size1, Vector size2 ->
            if size1 = size2 then Scalar else raise (Wrong e)
        | _, _ -> raise (Wrong e))
    | Mag e1 -> (
        let t_e1 = type_of e1 in
        match t_e1 with
        | Scalar -> Scalar
        | Vector _ -> Scalar
        | _ -> raise (Wrong e))
    | Angle (e1, e2) -> (
        let t_e1 = type_of e1 and t_e2 = type_of e2 in
        match (t_e1, t_e2) with
        | Vector size1, Vector size2 ->
            if size1 = size2 then Scalar else raise (Wrong e)
        | _, _ -> raise (Wrong e))
    | IsZero e1 ->
        let _ = type_of e1 in
        Bool
    | Cond (e1, e2, e3) -> (
        let t_e1 = type_of e1 and t_e2 = type_of e2 and t_e3 = type_of e3 in
        match t_e1 with
        | Bool -> if t_e2 = t_e3 then t_e2 else raise (Wrong e)
        | _ -> raise (Wrong e))
  with _ -> raise (Wrong e)

(*Definitional Interpreter*)

type values = B of bool | S of float | V of vector

let rec eval (e : expr) =
  try
    match e with
    | T -> B true
    | F -> B false
    | GenUnit (n, j) -> V (Vector.unit n j)
    | GenZero n -> V (Vector.create n 0.0)
    | Sub (e1, e2) -> (
        let v1 = eval e1 and v2 = eval e2 in
        match (v1, v2) with
        | S f1, S f2 -> S (f1 -. f2)
        | V v1, V v2 -> V (Vector.addv v1 (Vector.scale (-1.0) v2))
        | _, _ -> raise (Wrong e))
    | ConstS f -> S f
    | ConstV flst -> if Vector.dim flst > 0 then V flst else raise (Wrong e)
    | Add (e1, e2) -> (
        let v1 = eval e1 and v2 = eval e2 in
        match (v1, v2) with
        | B b1, B b2 -> B (b1 || b2)
        | S f1, S f2 -> S (f1 +. f2)
        | V v1, V v2 -> V (Vector.addv v1 v2)
        | _, _ -> raise (Wrong e))
    | Inv e1 -> (
        let v1 = eval e1 in
        match v1 with
        | B b -> B (not b)
        | S f -> S (-.f)
        | V v -> V (Vector.inv v))
    | ScalProd (e1, e2) -> (
        let v1 = eval e1 and v2 = eval e2 in
        match (v1, v2) with
        | B b1, B b2 -> B (b1 && b2)
        | S f1, S f2 -> S (f1 *. f2)
        | S f, V v -> V (Vector.scale f v)
        | V v, S f -> V (Vector.scale f v)
        | _, _ -> raise (Wrong e))
    | DotProd (e1, e2) -> (
        let v1 = eval e1 and v2 = eval e2 in
        match (v1, v2) with
        | V v1, V v2 -> S (Vector.dot_prod v1 v2)
        | _, _ -> raise (Wrong e))
    | Mag e1 -> (
        let v1 = eval e1 in
        match v1 with
        | S f -> S (abs_float f)
        | V v -> S (Vector.length v)
        | _ -> raise (Wrong e))
    | Angle (e1, e2) -> (
        let v1 = eval e1 and v2 = eval e2 in
        match (v1, v2) with
        | V v1, V v2 -> S (Vector.angle v1 v2)
        | _, _ -> raise (Wrong e))
    | IsZero e1 -> (
        let v1 = eval e1 in
        match v1 with
        | B b -> B (not b)
        | S f -> B (abs_float f < epsilon)
        | V v -> B (Vector.is_zero v))
    | Cond (e1, e2, e3) -> (
        let v1 = eval e1 in
        match v1 with
        | B b -> if b then eval e2 else eval e3
        | _ -> raise (Wrong e))
  with _ -> raise (Wrong e)
