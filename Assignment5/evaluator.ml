open Ast
open Lexing
open List

exception Eval_error of string

(* Value type remains the same *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VVecInt of int list
  | VVecFloat of float list
  | VMatInt of int list list
  | VMatFloat of float list list
  | VUnit

let type_of_value = function
  | VInt _ -> "int"
  | VFloat _ -> "float"
  | VBool _ -> "bool"
  | VVecInt _ -> "vector int"
  | VVecFloat _ -> "vector float"
  | VMatInt _ -> "matrix int"
  | VMatFloat _ -> "matrix float"
  | VUnit -> "unit"

(* Environment using Map *)
module Env = Map.Make(String)
type env = value Env.t

(* Helper function to print values *)
let string_of_value = function
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VVecInt v -> "[" ^ String.concat ", " (List.map string_of_int v) ^ "]"
  | VVecFloat v -> "[" ^ String.concat ", " (List.map string_of_float v) ^ "]"
  | VMatInt m -> "[" ^ String.concat ", " (List.map (fun row -> "[" ^ String.concat ", " (List.map string_of_int row) ^ "]") m) ^ "]"
  | VMatFloat m -> "[" ^ String.concat ", " (List.map (fun row -> "[" ^ String.concat ", " (List.map string_of_float row) ^ "]") m) ^ "]"
  | VUnit -> "unit"

(* Conversion functions *)
let string_to_vec_int s =
  (* Printf.printf "[DEBUG] string_to_vec_int: %s\n" s; *)
  let s = String.sub s 1 (String.length s - 2) in
  let trimmed_s = String.trim s in
  if trimmed_s = "" then [] 
  else List.map (fun x -> int_of_string (String.trim x)) (String.split_on_char ',' trimmed_s)

let string_to_vec_float s =
  (* Printf.printf "[DEBUG] string_to_vec_float: %s\n" s; *)
  let s = String.sub s 1 (String.length s - 2) in
  let trimmed_s = String.trim s in
  if trimmed_s = "" then [] 
  else List.map (fun x -> float_of_string (String.trim x)) (String.split_on_char ',' trimmed_s)

let string_to_mat_int s =
  let s = String.trim s in
  let s =
    if String.length s >= 4 &&
       s.[0] = '[' && s.[1] = '[' &&
       s.[String.length s - 2] = ']' && s.[String.length s - 1] = ']' then
      String.sub s 2 (String.length s - 4) |> String.trim
    else if String.length s >= 2 &&
            s.[0] = '[' && s.[String.length s - 1] = ']' then
      String.sub s 1 (String.length s - 2) |> String.trim
    else s
  in
  (* print_endline ("[DEBUG] matrix string after outer trim (int): " ^ s); *)
  if s = "" then []
  else
    let row_strings = String.split_on_char ']' s in
    (* Remove leading comma then leading '[' and trim trailing comma and spaces *)
    let row_strings = List.map (fun row ->
      let row = String.trim row in
      let row = if String.starts_with ~prefix:"," row then String.sub row 1 (String.length row - 1) else row in
      let row = String.trim row in
      let row = if String.starts_with ~prefix:"[" row then String.sub row 1 (String.length row - 1) else row in
      let row = String.trim row in
      let row = if String.ends_with ~suffix:"," row then String.sub row 0 (String.length row - 1) else row in
      String.trim row
    ) row_strings in
    let row_strings = List.filter (fun row -> row <> "" && not (String.contains row '[')) row_strings in
    try
      List.map (fun row ->
        string_to_vec_int ("[" ^ row ^ "]")
      ) row_strings
    with
    | Failure msg -> raise (Eval_error ("Failed to parse matrix row: " ^ msg))
    | exn -> raise (Eval_error ("Unexpected error in matrix parsing: " ^ Printexc.to_string exn))

let string_to_mat_float s =
  let s = String.trim s in
  let s =
    if String.length s >= 4 &&
       s.[0] = '[' && s.[1] = '[' &&
       s.[String.length s - 2] = ']' && s.[String.length s - 1] = ']' then
      String.sub s 2 (String.length s - 4) |> String.trim
    else if String.length s >= 2 &&
            s.[0] = '[' && s.[String.length s - 1] = ']' then
      String.sub s 1 (String.length s - 2) |> String.trim
    else s
  in
  (* print_endline ("[DEBUG] matrix string after outer trim (float): " ^ s); *)
  if s = "" then []
  else
    let row_strings = String.split_on_char ']' s in
    let row_strings = List.map (fun row ->
      let row = String.trim row in
      let row = if String.starts_with ~prefix:"," row then String.sub row 1 (String.length row - 1) else row in
      let row = String.trim row in
      let row = if String.starts_with ~prefix:"[" row then String.sub row 1 (String.length row - 1) else row in
      let row = String.trim row in
      let row = if String.ends_with ~suffix:"," row then String.sub row 0 (String.length row - 1) else row in
      String.trim row
    ) row_strings in
    let row_strings = List.filter (fun row -> row <> "" && not (String.contains row '[')) row_strings in
    try
      List.map (fun row ->
        string_to_vec_float ("[" ^ row ^ "]")
      ) row_strings
    with
    | Failure msg -> raise (Eval_error ("Failed to parse matrix row: " ^ msg))
    | exn -> raise (Eval_error ("Unexpected error in matrix parsing: " ^ Printexc.to_string exn))

(* Helper functions for vector and matrix operations *)
let vec_add_int v1 v2 = if List.length v1 <> List.length v2 then raise (Eval_error "Vector addition dimension mismatch") else List.map2 (+) v1 v2
let vec_add_float v1 v2 = if List.length v1 <> List.length v2 then raise (Eval_error "Vector addition dimension mismatch") else List.map2 (+.) v1 v2
let dot_prod_int v1 v2 = if List.length v1 <> List.length v2 then raise (Eval_error "Dot product dimension mismatch") else List.fold_left2 (fun acc x y -> acc + (x * y)) 0 v1 v2
let dot_prod_float v1 v2 = if List.length v1 <> List.length v2 then raise (Eval_error "Dot product dimension mismatch") else List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 v1 v2
let vec_len_int v = sqrt (float_of_int (List.fold_left (fun acc x -> acc + (x * x)) 0 v))
let vec_len_float v = sqrt (List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 v)
let angle_rad_common dot_prod len1 len2 =
  if len1 = 0.0 || len2 = 0.0 then 0.0
  else acos (max (-1.0) (min 1.0 (dot_prod /. (len1 *. len2))))
let angle_int v1 v2 =
  if List.length v1 <> List.length v2 then raise (Eval_error "Angle dimension mismatch");
  angle_rad_common (float_of_int (dot_prod_int v1 v2)) (vec_len_int v1) (vec_len_int v2)
let angle_float v1 v2 =
  if List.length v1 <> List.length v2 then raise (Eval_error "Angle dimension mismatch");
  angle_rad_common (dot_prod_float v1 v2) (vec_len_float v1) (vec_len_float v2)
let transpose_matrix m =
  match m with
  | [] -> []
  | []::_ -> []
  | (r::rs) as matrix ->
      let num_cols = List.length r in
      if List.exists (fun row -> List.length row <> num_cols) rs then
        raise (Eval_error "Transpose: matrix rows have different lengths");
      let rec get_col col_idx =
        if col_idx >= num_cols then [] else
          (List.map (fun row -> List.nth row col_idx) matrix) :: get_col (col_idx + 1)
      in get_col 0

let check_square_matrix m =
  let n = List.length m in
  List.for_all (fun row -> List.length row = n) m
  
let rec determinant_int m =
  if not (check_square_matrix m) then raise (Eval_error "Determinant: matrix is not square");
  match m with
  | [] -> 1
  | [[a]] -> a
  | [[a; b]; [c; d]] -> (a * d) - (b * c)
  | _ ->
      let remove_at idx lst =
        let rec aux i = function
          | [] -> []
          | x :: xs -> if i = idx then xs else x :: aux (i + 1) xs
        in aux 0 lst
      in
      let first_row = List.hd m in
      List.mapi (fun j a ->
        let sign = if j mod 2 = 0 then 1 else -1 in
        let minor = List.tl m |> List.map (fun row -> remove_at j row) in
        sign * a * (determinant_int minor)
      ) first_row
      |> List.fold_left (+) 0

let rec determinant_float m =
  if not (check_square_matrix m) then raise (Eval_error "Determinant: matrix is not square");
  match m with
  | [] -> 1.0
  | [[a]] -> a
  | [[a; b]; [c; d]] -> (a *. d) -. (b *. c)
  | _ ->
      let remove_at idx lst =
        let rec aux i = function
          | [] -> []
          | x :: xs -> if i = idx then xs else x :: aux (i + 1) xs
        in aux 0 lst
      in
      let first_row = List.hd m in
      List.mapi (fun j a ->
        let sign = if j mod 2 = 0 then 1.0 else -1.0 in
        let minor = List.tl m |> List.map (fun row -> remove_at j row) in
        sign *. a *. (determinant_float minor)
      ) first_row
      |> List.fold_left (+.) 0.0

(* Single eval function with lexical scoping *)
let rec eval (rho : env) (e : expr) : value * env =
  match e with
  | IntLit i -> (VInt i, rho)
  | FloatLit f -> (VFloat f, rho)
  | BoolLit b -> (VBool b, rho)
  | VecLit (IntVec (n, s)) ->
      let v = string_to_vec_int s in
      if List.length v = n then (VVecInt v, rho)
      else raise (Eval_error ("Vector dimension mismatch: expected " ^ string_of_int n ^ ", got " ^ string_of_int (List.length v)))
  | VecLit (FloatVec (n, s)) ->
      let v = string_to_vec_float s in
      if List.length v = n then (VVecFloat v, rho)
      else raise (Eval_error ("Vector dimension mismatch: expected " ^ string_of_int n ^ ", got " ^ string_of_int (List.length v)))
  | MatLit (IntMat (r, c, s)) ->
      let m = string_to_mat_int s in
      let actual_r = List.length m in
      let actual_c = if actual_r = 0 then 0 else List.length (List.hd m) in
      if actual_r = r && actual_c = c && List.for_all (fun row -> List.length row = c) m then (VMatInt m, rho)
      else raise (Eval_error ("Matrix dimension mismatch: expected " ^ string_of_int r ^ "x" ^ string_of_int c ^ ", got " ^ string_of_int actual_r ^ "x" ^ string_of_int actual_c))
  | MatLit (FloatMat (r, c, s)) ->
      let m = string_to_mat_float s in
      let actual_r = List.length m in
      let actual_c = if actual_r = 0 then 0 else List.length (List.hd m) in
      if actual_r = r && actual_c = c && List.for_all (fun row -> List.length row = c) m then (VMatFloat m, rho)
      else raise (Eval_error ("Matrix dimension mismatch: expected " ^ string_of_int r ^ "x" ^ string_of_int c ^ ", got " ^ string_of_int actual_r ^ "x" ^ string_of_int actual_c))

  | Var v ->
      (match Env.find_opt v rho with
       | Some value -> (value, rho)
       | None -> raise (Eval_error ("Undefined variable: " ^ v)))

  | Raise s -> raise (Eval_error s)

  | Plus (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> (VInt (i1 + i2), rho2)
       | VFloat f1, VFloat f2 -> (VFloat (f1 +. f2), rho2)
       | VInt i, VFloat f -> (VFloat (float_of_int i +. f), rho2)
       | VFloat f, VInt i -> (VFloat (f +. float_of_int i), rho2)
       | VMatInt m1, VMatInt m2 ->
           if List.length m1 <> List.length m2 || List.length (List.hd m1) <> List.length (List.hd m2) then
             raise (Eval_error "Matrix addition dimension mismatch");
           let result = List.map2 (fun row1 row2 -> List.map2 (+) row1 row2) m1 m2 in
           (VMatInt result, rho2)
       | VVecInt v1, VVecInt v2 ->
           if List.length v1 <> List.length v2 then
             raise (Eval_error "Vector addition dimension mismatch");
           let result = vec_add_int v1 v2 in
           (VVecInt result, rho2)
        | VVecFloat v1, VVecFloat v2 ->
            if List.length v1 <> List.length v2 then
              raise (Eval_error "Vector addition dimension mismatch");
            let result = vec_add_float v1 v2 in
            (VVecFloat result, rho2)
        | _ -> raise (Eval_error "Plus: incompatible types"))
  | Exp (e1, e2) -> 
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> (VInt (int_of_float ((float_of_int i1) ** (float_of_int i2))), rho2)
       | VFloat f1, VFloat f2 -> (VFloat (f1 ** f2), rho2)
       | VInt i, VFloat f -> (VFloat ((float_of_int i) ** f), rho2)
       | VFloat f, VInt i -> (VFloat (f ** float_of_int i), rho2)
       | _ -> raise (Eval_error "Exp: incompatible types"))
  | Minus (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> (VInt (i1 - i2), rho2)
       | VFloat f1, VFloat f2 -> (VFloat (f1 -. f2), rho2)
       | VInt i, VFloat f -> (VFloat (float_of_int i -. f), rho2)
       | VFloat f, VInt i -> (VFloat (f -. float_of_int i), rho2)
       | VMatInt m1, VMatInt m2 ->
           if List.length m1 <> List.length m2 || List.length (List.hd m1) <> List.length (List.hd m2) then
             raise (Eval_error "Matrix subtraction dimension mismatch");
           let result = List.map2 (fun row1 row2 -> List.map2 (-) row1 row2) m1 m2 in
           (VMatInt result, rho2)
        | VMatFloat m1, VMatFloat m2 -> 
            if List.length m1 <> List.length m2 || List.length (List.hd m1) <> List.length (List.hd m2) then
              raise (Eval_error "Matrix subtraction dimension mismatch");
            let result = List.map2 (fun row1 row2 -> List.map2 (-.) row1 row2) m1 m2 in
            (VMatFloat result, rho2)
        | VVecInt v1, VVecInt v2 ->
            if List.length v1 <> List.length v2 then
              raise (Eval_error "Vector subtraction dimension mismatch");
            let result = List.map2 (-) v1 v2 in
            (VVecInt result, rho2)
        | VVecFloat v1, VVecFloat v2 ->
            if List.length v1 <> List.length v2 then
              raise (Eval_error "Vector subtraction dimension mismatch");
            let result = List.map2 (-.) v1 v2 in
            (VVecFloat result, rho2)
       | _ -> raise (Eval_error "Minus: incompatible types"))
  | Times (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> (VInt (i1 * i2), rho2)
       | VFloat f1, VFloat f2 -> (VFloat (f1 *. f2), rho2)
       | VInt i, VFloat f -> (VFloat (float_of_int i *. f), rho2)
       | VFloat f, VInt i -> (VFloat (f *. float_of_int i), rho2)
       | VMatInt m1, VMatInt m2 ->
           let c1 = List.length (List.hd m1) in
           let r2 = List.length m2 in
           if c1 <> r2 then
             raise (Eval_error "Matrix multiplication dimension mismatch")
           else
             let m2_t = transpose_matrix m2 in
             let result = List.map (fun row ->
                             List.map (fun col ->
                               dot_prod_int row col
                             ) m2_t
                           ) m1 in
             (VMatInt result, rho2)
      | VMatFloat m1, VMatFloat m2 ->
           let c1 = List.length (List.hd m1) in
           let r2 = List.length m2 in
           if c1 <> r2 then
             raise (Eval_error "Matrix multiplication dimension mismatch")
           else
             let m2_t = transpose_matrix m2 in
             let result = List.map (fun row ->
                             List.map (fun col ->
                               dot_prod_float row col
                             ) m2_t
                           ) m1 in
             (VMatFloat result, rho2)
      | VMatInt m1, VVecInt v2 ->
           let c1 = List.length (List.hd m1) in
           if c1 <> List.length v2 then
             raise (Eval_error "Matrix-vector multiplication dimension mismatch")
           else
             let result = List.map (fun row ->
                             dot_prod_int row v2
                           ) m1 in
             (VVecInt result, rho2)
      | VMatFloat m1, VVecFloat v2 ->
           let c1 = List.length (List.hd m1) in
           if c1 <> List.length v2 then
             raise (Eval_error "Matrix-vector multiplication dimension mismatch")
           else
             let result = List.map (fun row ->
                             dot_prod_float row v2
                           ) m1 in
             (VVecFloat result, rho2)
       | _ -> raise (Eval_error "Times: incompatible types"))
  | Div (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> if i2 = 0 then raise (Eval_error "Division by zero") else (VInt (i1 / i2), rho2)
       | VFloat f1, VFloat f2 -> if f2 = 0.0 then raise (Eval_error "Division by zero") else (VFloat (f1 /. f2), rho2)
       | VInt i, VFloat f -> if f = 0.0 then raise (Eval_error "Division by zero") else (VFloat (float_of_int i /. f), rho2)
       | VFloat f, VInt i -> if i = 0 then raise (Eval_error "Division by zero") else (VFloat (f /. float_of_int i), rho2)
       | _ -> raise (Eval_error "Div: incompatible types"))
  | And (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      (match v1 with
       | VBool false -> (VBool false, rho1)
       | VBool true -> eval rho1 e2
       | _ -> raise (Eval_error "And: first operand must be boolean"))
  | Or (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      (match v1 with
       | VBool true -> (VBool true, rho1)
       | VBool false -> eval rho1 e2
       | _ -> raise (Eval_error "Or: first operand must be boolean"))
  | Eq (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> (VBool (i1 = i2), rho2)
       | VFloat f1, VFloat f2 -> (VBool (abs_float (f1 -. f2) < 1e-9), rho2)
       | VBool b1, VBool b2 -> (VBool (b1 = b2), rho2)
       | VVecInt v1', VVecInt v2' -> (VBool (v1' = v2'), rho2)
       | VVecFloat v1', VVecFloat v2' -> (VBool (List.length v1' = List.length v2' && List.for_all2 (fun a b -> abs_float (a -. b) < 1e-9) v1' v2'), rho2)
       | VMatInt m1', VMatInt m2' -> (VBool (m1' = m2'), rho2)
       | VMatFloat m1', VMatFloat m2' -> (VBool (List.length m1' = List.length m2' && List.for_all2 (fun r1 r2 -> List.length r1 = List.length r2 && List.for_all2 (fun a b -> abs_float (a -. b) < 1e-9) r1 r2) m1' m2'), rho2)
       | VUnit, VUnit -> (VBool true, rho2)
       | _ -> (VBool false, rho2))
  | Neq (e1, e2) ->
      let (v_eq, rho') = eval rho (Eq (e1, e2)) in
      (match v_eq with VBool b -> (VBool (not b), rho') | _ -> failwith "Internal error: Eq should return VBool")
  | Lt (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> (VBool (i1 < i2), rho2)
       | VFloat f1, VFloat f2 -> (VBool (f1 < f2), rho2)
       | VInt i, VFloat f -> (VBool (float_of_int i < f), rho2)
       | VFloat f, VInt i -> (VBool (f < float_of_int i), rho2)
       | _ -> raise (Eval_error "Lt: incompatible types"))
  | Gt (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> (VBool (i1 > i2), rho2)
       | VFloat f1, VFloat f2 -> (VBool (f1 > f2), rho2)
       | VInt i, VFloat f -> (VBool (float_of_int i > f), rho2)
       | VFloat f, VInt i -> (VBool (f > float_of_int i), rho2)
       | _ -> raise (Eval_error "Gt: incompatible types"))
  | Leq (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> (VBool (i1 <= i2), rho2)
       | VFloat f1, VFloat f2 -> (VBool (f1 <= f2), rho2)
       | VInt i, VFloat f -> (VBool (float_of_int i <= f), rho2)
       | VFloat f, VInt i -> (VBool (f <= float_of_int i), rho2)
       | _ -> raise (Eval_error "Leq: incompatible types"))
  | Geq (e1, e2) ->
      let (v1, rho1) = eval rho e1 in
      let (v2, rho2) = eval rho1 e2 in
      (match v1, v2 with
       | VInt i1, VInt i2 -> (VBool (i1 >= i2), rho2)
       | VFloat f1, VFloat f2 -> (VBool (f1 >= f2), rho2)
       | VInt i, VFloat f -> (VBool (float_of_int i >= f), rho2)
       | VFloat f, VInt i -> (VBool (f >= float_of_int i), rho2)
       | _ -> raise (Eval_error "Geq: incompatible types"))

  | Uminus e1 ->
      let (v1, rho1) = eval rho e1 in
      (match v1 with
       | VInt i -> (VInt (-i), rho1)
       | VFloat f -> (VFloat (-.f), rho1)
       | VVecInt v -> (VVecInt (List.map (~-) v), rho1)
       | VVecFloat v -> (VVecFloat (List.map (~-.) v), rho1)
       | _ -> raise (Eval_error "Uminus: operand not numeric or vector"))
  | Not e1 ->
      let (v1, rho1) = eval rho e1 in
      (match v1 with VBool b -> (VBool (not b), rho1) | _ -> raise (Eval_error "Not: operand must be boolean"))
  | Abs e1 ->
      let (v1, rho1) = eval rho e1 in
      (match v1 with
       | VInt i -> (VInt (abs i), rho1)
       | VFloat f -> (VFloat (abs_float f), rho1)
       | _ -> raise (Eval_error "Abs: operand must be numeric"))
  | Sqrt e1 ->
      let (v1, rho1) = eval rho e1 in
      (match v1 with
       | VInt i -> if i < 0 then raise (Eval_error "Sqrt: negative integer") else (VFloat (sqrt (float_of_int i)), rho1)
       | VFloat f -> if f < 0.0 then raise (Eval_error "Sqrt: negative float") else (VFloat (sqrt f), rho1)
       | _ -> raise (Eval_error "Sqrt: operand must be numeric"))

  | Scale (scalar_expr, vm_expr) ->
      let (s_val, rho1) = eval rho scalar_expr in
      let (vm_val, rho2) = eval rho1 vm_expr in
      (match s_val, vm_val with
       | VInt s, VVecInt v -> (VVecInt (List.map (( * ) s) v), rho2)
       | VFloat s, VVecFloat v -> (VVecFloat (List.map (( *. ) s) v), rho2)
       (* | VInt s, VVecFloat v -> (VVecFloat (List.map (( *. ) (float_of_int s)) v), rho2)
       | VFloat s, VVecInt v -> (VVecFloat (List.map (fun x -> s *. float_of_int x) v), rho2) *)
       | VInt s, VMatInt m -> (VMatInt (List.map (List.map (( * ) s)) m), rho2)
       | VFloat s, VMatFloat m -> (VMatFloat (List.map (List.map (( *. ) s)) m), rho2)
       (* | VInt s, VMatFloat m -> (VMatFloat (List.map (List.map (( *. ) (float_of_int s))) m), rho2)
       | VFloat s, VMatInt m -> (VMatFloat (List.map (List.map (fun x -> s *. float_of_int x)) m), rho2) *)
       | _ -> raise (Eval_error "Scale: incompatible types"))
  | AddV (v1_expr, v2_expr) ->
      let (v1, rho1) = eval rho v1_expr in
      let (v2, rho2) = eval rho1 v2_expr in
      (match v1, v2 with
       | VVecInt v1', VVecInt v2' -> (VVecInt (vec_add_int v1' v2'), rho2)
       | VVecFloat v1', VVecFloat v2' -> (VVecFloat (vec_add_float v1' v2'), rho2)
       | _ -> raise (Eval_error "AddV: incompatible types"))
  | DotProd (v1_expr, v2_expr) ->
      let (v1, rho1) = eval rho v1_expr in
      let (v2, rho2) = eval rho1 v2_expr in
      (match v1, v2 with
       | VVecInt v1', VVecInt v2' -> (VInt (dot_prod_int v1' v2'), rho2)
       | VVecFloat v1', VVecFloat v2' -> (VFloat (dot_prod_float v1' v2'), rho2)
       | _ -> raise (Eval_error "DotProd: incompatible types"))
  | Angle (v1_expr, v2_expr) ->
      let (v1, rho1) = eval rho v1_expr in
      let (v2, rho2) = eval rho1 v2_expr in
      (match v1, v2 with
       | VVecInt v1', VVecInt v2' -> (VFloat (angle_int v1' v2'), rho2)
       | VVecFloat v1', VVecFloat v2' -> (VFloat (angle_float v1' v2'), rho2)
       | _ -> raise (Eval_error "Angle: incompatible types"))
  | Len v_expr ->
      let (v, rho1) = eval rho v_expr in
      (match v with
       | VVecInt v' -> (VFloat (vec_len_int v'), rho1)
       | VVecFloat v' -> (VFloat (vec_len_float v'), rho1)
       | _ -> raise (Eval_error "Len: operand is not a vector"))
  | Dimension v_expr ->
      let (v, rho1) = eval rho v_expr in
      (match v with
       | VVecInt v' -> (VInt (List.length v'), rho1)
       | VVecFloat v' -> (VInt (List.length v'), rho1)
       | _ -> raise (Eval_error "Dimension: operand is not a vector"))
  | Transpose m_expr ->
      let (m, rho1) = eval rho m_expr in
      (match m with
       | VMatInt m' -> (VMatInt (transpose_matrix m'), rho1)
       | VMatFloat m' -> (VMatFloat (transpose_matrix m'), rho1)
       | _ -> raise (Eval_error "Transpose: operand is not a matrix"))
  | Determinant m_expr ->
      let (m, rho1) = eval rho m_expr in
      (match m with
       | VMatInt m' -> (VInt (determinant_int m'), rho1)
       | VMatFloat m' -> (VFloat (determinant_float m'), rho1)
       | _ -> raise (Eval_error "Determinant: operand is not a matrix"))

  | Inv (mat_expr) ->
      let (m_val, rho1) = eval rho mat_expr in
      (match m_val with
       | VMatFloat m ->
           if not (check_square_matrix m) then raise (Eval_error "Inv: matrix is not square");
           let det = determinant_float m in
           if abs_float det < 1e-9 then raise (Eval_error "Inv: matrix is singular");
           let remove_at idx lst =
             let rec aux i = function
               | [] -> []
               | x :: xs -> if i = idx then xs else x :: aux (i + 1) xs
             in aux 0 lst
           in
           (* Compute the cofactor matrix *)
           let cofactor_matrix =
             List.mapi (fun i row ->
               List.mapi (fun j _ ->
                 let minor =
                   m
                   |> List.mapi (fun r row -> if r = i then None else Some (remove_at j row))
                   |> List.filter_map (fun x -> x)
                 in
                 let minor_det = determinant_float minor in
                 let sign = if (i + j) mod 2 = 0 then 1.0 else -1.0 in
                 sign *. minor_det
               ) row
             ) m
           in
           let adjugate = transpose_matrix cofactor_matrix in
           let inv_matrix = List.map (fun row -> List.map (fun x -> x /. det) row) adjugate in
           (VMatFloat inv_matrix, rho1)

       | VMatInt m ->
           (* Convert int matrix to float matrix and compute the inverse *)
           let m_float = List.map (fun row -> List.map float_of_int row) m in
           if not (check_square_matrix m_float) then raise (Eval_error "Inv: matrix is not square");
           let det = determinant_float m_float in
           if abs_float det < 1e-9 then raise (Eval_error "Inv: matrix is singular");
           let remove_at idx lst =
             let rec aux i = function
               | [] -> []
               | x :: xs -> if i = idx then xs else x :: aux (i + 1) xs
             in aux 0 lst
           in
           let cofactor_matrix =
             List.mapi (fun i row ->
               List.mapi (fun j _ ->
                 let minor =
                   m_float
                   |> List.mapi (fun r row -> if r = i then None else Some (remove_at j row))
                   |> List.filter_map (fun x -> x)
                 in
                 let minor_det = determinant_float minor in
                 let sign = if (i + j) mod 2 = 0 then 1.0 else -1.0 in
                 sign *. minor_det
               ) row
             ) m_float
           in
           let adjugate = transpose_matrix cofactor_matrix in
           let inv_matrix = List.map (fun row -> List.map (fun x -> x /. det) row) adjugate in
           (VMatFloat inv_matrix, rho1)
       | _ -> raise (Eval_error "Inv: operand is not a matrix"))
  | Minor (mat_expr, i_expr, j_expr) ->
      let (m, rho1) = eval rho mat_expr in
      let (i_val, rho2) = eval rho1 i_expr in
      let (j_val, rho3) = eval rho2 j_expr in
      let remove_at idx l =
        let rec aux i = function
          | [] -> []
          | x :: xs -> if i = idx then xs else x :: aux (i + 1) xs
        in
        aux 0 l
      in
      (match m, i_val, j_val with
       | VMatInt m_list, VInt i, VInt j ->
           if i < 0 || i >= List.length m_list then
             raise (Eval_error ("Minor: row index " ^ string_of_int i ^ " out of bounds"));
           let num_cols = List.length (List.hd m_list) in
           if j < 0 || j >= num_cols then
             raise (Eval_error ("Minor: column index " ^ string_of_int j ^ " out of bounds"));
           let new_matrix =
             m_list
             |> List.mapi (fun idx row -> if idx = i then None else Some (remove_at j row))
             |> List.filter_map (fun x -> x)
           in
           (VMatInt new_matrix, rho3)
       | VMatFloat m_list, VInt i, VInt j ->
           if i < 0 || i >= List.length m_list then
             raise (Eval_error ("Minor: row index " ^ string_of_int i ^ " out of bounds"));
           let num_cols = List.length (List.hd m_list) in
           if j < 0 || j >= num_cols then
             raise (Eval_error ("Minor: column index " ^ string_of_int j ^ " out of bounds"));
           let new_matrix =
             m_list
             |> List.mapi (fun idx row -> if idx = i then None else Some (remove_at j row))
             |> List.filter_map (fun x -> x)
           in
           (VMatFloat new_matrix, rho3)
       | _ -> raise (Eval_error "Minor: expected a matrix and integer indices"))

  | Index (vec_expr, i_expr) ->
      let (vec, rho1) = eval rho vec_expr in
      let (i_val, rho2) = eval rho1 i_expr in
      (match vec, i_val with
       | VVecInt v, VInt i' ->
           if i' < 0 || i' >= List.length v then raise (Eval_error ("Index: out of bounds " ^ string_of_int i'))
           else (VInt (List.nth v i'), rho2)
       | VVecFloat v, VInt i' ->
           if i' < 0 || i' >= List.length v then raise (Eval_error ("Index: out of bounds " ^ string_of_int i'))
           else (VFloat (List.nth v i'), rho2)
       | VMatInt m, VInt i' ->
           if i' < 0 || i' >= List.length m then raise (Eval_error ("Index: out of bounds " ^ string_of_int i'))
           else (VVecInt (List.nth m i'), rho2)
       | VMatFloat m, VInt i' ->
           if i' < 0 || i' >= List.length m then raise (Eval_error ("Index: out of bounds " ^ string_of_int i'))
           else (VVecFloat (List.nth m i'), rho2)
       | _, VInt _ -> raise (Eval_error "Index: not a vector")
       | _, _ -> raise (Eval_error "Index: index must be an integer"))
  | IndexMat (m_expr, i_expr, j_expr) ->
      let (m, rho1) = eval rho m_expr in
      let (i_val, rho2) = eval rho1 i_expr in
      let (j_val, rho3) = eval rho2 j_expr in
      (match m, i_val, j_val with
       | VMatInt m', VInt i', VInt j' ->
           if i' < 0 || i' >= List.length m' then raise (Eval_error ("IndexMat: row index " ^ string_of_int i' ^ " out of bounds"))
           else let row = List.nth m' i' in
           if j' < 0 || j' >= List.length row then raise (Eval_error ("IndexMat: column index " ^ string_of_int j' ^ " out of bounds"))
           else (VInt (List.nth row j'), rho3)
       | VMatFloat m', VInt i', VInt j' ->
           if i' < 0 || i' >= List.length m' then raise (Eval_error ("IndexMat: row index " ^ string_of_int i' ^ " out of bounds"))
           else let row = List.nth m' i' in
           if j' < 0 || j' >= List.length row then raise (Eval_error ("IndexMat: column index " ^ string_of_int j' ^ " out of bounds"))
           else (VFloat (List.nth row j'), rho3)
       | _, VInt _, VInt _ -> raise (Eval_error "IndexMat: not a matrix")
       | _, _, _ -> raise (Eval_error "IndexMat: indices must be integers"))

  | AssignExpr (lhs, rhs) ->
  let (value_rhs, rho1) = eval rho rhs in
  (match lhs with
    | Var v ->
        (* For simple variable assignment, we need to check type compatibility *)
        (match Env.find_opt v rho1 with
        | Some existing_val ->
            let expected_type = type_of_value existing_val in
            let actual_type = type_of_value value_rhs in
            if expected_type <> actual_type then
              raise (Eval_error ("AssignExpr: type mismatch - expected " ^ expected_type ^ ", got " ^ actual_type))
            else
              (VUnit, Env.add v value_rhs rho1)
        | None ->
            (* New variable; assume type from RHS (if declared earlier via VarType, typecheck_program should catch mismatches) *)
            (VUnit, Env.add v value_rhs rho1))
    | Index (vec_expr, i_expr) ->
        let (idx_val, rho2) = eval rho1 i_expr in
        (match vec_expr, idx_val with
        | Var vec_name, VInt idx ->
            let current_vec_val = match Env.find_opt vec_name rho2 with
              | Some v -> v
              | None -> raise (Eval_error ("Assign Index: undefined vector '" ^ vec_name ^ "'"))
            in
            (match current_vec_val with
              | VVecInt v_list ->
                  (match value_rhs with
                  | VInt new_elem_val ->
                      if idx < 0 || idx >= List.length v_list then raise (Eval_error ("Assign Index: out of bounds " ^ string_of_int idx));
                      let new_vec_list = List.mapi (fun i old_val -> if i = idx then new_elem_val else old_val) v_list in
                      (VUnit, Env.add vec_name (VVecInt new_vec_list) rho2)
                  | _ -> raise (Eval_error "Assign Index: type mismatch - expected int"))
              | VVecFloat v_list ->
                  (match value_rhs with
                  | VFloat new_elem_val ->
                      if idx < 0 || idx >= List.length v_list then raise (Eval_error ("Assign Index: out of bounds " ^ string_of_int idx));
                      let new_vec_list = List.mapi (fun i old_val -> if i = idx then new_elem_val else old_val) v_list in
                      (VUnit, Env.add vec_name (VVecFloat new_vec_list) rho2)
                  | _ -> raise (Eval_error "Assign Index: type mismatch - expected float"))
              | _ -> raise (Eval_error ("Assign Index: '" ^ vec_name ^ "' is not a vector")))
        | Var _, _ -> raise (Eval_error "Assign Index: index must be an integer")
        | _ -> raise (Eval_error "Assign Index: LHS must be a variable"))
    | IndexMat (m_expr, i_expr, j_expr) ->
        let (i_val, rho2) = eval rho1 i_expr in
        let (j_val, rho3) = eval rho2 j_expr in
        (match m_expr, i_val, j_val with
        | Var mat_name, VInt r_idx, VInt c_idx ->
            let current_mat_val = match Env.find_opt mat_name rho3 with
              | Some m -> m
              | None -> raise (Eval_error ("Assign IndexMat: undefined matrix '" ^ mat_name ^ "'"))
            in
            (match current_mat_val with
              | VMatInt m_list ->
                  (match value_rhs with
                  | VInt new_elem_val ->
                      if r_idx < 0 || r_idx >= List.length m_list then raise (Eval_error ("Assign IndexMat: row index " ^ string_of_int r_idx ^ " out of bounds"));
                      let new_mat_list = List.mapi (fun r row ->
                        if r = r_idx then
                          (if c_idx < 0 || c_idx >= List.length row then raise (Eval_error ("Assign IndexMat: column index " ^ string_of_int c_idx ^ " out of bounds"));
                            List.mapi (fun c old_val -> if c = c_idx then new_elem_val else old_val) row)
                        else row
                      ) m_list in
                      (VUnit, Env.add mat_name (VMatInt new_mat_list) rho3)
                  | _ -> raise (Eval_error "Assign IndexMat: type mismatch - expected int"))
              | VMatFloat m_list ->
                  (match value_rhs with
                  | VFloat new_elem_val ->
                      if r_idx < 0 || r_idx >= List.length m_list then raise (Eval_error ("Assign IndexMat: row index " ^ string_of_int r_idx ^ " out of bounds"));
                      let new_mat_list = List.mapi (fun r row ->
                        if r = r_idx then
                          (if c_idx < 0 || c_idx >= List.length row then raise (Eval_error ("Assign IndexMat: column index " ^ string_of_int c_idx ^ " out of bounds"));
                            List.mapi (fun c old_val -> if c = c_idx then new_elem_val else old_val) row)
                        else row
                      ) m_list in
                      (VUnit, Env.add mat_name (VMatFloat new_mat_list) rho3)
                  | _ -> raise (Eval_error "Assign IndexMat: type mismatch - expected float"))
              | _ -> raise (Eval_error ("Assign IndexMat: '" ^ mat_name ^ "' is not a matrix")))
        | Var _, _, _ -> raise (Eval_error "Assign IndexMat: indices must be integers")
        | _ -> raise (Eval_error "Assign IndexMat: LHS must be a variable"))
    | _ -> raise (Eval_error "AssignExpr: LHS not assignable"))

  | Seq (e1, e2) ->
      let (_, rho1) = eval rho e1 in
      eval rho1 e2

  | If (cond, thn, els) ->
      let (cond_val, rho1) = eval rho cond in
      (match cond_val with
       | VBool true -> eval rho1 thn
       | VBool false -> eval rho1 els
       | v -> raise (Eval_error ("If: condition must be boolean, got " ^ string_of_value v)))

  | For (v, start_expr, stop_expr, body) ->
      let (start_val, rho1) = eval rho start_expr in
      let (stop_val, rho2) = eval rho1 stop_expr in
      (match start_val, stop_val with
       | VInt s, VInt e ->
           if s <= e then
             let rec loop i env =
               if i > e then (VUnit, env)
               else
                 let env' = Env.add v (VInt i) env in
                 let (_, env'') = eval env' body in
                 loop (i + 1) env''
             in
             loop s rho2
           else
             let rec loop i env =
               if i < e then (VUnit, env)
               else
                 let env' = Env.add v (VInt i) env in
                 let (_, env'') = eval env' body in
                 loop (i - 1) env''
             in
             loop s rho2
       | _, _ -> raise (Eval_error "For: loop bounds must be integers"))

  | While (cond_expr, body) ->
      let rec loop env =
        let (cond_val, env') = eval env cond_expr in
        match cond_val with
        | VBool true ->
            let (_, env'') = eval env' body in
            loop env''
        | VBool false -> (VUnit, env')
        | v -> raise (Eval_error ("While: condition must be boolean, got " ^ string_of_value v))
      in
      loop rho

  | Print s ->
      (match Env.find_opt s rho with
       | Some value -> print_endline (string_of_value value); (VUnit, rho)
       | None -> print_endline s; (VUnit, rho))

  | Input prompt_string ->
      flush stdout;
      let input_str =
        if String.ends_with ~suffix:".txt" prompt_string then
          (* File input: read the entire file and append a ";" at the end *)
          try
            let ic = open_in prompt_string in
            let len = in_channel_length ic in
            let content = really_input_string ic len in
            close_in ic;
            content  (* Append semicolon *)
          with Sys_error msg ->
            raise (Eval_error ("Input: failed to read file '" ^ prompt_string ^ "': " ^ msg))
        else
          (* Stdin input: read until EOF is encountered *)
          let buffer = Buffer.create 256 in
          let rec read_all () =
            try
              let line = read_line () in
              Buffer.add_string buffer line;
              Buffer.add_char buffer '\n';
              read_all ()
            with End_of_file ->
              ()
          in
          print_string prompt_string;
          flush stdout;
          read_all ();
          Buffer.contents buffer
      in
      flush stdout;
      let lexbuf = Lexing.from_string (input_str ^ ";") in
      let parsed_expr =
        try 
          Parser.program Lexer.tokenize lexbuf
        with e ->
          flush stdout;
          raise (Eval_error ("Input: invalid syntax in '" ^ input_str ^ "'"))
      in
      flush stdout;
      let (value, rho') = eval rho parsed_expr in
      flush stdout;
      (value, rho')

  | VarType (typ_name, v, dims, base_type_opt) ->
      let default_value = match typ_name, dims, base_type_opt with
        | "int", None, None -> VInt 0
        | "float", None, None -> VFloat 0.0
        | "bool", None, None -> VBool false
        | "vector", Some (n, None), Some "int" -> VVecInt (List.init n (fun _ -> 0))
        | "vector", Some (n, None), Some "float" -> VVecFloat (List.init n (fun _ -> 0.0))
        | "matrix", Some (r, Some c), Some "int" -> VMatInt (List.init r (fun _ -> List.init c (fun _ -> 0)))
        | "matrix", Some (r, Some c), Some "float" -> VMatFloat (List.init r (fun _ -> List.init c (fun _ -> 0.0)))
        | _ -> raise (Eval_error ("Invalid variable type declaration"))
      in
      (VUnit, Env.add v default_value rho)

(* Entry point for evaluating a program *)
let eval_program (initial_rho : env) (prog : expr) : value * env =
  eval initial_rho prog