(* ast.ml *)
type typ =
  | TInt
  | TFloat
  | TBool
  | TVector of int * typ
  | TMatrix of int * int * typ
  | TUnit

and vector_type =
  | IntVec of int * string
  | FloatVec of int * string

and matrix_type =
  | IntMat of int * int * string
  | FloatMat of int * int * string

and expr =
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | VecLit of vector_type
  | MatLit of matrix_type
  | Var of string
  | Raise of string
  | Plus of expr * expr
  | Exp of expr * expr
  | Minus of expr * expr
  | Uminus of expr
  | Times of expr * expr
  | Div of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Leq of expr * expr
  | Geq of expr * expr
  | Not of expr
  | Abs of expr
  | Sqrt of expr
  | Scale of expr * expr
  | AddV of expr * expr
  | DotProd of expr * expr
  | Angle of expr * expr
  | Len of expr
  | Dimension of expr
  | Transpose of expr
  | Determinant of expr
  | Inv of expr
  | Minor of expr * expr * expr
  | Index of expr * expr
  | IndexMat of expr * expr * expr
  | AssignExpr of expr * expr
  | Seq of expr * expr
  | If of expr * expr * expr
  | For of string * expr * expr * expr
  | While of expr * expr
  | Print of string
  | Input of string
  | VarType of string * string * (int * int option) option * string option

type ast = expr

module Env = Map.Make(String)
type env = typ Env.t


(*make a typ to string function to use for debugging in print statements: *)
let rec string_of_typ = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TVector (n, t) -> Printf.sprintf "vector(%d, %s)" n (string_of_typ t)
  | TMatrix (r, c, t) -> Printf.sprintf "matrix(%d, %d, %s)" r c (string_of_typ t)
  | TUnit -> "unit"

let rec string_of_expr = function
  | IntLit i -> Printf.sprintf "IntLit(%d)" i
  | FloatLit f -> Printf.sprintf "FloatLit(%f)" f
  | BoolLit b -> Printf.sprintf "BoolLit(%b)" b
  | VecLit (IntVec (n, s)) -> Printf.sprintf "VecLit(IntVec(%d, %s))" n s
  | VecLit (FloatVec (n, s)) -> Printf.sprintf "VecLit(FloatVec(%d, %s))" n s
  | MatLit (IntMat (r, c, s)) -> Printf.sprintf "MatLit(IntMat(%d, %d, %s))" r c s
  | MatLit (FloatMat (r, c, s)) -> Printf.sprintf "MatLit(FloatMat(%d, %d, %s))" r c s
  | Var v -> Printf.sprintf "Var(%s)" v
  | Raise s -> Printf.sprintf "Raise(%s)" s
  | Plus (e1, e2) -> Printf.sprintf "Plus(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Minus (e1, e2) -> Printf.sprintf "Minus(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Uminus e -> Printf.sprintf "Uminus(%s)" (string_of_expr e)
  | Times (e1, e2) -> Printf.sprintf "Times(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Div (e1, e2) -> Printf.sprintf "Div(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | And (e1, e2) -> Printf.sprintf "And(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Or (e1, e2) -> Printf.sprintf "Or(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Eq (e1, e2) -> Printf.sprintf "Eq(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Neq (e1, e2) -> Printf.sprintf "Neq(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Lt (e1, e2) -> Printf.sprintf "Lt(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Gt (e1, e2) -> Printf.sprintf "Gt(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Leq (e1, e2) -> Printf.sprintf "Leq(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Geq (e1, e2) -> Printf.sprintf "Geq(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Not e -> Printf.sprintf "Not(%s)" (string_of_expr e)
  | Abs e -> Printf.sprintf "Abs(%s)" (string_of_expr e)
  | Sqrt e -> Printf.sprintf "Sqrt(%s)" (string_of_expr e)
  | Scale (scalar, vec) -> Printf.sprintf "Scale(%s, %s)" (string_of_expr scalar) (string_of_expr vec)
  | AddV (v1, v2) -> Printf.sprintf "AddV(%s, %s)" (string_of_expr v1) (string_of_expr v2)
  | DotProd (v1, v2) -> Printf.sprintf "DotProd(%s, %s)" (string_of_expr v1) (string_of_expr v2)
  | Angle (v1, v2) -> Printf.sprintf "Angle(%s, %s)" (string_of_expr v1) (string_of_expr v2)
  | Len v -> Printf.sprintf "Len(%s)" (string_of_expr v)
  | Index (v, i) -> Printf.sprintf "Index(%s, %s)" (string_of_expr v) (string_of_expr i)
  | IndexMat (m, i, j) -> Printf.sprintf "IndexMat(%s, %s, %s)" (string_of_expr m) (string_of_expr i) (string_of_expr j)
  | AssignExpr (lhs, rhs) -> Printf.sprintf "AssignExpr(%s, %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Seq (e1, e2) -> Printf.sprintf "Seq(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | If (cond, thn, els) -> Printf.sprintf "If(%s, %s, %s)" (string_of_expr cond) (string_of_expr thn) (string_of_expr els)
  | For (v, start, stop, body) -> Printf.sprintf "For(%s, %s, %s, %s)" v (string_of_expr start) (string_of_expr stop) (string_of_expr body)
  | While (cond, body) -> Printf.sprintf "While(%s, %s)" (string_of_expr cond) (string_of_expr body)
  | Print s -> Printf.sprintf "Print(%s)" s
  | Input s -> Printf.sprintf "Input(%s)" s
  | _ -> "Other expressions not implemented for string_of_expr"
let empty = Env.empty

exception TypeError of string

let typ_of_vartype typ_name dims third =
  match typ_name, dims, third with
  | "int", None, None -> TInt
  | "float", None, None -> TFloat
  | "bool", None, None -> TBool
  | "vector", Some (n, None), Some ("int") -> TVector (n, TInt)
  | "vector", Some (n, None), Some ("float") -> TVector (n, TFloat)
  | "matrix", Some (r, Some c), Some ("int")  -> TMatrix (r, c, TInt)
  | "matrix", Some (r, Some c), Some ("float")  -> TMatrix (r, c, TFloat)
  | _ -> raise (TypeError ("Invalid type declaration: " ^ typ_name))

let check_compatible t1 t2 =
  match t1, t2 with
  | TInt, TInt | TFloat, TFloat | TBool, TBool -> true
  | TVector (n1, et1), TVector (n2, et2) -> n1 = n2 && et1 = et2
  | TMatrix (r1, c1, et1), TMatrix (r2, c2, et2) -> r1 = r2 && c1 = c2 && et1 = et2
  | TUnit, TUnit -> true
  | _ -> false

let check_compatible_assign t1 t2 =
  match t1, t2 with
  | TInt, TInt | TFloat, TFloat | TBool, TBool -> true
  | TVector (n1, et1), TVector (n2, et2) -> n1 = n2 && et1 = et2
  | TMatrix (r1, c1, et1), TMatrix (r2, c2, et2) -> r1 = r2 && c1 = c2 && et1 = et2
  | TUnit, TUnit -> true
  | _, TUnit -> true
  | _ -> false

let check_compatible_arith t1 t2 =
  match t1, t2 with
  | TInt, TInt | TFloat, TFloat -> true
  | TInt, TFloat | TFloat, TInt -> true
  | _ -> false

let arith_result_type t1 t2 =
  match t1, t2 with
  | TInt, TInt -> TInt
  | TFloat, TFloat -> TFloat
  | TInt, TFloat | TFloat, TInt -> TFloat
  | _ -> raise (TypeError "Arithmetic operation requires numeric types")

let check_compatible_compare t1 t2 =
  match t1, t2 with
  | TInt, TInt | TFloat, TFloat -> true
  | TFloat, TInt | TInt, TFloat -> true
  | _ -> false

let rec typecheck_expr env = function
  | IntLit _ -> (TInt, env)
  | FloatLit _ -> (TFloat, env)
  | BoolLit _ -> (TBool, env)
  | VecLit (IntVec (n, _)) -> (TVector (n, TInt), env)
  | VecLit (FloatVec (n, _)) -> (TVector (n, TFloat), env)
  | MatLit (IntMat (r, c, _)) -> (TMatrix (r, c, TInt), env)
  | MatLit (FloatMat (r, c, _)) -> (TMatrix (r, c, TFloat), env)
  | Var v -> 
      (match Env.find_opt v env with
       | Some t -> (t, env)
       | None -> raise (TypeError ("Undefined variable: " ^ v)))
  | Plus (e1, e2) | Minus (e1, e2) ->
    let (t1, env1) = typecheck_expr env e1 in
    let (t2, env2) = typecheck_expr env1 e2 in
    (match t1, t2 with
     | TMatrix (r1, c1, et1), TMatrix (r2, c2, et2) when r1 = r2 && c1 = c2 && et1 = et2 ->
         (TMatrix (r1, c1, et1), env2)
     | TVector (n1, et1), TVector (n2, et2) when n1 = n2 && et1 = et2 ->
          (TVector (n1, et1), env2)
     | _ ->
         if check_compatible_arith t1 t2 then (arith_result_type t1 t2, env2)
         else raise (TypeError "Incompatible types for arithmetic operation"))
  | Exp (e1, e2) -> 
    let (t1, env1) = typecheck_expr env e1 in
    let (t2, env2) = typecheck_expr env1 e2 in
    if check_compatible_arith t1 t2 then (arith_result_type t1 t2, env2)
    else raise (TypeError "Incompatible types for exponentiation")
  | Div (e1, e2) ->
      let (t1, env1) = typecheck_expr env e1 in
      let (t2, env2) = typecheck_expr env1 e2 in
      if check_compatible_arith t1 t2 then (arith_result_type t1 t2, env2)
      else raise (TypeError "Incompatible types for arithmetic operation")
  | Times (e1, e2) ->
    let (t1, env1) = typecheck_expr env e1 in
    let (t2, env2) = typecheck_expr env1 e2 in
    (match t1, t2 with
      | TInt, TInt | TFloat, TFloat | TInt, TFloat | TFloat, TInt ->
          (arith_result_type t1 t2, env2)
      | TMatrix (r1, c1, et1), TMatrix (r2, c2, et2) when c1 = r2 && et1 = et2 ->
          (TMatrix (r1, c2, et1), env2)
      | TMatrix (r1, c1, et1), TVector (n, et2) when c1 = n && et1 = et2 ->
          (TVector (r1, et1), env2)
      | _ -> raise (TypeError "Times requires compatible scalar or matrix types"))
  | Uminus (e) ->
    let (t, env1) = typecheck_expr env e in
    if t = TInt || t = TFloat then (t, env1)
    else raise (TypeError "Unary minus requires numeric operand")
  | And (e1, e2) | Or (e1, e2) ->
      let (t1, env1) = typecheck_expr env e1 in
      let (t2, env2) = typecheck_expr env1 e2 in
      if t1 = TBool && t2 = TBool then (TBool, env2)
      else raise (TypeError "Logical operation requires boolean operands")
  | Eq (e1, e2) | Neq (e1, e2) | Lt (e1, e2) | Gt (e1, e2) | Leq (e1, e2) | Geq (e1, e2) ->
      let (t1, env1) = typecheck_expr env e1 in
      let (t2, env2) = typecheck_expr env1 e2 in
      if check_compatible_compare t1 t2 then (TBool, env2)
      else raise (TypeError "Comparison requires compatible types")
  | Not e ->
      let (t, env') = typecheck_expr env e in
      if t = TBool then (TBool, env')
      else raise (TypeError "Not requires boolean operand")
  | Abs e ->
      let (t, env') = typecheck_expr env e in
      if t = TInt || t = TFloat then (t, env')
      else raise (TypeError "Abs requires numeric operand")
  | Sqrt e ->
      let (t, env') = typecheck_expr env e in
      if t = TInt || t = TFloat then (TFloat, env')
      else raise (TypeError "Sqrt requires int or float operand")
  | Scale (scalar, vec) ->
      let (tv, env1) = typecheck_expr env vec in
      (match scalar, tv with
       | IntLit _, TVector (n, TInt) -> (TVector (n, TInt), env1)
       | FloatLit _, TVector (n, TFloat) -> (TVector (n, TFloat), env1)
       | IntLit _, TMatrix (r, c, TInt) -> (TMatrix (r, c, TInt), env1)
       | FloatLit _, TMatrix (r, c, TFloat) -> (TMatrix (r, c, TFloat), env1)
       | _ -> raise (TypeError "Scale requires int literal for int vector or float literal for float vector"))
  | AddV (v1, v2) ->
    let (t1, env1) = typecheck_expr env v1 in
    let (t2, env2) = typecheck_expr env1 v2 in
    (match t1, t2 with
      | TVector (n1, et1), TVector (n2, et2) when n1 = n2 && et1 = et2 -> (TVector (n1, et1), env2)
      | _ -> raise (TypeError "AddV requires vectors of same length and type"))
  | DotProd (v1, v2) ->
      let (t1, env1) = typecheck_expr env v1 in
      let (t2, env2) = typecheck_expr env1 v2 in
      (match t1, t2 with
       | TVector (n1, et1), TVector (n2, et2) when n1 = n2 && et1 = et2 -> (et1, env2)
       | _ -> raise (TypeError "DotProd requires vectors of same length and type"))
  | Angle (v1, v2) ->
      let (t1, env1) = typecheck_expr env v1 in
      let (t2, env2) = typecheck_expr env1 v2 in
      (match t1, t2 with
       | TVector (n1, et1), TVector (n2, et2) when n1 = n2 && et1 = et2 -> (TFloat, env2)
       | _ -> raise (TypeError "Angle requires vectors of same length"))
  | Len v ->
      let (tv, env') = typecheck_expr env v in
      (match tv with
       | TVector _ -> (TFloat, env')
       | _ -> raise (TypeError "Len requires a vector"))
  | Dimension v ->
      let (tv, env') = typecheck_expr env v in
      (match tv with
       | TVector _ -> (TInt, env')
       | _ -> raise (TypeError "Dimension requires a vector"))
  | Transpose m ->
      let (tm, env') = typecheck_expr env m in
      (match tm with
       | TMatrix (r, c, et) -> (TMatrix (c, r, et), env')
       | _ -> raise (TypeError "Transpose requires a matrix"))
  | Determinant m ->
      let (tm, env') = typecheck_expr env m in
      (match tm with
       | TMatrix (r, c, TFloat) when r = c -> (TFloat, env')
       | TMatrix (r, c, TInt) when r = c -> (TInt, env')
       | _ -> raise (TypeError "Determinant requires a square matrix"))
  | Inv m ->
      let (tm, env') = typecheck_expr env m in
      (match tm with
       | TMatrix (r, c, et) when r = c -> (TMatrix (r, c, et), env')
       | _ -> raise (TypeError "Inv requires a square matrix"))
  | Minor (m, i, j) ->
      let (tm, env1) = typecheck_expr env m in
      let (ti, env2) = typecheck_expr env1 i in
      let (tj, env3) = typecheck_expr env2 j in
      (match tm, ti, tj with
       | TMatrix (r, c, _), TInt, TInt -> (TMatrix (r - 1, c - 1, TInt), env3)
       | _ -> raise (TypeError "Minor requires a matrix and two integer indices"))
  | Index (v, i) ->
      let (tv, env1) = typecheck_expr env v in
      let (ti, env2) = typecheck_expr env1 i in
      (match tv, ti with
       | TVector (_, et), TInt -> (et, env2)
       | TMatrix (_, c, et), TInt -> (TVector (c, et), env2)
       | _ -> raise (TypeError "Vector indexing requires vector and integer index"))
  | IndexMat (m, i, j) ->
      let (tm, env1) = typecheck_expr env m in
      let (ti, env2) = typecheck_expr env1 i in
      let (tj, env3) = typecheck_expr env2 j in
      (match tm, ti, tj with
       | TMatrix (_, _, et), TInt, TInt -> (et, env3)
       | _ -> raise (TypeError "Matrix indexing requires matrix and integer indices"))
  | AssignExpr (lhs, rhs) ->
    let tlhs = typecheck_lvalue env lhs in
    let (trhs, env') = typecheck_expr env rhs in
    (* Printf.printf "LHS is : %s\n " (string_of_expr lhs); *)
    (match lhs with
      | Index (v, i) -> 
        let (tv, _) = typecheck_expr env v in
        (match tv with
        | TVector (_, et) -> 
            if check_compatible_assign et trhs then (TUnit, env')
            else raise (TypeError "Type mismatch in vector indexed assignment")
        | _ -> raise (TypeError "Vector indexing in assignment requires vector"))

      | Var v -> 
        (* Printf.printf "type of variable %s is : %s and type of rhs is : %s\n" v (string_of_typ tlhs) (string_of_typ trhs); *)
          if check_compatible_assign tlhs trhs then
            (match rhs, trhs with
            | Input _, _ ->  (TUnit, Env.add v tlhs env')  (* Input can assign any compatible type *)
            | _, TVector (n, et) when tlhs = TVector (n, et) -> (TUnit, Env.add v (TVector (n, et)) env')
            | _, TMatrix (r, c, et) when tlhs = TMatrix (r, c, et) -> (TUnit, Env.add v (TMatrix (r, c, et)) env')
            | _, t -> (TUnit, Env.add v t env'))  (* Update env with rhs type *)
          else raise (TypeError ("Type mismatch in assignment to variable " ^ v))
      
      | IndexMat (m, i, j) ->
          let (tm, _) = typecheck_expr env m in
          (match tm with
          | TMatrix (_, _, et) ->
              if check_compatible_assign et trhs then (TUnit, env')
              else raise (TypeError "Type mismatch in matrix indexed assignment")
          | _ -> raise (TypeError "Matrix indexing in assignment requires matrix"))
      | _ -> raise (TypeError "Left-hand side of assignment must be a variable or indexed expression"))
  | Seq (e1, e2) ->
      let (t1, env1) = typecheck_expr env e1 in
      let (t2, env2) = typecheck_expr env1 e2 in
      if t1 = TUnit then (t2, env2)
      else raise (TypeError "Sequence expressions must return unit type except the last")
  | If (cond, thn, els) ->
      let (tcond, env1) = typecheck_expr env cond in
      if tcond <> TBool then raise (TypeError "If condition must be boolean");
      let (tthn, _) = typecheck_expr env thn in
      let (tels, _) = typecheck_expr env els in
      if tthn = TUnit && tels = TUnit then (TUnit, env1)
      else raise (TypeError "If branches must be unit type for statements")
  | For (v, start, stop, body) ->
      let (tstart, env1) = typecheck_expr env start in
      let (tstop, env2) = typecheck_expr env1 stop in
      if tstart <> TInt || tstop <> TInt then raise (TypeError "For loop bounds must be integers");
      let env' = Env.add v TInt env2 in
      let (tbody, _) = typecheck_expr env' body in
      if tbody = TUnit then (TUnit, env2)
      else raise (TypeError "For loop body must be unit type")
  | While (cond, body) ->
      let (tcond, env1) = typecheck_expr env cond in
      if tcond <> TBool then raise (TypeError "While requires boolean condition");
      let (tbody, _) = typecheck_expr env1 body in
      if tbody = TUnit then (TUnit, env1)
      else raise (TypeError "While body must be unit type")
  | Print s -> (TUnit, env)
  | Input s -> (TUnit, env)
  | Raise s -> (TUnit, env)
  | VarType (typ_name, v, dims, third) ->
      let t = typ_of_vartype typ_name dims third in
      let env' = Env.add v t env in
      (TUnit, env')

and typecheck_lvalue env = function
  | Var v ->
    (*add debug print statements here:*)
    (* Printf.printf "Checking variable: %s : " v; *)
      (match Env.find_opt v env with
       | Some t -> t
       | None -> raise (TypeError ("Undefined variable in assignment: " ^ v)))
  | Index (v, i) ->
      (*add debug print statements here:*)
      (* Printf.printf "Checking index: %s\n" (string_of_int (match i with IntLit n -> n | _ -> 0)); *)
      (* Check the type of the vector and index *)
      let (tv, env1) = typecheck_expr env v in
      let (ti, _) = typecheck_expr env1 i in
      (match tv, ti with
       | TVector (_, et), TInt -> et
       | _ -> raise (TypeError "Vector indexing in assignment requires vector and integer index"))
  | IndexMat (m, i, j) ->
      let (tm, env1) = typecheck_expr env m in
      let (ti, env2) = typecheck_expr env1 i in
      let (tj, _) = typecheck_expr env2 j in
      (match tm, ti, tj with
       | TMatrix (_, _, et), TInt, TInt -> et
       | _ -> raise (TypeError "Matrix indexing in assignment requires matrix and integer indices"))
  | _ -> raise (TypeError "Left-hand side of assignment must be a variable or indexed expression")

let typecheck_program e =
  let empty_env = Env.empty in
  let (t, _) = typecheck_expr empty_env e in
  if t = TUnit then ()
  else raise (TypeError "Program must return unit type")