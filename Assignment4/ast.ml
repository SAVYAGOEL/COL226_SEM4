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
  | Plus of expr * expr
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
  | Scale of expr * expr
  | AddV of expr * expr
  | DotProd of expr * expr
  | Angle of expr * expr
  | Len of expr
  | Dimension of expr
  | Transpose of expr
  | Determinant of expr
  | Index of expr * expr
  | IndexMat of expr * expr * expr
  | AssignExpr of expr * expr
  | Seq of expr * expr
  | If of expr * expr * expr
  | For of string * expr * expr * expr
  | While of expr * expr
  | Print of string
  | Input of string
  | VarType of string * string * (int * int option) option

type ast = expr

module Env = Map.Make(String)
type env = typ Env.t

let empty = Env.empty

exception TypeError of string

let typ_of_vartype typ_name dims =
  match typ_name, dims with
  | "int", None -> TInt
  | "float", None -> TFloat
  | "bool", None -> TBool
  | "vector", Some (n, None) -> TVector (n, TInt)
  | "matrix", Some (r, Some c) -> TMatrix (r, c, TInt)
  | _ -> raise (TypeError ("Invalid type declaration: " ^ typ_name))

let check_compatible t1 t2 =
  match t1, t2 with
  | TInt, TInt | TFloat, TFloat | TBool, TBool -> true
  | TVector (n1, et1), TVector (n2, et2) -> n1 = n2 && et1 = et2
  | TMatrix (r1, c1, et1), TMatrix (r2, c2, et2) -> r1 = r2 && c1 = c2 && et1 = et2
  | TUnit, TUnit -> true
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
  | Plus (e1, e2) | Minus (e1, e2) | Div (e1, e2) ->
      let (t1, env1) = typecheck_expr env e1 in
      let (t2, env2) = typecheck_expr env1 e2 in
      if check_compatible_arith t1 t2 then (arith_result_type t1 t2, env2)
      else raise (TypeError "Incompatible types for arithmetic operation")
  | Times (e1, e2) ->
    let (t1, env1) = typecheck_expr env e1 in
    let (t2, env2) = typecheck_expr env1 e2 in
    (match t1, t2 with
     | TInt, TInt | TFloat, TFloat | TInt, TFloat | TFloat, TInt ->
         (arith_result_type t1 t2, env2)  (* Scalar multiplication *)
     | TMatrix (r1, c1, et1), TMatrix (r2, c2, et2) when c1 = r2 && et1 = et2 ->
         (TMatrix (r1, c2, et1), env2)  (* Matrix multiplication *)
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
      if check_compatible t1 t2 then (TBool, env2)
      else raise (TypeError "Comparison requires compatible types")
  | Not e ->
      let (t, env') = typecheck_expr env e in
      if t = TBool then (TBool, env')
      else raise (TypeError "Not requires boolean operand")
  | Abs e ->
      let (t, env') = typecheck_expr env e in
      if t = TInt || t = TFloat then (t, env')
      else raise (TypeError "Abs requires numeric operand")
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
       | TVector (n1, _), TVector (n2, _) when n1 = n2 -> (TFloat, env2)
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
       | TMatrix (r, c, _) when r = c -> (TFloat, env')
       | _ -> raise (TypeError "Determinant requires a square matrix"))
  | Index (v, i) ->
      let (tv, env1) = typecheck_expr env v in
      let (ti, env2) = typecheck_expr env1 i in
      (match tv, ti with
       | TVector (_, et), TInt -> (et, env2)
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
      (match lhs, rhs with
       | Var v, Input _ ->  (* Updated: Allow all types for Input *)
           (TUnit, Env.add v tlhs env')  (* No type restriction *)
       | Var v, _ ->
           if check_compatible tlhs trhs then
             (match tlhs, trhs with
              | TVector (n, _), TVector (m, et) when n = m -> (TUnit, Env.add v (TVector (n, et)) env')
              | TMatrix (r, c, _), TMatrix (r', c', et) when r = r' && c = c' -> (TUnit, Env.add v (TMatrix (r, c, et)) env')
              | _ -> (TUnit, env'))
           else raise (TypeError ("Type mismatch in assignment to " ^ v))
       | _, _ ->
           if check_compatible tlhs trhs then (TUnit, env')
           else raise (TypeError "Type mismatch in indexed assignment"))
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
  | VarType (typ_name, v, dims) ->
      let t = typ_of_vartype typ_name dims in
      let env' = Env.add v t env in
      (TUnit, env')

and typecheck_lvalue env = function
  | Var v ->
      (match Env.find_opt v env with
       | Some t -> t
       | None -> raise (TypeError ("Undefined variable in assignment: " ^ v)))
  | Index (v, i) ->
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