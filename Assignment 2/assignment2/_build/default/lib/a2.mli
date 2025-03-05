exception DimensionError
type vector = float list
val epsilon : float
module Vector :
  sig
    val _rev : vector -> vector
    val create : int -> float -> vector
    val dim : vector -> int
    val is_zero : vector -> bool
    val unit : int -> int -> vector
    val scale : float -> vector -> vector
    val addv : vector -> vector -> vector
    val dot_prod : vector -> vector -> float
    val inv : vector -> vector
    val length : vector -> float
    val angle : vector -> vector -> float
  end
type types = Bool | Scalar | Vector of int
type expr =
    T
  | F
  | GenUnit of int * int
  | GenZero of int
  | Sub of expr * expr
  | ConstS of float
  | ConstV of float list
  | Add of expr * expr
  | Inv of expr
  | ScalProd of expr * expr
  | DotProd of expr * expr
  | Mag of expr
  | Angle of expr * expr
  | IsZero of expr
  | Cond of expr * expr * expr
exception Wrong of expr
val type_of : expr -> types
type values = B of bool | S of float | V of vector
val eval : expr -> values
