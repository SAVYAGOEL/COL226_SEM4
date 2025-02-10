
type myBool = T|F;;
let myBooltobool b = match b with
	| T -> true
	| F -> false
;;

let myBooltoint b = match b with
	| T -> 1
	| F -> 0
;;

type exp = | Num of int
			| Bl of myBool
			| Plus of exp*exp
			| Times of exp*exp
			| And of exp*exp
			| Or of exp*exp
			| Not of exp
			| Eq of exp*exp
			| Gt of exp*exp
;;

let rec opval e = match e with
				| Num n -> 0
				| Bl b -> -1
				| Plus (e1, e2) -> -2
				| Times (e1, e2) -> -3
				| And (e1, e2) -> -4
				| Or (e1, e2) -> -5
				| Not e1 -> -6
				| Eq (e1, e2) -> -7
				| Gt (e1, e2) -> -8
;;
			
let rec ht e = match e with
				| Num n -> 0
				| Bl b -> 0
				| Plus (e1, e2) -> 1 + (max (ht e1) (ht e2))
				| Times (e1, e2) -> 1 + (max (ht e1) (ht e2))
				| And (e1, e2) -> 1 + (max (ht e1) (ht e2))
				| Or (e1, e2) -> 1 + (max (ht e1) (ht e2))
				| Not e1 -> 1 + (ht e1)
				| Eq (e1, e2) -> 1 + (max (ht e1) (ht e2))
				| Gt (e1, e2) -> 1 + (max (ht e1) (ht e2))
;;

let rec size e = match e with
				| Num n -> 1
				| Bl b -> 1
				| Plus (e1, e2) -> 1 + (size e1) + (size e2)
				| Times (e1, e2) -> 1 + (size e1) + (size e2)
				| And (e1, e2) -> 1 + (size e1) + (size e2)
				| Or (e1, e2) -> 1 + (size e1) + (size e2)
				| Not e1 -> 1 + (size e1)
				| Eq (e1, e2) -> 1 + (size e1) + (size e2)
				| Gt (e1, e2) -> 1 + (size e1) + (size e2)
;;
												

let rec posttrav e = match e with
				| Num n -> [n]
				| Bl b -> [myBooltoint b]
				| Plus (e1, e2) -> (posttrav e1)@(posttrav e2)@[opval e]
				| Times (e1, e2) -> (posttrav e1)@(posttrav e2)@[opval e]
				| And (e1, e2) -> (posttrav e1)@(posttrav e2)@[opval e]
				| Or (e1, e2) -> (posttrav e1)@(posttrav e2)@[opval e]
				| Not e1 -> (posttrav e1)@[opval e]
				| Eq (e1, e2) -> (posttrav e1)@(posttrav e2)@[opval e]
				| Gt (e1, e2) -> (posttrav e1)@(posttrav e2)@[opval e]
;;

type values = | N of int | B of bool;;
let rec eval e = match e with
					| Num n -> N n
					| Bl b -> B (myBooltobool b)
					| Plus (e1, e2) -> (match (eval e1, eval e2) with
										| (N n1, N n2) -> N (n1+n2)
										| _ -> failwith "Type error")
					| Times (e1, e2) -> (match (eval e1, eval e2) with
										| (N n1, N n2) -> N (n1*n2)
										| _ -> failwith "Type error")
					| And (e1, e2) -> (match (eval e1, eval e2) with
										| (B b1, B b2) -> B (b1 && b2)
										| _ -> failwith "Type error")
					| Or (e1, e2) -> (match (eval e1, eval e2) with
										| (B b1, B b2) -> B (b1 || b2)
										| _ -> failwith "Type error")
					| Not e1 -> (match (eval e1) with
										| B b1 -> B (not b1)
										| _ -> failwith "Type error")
					| Eq (e1, e2) -> (match (eval e1, eval e2) with
										| (N n1, N n2) -> B (n1 = n2)
										| (B b1, B b2) -> B (b1 = b2)
										| _ -> failwith "Type error")
					| Gt (e1, e2) -> (match (eval e1, eval e2) with
										| (N n1, N n2) -> B (n1 > n2)
										| _ -> failwith "Type error")
;;

type opcode = LD of int | PLUS | TIMES | AND | OR | NOT | EQ | GT;;

let rec compile e = match e with
					| Num n -> [LD n]
					| Bl b -> [LD (myBooltoint b)]
					| Plus (e1, e2) -> (compile e1)@(compile e2)@[PLUS]
					| Times (e1, e2) -> (compile e1)@(compile e2)@[TIMES]
					| And (e1, e2) -> (compile e1)@(compile e2)@[AND]
					| Or (e1, e2) -> (compile e1)@(compile e2)@[OR]
					| Not e1 -> (compile e1)@[NOT]
					| Eq (e1, e2) -> (compile e1)@(compile e2)@[EQ]
					| Gt (e1, e2) -> (compile e1)@(compile e2)@[GT]
;;

compile (Plus (Times (Num 2, Num 3), Num 4));;

let rec stackmc s c = match s,c with
          | s', (LD n)::c' -> stackmc (N n::s') c'
          | (N n1)::(N n2)::s', PLUS::c' -> stackmc (N (n1+n2)::s') c'
          | (N n1)::(N n2)::s', TIMES::c' -> stackmc (N (n1*n2)::s') c'
          | (B b1)::(B b2)::s', AND::c' -> stackmc (B (b1 && b2)::s') c'
          | (B b1)::(B b2)::s', OR::c' -> stackmc (B (b1 || b2)::s') c'
          | (B b1)::s', NOT::c' -> stackmc (B (not b1)::s') c'
          | (N n1)::(N n2)::s', EQ::c' -> stackmc (B (n1 = n2)::s') c'
          | (N n1)::(N n2)::s', GT::c' -> stackmc (B (n1 > n2)::s') c'
          | s',[] -> s'
          | _ -> failwith "Type error"






