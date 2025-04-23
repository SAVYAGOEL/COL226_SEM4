type variable = string

type lamexp = 
  | V of variable         
  | Lam of variable * lamexp   
  | App of lamexp * lamexp      
  | Num of int | Bool of bool
  | Plus of lamexp * lamexp | Times of lamexp * lamexp
  | And of lamexp * lamexp | Or of lamexp * lamexp | Not of lamexp | Eq of lamexp * lamexp
  | Gt of lamexp * lamexp | Lt of lamexp * lamexp
  | Pair of lamexp * lamexp | Fst of lamexp | Snd of lamexp
  | If of lamexp * lamexp * lamexp

type opcode = 
| LOOKUP of variable            
| APP                           
| MkCLOS of variable * opcode list 
| RET                          
| PLUS | TIMES
| AND | OR | NOT
| EQ | GT | LT
| PAIR | FST | SND
| IF of opcode list * opcode list
| PUSHN of int | PUSHB of bool    


module Env = Map.Make(String)

type closure = 
  | Clos of variable * opcode list * gamma
  | Num of int
  | Bool of bool
  | VPair of closure * closure
and gamma = closure Env.t

              
exception Error of string

let rec compile e = 
  match e with
  | V x -> [LOOKUP x]
  | Lam (x, e1) -> [MkCLOS (x, compile e1 @ [RET])]
  | App (e1, e2) -> compile e1 @ compile e2 @ [APP]
  | Num n -> [PUSHN n]
  | Bool b -> [PUSHB b]
  | Plus (e1, e2) -> compile e1 @ compile e2 @ [PLUS]
  | Times (e1, e2) -> compile e1 @ compile e2 @ [TIMES]
  | And (e1, e2) -> compile e1 @ compile e2 @ [AND]
  | Or (e1, e2) -> compile e1 @ compile e2 @ [OR]
  | Not e1 -> compile e1 @ [NOT]
  | Eq (e1, e2) -> compile e1 @ compile e2 @ [EQ]
  | Gt (e1, e2) -> compile e1 @ compile e2 @ [GT]
  | Lt (e1, e2) -> compile e1 @ compile e2 @ [LT]
  | Pair (e1, e2) -> compile e1 @ compile e2 @ [PAIR]
  | Fst e1 -> compile e1 @ [FST]
  | Snd e1 -> compile e1 @ [SND]
  | If (e1, e2, e3) -> compile e1 @ [IF(compile e2, compile e3)]

let update_env x v env = Env.add x v env

let lookup x env = 
  try Some (Env.find x env)
  with Not_found -> None

let rec secd (s, e, c, d) =
  match c with
  | [] -> 
      (match d with
       | [] -> (match s with 
                | top::[] -> top  
                | _ -> raise (Error "Inappropriate stack"))
       | _ -> raise (Error "Dump not empty, but opcode list is empty"))
  | op :: c' ->
      (match op with
       | LOOKUP x -> 
           (match lookup x e with
            | Some v -> secd (v :: s, e, c', d)
            | None -> raise (Error ("Variable " ^ x ^ " not found in environment")))
       | MkCLOS (x, c1) ->
           let clos = Clos (x, c1, e) in
           secd (clos :: s, e, c', d)

       | APP ->
           (match s with
            | v2 :: Clos (x, c1, gamma1) :: s' ->
                let new_env = update_env x v2 gamma1 in
                secd ([], new_env, c1, (s', e, c') :: d)
            | _ -> raise (Error "Invalid configuration for APP"))
       
       | RET ->
           (match s, d with
            | v :: s', (s'', gamma, c'') :: d' ->
                secd (v :: s'', gamma, c'', d')
            | _ -> raise (Error "Invalid configuration for RET"))
       | PUSHN n -> secd (Num n :: s, e, c', d)
       | PUSHB b -> secd (Bool b :: s, e, c', d)
       | PLUS ->
           (match s with
            | Num n2 :: Num n1 :: s' -> secd (Num (n1 + n2) :: s', e, c', d)
            | _ -> raise (Error "Invalid configuration for PLUS"))
       | TIMES ->
           (match s with
             | Num n2 :: Num n1 :: s' -> secd (Num (n1 * n2) :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for TIMES"))
       | AND ->
           (match s with
             | Bool b2 :: Bool b1 :: s' -> secd (Bool (b1 && b2) :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for AND"))
       | OR ->
           (match s with
             | Bool b2 :: Bool b1 :: s' -> secd (Bool (b1 || b2) :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for OR"))
       | NOT ->
           (match s with
             | Bool b1 :: s' -> secd (Bool (not b1) :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for NOT"))
       | EQ ->
           (match s with
             | Num n2 :: Num n1 :: s' -> secd (Bool (n1 = n2) :: s', e, c', d)
             | Bool b2 :: Bool b1 :: s' -> secd (Bool (b1 = b2) :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for EQ"))
       | GT ->
           (match s with
             | Num n2 :: Num n1 :: s' -> secd (Bool (n1 > n2) :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for GT"))
       | LT ->
           (match s with
             | Num n2 :: Num n1 :: s' -> secd (Bool (n1 < n2) :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for LT"))
       | PAIR ->
           (match s with
             | v2 :: v1 :: s' -> secd (VPair (v1, v2) :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for PAIR"))
       | FST ->
           (match s with
             | VPair (v1, _) :: s' -> secd (v1 :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for FST"))
       | SND ->
           (match s with
             | VPair (_, v2) :: s' -> secd (v2 :: s', e, c', d)
             | _ -> raise (Error "Invalid configuration for SND"))
       | IF(c1, c2)
           -> (match s with
             | Bool b :: s' ->
                 if b then secd (s', e, c1 @ c', d)
                 else secd (s', e, c2 @ c', d)
             | _ -> raise (Error "Invalid configuration for IF")))


(* Pretty-print functions *)
let rec string_of_value = function
  | Clos (x, c, _) -> "<<" ^ x ^ ", " ^ string_of_opcodes c ^ ", env>>"
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | VPair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"

and string_of_opcodes ops =
  "[" ^ String.concat "; " (List.map (function
    | LOOKUP x -> "LOOKUP " ^ x
    | APP -> "APP"
    | MkCLOS (x, c) -> "MkCLOS(" ^ x ^ ", " ^ string_of_opcodes c ^ ")"
    | RET -> "RET"
    | PUSHN n -> "PUSHN " ^ string_of_int n
    | PUSHB b -> "PUSHB " ^ string_of_bool b
    | PLUS -> "PLUS"
    | TIMES -> "TIMES"
    | AND -> "AND"
    | OR -> "OR"
    | NOT -> "NOT"
    | EQ -> "EQ"
    | GT -> "GT"
    | LT -> "LT"
    | PAIR -> "PAIR"
    | FST -> "FST"
    | SND -> "SND"
    | IF (c1, c2) -> "IF(" ^ string_of_opcodes c1 ^ ", " ^ string_of_opcodes c2 ^ ")"
  ) ops) ^ "]"

let test () =
  let empty_env = Env.empty in

  (* Test 1: Basic identity function applied to itself *)
  let e1 = App (Lam ("x", V "x"), Lam ("x", V "x")) in
  let code1 = compile e1 in
  let result1 = secd ([], empty_env, code1, []) in
  Printf.printf "Test 1: Identity function applied to itself\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code1);
  Printf.printf "Result: %s\n\n" (string_of_value result1);

  (* Test 2: Identity function with bound variable *)
  let e2 = App (Lam ("x", V "x"), Num 42) in
  let code2 = compile e2 in
  let result2 = secd ([], empty_env, code2, []) in
  Printf.printf "Test 2: Identity function applied to 42\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code2);
  Printf.printf "Result: %s\n\n" (string_of_value result2);

  (* Test 3: Arithmetic operation (Plus) *)
  let e3 = Plus (Num 5, Num 3) in
  let code3 = compile e3 in
  let result3 = secd ([], empty_env, code3, []) in
  Printf.printf "Test 3: 5 + 3\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code3);
  Printf.printf "Result: %s\n\n" (string_of_value result3);

  (* Test 4: Nested arithmetic (Times and Plus) *)
  let e4 = Times (Plus (Num 2, Num 3), Num 4) in
  let code4 = compile e4 in
  let result4 = secd ([], empty_env, code4, []) in
  Printf.printf "Test 4: (2 + 3) * 4\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code4);
  Printf.printf "Result: %s\n\n" (string_of_value result4);

  (* Test 5: Boolean operations (And) *)
  let e5 = And (Bool true, Bool false) in
  let code5 = compile e5 in
  let result5 = secd ([], empty_env, code5, []) in
  Printf.printf "Test 5: true && false\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code5);
  Printf.printf "Result: %s\n\n" (string_of_value result5);

  (* Test 6: Not operation *)
  let e6 = Not (Bool true) in
  let code6 = compile e6 in
  let result6 = secd ([], empty_env, code6, []) in
  Printf.printf "Test 6: not true\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code6);
  Printf.printf "Result: %s\n\n" (string_of_value result6);

  (* Test 7: Pair creation and projection *)
  let e7 = Fst (Pair (Num 1, Bool true)) in
  let code7 = compile e7 in
  let result7 = secd ([], empty_env, code7, []) in
  Printf.printf "Test 7: fst (1, true)\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code7);
  Printf.printf "Result: %s\n\n" (string_of_value result7);

  (* Test 8: If conditional *)
  let e8 = If (Bool true, Num 1, Num 2) in
  let code8 = compile e8 in
  let result8 = secd ([], empty_env, code8, []) in
  Printf.printf "Test 8: if true then 1 else 2\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code8);
  Printf.printf "Result: %s\n\n" (string_of_value result8);

  (* Test 9: Comparison (Gt) *)
  let e9 = Gt (Num 5, Num 3) in
  let code9 = compile e9 in
  let result9 = secd ([], empty_env, code9, []) in
  Printf.printf "Test 9: 5 > 3\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code9);
  Printf.printf "Result: %s\n\n" (string_of_value result9);

  (* Test 10: Complex lambda with application *)
  let e10 = App (Lam ("x", Plus (V "x", Num 1)), Num 10) in
  let code10 = compile e10 in
  let result10 = secd ([], empty_env, code10, []) in
  Printf.printf "Test 10: (\\x.x + 1) 10\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code10);
  Printf.printf "Result: %s\n\n" (string_of_value result10);

  (* Test 11: Nested lambda (constant function) *)
  let e11 = App (App (Lam ("x", Lam ("y", V "x")), Num 5), Num 42) in
  let code11 = compile e11 in
  let result11 = secd ([], empty_env, code11, []) in
  Printf.printf "Test 11: (\\x.\\y.x) 5 42\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code11);
  Printf.printf "Result: %s\n\n" (string_of_value result11);

  (* Test 12: Corner case - unbound variable *)
  let e12 = V "unbound" in
  let code12 = compile e12 in
  Printf.printf "Test 12: Unbound variable 'unbound'\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code12);
  (try
    let _ = secd ([], empty_env, code12, []) in
    Printf.printf "Result: Should not reach here\n"
  with Error msg ->
    Printf.printf "Result: Error caught - %s\n\n" msg);

  (* Test 13: Complex conditional with pairs *)
  let e13 = If (Eq (Num 2, Num 2), Pair (Num 1, Bool false), Pair (Num 0, Bool true)) in
  let code13 = compile e13 in
  let result13 = secd ([], empty_env, code13, []) in
  Printf.printf "Test 13: if 2 = 2 then (1, false) else (0, true)\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code13);
  Printf.printf "Result: %s\n\n" (string_of_value result13);

  (* Test 14: Deeply nested expression *)
  let e14 = App (Lam ("x", If (Gt (V "x", Num 0), Plus (V "x", Num 1), Times (V "x", Num 2))), Num 3) in
  let code14 = compile e14 in
  let result14 = secd ([], empty_env, code14, []) in
  Printf.printf "Test 14: (\\x.if x > 0 then x + 1 else x * 2) 3\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code14);
  Printf.printf "Result: %s\n\n" (string_of_value result14);

  (* Test 15: Environment with multiple bindings *)
  let env15 = Env.add "x" (Num 10) (Env.add "y" (Bool true) empty_env) in
  let e15 = Plus (V "x", If (V "y", Num 5, Num 0)) in
  let code15 = compile e15 in
  let result15 = secd ([], env15, code15, []) in
  Printf.printf "Test 15: x + (if y then 5 else 0) with x=10, y=true\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code15);
  Printf.printf "Result: %s\n\n" (string_of_value result15);

  (* Test 16: Lambda with environment-bound closure *)
  let env16 = Env.singleton "a" (Clos ("b", [LOOKUP "b"; PLUS; PUSHN 7; RET], Env.empty)) in
  let e16 = App (Lam ("x", V "a"), Num 15) in
  let code16 = compile e16 in 
  let result16 = secd ([], env16, code16, []) in
  Printf.printf "Test 16: (\\x.a) 15 with a bound to \\b.b + 7\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code16);
  Printf.printf "Result: %s\n\n" (string_of_value result16);

  (* Test 17: Decrement function *)
  let e17 = App (Lam ("n", Plus (V "n", Num (-1))), Num 8) in
  let code17 = compile e17 in
  let result17 = secd ([], empty_env, code17, []) in
  Printf.printf "Test 17: (\\n.n + (-1)) 8\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code17);
  Printf.printf "Result: %s\n\n" (string_of_value result17);

  (* Test 18: Equality with numbers *)
  let e18 = If (Eq (Num 7, Num 7), Bool true, Bool false) in
  let code18 = compile e18 in
  let result18 = secd ([], empty_env, code18, []) in
  Printf.printf "Test 18: if 7 = 7 then true else false\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code18);
  Printf.printf "Result: %s\n\n" (string_of_value result18);

  (* Test 19: Boolean OR with nested NOT *)
  let e19 = Or (Bool false, Not (Bool true)) in
  let code19 = compile e19 in
  let result19 = secd ([], empty_env, code19, []) in
  Printf.printf "Test 19: false || (not true)\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code19);
  Printf.printf "Result: %s\n\n" (string_of_value result19);

  (* Test 20: Pair selector based on condition *)
  let e20 = App (Lam ("pair", If (Gt (Fst (V "pair"), Num 0), Snd (V "pair"), Fst (V "pair"))), Pair (Num 5, Num 10)) in
  let code20 = compile e20 in
  let result20 = secd ([], empty_env, code20, []) in
  Printf.printf "Test 20: (\\pair.if fst pair > 0 then snd pair else fst pair) (5, 10)\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code20);
  Printf.printf "Result: %s\n\n" (string_of_value result20);

  (* Test 21: Second element extraction *)
  let e21 = Snd (Pair (Bool false, Num 99)) in
  let code21 = compile e21 in
  let result21 = secd ([], empty_env, code21, []) in
  Printf.printf "Test 21: snd (false, 99)\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code21);
  Printf.printf "Result: %s\n\n" (string_of_value result21);

  (* Test 22: Double nested lambda multiplication *)
  let e22 = App (App (Lam ("a", Lam ("b", Times (V "a", V "b"))), Num 6), Num 3) in
  let code22 = compile e22 in
  let result22 = secd ([], empty_env, code22, []) in
  Printf.printf "Test 22: (\\a.\\b.a * b) 6 3\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code22);
  Printf.printf "Result: %s\n\n" (string_of_value result22);

  (* Test 23: Conditional with pair comparison *)
  let e23 = If (Lt (Snd (Pair (Num 1, Num 2)), Num 3), Fst (Pair (Num 8, Num 9)), Num 100) in
  let code23 = compile e23 in
  let result23 = secd ([], empty_env, code23, []) in
  Printf.printf "Test 23: if snd (1, 2) < 3 then fst (8, 9) else 100\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code23);
  Printf.printf "Result: %s\n\n" (string_of_value result23);

  (* Test 24: Triple lambda subtraction *)
  let e24 = App (App (App (Lam ("x", Lam ("y", Lam ("z", Plus (V "x", Plus (V "y", Times (Num (-1), V "z")))))), Num 10), Num 5), Num 2) in
  let code24 = compile e24 in
  let result24 = secd ([], empty_env, code24, []) in
  Printf.printf "Test 24: (\\x.\\y.\\z.x + (y - z)) 10 5 2\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code24);
  Printf.printf "Result: %s\n\n" (string_of_value result24);

  (* Test 25: Complex arithmetic in lambda *)
  let e25 = App (Lam ("x", Times (Plus (V "x", Num 2), Num 3)), Num 4) in
  let code25 = compile e25 in
  let result25 = secd ([], empty_env, code25, []) in
  Printf.printf "Test 25: (\\x.(x + 2) * 3) 4\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code25);
  Printf.printf "Result: %s\n\n" (string_of_value result25);

  (* Test 26: Nested pairs and projections *)
  let e26 = Fst (Snd (Pair (Num 1, Pair (Num 2, Bool false)))) in
  let code26 = compile e26 in
  let result26 = secd ([], empty_env, code26, []) in
  Printf.printf "Test 26: fst (snd (1, (2, false)))\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code26);
  Printf.printf "Result: %s\n\n" (string_of_value result26);

  (* Test 27: Lambda composition *)
  let e27 = App (Lam ("x", V "x"), App (Lam ("y", Times (V "y", Num 3)), Num 5)) in
  let code27 = compile e27 in
  let result27 = secd ([], empty_env, code27, []) in
  Printf.printf "Test 27: (\\x.x) ((\\y.y * 3) 5)\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code27);
  Printf.printf "Result: %s\n\n" (string_of_value result27);

  (* Test 28: Complex conditional with boolean ops *)
  let e28 = If (Or (Not (Bool false), Bool false), Times (Num 5, Num 2), Plus (Num 1, Num 3)) in
  let code28 = compile e28 in
  let result28 = secd ([], empty_env, code28, []) in
  Printf.printf "Test 28: if (not false) || false then 5 * 2 else 1 + 3\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code28);
  Printf.printf "Result: %s\n\n" (string_of_value result28);

  (* Test 29: Pair with mixed operations *)
  let e29 = Pair (App (Lam ("x", Plus (V "x", Num 1)), Num 6), Snd (Pair (Bool true, Num 8))) in
  let code29 = compile e29 in
  let result29 = secd ([], empty_env, code29, []) in
  Printf.printf "Test 29: ((\\x.x + 1) 6, snd (true, 8))\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code29);
  Printf.printf "Result: %s\n\n" (string_of_value result29);

  (* Test 30: Nested conditionals with arithmetic *)
  let e30 = If (Bool false, Num 10, If (Gt (Num 4, Num 2), Plus (Num 3, Num 3), Num 0)) in
  let code30 = compile e30 in
  let result30 = secd ([], empty_env, code30, []) in
  Printf.printf "Test 30: if false then 10 else (if 4 > 2 then 3 + 3 else 0)\n";
  Printf.printf "Code: %s\n" (string_of_opcodes code30);
  Printf.printf "Result: %s\n" (string_of_value result30)

(* Run the test *)
let () = test ()