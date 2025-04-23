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

exception Stuck of string

module Env = Map.Make(String)

type closure = Clos of lamexp * gamma 
and gamma = closure Env.t              

let rec unpack clos =
  match clos with
  | Clos (e, gamma) ->
      (match e with
       | V x ->
           (try
              let cl = Env.find x gamma in
              unpack cl  
            with Not_found -> V x)  
       | Lam (x, e1) ->
           let gamma' = Env.remove x gamma in
           Lam (x, unpack (Clos (e1, gamma')))
       | App (e1, e2) ->
           App (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
       | Num n -> Num n
       | Bool b -> Bool b
       | Plus (e1, e2) ->
          Plus (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
        | Times (e1, e2) ->
          Times (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
        | And (e1, e2) ->
          And (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
        | Or (e1, e2) ->
          Or (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
        | Not e1 ->
          Not (unpack (Clos (e1, gamma)))
        | Eq (e1, e2) ->
          Eq (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
        | Gt (e1, e2) ->
          Gt (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
        | Lt (e1, e2) ->
          Lt (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)))
        | Fst e1 ->
          Fst (unpack (Clos (e1, gamma)))
        | Snd e1 ->
          Snd (unpack (Clos (e1, gamma)))
        | If (e1, e2, e3) ->
          If (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma)), unpack (Clos (e3, gamma)))
       | Pair (e1, e2) ->
           Pair (unpack (Clos (e1, gamma)), unpack (Clos (e2, gamma))))

let rec krivine (focus, stack) =
  (match focus with
  | Clos (e, gamma) ->
      (match e with
       | Lam (_, _) when stack = [] -> focus
       | App (e1, e2) ->
           let cl2 = Clos (e2, gamma) in
           krivine (Clos (e1, gamma), cl2 :: stack)
       | V x ->
           (try
              let cl = Env.find x gamma in
              krivine (cl, stack)
            with Not_found -> focus)
       | Lam (x, e1) ->
           (match stack with
            | cl :: s ->
                let new_gamma = Env.add x cl gamma in
                krivine (Clos (e1, new_gamma), s)
            | [] ->
                focus)
        | Num n -> 
          (match stack with
           | [] -> focus
           | _ -> raise (Stuck "Non empty stack"))
        | Bool b ->
          (match stack with
            | [] -> focus
            | _ -> raise (Stuck "Non empty stack"))
        | Plus (e1, e2) ->
          (match (krivine (Clos (e1, gamma), []), krivine (Clos (e2, gamma), [])) with
            | (Clos (Num n1, _), Clos (Num n2, _)) ->
                krivine (Clos (Num (n1 + n2), gamma), stack)
            | _ -> raise (Stuck "Invalid addition"))
        | Times (e1, e2) ->
          (match (krivine (Clos (e1, gamma), []), krivine (Clos (e2, gamma), [])) with
            | (Clos (Num n1, _), Clos (Num n2, _)) ->
                krivine (Clos (Num (n1 * n2), gamma), stack)
            | _ -> raise (Stuck "Invalid multiplication"))
        | And (e1, e2) ->
          (match (krivine (Clos (e1, gamma), []), krivine (Clos (e2, gamma), [])) with
            | (Clos (Bool b1, _), Clos (Bool b2, _)) ->
                krivine (Clos (Bool (b1 && b2), gamma), stack)
            | _ -> raise (Stuck "Invalid logical AND"))
        | Or (e1, e2) ->
          (match (krivine (Clos (e1, gamma), []), krivine (Clos (e2, gamma), [])) with
            | (Clos (Bool b1, _), Clos (Bool b2, _)) ->
                krivine (Clos (Bool (b1 || b2), gamma), stack)
            | _ -> raise (Stuck "Invalid logical OR"))
        | Not e1 ->
          (match krivine (Clos (e1, gamma), []) with
            | Clos (Bool b1, _) ->
                krivine (Clos (Bool (not b1), gamma), stack)
            | _ -> raise (Stuck "Invalid logical NOT"))
        | Eq (e1, e2) ->
          (match (krivine (Clos (e1, gamma), []), krivine (Clos (e2, gamma), [])) with
            | (Clos (Num n1, _), Clos (Num n2, _)) ->
                krivine (Clos (Bool (n1 = n2), gamma), stack)
            | (Clos (Bool b1, _), Clos (Bool b2, _)) ->
                krivine (Clos (Bool (b1 = b2), gamma), stack)
            | (Clos (V x1, _), Clos (V x2, _)) ->
                krivine (Clos (Bool (x1 = x2), gamma), stack)
            | _ -> raise (Stuck "Invalid equality check"))
        | Gt (e1, e2) ->
          (match (krivine (Clos (e1, gamma), []), krivine (Clos (e2, gamma), [])) with
            | (Clos (Num n1, _), Clos (Num n2, _)) ->
                krivine (Clos (Bool (n1 > n2), gamma), stack)
            | _ -> raise (Stuck "Invalid greater than check"))
        | Lt (e1, e2) ->
          (match (krivine (Clos (e1, gamma), []), krivine (Clos (e2, gamma), [])) with
            | (Clos (Num n1, _), Clos (Num n2, _)) ->
                krivine (Clos (Bool (n1 < n2), gamma), stack)
            | _ -> raise (Stuck "Invalid less than check"))
        | Pair (e1, e2) ->
          (match (krivine (Clos (e1, gamma), []), krivine (Clos (e2, gamma), [])) with
            | (Clos (v1, _), Clos (v2, _)) ->
                Clos (Pair (v1, v2), gamma))
        | Fst e1 ->
          (match krivine (Clos (e1, gamma), []) with
            | Clos (Pair (v1, _), _) ->
                krivine (Clos (v1, gamma), stack)
            | _ -> raise (Stuck "Invalid first element extraction"))
        | Snd e1 ->
          (match krivine (Clos (e1, gamma), []) with
            | Clos (Pair (_, v2), _) ->
                krivine (Clos (v2, gamma), stack)
            | _ -> raise (Stuck "Invalid second element extraction"))
        | If (e1, e2, e3) ->
          (match krivine (Clos (e1, gamma), []) with
            | Clos (Bool b, _) ->
                if b then krivine (Clos (e2, gamma), stack)
                else krivine (Clos (e3, gamma), stack)
            | _ -> raise (Stuck "Invalid if condition"))))

let rec string_of_exp e =
  match e with
  | V x -> x
  | Lam (x, e1) -> "\\" ^ x ^ "." ^ string_of_exp e1
  | App (e1, e2) -> "(" ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ")"
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Plus (e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
  | Times (e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"
  | And (e1, e2) -> "(" ^ string_of_exp e1 ^ " && " ^ string_of_exp e2 ^ ")"
  | Or (e1, e2) -> "(" ^ string_of_exp e1 ^ " || " ^ string_of_exp e2 ^ ")"
  | Not e1 -> "(not " ^ string_of_exp e1 ^ ")"
  | Eq (e1, e2) -> "(" ^ string_of_exp e1 ^ " = " ^ string_of_exp e2 ^ ")"
  | Gt (e1, e2) -> "(" ^ string_of_exp e1 ^ " > " ^ string_of_exp e2 ^ ")"
  | Lt (e1, e2) -> "(" ^ string_of_exp e1 ^ " < " ^ string_of_exp e2 ^ ")"
  | Pair (e1, e2) -> "(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | Fst e1 -> "fst " ^ string_of_exp e1
  | Snd e1 -> "snd " ^ string_of_exp e1
  | If (e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else " ^ string_of_exp e3

(* Test cases *)
let test () =
  let empty_env = Env.empty in

  (* Test 1: Basic identity function applied to itself *)
  let e1 = App (Lam ("x", V "x"), Lam ("x", V "x")) in
  let final_focus1 = krivine (Clos (e1, empty_env), []) in
  let result1 = unpack final_focus1 in
  Printf.printf "Test 1: Identity function applied to itself\n";
  Printf.printf "Input: %s\n" (string_of_exp e1);
  Printf.printf "Result: %s\n\n" (string_of_exp result1);

  (* Test 2: Identity function with bound variable *)
  let e2 = App (Lam ("x", V "x"), Num 42) in
  let final_focus2 = krivine (Clos (e2, empty_env), []) in
  let result2 = unpack final_focus2 in
  Printf.printf "Test 2: Identity function applied to 42\n";
  Printf.printf "Input: %s\n" (string_of_exp e2);
  Printf.printf "Result: %s\n\n" (string_of_exp result2);

  (* Test 3: Arithmetic operation (Plus) *)
  let e3 = Plus (Num 5, Num 3) in
  let final_focus3 = krivine (Clos (e3, empty_env), []) in
  let result3 = unpack final_focus3 in
  Printf.printf "Test 3: 5 + 3\n";
  Printf.printf "Input: %s\n" (string_of_exp e3);
  Printf.printf "Result: %s\n\n" (string_of_exp result3);

  (* Test 4: Nested arithmetic (Times and Plus) *)
  let e4 = Times (Plus (Num 2, Num 3), Num 4) in
  let final_focus4 = krivine (Clos (e4, empty_env), []) in
  let result4 = unpack final_focus4 in
  Printf.printf "Test 4: (2 + 3) * 4\n";
  Printf.printf "Input: %s\n" (string_of_exp e4);
  Printf.printf "Result: %s\n\n" (string_of_exp result4);
 
  (* Test 5: Boolean operations (And) *)
  let e5 = And (Bool true, Bool false) in
  let final_focus5 = krivine (Clos (e5, empty_env), []) in
  let result5 = unpack final_focus5 in
  Printf.printf "Test 5: true && false\n";
  Printf.printf "Input: %s\n" (string_of_exp e5);
  Printf.printf "Result: %s\n\n" (string_of_exp result5);

  (* Test 6: Not operation *)
  let e6 = Not (Bool true) in
  let final_focus6 = krivine (Clos (e6, empty_env), []) in
  let result6 = unpack final_focus6 in
  Printf.printf "Test 6: not true\n";
  Printf.printf "Input: %s\n" (string_of_exp e6);
  Printf.printf "Result: %s\n\n" (string_of_exp result6);

  (* Test 7: Pair creation and projection *)
  let e7 = Fst (Pair (Num 1, Bool true)) in
  let final_focus7 = krivine (Clos (e7, empty_env), []) in
  let result7 = unpack final_focus7 in
  Printf.printf "Test 7: fst (1, true)\n";
  Printf.printf "Input: %s\n" (string_of_exp e7);
  Printf.printf "Result: %s\n\n" (string_of_exp result7);

  (* Test 8: If conditional *)
  let e8 = If (Bool true, Num 1, Num 2) in
  let final_focus8 = krivine (Clos (e8, empty_env), []) in
  let result8 = unpack final_focus8 in
  Printf.printf "Test 8: if true then 1 else 2\n";
  Printf.printf "Input: %s\n" (string_of_exp e8);
  Printf.printf "Result: %s\n\n" (string_of_exp result8);

  (* Test 9: Comparison (Gt) *)
  let e9 = Gt (Num 5, Num 3) in
  let final_focus9 = krivine (Clos (e9, empty_env), []) in
  let result9 = unpack final_focus9 in
  Printf.printf "Test 9: 5 > 3\n";
  Printf.printf "Input: %s\n" (string_of_exp e9);
  Printf.printf "Result: %s\n\n" (string_of_exp result9);

  (* Test 10: Complex lambda with application *)
  let e10 = App (Lam ("x", Plus (V "x", Num 1)), Num 10) in
  let final_focus10 = krivine (Clos (e10, empty_env), []) in
  let result10 = unpack final_focus10 in
  Printf.printf "Test 10: (\\x.x + 1) 10\n";
  Printf.printf "Input: %s\n" (string_of_exp e10);
  Printf.printf "Result: %s\n\n" (string_of_exp result10);

  (* Test 11: Nested lambda (constant function) *)
  let e11 = App (App (Lam ("x", Lam ("y", V "x")), Num 5), Num 42) in
  let final_focus11 = krivine (Clos (e11, empty_env), []) in
  let result11 = unpack final_focus11 in
  Printf.printf "Test 11: (\\x.\\y.x) 5 42\n";
  Printf.printf "Input: %s\n" (string_of_exp e11);
  Printf.printf "Result: %s\n\n" (string_of_exp result11);

  (* Test 12: Corner case - unbound variable *)
  let e12 = V "unbound" in
  Printf.printf "Test 12: Unbound variable 'unbound'\n";
  Printf.printf "Input: %s\n" (string_of_exp e12);
  (try
    let final_focus12 = krivine (Clos (e12, empty_env), []) in
    let result12 = unpack final_focus12 in
    Printf.printf "Result: %s\n\n" (string_of_exp result12)
  with Stuck msg ->
    Printf.printf "Result: Stuck - %s\n\n" msg);

  (* Test 13: Complex conditional with pairs *)
  let e13 = If (Eq (Num 2, Num 2), Pair (Num 1, Bool false), Pair (Num 0, Bool true)) in
  let final_focus13 = krivine (Clos (e13, empty_env), []) in
  let result13 = unpack final_focus13 in
  Printf.printf "Test 13: if 2 = 2 then (1, false) else (0, true)\n";
  Printf.printf "Input: %s\n" (string_of_exp e13);
  Printf.printf "Result: %s\n\n" (string_of_exp result13);

  (* Test 14: Deeply nested expression *)
  let e14 = App (Lam ("x", If (Gt (V "x", Num 0), Plus (V "x", Num 1), Times (V "x", Num 2))), Num 3) in
  let final_focus14 = krivine (Clos (e14, empty_env), []) in
  let result14 = unpack final_focus14 in
  Printf.printf "Test 14: (\\x.if x > 0 then x + 1 else x * 2) 3\n";
  Printf.printf "Input: %s\n" (string_of_exp e14);
  Printf.printf "Result: %s\n\n" (string_of_exp result14);

  (* Test 15: Environment with multiple bindings *)
  let env15 = Env.add "x" (Clos (Num 10, empty_env)) (Env.add "y" (Clos (Bool true, empty_env)) empty_env) in
  let e15 = Plus (V "x", If (V "y", Num 5, Num 0)) in
  let final_focus15 = krivine (Clos (e15, env15), []) in
  let result15 = unpack final_focus15 in
  Printf.printf "Test 15: x + (if y then 5 else 0) with x=10, y=true\n";
  Printf.printf "Input: %s\n" (string_of_exp e15);
  Printf.printf "Result: %s\n\n" (string_of_exp result15);

  (* Test 16: Lambda with environment-bound closure *)
  let env16 = Env.singleton "a" (Clos (Lam ("b", Plus (V "b", Num 7)), empty_env)) in
  let e16 = App (Lam ("x", V "a"), Num 15) in
  let final_focus16 = krivine (Clos (e16, env16), []) in
  let result16 = unpack final_focus16 in
  Printf.printf "Test 16: (\\x.a) 15 with a bound to \\b.b + 7\n";
  Printf.printf "Input: %s\n" (string_of_exp e16);
  Printf.printf "Result: %s\n\n" (string_of_exp result16);

  (* Test 17: Decrement function *)
  let e17 = App (Lam ("n", Plus (V "n", Num (-1))), Num 8) in
  let final_focus17 = krivine (Clos (e17, empty_env), []) in
  let result17 = unpack final_focus17 in
  Printf.printf "Test 17: (\\n.n + (-1)) 8\n";
  Printf.printf "Input: %s\n" (string_of_exp e17);
  Printf.printf "Result: %s\n\n" (string_of_exp result17);

  (* Test 18: Equality with numbers *)
  let e18 = If (Eq (Num 7, Num 7), Bool true, Bool false) in
  let final_focus18 = krivine (Clos (e18, empty_env), []) in
  let result18 = unpack final_focus18 in
  Printf.printf "Test 18: if 7 = 7 then true else false\n";
  Printf.printf "Input: %s\n" (string_of_exp e18);
  Printf.printf "Result: %s\n\n" (string_of_exp result18);

  (* Test 19: Boolean OR with nested NOT *)
  let e19 = Or (Bool false, Not (Bool true)) in
  let final_focus19 = krivine (Clos (e19, empty_env), []) in
  let result19 = unpack final_focus19 in
  Printf.printf "Test 19: false || (not true)\n";
  Printf.printf "Input: %s\n" (string_of_exp e19);
  Printf.printf "Result: %s\n\n" (string_of_exp result19);

  (* Test 20: Pair selector based on condition *)
  let e20 = App (Lam ("pair", If (Gt (Fst (V "pair"), Num 0), Snd (V "pair"), Fst (V "pair"))), Pair (Num 5, Num 10)) in
  let final_focus20 = krivine (Clos (e20, empty_env), []) in
  let result20 = unpack final_focus20 in
  Printf.printf "Test 20: (\\pair.if fst pair > 0 then snd pair else fst pair) (5, 10)\n";
  Printf.printf "Input: %s\n" (string_of_exp e20);
  Printf.printf "Result: %s\n\n" (string_of_exp result20);

  (* Test 21: Second element extraction *)
  let e21 = Snd (Pair (Bool false, Num 99)) in
  let final_focus21 = krivine (Clos (e21, empty_env), []) in
  let result21 = unpack final_focus21 in
  Printf.printf "Test 21: snd (false, 99)\n";
  Printf.printf "Input: %s\n" (string_of_exp e21);
  Printf.printf "Result: %s\n\n" (string_of_exp result21);

  (* Test 22: Double nested lambda multiplication *)
  let e22 = App (App (Lam ("a", Lam ("b", Times (V "a", V "b"))), Num 6), Num 3) in
  let final_focus22 = krivine (Clos (e22, empty_env), []) in
  let result22 = unpack final_focus22 in
  Printf.printf "Test 22: (\\a.\\b.a * b) 6 3\n";
  Printf.printf "Input: %s\n" (string_of_exp e22);
  Printf.printf "Result: %s\n\n" (string_of_exp result22);

  (* Test 23: Conditional with pair comparison *)
  let e23 = If (Lt (Snd (Pair (Num 1, Num 2)), Num 3), Fst (Pair (Num 8, Num 9)), Num 100) in
  let final_focus23 = krivine (Clos (e23, empty_env), []) in
  let result23 = unpack final_focus23 in
  Printf.printf "Test 23: if snd (1, 2) < 3 then fst (8, 9) else 100\n";
  Printf.printf "Input: %s\n" (string_of_exp e23);
  Printf.printf "Result: %s\n\n" (string_of_exp result23);

  (* Test 24: Triple lambda subtraction *)
  let e24 = App (App (App (Lam ("x", Lam ("y", Lam ("z", Plus (V "x", Plus (V "y", Times (Num (-1), V "z")))))), Num 10), Num 5), Num 2) in
  let final_focus24 = krivine (Clos (e24, empty_env), []) in
  let result24 = unpack final_focus24 in
  Printf.printf "Test 24: (\\x.\\y.\\z.x + (y - z)) 10 5 2\n";
  Printf.printf "Input: %s\n" (string_of_exp e24);
  Printf.printf "Result: %s\n\n" (string_of_exp result24);

  (* Test 25: Complex arithmetic in lambda *)
  let e25 = App (Lam ("x", Times (Plus (V "x", Num 2), Num 3)), Num 4) in
  let final_focus25 = krivine (Clos (e25, empty_env), []) in
  let result25 = unpack final_focus25 in
  Printf.printf "Test 25: (\\x.(x + 2) * 3) 4\n";
  Printf.printf "Input: %s\n" (string_of_exp e25);
  Printf.printf "Result: %s\n\n" (string_of_exp result25);

  (* Test 26: Nested pairs and projections *)
  let e26 = Fst (Snd (Pair (Num 1, Pair (Num 2, Bool false)))) in
  let final_focus26 = krivine (Clos (e26, empty_env), []) in
  let result26 = unpack final_focus26 in
  Printf.printf "Test 26: fst (snd (1, (2, false)))\n";
  Printf.printf "Input: %s\n" (string_of_exp e26);
  Printf.printf "Result: %s\n\n" (string_of_exp result26);

  (* Test 27: Lambda composition *)
  let e27 = App (Lam ("x", V "x"), App (Lam ("y", Times (V "y", Num 3)), Num 5)) in
  let final_focus27 = krivine (Clos (e27, empty_env), []) in
  let result27 = unpack final_focus27 in
  Printf.printf "Test 27: (\\x.x) ((\\y.y * 3) 5)\n";
  Printf.printf "Input: %s\n" (string_of_exp e27);
  Printf.printf "Result: %s\n\n" (string_of_exp result27);

  (* Test 28: Complex conditional with boolean ops *)
  let e28 = If (Or (Not (Bool false), Bool false), Times (Num 5, Num 2), Plus (Num 1, Num 3)) in
  let final_focus28 = krivine (Clos (e28, empty_env), []) in
  let result28 = unpack final_focus28 in
  Printf.printf "Test 28: if (not false) || false then 5 * 2 else 1 + 3\n";
  Printf.printf "Input: %s\n" (string_of_exp e28);
  Printf.printf "Result: %s\n\n" (string_of_exp result28);

  (* Test 29: Pair with mixed operations *)
  let e29 = Pair (App (Lam ("x", Plus (V "x", Num 1)), Num 6), Snd (Pair (Bool true, Num 8))) in
  let final_focus29 = krivine (Clos (e29, empty_env), []) in
  let result29 = unpack final_focus29 in
  Printf.printf "Test 29: ((\\x.x + 1) 6, snd (true, 8))\n";
  Printf.printf "Input: %s\n" (string_of_exp e29);
  Printf.printf "Result: %s\n\n" (string_of_exp result29);

  (* Test 30: Nested conditionals with arithmetic *)
  let e30 = If (Bool false, Num 10, If (Gt (Num 4, Num 2), Plus (Num 3, Num 3), Num 0)) in
  let final_focus30 = krivine (Clos (e30, empty_env), []) in
  let result30 = unpack final_focus30 in
  Printf.printf "Test 30: if false then 10 else (if 4 > 2 then 3 + 3 else 0)\n";
  Printf.printf "Input: %s\n" (string_of_exp e30);
  Printf.printf "Result: %s\n\n" (string_of_exp result30);

  (* Test 31: Lazy evaluation with non-terminating argument (terminates in Krivine) *)
  let omega = Lam ("x", App (V "x", V "x")) in
  let e31 = App (Lam ("y", Num 7), App (omega, omega)) in
  let final_focus31 = krivine (Clos (e31, empty_env), []) in
  let result31 = unpack final_focus31 in
  Printf.printf "Test 31: (\\y.7) ((\\x.x x) (\\x.x x)) - Lazy evaluation terminates\n";
  Printf.printf "Input: %s\n" (string_of_exp e31);
  Printf.printf "Result: %s\n\n" (string_of_exp result31);

  (* Test 32: Lazy evaluation avoids unnecessary computation *)
  let e32 = App (Lam ("x", Num 42), Plus (Num 1, App (omega, omega))) in
  let final_focus32 = krivine (Clos (e32, empty_env), []) in
  let result32 = unpack final_focus32 in
  Printf.printf "Test 32: (\\x.42) (1 + ((\\x.x x) (\\x.x x))) - Lazy skips infinite loop\n";
  Printf.printf "Input: %s\n" (string_of_exp e32);
  Printf.printf "Result: %s\n\n" (string_of_exp result32);

  (* Test 34: Conditional with non-terminating else branch (terminates in Krivine) *)
  let e33 = If (Bool true, Num 5, App (omega, omega)) in
  let final_focus34 = krivine (Clos (e33, empty_env), []) in
  let result34 = unpack final_focus34 in
  Printf.printf "Test 33: if true then 5 else ((\\x.x x) (\\x.x x)) - Lazy skips else\n";
  Printf.printf "Input: %s\n" (string_of_exp e33);
  Printf.printf "Result: %s\n\n" (string_of_exp result34)

(* Run the test *)
let () = test ()