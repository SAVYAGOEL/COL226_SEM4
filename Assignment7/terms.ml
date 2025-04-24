open Array

type variable = string

type symbol = string * int

type signature = symbol list

type term = V of variable | Node of symbol * term array

exception NOT_UNIFIABLE of string

(* Check if a signature is valid: no repeated symbols, non-negative arities *)
let check_sig (s: signature) : bool =
  let tbl = Hashtbl.create (List.length s) in
  try
    List.iter (fun (name, arity) ->
      if arity < 0 then raise Exit;
      if Hashtbl.mem tbl name then raise Exit;
      Hashtbl.add tbl name arity
    ) s;
    true
  with Exit -> false

(* Check if a term is well-formed according to the signature *)
let wfterm (s: signature) (t:term) : bool =
  let rec get_symbol_arity (sign: signature) (sym: symbol) = match sign with
    | [] -> -1
    | (name, arity)::xsign ->
      if name = (fst sym) then arity
      else get_symbol_arity xsign sym
  in
  let get_arity symbol = match symbol with
    | (name, arity) -> arity
  in
  let rec check_term t' = match t' with
    | V _ -> true
    | Node (symbol, terms) ->
      let check_curr_node = ((length terms) = (get_arity symbol) && (get_arity symbol) = (get_symbol_arity s symbol)) in
      let check_children = fold_left (fun acc term -> acc && check_term term) true terms in
      check_curr_node && check_children
  in
  check_term t

(* Compute the height of a well-formed term *)
let rec ht (t:term) : int =
  match t with
  | V _ -> 0
  | Node (symbol, terms) ->
    if snd symbol = 0 then 0
    else
      fold_left max 0 (map ht terms) + 1

(* Compute the size (number of nodes) of a well-formed term *)
let rec size (t:term) : int =
  match t with
  | V _ -> 1
  | Node (symbol, terms) ->
    if snd symbol = 0 then 1
    else
      fold_left (+) 1 (map size terms)

(* Compute the list of variables in a well-formed term *)
let vars (t:term) : variable list =
  let rec vars' t' acc = match t' with
    | V v -> if List.mem v acc then acc else v::acc
    | Node (_, terms) ->
      fold_left (fun acc term -> vars' term acc) acc terms
  in
  vars' t []

(* Check if a variable occurs in a term *)
let occurs (v: variable) (t: term) : bool =
  let set_vars = vars t in
  let rec occurs' v' set_vars' = match set_vars' with
    | [] -> false
    | x::xs -> if x = v' then true else occurs' v' xs
  in
  occurs' v set_vars

(* Substitution type: maps variables to terms *)
type sigma = (string -> term)

(* Apply a substitution to a term *)
let rec subst (s: sigma) (t: term) : term =
  match t with
  | V v -> s v
  | Node (symbol, terms) ->
    let new_terms = map (subst s) terms in
    Node (symbol, new_terms)

(* Compose two substitutions *)
let compose_subst (s1: sigma) (s2: sigma) : sigma = fun v -> subst s2 (s1 v)

(* Identity substitution *)
let id_sub : sigma = fun v -> V v

(* Compute the most general unifier of two terms *)
let mgu (t1: term) (t2: term) : sigma =
  let rec mgu2 t1' t2' = match (t1', t2') with
    | (V v1, V v2) -> if v1 = v2 then id_sub else (fun x -> if x = v1 then V v2 else V x)
    | (V v, Node (symbol, terms)) -> if occurs v t2' then raise (NOT_UNIFIABLE "occurs check failed") else (fun x -> if x = v then t2' else V x)
    | (Node (symbol, terms), V v) -> if occurs v t1' then raise (NOT_UNIFIABLE "occurs check failed") else (fun x -> if x = v then t1' else V x)
    | (Node (symbol1, terms1), Node (symbol2, terms2)) ->
      if symbol1 <> symbol2 then raise (NOT_UNIFIABLE "symbols do not match") else
        try
          let rec iterate_array i acc' =
            if i = length terms1 && i = length terms2 then acc'
            else if (i = length terms1) || (i = length terms2) then raise (NOT_UNIFIABLE "terms do not match")
            else
              let t1'' = subst acc' (terms1.(i)) in
              let t2'' = subst acc' (terms2.(i)) in
              let mgu_t1t2 = mgu2 t1'' t2'' in
              let temp = compose_subst acc' mgu_t1t2 in
              iterate_array (i+1) temp
          in
          iterate_array 0 id_sub
        with NOT_UNIFIABLE msg -> raise (NOT_UNIFIABLE msg)
  in
  mgu2 t1 t2

(* Edit a term by replacing the subtree at the given position with a new term *)
let edit (t: term) (pos: int list) (new_term: term) : term =
  let rec edit2 t' pos' =
    match (t', pos') with
    | (_, []) -> new_term
    | (V _, _::_) -> raise (Invalid_argument "Position too deep for variable")
    | (Node (sym, terms), p::ps) ->
      if p < 0 || p >= length terms then raise (Invalid_argument "Invalid position")
      else
        let new_terms = Array.copy terms in
        new_terms.(p) <- edit2 terms.(p) ps;
        (* terms.(p) <- edit_aux terms.(p) ps; *)
        Node (sym, new_terms)
  in
  edit2 t pos

(* In-place substitution: modifies the term by replacing variables *)
let in_place_subst (s: sigma) (t: term) : term =
  let rec subst2 t' =
    match t' with
    | V v ->
      let new_term = s v in
      if new_term <> V v then new_term else t'
    | Node (sym, terms) ->
      Array.iteri (fun i ter -> let new_term = subst2 ter in
        if new_term != ter then terms.(i) <- new_term) terms;
      t'
  in
  subst2 t

(* Test cases *)
let () =
  (* Test check_sig *)
  let s1 = [("f", 2); ("g", 1); ("h", 0)] in
  let s2 = [("f", 2); ("g", -1); ("h", 0)] in
  let s3 = [("f", 2); ("g", 1); ("h", 0); ("i", 2)] in
  let s4 = [("f", 2); ("g", 1); ("h", 0); ("g", -1)] in
  let s5 = [("f", 2); ("g", 1); ("h", 0); ("i", 1); ("j", 0); ("k", 2)] in
  let s6 = [("f", 2); ("g", 1); ("h", 0); ("g", 1); ("h", 0); ("f", -1)] in
  Printf.printf "s1: %b\n" (check_sig s1);
  Printf.printf "s2: %b\n" (check_sig s2);
  Printf.printf "s3: %b\n" (check_sig s3);
  Printf.printf "s4: %b\n" (check_sig s4);
  Printf.printf "s5: %b\n" (check_sig s5);
  Printf.printf "s6: %b\n" (check_sig s6);

  (* Test wfterm *)
  let s1 = [("f", 2); ("g", 1); ("h", 0)] in
  let t1 = Node (("f", 2), [|Node (("g", 1), [|V "x"|]); Node (("h", 0), [||])|]) in
  let t2 = Node (("f", 2), [|Node (("h", 1), [|V "x"|]); Node (("h", 0), [||])|]) in
  Printf.printf "t1: %b\n" (wfterm s1 t1);
  Printf.printf "t2: %b\n" (wfterm s1 t2);

  (* Test ht and size *)
  let t1 = Node (("f", 2), [|Node (("g", 1), [|V "x"|]); Node (("h", 0), [||])|]) in
  let t2 = Node (("f", 2), [|Node (("h", 1), [|V "x"|]); Node (("h", 0), [||])|]) in
  Printf.printf "t1 ht, size: %d, %d\n" (ht t1) (size t1);
  Printf.printf "t2 ht, size: %d, %d\n" (ht t2) (size t2);
  let t3 = Node (("f", 0), [||]) in
  Printf.printf "t3 ht, size: %d, %d\n" (ht t3) (size t3);
  let t4 = Node (("f", 1), [|Node (("g", 0), [||])|]) in
  Printf.printf "t4 ht, size: %d, %d\n" (ht t4) (size t4);
  let t5 = Node (("f", 2), [|Node (("g", 1), [|Node (("h", 0), [||])|]); Node (("i", 0), [||])|]) in
  Printf.printf "t5 ht, size: %d, %d\n" (ht t5) (size t5);
  let t6 = Node (("f", 3), [|Node (("g", 2), [|Node (("h", 1), [|Node (("i", 0), [||])|])|]); Node (("j", 0), [||])|]) in
  Printf.printf "t6 ht, size: %d, %d\n" (ht t6) (size t6);
  let t7 = Node (("f", 4), [|Node (("g", 3), [|Node (("h", 2), [|Node (("i", 1), [|Node (("j", 0), [||])|])|])|]); Node (("k", 0), [||])|]) in
  Printf.printf "t7 ht, size: %d, %d\n" (ht t7) (size t7);
  let t8 = V "x" in
  Printf.printf "t8 ht, size: %d, %d\n" (ht t8) (size t8);

  (* Test vars *)
  let t1 = Node (("f", 2), [|Node (("g", 1), [|V "x"|]); Node (("h", 0), [||])|]) in
  let t2 = Node (("f", 2), [|Node (("g", 1), [|V "x"; V "y"|]); Node (("h", 0), [||])|]) in
  let t3 = Node (("f", 2), [|Node (("g", 1), [|V "x"; V "x"; V "x"|]); Node (("h", 0), [||])|]) in
  let t4 = Node (("f", 2), [|Node (("g", 1), [|V "x"; V "z"; V "z"; V "w"|]); Node (("h", 0), [||])|]) in
  let t5 = Node (("f", 2), [|Node (("g", 1), [|V "x"; V "y"; V "z"; V "w"; V "v"|]); Node (("h", 0), [||])|]) in
  let t6 = Node (("f", 0), [||]) in
  Printf.printf "t1 vars: %s\n" (String.concat ", " (vars t1));
  Printf.printf "t2 vars: %s\n" (String.concat ", " (vars t2));
  Printf.printf "t3 vars: %s\n" (String.concat ", " (vars t3));
  Printf.printf "t4 vars: %s\n" (String.concat ", " (vars t4));
  Printf.printf "t5 vars: %s\n" (String.concat ", " (vars t5));
  Printf.printf "t6 vars: %s\n" (String.concat ", " (vars t6));

  (* Test mgu *)
  let t1 = V "x" in
  let t2 = V "y" in
  let mgu_t1t2 = mgu t1 t2 in
  Printf.printf "mgu t1 t2: %s\n" (match mgu_t1t2 "x" with V v -> v | _ -> "");
  let t3 = Node (("f", 2), [|Node (("g", 1), [|V "x"|]); V "z"|]) in
  let t4 = Node (("f", 2), [|Node (("g", 1), [|V "y"|]); Node (("h", 0), [||])|]) in
  let mgu_t3t4 = mgu t3 t4 in
  assert (subst mgu_t3t4 t3 = subst mgu_t3t4 t4);

  (* Test edit *)
  let t = Node (("f", 2), [|Node (("g", 1), [|V "x"|]); Node (("h", 0), [||])|]) in
  let new_t = edit t [0; 0] (V "y") in
  Printf.printf "edit t [0;0] (V \"y\") vars: %s\n" (String.concat ", " (vars new_t));

  (* Test in_place_subst *)
  let t_ref = (Node (("f", 2), [|Node (("g", 1), [|V "x"|]); Node (("h", 0), [||])|])) in
  let s = fun v -> if v = "x" then V "y" else V v in
  let new_term = in_place_subst s t_ref in 
  Printf.printf "in_place_subst vars: %s\n" (String.concat ", " (vars new_term));