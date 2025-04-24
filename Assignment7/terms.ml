open Array

module Term = struct 
  type variable = string

  type symbol = string * int

  type signature = symbol list

  type term = V of variable | Node of symbol * term array

  exception NOT_UNIFIABLE of string;;
  exception NOT_EDITABLE of string;;

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

  let rec ht (t:term) : int =
    match t with
    | V _ -> 0
    | Node (symbol, terms) ->
      if snd symbol = 0 then 0
      else
        fold_left max 0 (map ht terms) + 1

  let rec size (t:term) : int =
    match t with
    | V _ -> 1
    | Node (symbol, terms) ->
      if snd symbol = 0 then 1
      else
        fold_left (+) 1 (map size terms)

  let vars (t:term) : variable list =
    let rec vars' t' acc = match t' with
      | V v -> if List.mem v acc then acc else v::acc
      | Node (_, terms) ->
        fold_left (fun acc term -> vars' term acc) acc terms
    in
    vars' t []

  let occurs (v: variable) (t: term) : bool =
    let set_vars = vars t in
    let rec occurs' v' set_vars' = match set_vars' with
      | [] -> false
      | x::xs -> if x = v' then true else occurs' v' xs
    in
    occurs' v set_vars

  type sigma = (string, term) Hashtbl.t

  (* Apply a substitution to a term *)
  let rec subst (s: sigma) (t: term) : term =
    match t with
    | V v -> 
        (try Hashtbl.find s v 
         with Not_found -> V v)  
    | Node (symbol, terms) ->
        let new_terms = map (subst s) terms in
        Node (symbol, new_terms)

 
  let compose_subst (s1: sigma) (s2: sigma) : sigma =
    let result = Hashtbl.create (Hashtbl.length s1 + Hashtbl.length s2) in
    
    (* First copy all mappings from s2 to result *)
    Hashtbl.iter (fun var term ->
      Hashtbl.add result var term
    ) s2;
    
    (* Then for each variable x in s1 *)
    Hashtbl.iter (fun var term ->
      (* Apply s2 to the term that s1 maps var to *)
      let new_term = subst s2 term in
      (* Add this mapping to result *)
      Hashtbl.replace result var new_term
    ) s1;
    
    result

  let id_sub : sigma = Hashtbl.create 0

  let singleton_subst (var: string) (term: term) : sigma =
    let s = Hashtbl.create 1 in
    Hashtbl.add s var term;
    s

  (* Compute the most general unifier of two terms *)
  let mgu (t1: term) (t2: term) : sigma =
    let rec mgu2 t1' t2' : sigma = 
      match (t1', t2') with
      | (V v1, V v2) -> 
          if v1 = v2 then id_sub 
          else singleton_subst v1 (V v2)
      | (V v, Node (symbol, terms)) -> 
          if occurs v t2' then 
            raise (NOT_UNIFIABLE "occurs check failed") 
          else 
            singleton_subst v t2'
      | (Node (symbol, terms), V v) -> 
          if occurs v t1' then 
            raise (NOT_UNIFIABLE "occurs check failed") 
          else 
            singleton_subst v t1'
      | (Node (symbol1, terms1), Node (symbol2, terms2)) ->
          if symbol1 <> symbol2 then 
            raise (NOT_UNIFIABLE "symbols do not match") 
          else
            try
              let rec iterate_array i acc' =
                if i = length terms1 && i = length terms2 then 
                  acc'
                else if (i = length terms1) || (i = length terms2) then 
                  raise (NOT_UNIFIABLE "terms do not match")
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
      | (V _, _::_) -> raise (NOT_EDITABLE "Reached a variable and position not empty")
      | (Node (sym, terms), p::ps) ->
        if p < 0 || p >= length terms then raise (NOT_EDITABLE "Invalid position")
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
          (try 
            let new_term = Hashtbl.find s v in
            if new_term <> V v then new_term else t'
           with Not_found -> t')
      | Node (sym, terms) ->
          Array.iteri (fun i ter -> 
            let new_term = subst2 ter in
            if new_term != ter then terms.(i) <- new_term
          ) terms;
          t'
    in
    subst2 t
end

(* Export module contents to the top level for testing *)
include Term

(* Helper functions for printing *)
let print_section title =
  let line = String.make 80 '=' in
  Printf.printf "\n%s\n%s\n%s\n" line title line

let print_subsection title =
  let line = String.make 60 '-' in
  Printf.printf "\n%s\n%s\n" title line

let print_result name result =
  Printf.printf "%-30s: %s\n" name result

(* Convert terms to string for display *)
let rec string_of_term t =
  match t with
  | V var -> var
  | Node ((name, _), terms) ->
      if length terms = 0 then
        name
      else
        name ^ "(" ^ 
        (Array.fold_left 
          (fun acc term -> 
            if acc = "" then string_of_term term 
            else acc ^ ", " ^ string_of_term term) 
          "" terms) ^ 
        ")"

(* Print a list of variables *)
let string_of_vars vars =
  "[" ^ (List.fold_left (fun acc v -> if acc = "" then v else acc ^ ", " ^ v) "" vars) ^ "]"

(* Print a substitution *)
let string_of_subst s =
  let buf = Buffer.create 100 in
  Buffer.add_string buf "{";
  let first = ref true in
  Hashtbl.iter (fun var term ->
    if not !first then Buffer.add_string buf ", ";
    Buffer.add_string buf (var ^ " -> " ^ string_of_term term);
    first := false
  ) s;
  Buffer.add_string buf "}";
  Buffer.contents buf

(* Test signatures *)
let test_signatures () =
  print_section "Testing Signature Functions";
  
  (* Test valid signatures *)
  let sig1 = [("f", 2); ("g", 1); ("h", 0)] in
  print_result "check_sig valid" (string_of_bool (check_sig sig1));
  
  (* Test invalid signatures (negative arity) *)
  let sig2 = [("f", 2); ("g", -1); ("h", 0)] in
  print_result "check_sig negative arity" (string_of_bool (check_sig sig2));
  
  (* Test invalid signatures (duplicate symbol) *)
  let sig3 = [("f", 2); ("f", 1); ("h", 0)] in
  print_result "check_sig duplicate symbol" (string_of_bool (check_sig sig3))

(* Test term creation and well-formedness *)
let test_wfterm () =
  print_section "Testing Well-Formed Terms";
  
  let sig1 = [("f", 2); ("g", 1); ("h", 0); ("c", 0)] in
  
  (* Create some terms *)
  let x = V "x" in
  let y = V "y" in
  let c = Node (("c", 0), [||]) in
  let h = Node (("h", 0), [||]) in
  let g_x = Node (("g", 1), [|x|]) in
  let g_y = Node (("g", 1), [|y|]) in
  let f_xy = Node (("f", 2), [|x; y|]) in
  let f_gx_h = Node (("f", 2), [|g_x; h|]) in
  
  print_subsection "Term Representations";
  print_result "Variable x" (string_of_term x);
  print_result "Constant c" (string_of_term c);
  print_result "g(x)" (string_of_term g_x);
  print_result "g(y)" (string_of_term g_y);
  print_result "f(x, y)" (string_of_term f_xy);
  print_result "f(g(x), h)" (string_of_term f_gx_h);
  
  print_subsection "Well-Formed Term Checks";
  print_result "wfterm x" (string_of_bool (wfterm sig1 x));
  print_result "wfterm c" (string_of_bool (wfterm sig1 c));
  print_result "wfterm g(x)" (string_of_bool (wfterm sig1 g_x));
  print_result "wfterm g(y)" (string_of_bool (wfterm sig1 g_y));
  print_result "wfterm f(x, y)" (string_of_bool (wfterm sig1 f_xy));
  print_result "wfterm f(g(x), h)" (string_of_bool (wfterm sig1 f_gx_h));
  
  (* Create an ill-formed term (wrong arity) *)
  let ill_g = Node (("g", 1), [|x; y|]) in
  print_result "wfterm g(x, y) [ill-formed]" (string_of_bool (wfterm sig1 ill_g));
  
  (* Create an ill-formed term (unknown symbol) *)
  let ill_k = Node (("k", 1), [|x|]) in
  print_result "wfterm k(x) [ill-formed]" (string_of_bool (wfterm sig1 ill_k))

(* Test term properties (height, size, vars) *)
let test_term_properties () =
  print_section "Testing Term Properties";
  
  let x = V "x" in
  let y = V "y" in
  let z = V "z" in
  let c = Node (("c", 0), [||]) in
  let h = Node (("h", 0), [||]) in
  let g_x = Node (("g", 1), [|x|]) in
  let g_y = Node (("g", 1), [|y|]) in
  let f_xy = Node (("f", 2), [|x; y|]) in
  let f_gx_h = Node (("f", 2), [|g_x; h|]) in
  let nested = Node (("f", 2), [|Node (("f", 2), [|g_x; g_y|]); g_y|]) in
  
  print_subsection "Term Height";
  print_result "ht x" (string_of_int (ht x));
  print_result "ht c" (string_of_int (ht c));
  print_result "ht g(x)" (string_of_int (ht g_x));
  print_result "ht f(x, y)" (string_of_int (ht f_xy));
  print_result "ht f(g(x), h)" (string_of_int (ht f_gx_h));
  print_result "ht f(f(g(x), g(y)), g(y))" (string_of_int (ht nested));
  
  print_subsection "Term Size";
  print_result "size x" (string_of_int (size x));
  print_result "size c" (string_of_int (size c));
  print_result "size g(x)" (string_of_int (size g_x));
  print_result "size f(x, y)" (string_of_int (size f_xy));
  print_result "size f(g(x), h)" (string_of_int (size f_gx_h));
  print_result "size f(f(g(x), g(y)), g(y))" (string_of_int (size nested));
  
  print_subsection "Term Variables";
  print_result "vars x" (string_of_vars (vars x));
  print_result "vars c" (string_of_vars (vars c));
  print_result "vars g(x)" (string_of_vars (vars g_x));
  print_result "vars f(x, y)" (string_of_vars (vars f_xy));
  print_result "vars f(g(x), h)" (string_of_vars (vars f_gx_h));
  print_result "vars f(f(g(x), g(y)), g(y))" (string_of_vars (vars nested));
  print_result "vars f(f(g(x), g(y)), g(z))" (string_of_vars (vars (Node (("f", 2), [|Node (("f", 2), [|g_x; g_y|]); Node (("g", 1), [|z|])|]))));
  
  print_subsection "Variable Occurrence Checks";
  print_result "occurs 'x' in x" (string_of_bool (occurs "x" x));
  print_result "occurs 'y' in x" (string_of_bool (occurs "y" x));
  print_result "occurs 'x' in g(x)" (string_of_bool (occurs "x" g_x));
  print_result "occurs 'y' in g(x)" (string_of_bool (occurs "y" g_x));
  print_result "occurs 'x' in f(x, y)" (string_of_bool (occurs "x" f_xy));
  print_result "occurs 'z' in f(g(x), h)" (string_of_bool (occurs "z" f_gx_h))

(* Test substitutions *)
let test_substitutions () =
  print_section "Testing Substitutions";
  
  let x = V "x" in
  let y = V "y" in
  let z = V "z" in
  let c = Node (("c", 0), [||]) in
  let g_x = Node (("g", 1), [|x|]) in
  let g_y = Node (("g", 1), [|y|]) in
  let f_xy = Node (("f", 2), [|x; y|]) in
  let f_gx_y = Node (("f", 2), [|g_x; y|]) in
  
  print_subsection "Simple Substitutions";
  let s1 = Hashtbl.create 2 in
  Hashtbl.add s1 "x" c;
  print_result "s1" (string_of_subst s1);
  print_result "subst s1 x" (string_of_term (subst s1 x));
  print_result "subst s1 y" (string_of_term (subst s1 y));
  print_result "subst s1 g(x)" (string_of_term (subst s1 g_x));
  print_result "subst s1 f(x, y)" (string_of_term (subst s1 f_xy));
  print_result "subst s1 f(g(x), y)" (string_of_term (subst s1 f_gx_y));
  
  print_subsection "Multiple Variable Substitutions";
  let s2 = Hashtbl.create 2 in
  Hashtbl.add s2 "x" g_y;
  Hashtbl.add s2 "y" z;
  print_result "s2" (string_of_subst s2);
  print_result "subst s2 x" (string_of_term (subst s2 x));
  print_result "subst s2 f(x, y)" (string_of_term (subst s2 f_xy));
  print_result "subst s2 f(g(x), y)" (string_of_term (subst s2 f_gx_y));
  
  print_subsection "Composition of Substitutions";
  let s3 = Hashtbl.create 1 in
  Hashtbl.add s3 "y" c;
  print_result "s3" (string_of_subst s3);
  
  let s2s3 = compose_subst s2 s3 in
  print_result "s2 ∘ s3" (string_of_subst s2s3);
  print_result "subst (s2 ∘ s3) f(x, y)" (string_of_term (subst s2s3 f_xy));
  
  (* Check if composition works correctly by comparing with sequential application *)
  let result1 = subst s2s3 f_xy in
  let result2 = subst s3 (subst s2 f_xy) in
  print_result "subst (s2 ∘ s3) f(x, y) = subst s3 (subst s2 f(x, y))" 
               (if result1 = result2 then "true" else "false (ERROR)");
  
  print_subsection "In-Place Substitution";
  let f_xy_copy = Node (("f", 2), [|x; y|]) in
  let result3 = in_place_subst s2 f_xy_copy in
  print_result "in_place_subst s2 f(x, y)" (string_of_term result3)

(* Test unification *)
let test_unification () =
  print_section "Testing Unification";
  
  let x = V "x" in
  let y = V "y" in
  let z = V "z" in
  let c = Node (("c", 0), [||]) in
  let d = Node (("d", 0), [||]) in
  let g_x = Node (("g", 1), [|x|]) in
  let g_y = Node (("g", 1), [|y|]) in
  let f_xy = Node (("f", 2), [|x; y|]) in
  let f_xz = Node (("f", 2), [|x; z|]) in
  let f_cy = Node (("f", 2), [|c; y|]) in
  let f_gx_y = Node (("f", 2), [|g_x; y|]) in
  let f_gy_z = Node (("f", 2), [|g_y; z|]) in
  
  print_subsection "Simple Unification Cases";
  
  (* Case 1: Variable and Variable *)
  (try
    let u1 = mgu x y in
    print_result "mgu(x, y)" (string_of_subst u1);
  with NOT_UNIFIABLE msg ->
    print_result "mgu(x, y)" ("NOT UNIFIABLE: " ^ msg));
  
  (* Case 2: Variable and Constant *)
  (try
    let u2 = mgu x c in
    print_result "mgu(x, c)" (string_of_subst u2);
  with NOT_UNIFIABLE msg ->
    print_result "mgu(x, c)" ("NOT UNIFIABLE: " ^ msg));
  
  (* Case 3: Identical Constants *)
  (try
    let u3 = mgu c c in
    print_result "mgu(c, c)" (string_of_subst u3);
  with NOT_UNIFIABLE msg ->
    print_result "mgu(c, c)" ("NOT UNIFIABLE: " ^ msg));
  
  (* Case 4: Different Constants *)
  (try
    let u4 = mgu c d in
    print_result "mgu(c, d)" (string_of_subst u4);
  with NOT_UNIFIABLE msg ->
    print_result "mgu(c, d)" ("NOT UNIFIABLE: " ^ msg));
  
  print_subsection "Complex Unification Cases";
  
  (* Case 5: Simple Function Terms *)
  (try
    let u5 = mgu g_x g_y in
    print_result "mgu(g(x), g(y))" (string_of_subst u5);
  with NOT_UNIFIABLE msg ->
    print_result "mgu(g(x), g(y))" ("NOT UNIFIABLE: " ^ msg));
  
  (* Case 6: Function Terms With Different Variables *)
  (try
    let u6 = mgu f_xy f_xz in
    print_result "mgu(f(x, y), f(x, z))" (string_of_subst u6);
  with NOT_UNIFIABLE msg ->
    print_result "mgu(f(x, y), f(x, z))" ("NOT UNIFIABLE: " ^ msg));
  
  (* Case 7: More Complex Function Terms *)
  (try
    let u7 = mgu f_gx_y f_gy_z in
    print_result "mgu(f(g(x), y), f(g(y), z))" (string_of_subst u7);
    print_result "Apply to f(g(x), y)" (string_of_term (subst u7 f_gx_y));
    print_result "Apply to f(g(y), z)" (string_of_term (subst u7 f_gy_z));
  with NOT_UNIFIABLE msg ->
    print_result "mgu(f(g(x), y), f(g(y), z))" ("NOT UNIFIABLE: " ^ msg));
  
  (* Case 8: Occurs Check Failure *)
  (try
    let u8 = mgu x g_x in
    print_result "mgu(x, g(x))" (string_of_subst u8);
  with NOT_UNIFIABLE msg ->
    print_result "mgu(x, g(x))" ("NOT UNIFIABLE: " ^ msg));
  
  (* Case 9: Different Function Symbols *)
  (try
    let u9 = mgu g_x f_cy in
    print_result "mgu(g(x), f(c, y))" (string_of_subst u9);
  with NOT_UNIFIABLE msg ->
    print_result "mgu(g(x), f(c, y))" ("NOT UNIFIABLE: " ^ msg))

(* Test term editing *)
let test_term_editing () =
  print_section "Testing Term Editing";
  
  let x = V "x" in
  let y = V "y" in
  let c = Node (("c", 0), [||]) in
  let g_x = Node (("g", 1), [|x|]) in
  let f_xy = Node (("f", 2), [|x; y|]) in
  let f_gx_y = Node (("f", 2), [|g_x; y|]) in
  
  print_subsection "Simple Edits";
  
  (* Replace top-level term *)
  let edit1 = edit f_xy [] g_x in
  print_result "edit f(x, y) at [] with g(x)" (string_of_term edit1);
  
  (* Replace first argument *)
  let edit2 = edit f_xy [0] c in
  print_result "edit f(x, y) at [0] with c" (string_of_term edit2);
  
  (* Replace second argument *)
  let edit3 = edit f_xy [1] g_x in
  print_result "edit f(x, y) at [1] with g(x)" (string_of_term edit3);
  
  print_subsection "Nested Edits";
  
  (* Replace subterm in nested structure *)
  let edit4 = edit f_gx_y [0; 0] c in
  print_result "edit f(g(x), y) at [0, 0] with c" (string_of_term edit4);
  
  (* Try invalid position *)
  (try
    let edit5 = edit f_xy [2] c in
    print_result "edit f(x, y) at [2] with c" (string_of_term edit5);
  with NOT_EDITABLE msg ->
    print_result "edit f(x, y) at [2] with c" ("NOT EDITABLE: " ^ msg));
  
  (* Try to edit inside variable *)
  (try
    let edit6 = edit x [0] c in
    print_result "edit x at [0] with c" (string_of_term edit6);
  with NOT_EDITABLE msg ->
    print_result "edit x at [0] with c" ("NOT EDITABLE: " ^ msg))

(* Run all tests *)
let () =
  print_endline "\n🧪 TERMS MODULE TEST SUITE 🧪\n";
  test_signatures ();
  test_wfterm ();
  test_term_properties ();
  test_substitutions ();
  test_unification ();
  test_term_editing ();
  print_endline "\n✅ All tests completed\n"