(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)



   type vector = float list
   exception DimensionError of string
   exception ZeroVectorError of string
               
               
   let create n x : vector =
     if n < 1 then raise (DimensionError "Dimension must be an integer >= 1")
     else
       let rec create2 n x acc =
         if n = 0 then acc  
         else create2 (n - 1) x (x :: acc)  
       in
       create2 n x []
   ;;            
               
   let rec dim (v:vector) =                       
     let rec dim2 v acc = match v with 
         [] -> acc
       | x::xs -> dim2 xs (acc+1)
     in 
     dim2 v 0 
   ;;        
               
   let rec is_zero (v:vector) = match v with
       [] -> true
     | x::xs -> if x == 0. then is_zero xs
         else false 
   ;;
   
   let unit n j : vector =
     if (n < 1) || (j<1) || (j>n) then raise (DimensionError "j must be between 1 and n inclusive")
     else 
       let rec unit2 n j acc v =
         if acc > n then v 
         else if acc == (n-j+1) then unit2 n j (acc + 1) (1.0 :: v)  
         else unit2 n j (acc + 1) (0.0 :: v)  
       in
       unit2 n j 1 []
   ;;    
           
   let rec scale c (v:vector) : vector = match v with 
       [] -> []
     | x::xs -> (c *. x)::(scale c xs)
   ;;                                 
              
   let rec addv (v1:vector) (v2:vector) : vector = 
     if (dim v1) == (dim v2 ) then match v1 with 
         [] -> []
       | x::xs -> (x +. (List.hd v2))::(addv xs (List.tl v2))
     else raise (DimensionError "Dimensions must match")
   ;;                   
               
   let rec dot_prod (v1:vector) (v2:vector) =
     if (dim v1) == (dim v2 ) then 
       let rec dot2 v1 v2 acc = match v1 with
           [] -> acc
         | x::xs -> dot2 xs (List.tl v2) (acc +. x *. List.hd v2) 
       in
       dot2 v1 v2 0. 
     else raise (DimensionError "Dimensions must match")
   ;;              
         
   let inv (v:vector) : vector =
     scale (-1.0) v
   ;;                                       
               
   let length (v:vector) =
     let temp = dot_prod v v
     in
     Float.sqrt(temp)
   ;;              
              
   let angle (v1:vector) (v2:vector) = 
     if (is_zero v1) || (is_zero v2) then raise (ZeroVectorError "Angle can not be calculated with zero vector")
     else
       let l1 = length v1
       in
       let l2 = length v2
       in 
       let dot = dot_prod v1 v2
       in
       if (dim v1 > 0) && (dim v2 > 0) then
         if ((dot /. l1) /. l2 > 1.0) then acos (1.0)
         else if ((dot /. l1) /. l2 < -1.0) then acos (-1.0)
         else acos ((dot /. l1) /. l2)
       else raise (DimensionError "Dimension of vector must be > 0")
   ;;       
             
     
   (* Testing Vector Module *)
   Printf.printf "-------- TESTING VECTOR MODULE --------";;
   
   (* Test cases for create *)
   Printf.printf "------ create function test cases ------\n";;
   let v1 = create 4 3.2;; 
   Printf.printf "v1: ";;
   List.iter (Printf.printf "%0.2f ") v1;;
   print_newline ();;
   
   let v2 = create 3 (-5.0);; 
   Printf.printf "v2: ";;
   List.iter (Printf.printf "%0.2f ") v2;;
   print_newline ();;
   
   let v3 = create 1 0.0;; 
   Printf.printf "v3: ";;
   List.iter (Printf.printf "%0.2f ") v3;;
   print_newline ();;
   
   
   
   Printf.printf "case when n = 0 \n";;
   try 
     let v4 = create 0 2.0 in
     List.iter (Printf.printf "%0.2f ") v4
   with 
   | DimensionError s -> Printf.printf "DimensionError: %s\n" s;;
   
   print_newline ();;
   
   
   (* Test cases for dim *)
   Printf.printf "------ dim function test cases ------\n";;
   let d1 = dim v1;; 
   Printf.printf "dim v1: %d\n" d1;;
   
   let d2 = dim v2;; 
   Printf.printf "dim v2: %d\n" d2;;
   
   let d3 = dim v3;; 
   Printf.printf "dim v3: %d\n" d3;;
   
   (* Test cases for is_zero *)
   Printf.printf "------ is_zero function test cases ------\n";;
   let v5 = create 3 0.0;; 
   let z1 = is_zero v5;;
   Printf.printf "is_zero v5: %b\n" z1;;
   
   let z2 = is_zero v1;;
   Printf.printf "is_zero v1: %b\n" z2;;
   
   
   (* Test cases for unit *)
   Printf.printf "------ unit function test cases ------\n";;
   let u1 = unit 4 2;; 
   Printf.printf "u1: ";;
   List.iter (Printf.printf "%0.2f ") u1;;
   print_newline ();;
   
   let u2 = unit 5 5;; 
   Printf.printf "u2: ";;
   List.iter (Printf.printf "%0.2f ") u2;;
   print_newline ();; 
   
   
   Printf.printf "case j = 0 ( DimensionError ) \n";;
   try 
     let u3 = unit 3 0 in
     List.iter (Printf.printf "%0.2f ") u3
   with 
   | DimensionError s -> Printf.printf "DimensionError: %s\n" s;;
   
   print_newline ();;
   
   
   (* Test cases for scale *)
   Printf.printf "------ scale function test cases ------\n";;
   let scaled_v1 = scale 2.0 v1;; 
   Printf.printf "scaled_v1: ";;
   List.iter (Printf.printf "%0.2f ") scaled_v1;;
   print_newline ();;
   
   let scaled_zero = scale 0.0 v2;; 
   Printf.printf "scaled_zero: ";;
   List.iter (Printf.printf "%0.2f ") scaled_zero;;
   print_newline ();;
   
   let scaled_inv = scale (-1.0) v1;;
   Printf.printf "scaled_inv: ";;
   List.iter (Printf.printf "%0.2f ") scaled_inv;;
   print_newline ();;
   
   (* Test cases for addv *)
   Printf.printf "------ addv function test cases ------\n";;
   let v6 = create 4 1.0;;
   let add_v1_v6 = addv v1 v6;; 
   Printf.printf "v1+v6: ";;
   List.iter (Printf.printf "%0.2f ") add_v1_v6;;
   print_newline ();;
   
   let add_v1_u1 = addv v1 u1;;
   Printf.printf "v1+u1: ";;
   List.iter (Printf.printf "%0.2f ") add_v1_u1;;
   print_newline ();;
   
   
   let v7 = create 2 1.0;; 
   
   Printf.printf "case when dimensions of the two vectors dont match\n";;
   try 
     let add_v1_v7 = addv v1 v7 in
     List.iter (Printf.printf "%0.2f ") add_v1_v7
   with 
   | DimensionError s -> Printf.printf "DimensionError: %s\n" s;;
   
   print_newline ();;
   
   (* Test cases for dot_prod *)
   Printf.printf "------ dot_prod function test cases ------\n";;
   let dp_v1_v6 = dot_prod v1 v6;; 
   Printf.printf "v1.v6: %0.2f\n" dp_v1_v6;;
   
   let dp_v1_u1 = dot_prod v1 u1;; 
   Printf.printf "v1.u1 v1 u1: %0.2f\n" dp_v1_u1;;
   
   
   let v8 = create 2 1.0;; 
   Printf.printf "case when dimensions of the two vectors dont match\n";;
   try 
     let dp_v1_v8 = dot_prod v1 v8 in
     Printf.printf "%0.2f " dp_v1_v8
   with 
   | DimensionError s -> Printf.printf "DimensionError: %s\n" s;;
   
   print_newline ();;
   
   
   (* Test cases for inv *)
   Printf.printf "------ inv function test cases ------\n";;
   let inv_v1 = inv v1;; 
   Printf.printf "inv v1: ";;
   List.iter (Printf.printf "%0.2f ") inv_v1;;
   print_newline ();;
   
   let inv_v5 = inv v5;;
   Printf.printf "inv v5: ";;
   List.iter (Printf.printf "%0.2f ") inv_v5;;
   print_newline ();;
   
   (* Test cases for length *)
   Printf.printf "------ length function test cases ------\n";;
   let len_v1 = length v1;; 
   Printf.printf "length v1: %0.2f\n" len_v1;;
   
   let len_u1 = length u1;; 
   Printf.printf "length u1: %0.2f\n" len_u1;;
   
   let len_v5 = length v5;; 
   Printf.printf "length v5: %0.2f\n" len_v5;;
   
   (* Test cases for angle *)
   Printf.printf "------ angle function test cases ------\n";;
   
   Printf.printf "case of parallel vectors\n";;
   let angle_v1_v6 = angle v1 v6;; 
   Printf.printf "angle v1 v6: %0.2f\n" angle_v1_v6;;
   
   let v9 = create 4 (-3.2);;
   Printf.printf "case of anti-parallel vectors\n";;
   let angle_v1_v9 = angle v1 v9;; 
   Printf.printf "angle v1 v9: %0.2f\n" angle_v1_v9;;
   
   let v10 = unit 2 1;;
   let v11 = unit 2 2;;
   Printf.printf "case of perpendicular vectors\n";;
   let angle_v10_v11 = angle v10 v11;; 
   Printf.printf "angle v10 v11: %0.2f\n" angle_v10_v11;;
   
   Printf.printf "case of calculating angle with 0 vector\n";;
   try 
     let angle_v5_v1 = angle v5 v1 in
     Printf.printf "%0.2f " angle_v5_v1
   with 
   | DimensionError s -> Printf.printf "DimensionError: %s\n" s;;
   
   print_newline ();;
   
   
   (* Miscellaneous test cases verifying the mathematical properties stated in the assignment *)
   
   Printf.printf "------ miscellaneous tests (mathematical properties) ------ \n";;
   
   (* Checking commutativity: u + v = v + u *)
   Printf.printf "------ checking commutativity ------ \n";;
   let u = create 3 1.0;;
   let v = create 3 2.0;;
   let comm1 = addv u v;;
   let comm2 = addv v u;;
   Printf.printf "u + v = ";;
   List.iter (Printf.printf "%0.2f ") comm1;;
   print_newline ();;
   Printf.printf "v + u = ";;
   List.iter (Printf.printf "%0.2f ") comm2;;
   print_newline ();;
   
   (* Checking associativity: u + (v + w) = (u + v) + w *)
   Printf.printf "------ checking associativity ------ \n";;
   let w = create 3 3.0;;
   let assoc1 = addv u (addv v w);;
   let assoc2 = addv (addv u v) w;;
   Printf.printf "u + (v + w) = ";;
   List.iter (Printf.printf "%0.2f ") assoc1;;
   print_newline ();;
   Printf.printf "(u + v) + w = ";;
   List.iter (Printf.printf "%0.2f ") assoc2;;
   print_newline ();;
   
   (* Checking identity of addition: v + O = v *)
   Printf.printf "------ checking identity of addition ------ \n";;
   let zero_vec = create 3 0.0;;
   let identity_add = addv v zero_vec;;
   Printf.printf "v + O = ";;
   List.iter (Printf.printf "%0.2f ") identity_add;;
   print_newline ();;
   
   (* Checking identity scalar: 1.v = v *)
   Printf.printf "------ checking identity scalar ------ \n";;
   let identity_scalar = scale 1.0 v;;
   Printf.printf "1.v = ";;
   List.iter (Printf.printf "%0.2f ") identity_scalar;;
   print_newline ();;
   
   (* Checking annihilator scalar: 0.v = O *)
   Printf.printf "------ checking annihilator scalar ------ \n";;
   let annihilator_scalar = scale 0.0 v;;
   Printf.printf "0.v = ";;
   List.iter (Printf.printf "%0.2f ") annihilator_scalar;;
   print_newline ();;
   
   (* Checking additive inverse: v + (-v) = O *)
   Printf.printf "------ checking additive inverse ------ \n";;
   let negative_v = scale (-1.0) v;;
   let additive_inverse = addv v negative_v;;
   Printf.printf "v + (-v) = ";;
   List.iter (Printf.printf "%0.2f ") additive_inverse;;
   print_newline ();;
   
   (* Checking scalar product combination: b.(c.v) = (b.c).v *)
   Printf.printf "------ checking scalar product combination ------ \n";;
   let b = 2.0;;
   let c = 3.0;;
   let scalar_comb1 = scale b (scale c v);;
   let scalar_comb2 = scale (b *. c) v;;
   Printf.printf "b.(c.v) = ";;
   List.iter (Printf.printf "%0.2f ") scalar_comb1;;
   print_newline ();;
   Printf.printf "(b.c).v = ";;
   List.iter (Printf.printf "%0.2f ") scalar_comb2;;
   print_newline ();;
   
   (* Checking scalar sum-product distribution: (b + c).v = b.v + c.v *)
   Printf.printf "------ checking scalar sum-product distribution ------ \n";;
   let scalar_sum1 = scale (b +. c) v;;
   let scalar_sum2 = addv (scale b v) (scale c v);;
   Printf.printf "(b + c).v = ";;
   List.iter (Printf.printf "%0.2f ") scalar_sum1;;
   print_newline ();;
   Printf.printf "b.v + c.v = ";;
   List.iter (Printf.printf "%0.2f ") scalar_sum2;;
   print_newline ();;
   
   (* Checking scalar distribution over vector sums: b.(u + v) = b.u + b.v *)
   Printf.printf "------ checking scalar distribution over vector sums ------ \n";;
   let scalar_dist1 = scale b (addv u v);;
   let scalar_dist2 = addv (scale b u) (scale b v);;
   Printf.printf "b.(u + v) = ";;
   List.iter (Printf.printf "%0.2f ") scalar_dist1;;
   print_newline ();;
   Printf.printf "b.u + b.v = ";;
   List.iter (Printf.printf "%0.2f ") scalar_dist2;;
   print_newline ();;
   

 (*-----------PROOFS---------*)


(*
  Proof 1: 
    Commutativity : u + v = v + u
    To prove: addv u v = addv v u
    Proof by induction on dimension n of u and v:
    Base case : n=1, u = [x], v = [y]
    -> u + v = addv u v = (x +. y)::(addv [] [])  // defn of addv
       (x +. y)::(addv [] []) = [x +. y]          // base case of addv
       [x +. y] = [y +. x]                        // commutativity of +. 
       [y +. x] = (y +. x)::(addv [] []) = addv v u = v + u   // defn of addv (<-)
    Induction Hypothesis: for all vectors u and v of dim n, for n = k:
          u + v = v + u or basically addv u v = addv v u
    Induction Step:
      consider u = x::xs & v = y::ys where u and v are of dimension (k+1)
      then:
          addv u v = (x +. y)::(addv xs ys)  // defn of addv
          (x +. y)::(addv xs ys) = (y +. x)::(addv xs ys)  // commutativity of +. 
          (y +. x)::(addv xs ys) = (y +. x)::(addv ys xs)  // Induction Hypothesis of n=k
          (y +. x)::(addv ys xs) = addv v u                // defn of addv (<-)

          Hence by induction, for all vectors u and v of any dimension(>=1), addv u v = addv v u

    
    Proof 2:
      Associativity: u + (v+w) = (u+v) + w
      To prove : addv (addv u v) w = addv u (addv v w)
      Proof by induction on dimension n of u, v and w:
      Base case : n = 1, u = [x], v = [y], w = [z]
      -> addv (addv u v) w = addv ((x +. y)::(addv [] [])) w  // defn of addv
        addv ((x+.y)::(addv [] [])) w = addv [x +. y] w      // base case of addv
        addv [x +. y] w = (x +. y +. z)::(addv [] [])        //defn of addv
        (x +. y +. z)::(addv [] []) = addv u [y +. z]        //associativity of +. and defn of addv <-
        addv u [y +. z] = addv u ((y +. z)::(addv [] []))    //defn of addv <-
        addv u ((y +. z)::(addv [] [])) = addv u (addv v w)  //defn of addv <-
          Hence base case is proved
      Induction Hypothesis: for all vectors u, v and w of dim n, for n = k:
          addv (addv u v) w = addv u (addv v w)
      Induction Step: 
          consider u = x::xs, v = y::ys and w = z::zs where u, v and w are of dimension (k+1)
          then:
            addv (addv u v) w = addv ((x +. y)::(addv xs ys)) w   // defn of addv
            addv ((x +. y)::(addv xs ys)) w = (x +. y +. z)::(addv (addv xs ys) zs)  //defn of addv
            (x +. y +. z)::(addv (addv xs ys) zs) = (x +. (y +. z))::(addv xs (addv ys zs))  // associativity of +.
            (x +. (y +. z))::(addv xs (addv ys zs)) = addv u ((y +. z)::(addv ys zs))   //defn of addv <-
            addv u ((y +. z)::(addv ys zs)) = addv u (addv v w)      //defn of addv <-

            Hence by induction, for all vectors u,v and w of any dimension(>=1), addv (addv u v) w = addv u (addv v w)


    Proof 3:
      Identity of addition:  v + O = v , where O is the zero vector of dimension same as that of v 
      To prove : addv v + O = v       (note that addv O v = v follows directly from Proof 1 of associativity so we can just prove one side)
      Proof by induction on dimension n of v
      Base case: n = 1, v = [x], O = [0.0]
      -> addv v O = (x +. 0.0)::(addv [] [])           // defn of addv
         (x +. 0.0)::(addv [] []) = [x]            //base case of addv and 0.0 is additive identity for floats
         [x] = v 
         Hence base case is proved
      Induction Hypothesis: for all vectors v of dim n and zero vector O of dim n, for n = k:
          addv v O = v
      Induction Step:
        consider v = x::xs and O = 0.0::ys where v and O are of dim (k+1) and hence ys is the zero vector of dimension k
        then:
          addv v O  = (x +. 0.0)::(addv xs ys)       //defn of addv
          (x +. 0.0)::(addv xs ys) = (x)::xs         //induction hypothesis for ys, has dim n = k, also using 0.0 as add id of floats
          (x)::xs = v

          Hence by induction, for all vectors v of any dimension and O being the zero vector of the same dimension as v, addv v + O = v

    
    Proof 4:
      Identity scalar: 1.v = v ,  where 1 is a scalar (float value)
      To prove: scale 1.0 v = v
      Proof by structural induction on structure of v : (vector)
      Base case: v = [x]
      -> scale 1.0 v = (1.0 *. x)::(scale 1.0 [])    //defn of scale
        (1.0 *. x)::(scale 1.0 []) = (x)::[]     //base case of scale and 1.0 being the multiplicative identity for floats
        (x)::[] = [x] = v
        Hence base case is proved
      Induction Hypothesis: scale 1.0 v = v for  v = xs

      Induction Step:
        consider v = x::xs   (for any float x)
        then: 
          scale 1.0 v = (1.0 *. x)::(scale 1.0 xs)     //defn of scale
          (1.0 *. x)::(scale 1.0 xs) = (x)::xs       //using induction hypothesis for v = xs and 1.0 being multiplicative identity for floats
          (x)::xs = v
          
         Therefore, by structural induction on structure of v (vector), scale 1.0 v = v   OR   1.v = v    for all vectors v


    Proof 5:
      Annhilator Scalar: 0.v = O  , where 0 is float value 0.0 and O is the zero vector of same dimension as v
      To prove: scale 0.0 v = O
      Proof by structural induction on structure of v : (vector)
      Base case: v = [x]
      -> scale 0.0 v = (0.0 *. x)::(scale 0.0 [])  //defn of scale 
        (0.0 *. x)::(scale 0.0 []) = (0.0)::[]    //base case of scale and 0.0 being multiplicative annhilator of floats
        (0.0)::[] = O
        Hence base case is proved
      Induction Hypothesis: scale 0.0 v = O' for v = xs      (where O' is zero vector of dimension same as xs)
      Induction Step:
        consider v = x::xs   (for any float x)
        then:
          scale 0.0 v = (0.0 *. x)::(scale 0.0 xs)     //defn of scale
          (0.0 *. x)::(scale 0.0 xs) = (0.0)::O'       //using induction hypothesis and 0.0 being multiplicative annhilator of floats
          (0.0)::O' = O                         (where O is zero vector of dimension same as v)

          Therefore, by structural induction on structure of v (vector), scale 0.0 v = O    OR   0.v = O    for all vectors v


    Proof 6:
      Additive Inverse: v + (-v) = O
      To prove : addv v (inv v) = O
      Proof by structural induction on structure of v : (vector)
      Base case: v = [x]
      -> addv v (inv v) = addv v (scale -1.0 v)      //defn of inv 
        addv v (scale -1.0 v) = addv v ((-1.0 *. x)::(scale -1.0 []))     //defn of scale
        addv v ((-1.0 *. x)::(scale -1.0 [])) = addv v [-x] = (x +. (-x))::(addv [] [])    //defn of addv 
        (x +. (-x))::(addv [] []) = (0.0)::[] = O  
        Hence Base case is proved
      Induction Hypothesis: addv v (inv v) = O' for v = xs    (where O' is zero vector of dimension same as xs)
      Induction Step:
      consider v = x::xs
      then:
        addv v (inv v) = addv v (scale -1.0 x::xs) = addv v ((-x)::(scale -1.0 xs))   //defn of inv 
        addv v ((-x)::(scale -1.0 xs)) = addv v (-x)::(inv xs) = (x +. (-x))::(addv xs (inv xs))   //defn of addv
        (x +. (-x))::(addv xs (inv xs)) = (0.0)::O' = O           //induction hypothesis for v = xs (O is the zero vector dimension same as v)

        Therefore, by structural induction on structure of v (vector), addv v (inv v) = O   OR  v + (-v) = O   for all vectors v

    
    Proof 7:
      Scalar product combination: b.(c.v) = (b.c).v
      To prove : scale b (scale c v) = scale (b *. c) v
      Proof by structural induction on structure of v : (vector)
      Base case : v = [x]
      -> scale b (scale c v) = scale b (scale c [x]) = scale b ((c *. x)::[])   //defn of scale
        scale b ((c *. x)::[])  = scale b [c *. x] = (b *. c *. x)::(scale b [])   //defn of scale
        (b *. c *. x)::(scale b []) = scale (b *. c) [x] = scale (b *. c) v    //associativity of *. and defn of scale (<-)
        Hence base case is proved

      Induction Hypothesis: scale b (scale c v) = scale (b *. c) v for v = xs
      Induction Step : 
      consider v = x::xs
      then:
        scale b (scale c v) = scale b ((c *. x)::(scale c xs))        //defn of scale
        scale b ((c *. x)::(scale c xs)) = (b *. c *. x)::(scale b (scale c xs))   //defn of scale
        (b *. c *. x)::(scale b (scale c xs)) = (b *. c *. x)::(scale (b *. c) xs)   //using induction hypothesis for v = xs
        (b *. c *. x)::(scale (b *. c) xs) = scale (b *. c) v           //defn of scale (<-) and associativity of *.

        Therefore, by structural induction on structure of v (vector), scale b (scale c v) = scale (b *. c) v  for all vectors v


    Proof 8:
      Scalar sum-product distribution: (b+c).v = b.v + c.v
      To prove:  scale (b +. c) v = addv (scale b v) (scale c v)
      Proof by structural induction on structure of v : (vector)
      Base case : v = [x]
      -> scale (b +. c) v = ((b +. c) *. x)::(scale (b +. c) [])     //defn of scale
        ((b +. c) *. x)::(scale (b +. c) []) = (b *. x +. c *. x)::[]    //distribution of *. over +.
        (b *. x +. c *. x)::[] = addv ((b *. x)::[]) ((c *. x)::[])     //defn of addv (<-)
        addv ((b *. x)::[]) ((c *. x)::[]) = addv (scale b v) (scale c v)    //defn of scale (<-)
        Hence base case is proved
      
      Induction Hypothesis: scale (b +. c) v = addv (scale b v) (scale c v) for v = xs
      Induction Step: 
      consider v = x::xs
      then:
        scale (b +. c) v = ((b +. c) *. x)::(scale (b +. c) xs)      //defn of scale
        ((b +. c) *. x)::(scale (b +. c) xs) = (b *. x +. c *. x)::(addv (scale b xs) (scale c xs))   //distribution of *. over +. & induction hypothesis for v = xs
        (b *. x +. c *. x)::(addv (scale b xs) (scale c xs)) = addv ((b *. x)::(scale b xs)) ((c *. x)::(scale c xs))  //defn of addv (<-)
        addv ((b *. x)::(scale b xs)) ((c *. x)::(scale c xs)) = addv (scale b v) (scale c v)       //defn of scale (<-)

        Therefore, by structural induction on structure of v (vector), scale (b +. c) v = addv (scale b v) (scale c v)   for all vectors v


    Proof 9:
      Scalar distribution over vector sums: b.(u+v) = b.u + b.v
      To prove : scale b (addv u v) = addv (scale b u) (scale b v)
      Proof by induction on dimension n of u and v
      Base case: n = 1; u = [x], v = [y]
      -> scale b (addv u v) = scale b ((x +. y)::(addv [] [])) = scale b [x +. y]      //defn of addv
        scale b [x +. y] = (b *. (x +. y))::(scale b [])          //defn of scale
        (b *. (x +. y))::(scale b []) = (b *. x +. b *. y)::[]     //distribution of *. over +.
        (b *. x +. b *. y)::[] = (b *. x +. b *. y)::(addv [] [])   //defn of addv (<-)
        (b *. x +. b *. y)::(addv [] []) = addv ((b *. x)::[]) ((c *. x)::[])    //defn of addv (<-)
        addv ((b *. x)::[]) ((c *. x)::[]) = addv (scale b u) (scale b v)      //defn of scale (<-)
        Hence base case is proved

      Induction Hypothesis: for all vectors u and v of dimension n, for n = k:
              scale b (addv u v) = addv (scale b u) (scale b v)
      Induction Step:
      consider u = x::xs and v = y::ys where u and v are vectors of dimension (k+1) (hence xs and ys are of dimension k) and x and y are any 2 floats
      then:
        scale b (addv u v) = scale b ((x +. y)::(addv xs ys))    //defn of addv
        scale b ((x +. y)::(addv xs ys)) = (b *. (x +. y))::(scale b (addv xs ys))                    // defn of scale
        (b *. (x +. y))::(scale b (addv xs ys)) = (b *. x + b *. y)::(scale b (addv xs ys))          //distribution of *. over +.
        (b *. x + b *. y)::(scale b (addv xs ys)) = (b *. x + b *. y)::(addv (scale b xs) (scale b ys))       //induction hypothesis for n = k as xs and ys are of dim k
        (b *. x + b *. y)::(addv (scale b xs) (scale b ys) = addv ((b *. x)::(scale b xs)) ((b *. y)::(scale b ys))         //defn of addv (<-)
        addv ((b *. x)::(scale b xs)) ((b *. y)::(scale b ys)) = addv (scale b u) (scale b v)                 // defn of scale (<-)

        Hence by induction, for all vectors u and v of any dimension (but same), scale b (addv u v) = addv (scale b u) (scale b v)
 
      
    Additional Properties :-
    
    1. Commutativity of dot product: u.v = v.u for any 2 vectors u and v of same dimension (dot product not defined otherwise)
       To prove: dot_prod u v = dot_prod v u              for any 2 vectors u and v of same dimension
       Proof by induction on dimensiuon n of u and v
       Base case: n = 1; u = [x], v = [y]
       -> dot_prod [x] [y] = dot2 [x] [y] 0.0 = dot2 [] [] (0.0 +. x *. y)     //defn of dot_prod and defn of dot2
          dot2 [] [] (0.0 +. x *. y) = (x *. y)                                //base case of dot2
          (x *. y) = (y *. x) = dot2 [] [] (0.0 +. y *. x)                     //commutativity of *. and defn of dot2 (<-)
          dot2 [] [] (0.0 +. y *. x) = dot2 [y] [x] 0.0 = dot_prod [y] [x]      //defn of dot2 and dot_prod (<-)
          Hence base case is proved

      Induction Hypothesis: for all vectors u and v of dimension n, for n = k:
             dot_prod u v = dot_prod v u 
      Induction Step:
      consider u = x::xs and v = y::ys where u and v are both vectors of dim (k+1)
      then:
        dot_prod u v = dot2 u v 0.0           //defn of dot_prod
        dot2 u v 0.0 = dot2 xs ys (0.0 +. x*.y)    //defn of dot2

        Now to proceed from here we first prove something about dot2:
        So, we shall prove that for all vectors u and v of dimension n and float f, dot2 u v f = dot2 v u f
        Base Case: n = 1, u = [x], v = [y]
        -> dot2 [x] [y] f = dot2 [] [] (f +. x *. y)                       //defn of dot2
          dot2 [] [] (f +. x *. y) = (f +. x *. y)                                //base case of dot2
          (f +. x *. y) = (f +. y *. x) = dot2 [] [] (f +. y *. x)                     //commutativity of *. and defn of dot2 (<-)
          dot2 [] [] (f +. y *. x) = dot2 [y] [x] f                        //defn of dot2(<-)
          Hence base case is proved
        Induction Hypothesis: for all vectors u and v of dimension n and for any float f, for n = k:
              dot2 u v f = dot2 v u f
        Induction Step:
        consider u = x::xs and v = y::ys where u and v are both vectors of dimension (k+1)
        then:
          dot2 u v f = dot2 xs ys (f +. x *. y)                       //defn of dot2
          dot2 xs ys (f +. x *. y) = dot2 xs ys (f +. y *. x)         //commutativity of *.
          dot2 xs ys (f +. y *. x) = dot2 ys xs (f +. y *. x)         //using induction hypothesis on xs and ys for n = k
          dot2 ys xs (f +. y *. x) = dot2 v u f                       //defn of dot2 (<-)

          Hence by induction, dot2 u v f = dot2 v u f, for all vectors u and v of any dimension and for any float f

        Now we can continue with our parent proof:
        dot_prod u v = dot2 u v 0.0           //defn of dot_prod
        dot2 u v 0.0 = dot2 xs ys (0.0 +. x*.y)    //defn of dot2      (till here we did above already)
        dot2 xs ys (0.0 +. x*.y) = dot2 ys xs (0.0 +. y *. x)          //commutativity of *. and using the property regarding dot2 that we proved abvoe
        dot2 ys xs (0.0 +. y *. x) = dot2 v u 0.0 = dot_prod v u       //denf of dot2 and dot_prod (<-)

        Hence by induction, dot_prod u v = dot_prod v u, for all vectors u and v of any dimension


    2. Scaling vector scales length: length (scale c v) = |c| *. (length v)     for any float c and vector v
        To prove: length (scale c v) = |c| *. (length v)      for any float c and vector v where |c| denotes absolute value of c
        Proof by structural induction on structure of v : vector
        Base case: v = [x]
        -> length (scale c [x]) = length ((c*.x)::(scale c []))          //defn of scale
          length ((c*.x)::(scale c [])) = length ([c *. x])              //base case of scale
          length [c *. x] = Float.sqrt(dot_prod [c *. x] [c *. x])       //defn of length
          Float.sqrt(dot_prod [c *. x] [c *. x]) = Float.sqrt(dot2 [c*.x] [c*.x] 0.0)    //defn of dot_prod
          Float.sqrt(dot2 [c*.x] [c*.x] 0.0) = Float.sqrt(dot2 [] [] (c*.x*.c*.x))     //defn of dot2
          Float.sqrt(dot2 [] [] (c*.x*.c*.x)) = Float.sqrt(c*.x*.c*.x)    //base case of dot2
          Float.sqrt(c*.x*.c*.x) = |c|*.(Float.sqrt(x*.x))                  //assuming correctness of Float.sqrt we can take |c| out
          |c|*.(Float.sqrt(x*.x)) = |c|*.(Float.sqrt(dot2 [] [] (x*.x)))    //defn of dot2 (<-)
          |c|*.(Float.sqrt(dot2 [] [] (x*.x))) = |c|*.(Float.sqrt(dot2 [x] [x] 0.0))       //defn of dot2 (<-)
          |c|*.(Float.sqrt(dot2 [x] [x] 0.0)) = |c|*.(Float.sqrt(dot_prod [x] [x]))       //defn of dot_prod (<-)
          |c|*.(Float.sqrt(dot_prod [x] [x])) = |c|*.(length [x])             //defn of length (<-)
          Hence base case is proved

        Induction Hypothesis: length (scale c v) = |c| *. (length v) for any float c and v = xs
        Induction Step:
        consider v = x::xs
        then:
          length (scale c v) = Float.sqrt(dot_prod (scale c v) (scale c v))        //defn of length
          Float.sqrt(dot_prod (scale c v) (scale c v)) = Float.sqrt(dot2 (scale c v) (scale c v) 0.0)    //defn of dot_prod
          Float.sqrt(dot2 (scale c v) (scale c v) 0.0) = Float.sqrt(dot2 ((c*.x)::(scale c xs)) ((c*.x)::(scale c xs)) 0.0)     //defn of scale
          Float.sqrt(dot2 ((c*.x)::(scale c xs)) ((c*.x)::(scale c xs)) 0.0) = Float.sqrt(dot2 (scale c xs) (scale c xs) (c*.x*.c*.x))   //denf of dot2
          
          To proceed from here we need to prove something: dot2 u v f = f +. dot2 u v 0.0
          Proof by induction on dimension n of u and v (should be same)
          Base case: n = 1, u = [x], v = [y]
          -> dot2 [x] [y] f = dot2 [] [] (f +. x *. y)             //defn of dot2
            dot2 [] [] (f +. x *. y) = (f +. x *. y) = f +. (0.0 + x *. y)    //base case of dot2 and 0.0 as additive identity for floats
            f +. (0.0 + x *. y) = f +. (dot2 [] [] (0.0 +. x *. y))       //defn of dot2 (<-)
            f +. (dot2 [] [] (0.0 +. x *. y)) = f +. (dot2 [x] [y] 0.0)     //defn of dot2 (<-)
            f +. (dot2 [x] [y] 0.0) = f +. dot2 u v 0.0            //defn of dot2 (<-)
            Hence base case is proved

          Induction Hypothesis: for all vectors u and v of dimension n and for any float f, for n=k: 
                      dot2 u v f = f +. dot2 u v 0.0 
          Induction Step:
          consider u = x::xs and v = y::ys where u and v are vectors of dimension (k+1)
          then:
            dot2 x::xs y::ys f = dot2 xs ys (f +. x*.y)     //defn of dot2
            dot2 xs ys (f +. x*.y) = f +. x *. y + dot2 xs ys 0.0     //induction hypothesis on xs ys of dimension n
            f +. x *. y + dot2 xs ys 0.0 = f +. dot2 xs ys (0.0 +. x*.y)   //induction hypothesis (<-)
            f +. dot2 xs ys (0.0 +. x*.y) = f +. dot2 x::xs y::ys 0.0  = f +. dot2 u v 0.0    //defn of dot2 (<-)

            Hence by induction, dot2 u v f = f +. dot2 u v 0.0 , for all vectors u and v of any dimension and for any float f

          Now we can continue with our parent proof:
          Float.sqrt(dot2 ((c*.x)::(scale c xs)) ((c*.x)::(scale c xs)) 0.0) = Float.sqrt(dot2 (scale c xs) (scale c xs) (c*.x*.c*.x))   //denf of dot2  (previously done till here)
          Float.sqrt(dot2 (scale c xs) (scale c xs) (c*.x*.c*.x)) = Float.sqrt(c*.x*.c*.x +. dot2 (scale c xs) (scale c xs) (0.0))        //using what we just proved above
          Float.sqrt(c*.x*.c*.x +. dot2 (scale c xs) (scale c xs) (0.0)) = Float.sqrt(c*.x*.c*.x +. dot_prod (scale c xs) (scale c xs))    //defn of dot_prod (<-)
          Float.sqrt(c*.x*.c*.x +. dot_prod (scale c xs) (scale c xs)) = Float.sqrt(c*.x*.c*.x +. c*.(length xs)*.c*.(length xs)))         //using induction hypothesis(square it)
          Float.sqrt(c*.x*.c*.x +. c*.(length xs)*.c*.(length xs))) = |c|*.(Float.sqrt(x*.x +. (length xs))*.(length xs))                 //assuming correctness of Float.sqrt
          |c|*.(Float.sqrt(x*.x +. (length xs))*.(length xs)) = |c|*.(Float.sqrt(x*.x +. dot_prod xs xs)                               //defn of length (<-)
          |c|*.(Float.sqrt(x*.x +. dot_prod xs xs) = |c|*.(Float.sqrt(x*.x +. dot2 xs xs 0.0)                                           //denf of dot_prod
          |c|*.(Float.sqrt(x*.x +. dot2 xs xs 0.0) = |c|*.(Float.sqrt(dot2 xs xs (x*.x))                                                 //defn of dot2 (<-)
          |c|*.(Float.sqrt(dot2 xs xs (x*.x)) = |c|*.(Float.sqrt(dot2 x::xs x::xs 0.0)                               //defn of dot2(<-)
          |c|*.(Float.sqrt(dot2 x::xs x::xs 0.0) = |c|*.(Float.sqrt(dot_prod x::xs x::xs))                           //defn of dot(<-)
          |c|*.(Float.sqrt(dot_prod x::xs x::xs) = |c|*.(length v)

          Hence by induction, length (scale c v) = |c| *. (length v)   , for any float c and all vectors v


    3. Distributivity of dot product over vector sum :  dot_prod u (addv v w) = dot_prod u v +. dot_prod u w
        To Prove: dot_prod u (addv v w) = dot_prod u v +. dot_prod u w
        Proof by induction on dimension n of u v and w (should be same otherwise addv and dot_prod wouldn't be defined)
        Base case: n = 1; u = [x], v = [y], w = [z]
        -> dot_prod [x] (addv [y] [z]) = dot_prod [x] ((y+.z)::(addv [] []))        //defn of addv
          dot_prod [x] ((y+.z)::(addv [] [])) = dot_prod [x] [y +. z]                //base case of addv
          dot_prod [x] [y +. z] = dot2 [x] [y+.z] 0.0 = dot2 [] [] (x*.(y+.z))      //defn of dot_prod and defn of dot2
          dot2 [] [] (x*.(y+.z)) = (x*.y +. x*.z)                                    //base case of dot2 and distribution of *. over +.
          (x*.y +. x*.z) = (dot2 [] [] (x *. y)) +. (dot2 [] [] (x *. z))            //defn of dot2 (<-)
          (dot2 [] [] (x *. y)) +. (dot2 [] [] (x *. z)) = dot_prod [x] [y] +. dot_prod [x] [z]    //defn of dot2 and dot_prod (<-)
          Hence Base case is proved

        Induction Hypothesis: dot_prod u (addv v w) = dot_prod u v +. dot_prod u w for u = xs, v = ys, w = zs

        Induction Step:
        consider u = x::xs, v = y::ys, w = z::zs
        then:
          dot_prod u (addv v w) = dot_prod x::xs ((y+.z)::(addv ys zs))              //defn of addv
          dot_prod x::xs ((y+.z)::(addv ys zs)) = dot2 x::xs ((y+.z)::(addv ys zs)) 0.0 = dot2 xs (addv ys zs)) (0.0 + x*.(y+.z))           //defn of dot_prod and defn of dot2
          dot2 xs (addv ys zs)) (0.0 + x*.(y+.z)) = x*.y +. x*.z +. dot2 xs (addv ys zs) 0.0        //using the property of dot2 we proved in Proof 2: dot2 u v f = f +. dot2 u v 0.0
          x*.y +. x*.z +. dot2 xs (addv ys zs) 0.0 = x*.y +. x*.z +. dot_prod xs (addv ys zs) = x*.y +. x*.z +. dot_prod xs ys +. dot_prod xs zs        //Induction hypothesis for u = xs, v = ys, w = zs
          x*.y +. x*.z +. dot_prod xs ys +. dot_prod xs zs = (x*.y +. dot_prod xs ys) +. (x*.z +. dot_prod xs zs) 
          (x*.y +. dot_prod xs ys) +. (x*.z +. dot_prod xs zs) = (x*.y +. dot2 xs ys 0.0) +. (x*.z +. dot2 xs zs 0.0) = (dot2 xs ys x*.y) +. (dot2 xs zs x*.z)       //using again the property of dot2 :dot2 u v f = f +. dot2 u v 0.0
          (dot2 xs ys x*.y) +. (dot2 xs zs x*.z) = dot_prod x::xs y::ys +. dot_prod x::xs z::zs           //defn of dot_prod (<-)

          Hence by induction, dot_prod u (addv v w) = dot_prod u v +. dot_prod u w for all vectors u, v and w of same dimension


    4. Commutativity of angle: angle u v = angle v u for any 2 vectors u and v of same dimension (else angle would not be defined)
        To Prove : angle u v = angle v u     for any 2 vectors u and v of same dimension
        Explicit proof using commutativity of dot_prod:
        angle u v = (dot_prod u v)/((length u)*.(length v))                //defn of angle 
        (dot_prod u v)/((length u)*.(length v)) = (dot_prod v u)/((length v)*.(length u))     //commutativity of dot_prod and *.
        (dot_prod v u)/((length v)*.(length u)) = angle v u                //defn of angle (<-)
        Hence proved



   *)           
      