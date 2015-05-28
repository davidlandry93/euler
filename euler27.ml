
let time f x y =
    let t = Sys.time() in
    let fx = f x y in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx

let range a b =
    let rec range_rec x y l =
        if x > y then l else range_rec x (y-1) (y :: l)
    in
    range_rec a b [];; 

let length_of_prime_seq a b = 
    let f n = n*n + a*n + b in
    let rec length_of_prime_seq_rec i =
        if Euler.is_prime (f i) && i < b then 
            length_of_prime_seq_rec (i+1)
        else
            i
    in
    length_of_prime_seq_rec 1

let longest_seq min max =
    let values = range min max in
    let f a = 
        List.map (fun b -> (a,b)) values
    in
    let search_space = List.concat (List.map f values) in
    let g pair = 
        let x1,x2 = pair in 
        (length_of_prime_seq x1 x2,x1,x2) 
    in
    let solnts = List.map g search_space in
    let h (tuple1 : int*int*int) (tuple2 : int*int*int) = 
        let premier,_,_ = tuple1 
        and deuxieme,_,_ = tuple2 in
        if premier > deuxieme then tuple1 else tuple2
    in
    let victorieux = List.fold_right h solnts (0,0,0) in
    let produit = 
        let x1,x2,x3 = victorieux in
        x2*x3
    in
    print_string (string_of_int produit)

let _ = 
    time longest_seq (-1000) 1000;;
