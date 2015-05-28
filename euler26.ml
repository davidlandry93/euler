
(* Project euler: question 26 *)

open String;;

let range a b =
    let rec range_rec x y l =
        if x > y then l else range_rec x (y-1) (y :: l)
    in
    range_rec a b [];; 

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let index_of e l =
    let rec index_rec i = function
        | [] -> -1
        | hd::r -> if hd = e then i else index_rec (i+1) r
    in
    index_rec 0 l;;

let recuring_part x =
    let rec length_of_recurring_part_rec a b divider_list= 
        let index_of_a = index_of a divider_list in
        if index_of_a = -1 then
         if a < b then
             length_of_recurring_part_rec (a*10) b (a :: divider_list) 
         else
             length_of_recurring_part_rec ((a mod b)*10) b (a :: divider_list)
         else 
        index_of_a + 1
    in
    length_of_recurring_part_rec 10 x [];;

let rec g l m i j = 
    match l with
    | [] -> j
    | h::r -> 
            if h > m then
                g r h (i+1) i
            else
                g r m (i+1) j

let _ = 
    print_string " === Project Euler: Question 26 === \n";
    let all_numbers = range 1 5000 in
    let lengths = List.map recuring_part all_numbers in
    let toto = g lengths 0 1 0 in
    print_string (string_of_int toto)


