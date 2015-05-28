
open Euler;;

let is_noice n = 
    let string_of_n = string_of_int n in
    let chars = explode string_of_n in
    let expo = List.map (fun c -> pow (int_of_string (String.make 1 c)) 5) chars in
    let sum = List.fold_right (+) expo 0 in
    sum = n

let filtered_search_space = 
    List.filter is_noice (range 10 999999) 

let _ = print_string (string_of_int (List.fold_right (+) filtered_search_space 0));;
