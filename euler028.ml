
open Euler;;

let evil_diag layer = 
    let rec evil_diag_rec layer_n sum last_n =
        let first_add = last_n + layer_n + 1 in
        let second_add = first_add + layer_n + 1 in
        let third_add = second_add + layer_n + 1 in
        let fourth_add = third_add + layer_n + 1 in
        let new_sum = sum + first_add + second_add + third_add + fourth_add in
        if layer_n < layer then
            evil_diag_rec (layer_n + 2) new_sum fourth_add
        else
            sum
    in 
    evil_diag_rec 1 1 1;;

let _ = 
    print_string (string_of_int (evil_diag 1001) ^ "\n");;
