
open Euler;;

let table = Array.make_matrix 8 201 0;;

let values = [1;2;5;10;20;50;100;200];;

for i = 0 to 7 do
    table.(i).(0) <- 0
done;;

for j = 0 to 200 do
    table.(0).(j) <- 1
done;;

for i = 1 to 7 do
    for j = 1 to 200 do
        let current_value = List.nth values i in
        if current_value < j then
            table.(i).(j) <- table.(i-1).(j-current_value) + 1
        else
            table.(i).(j) <- table.(i-1).(j)
    done
done;;

let _ =
    print_string ((string_of_int table.(7).(200)) ^ "\n");;
