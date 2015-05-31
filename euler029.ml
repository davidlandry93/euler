
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
    in
let range a b =
    let rec range_rec x y l =
        if x > y then l else range_rec x (y-1) (y :: l)
    in
let lol = List.map (fun x -> List.fold_right (fun y acc -> (pow x y) :: acc) l []) l in
lol

