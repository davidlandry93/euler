
let is_prime n = 
    match n with
    | x when x <= 1 -> false
    | 2 | 3 -> true
    | x when ((x mod 2) = 0 || (x mod 3) = 0) -> false
    | _ ->  
        let rec is_prime_rec i = 
            if i*i >= n then
                true
            else
                (if ((n mod i) = 0 || (n mod (i+2)) = 0) then
                    false
                else
                    is_prime_rec (i+6))
        in
        is_prime_rec 5;;

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
    b * b * (if n mod 2 = 0 then 1 else a);;

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;
