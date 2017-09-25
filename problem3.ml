(* what is the largest prime factor of the number 600851475143? *)

let is_prime n = 
  (* evaluates to true if n has no divisors between 2 and sqrt(m), inclusive *)
  let rec no_divisors m =
    m * m > n || (n mod m != 0 && no_divisors (m+1)) in
  n >= 2 && no_divisors 2

let prime_factorization n = 
  let rec helper m sol div = 
    if m = 1 then sol
    else if is_prime div && m mod div = 0 then helper (m/div) (div::sol) div
    else helper m sol (div+1) in
  helper n [] 2

let ans = List.hd (prime_factorization 600851475143)
