(* computes the nth fibonnaci number, indexed from 0 *)
let nth_fibonnaci n = 
  let rec helper n n1 n2 =
    if n = 0 then n2 else helper (n-1) n2 (n1+n2) in
  helper n 1 1

(* computes the fibonnaci sequence to n positions *)
let fibonnaci_seq n = 
  let rec helper ind sol = 
    if ind = n then sol else helper (ind+1) (sol @ [nth_fibonnaci ind]) in
  helper 0 [1]

(* computes sum of even-valued terms in fibonnaci sequence whose
 * values do not exceed four million *)
let even_fib_sum = 
  let even x = x mod 2 = 0 in 
  let rec helper sum ind =
    let term = nth_fibonnaci ind in 
    if term > 4000000 then sum
    else if even term then helper (term+sum) (ind+1)
    else helper sum (ind+1) in
  helper 0 0
