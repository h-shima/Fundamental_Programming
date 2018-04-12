(* ふたつの自然数mとn ( m >= n >= 0)の最大公約数を求める *)
(* gcd : int -> int -> int *)
let rec gcd m n = if n = 0
                  then m
                  else gcd n ( m mod n ) 

(* 再帰の度にnが小さくなるため、やがてn = 0になり停止する *)

(* テスト *)
let test1 = gcd 0 0 = 0
let test2 = gcd 3 0 = 3
let test3 = gcd 8 4 = 4
let test4 = gcd 1071 1029 = 21







