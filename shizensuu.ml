(* 自然数は
　- 0       0、あるいは
  - n + 1   ひとつ小さい自然数nに1を加えたもの( nが自己参照のケース ) 
  
  という形 *)

(* 目的：自然数nの階乗を求める *)
(* fac : int -> int *)
let rec fac n = if n = 0 then 1
                         else n * fac (n - 1)  (* fac (n - 1) *)

(* テスト *)
let test1 = fac 0 = 1
let test2 = fac 1 = 1
let test3 = fac 2 = 2
let test4 = fac 3 = 6
let test5 = fac 4 = 24
let test6 = fac 10 = 3628800

(* 目的：自然数mとnを受け取ったらmのn乗を求める *)
(* power : int -> int -> int *)
let rec power m n = if n = 0 then 1 (* n = 0 のケース *)
                             else m * power m (n - 1) (* nが1以上のケース *)  (* power m (n - 1) *)

(* テスト *)
let test7 = power 3 0 = 1
let test8 = power 3 1 = 3
let test9 = power 3 2 = 9
let test10 = power 3 3 = 27

(* 目的：０から受け取った自然数nまでの２乗の和を返す *)
(* sum_of_square : int -> int *)
let rec sum_of_square n = if n = 0 then 0                              (* ０のケース *)
                                   else n * n + sum_of_square (n - 1)  (* 1以上のケース *) (* sum_of_square (n - 1) *)

(* テスト *)
let test11 = sum_of_square 0 = 0
let test12 = sum_of_square 1 = 1
let test13 = sum_of_square 2 = 5
let test14 = sum_of_square 4 = 30

(* 目的：与えられた漸化式anの第n項を求める関数aを作る *)
(* a : int -> int *)
let rec a n = if n = 0 then 3 (* ０のケース *)
                       else 2 * ( a (n - 1) ) - 1 (* １以上のケース *) (* a (n - 1) *)

(* テスト *)
let test15 = a 0 = 3
let test16 = a 1 = 5
let test17 = a 2 = 9
let test18 = a 3 = 17




