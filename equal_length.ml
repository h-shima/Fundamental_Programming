(* 受け取ったリストlstの長さを返す *)
(* len : 'a list -> int *)
let rec len lst = match lst with
    [] -> 0
  | first ::rest -> 1 + len rest (* len rest *)

(* テスト *)
let test1 = len [] = 0
let test2 = len ["buzz"; "fizz"] = 2
let test3 = len [3; 22; 44; 5] = 4
let test4 = len [1.23; 3.22] = 2


(* 目的：受け取ったリストlst1とlst2から、それらの長さが同じかどうか判定する *)
(* equal_length : 'a list -> a' list -> bool *)
let rec equal_length lst1 lst2 = match (lst1, lst2) with
    ([], []) -> true
  | (first1::rest1, []) -> false
  | ([], first2::rest2) -> false
  | (first1::rest1, first2::rest2) -> 
     if len (first1::rest1) = len (first2::rest2) then true
     else false

(* equal_length lst1 rest2, rest1 lst2, rest1 rest2 *)

(* テスト *)
let test5 = equal_length [] [] = true
let test6 = equal_length [1; 2] [] = false
let test7 = equal_length [] [3; 4] = false
let test8 = equal_length [3; 22; 2] [34; 1; 7] = true



