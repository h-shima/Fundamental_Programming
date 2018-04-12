(* int list は
    - [] 空リスト、あるいは
　　- first :: rest  最初の要素がfirstで残りのリストがrest（restが自己参照のケース
  という形 *)

(* 目的：受け取ったリストlstの長さを返す *)
(* length : int list -> int *)
let rec length lst = match lst with
    []            -> 0
  | first :: rest -> 1 + length rest (* length rest *)

(* テスト *)
let test1 = length [] = 0
let test2 = length [2] = 1
let test3 = length [2; 3] = 2
let test4 = length [1; 1; 1; 1; 1; 1] = 6
