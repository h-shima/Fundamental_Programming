(* 目的：受け取った昇順の整数のリストlstの要素を前から見ていき、昇順となる位置にnを挿入したリストを返す *)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with
    [] -> n :: []
  | first :: rest -> if n <= first then n :: ( first :: rest )
                     else first ::( insert rest n )  (* insert rest n  *)
(* テスト *)
let test1 = insert [] 1 = [1]
let test2 = insert [2] 1 = [1; 2]
let test3 = insert [2; 3] 4 = [2; 3; 4]
let test4 = insert [2; 4; 7] 6 = [2; 4; 6; 7]
let test5 = insert [2; 3; 4; 5; 6; 8] 7 = [2; 3; 4; 5; 6; 7; 8]

(* 目的：受け取った整数を要素に持つリストlstを昇順に整列させたリストを返す *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest -> insert ( ins_sort rest ) first (* ins_sort rest *)

(* テスト *)
let test1 = ins_sort [] = []
let test2 = ins_sort [1; 2] = [1; 2]
let test3 = ins_sort [2; 1] = [1; 2]
let test4 = ins_sort [3; 7; 1; 6; 4; 9; 8] = [1; 3; 4; 6; 7; 8; 9]



