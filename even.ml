(* int list は
  - [] 空リスト、または
　- first :: rest 最初の要素がfirstで残りのリストがrest（restが自己参照のケース）の形 *)

(* 目的：受け取ったリストlstから偶数の要素のみを含むリストを返す *)
(* even : int list -> int list *)
let rec even lst = match lst with
    []            -> []
  | first :: rest -> if first mod 2 = 0 then first :: even rest
                                        else even rest  (* even rest *)                                       

(* テスト *)
let test1 = even [] = []
let test2 = even [1; 2; 3; 4] = [2; 4]
let test3 = even [3; 1; 6; 0] = [6; 0]
let test4 = even [0] = [0]



