(* 目的：整数のリストlstを受け取ったら、それまでの数の合計からなるリストを返す *)
(* sum_list : int list -> int list *)
let sum_list lst =
(* 目的：整数のリストを受け取ったら、それまでの数の合計からなるリストを返す *)
(* hojo : int list -> int -> int list *)
(* ここでtotal0はこれまでの数の合計 *)
let rec hojo lst total0 = match lst with
    [] -> []
  | first :: rest -> ( first + total0 )
                     :: hojo rest ( first + total0 )
in hojo lst 0

(* テスト *)
let test1 = sum_list [] = []
let test2 = sum_list [2; 5; 2; 4; 7] = [2; 7; 9; 13; 20]







