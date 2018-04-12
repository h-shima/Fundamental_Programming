(* 目的：受け取った整数を要素に持つlstの中の最小値を返す *)
(* minimum : int list -> int *)
let rec minimum lst = match lst with
    [] -> max_int
  | first :: rest ->
    let min_rest = minimum rest in
    if first < min_rest then first
                        else min_rest


(*テスト *)
let test1 = minimum [] = max_int
let test2 = minimum [2; 43; 3; 5] = 2
let test3 = minimum [3; 53; 0; -1] = -1
let test4 = minimum [23; 32; 1; 0] = 0



