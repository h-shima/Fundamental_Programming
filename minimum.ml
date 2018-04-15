(* 目的：受け取った整数と整数を要素に持つlstの中の最小値を返す *)
(* minimum2 : int -> int list -> int *)
let rec minimum2 i lst = match lst with
    [] -> i
  | first :: rest -> 
      let rest_min = minimum2 first rest
      in if i <= rest_min then i
                          else rest_min

(* 目的：受け取ったリストlstの最小値を返す *)
(* minimum : int list -> int *)
let minimum lst = match lst with
    [] -> max_int
  | first :: rest ->
      minimum2 first rest

(*テスト *)
let test1 = minimum [3] = 3
let test2 = minimum [2; 43; 3; 5] = 2
let test3 = minimum [3; 53; 0; -1] = -1
let test4 = minimum [23; 32; 1; 0] = 0



