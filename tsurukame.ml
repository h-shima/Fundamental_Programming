(* 目的：鶴と亀の数の合計xと足の数の合計yに応じた鶴の数を計算する *)
(* tsurukame : int -> int -> int *)
let tsurukame x y = x - ( ( y - 2 * x ) / 2 )

(* テスト  *)
let test1 = tsurukame 3 6   = 3
let test2 = tsurukame 5 14  = 3
let test3 = tsurukame 10 30 = 5

