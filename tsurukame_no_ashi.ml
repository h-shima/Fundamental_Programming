(*目的：鶴の数xと亀の数yに応じた足の数を計算する*)
(* let tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi x y =  2 * x + 4 * y

(* テスト  *)
let test1 = tsurukame_no_ashi 5 5   = 30
let test2 = tsurukame_no_ashi 6 8   = 44
let test3 = tsurukame_no_ashi 30 20 = 140
