(*目的：亀の数xに応じた足の数を計算する*)
(* kame_no_ashi : int -> int *)
let kame_no_ashi x = x * 4

(* テスト  *)
let test1 = kame_no_ashi 10 = 40 
let test2 = kame_no_ashi 22 = 88
let test3 = kame_no_ashi 35 = 140
