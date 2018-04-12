(* 目的：鶴の数xに応じた足の本数を計算する  *)
(* tsuru_no_ashi : int -> int  *)
let tsuru_no_ashi x = x * 2

(* テスト  *)
let test1 = tsuru_no_ashi 10 = 20
let test2 = tsuru_no_ashi 25 = 50
let test3 = tsuru_no_ashi 31 = 62
