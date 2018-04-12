(* 目的：与えられたリストを逆順にして返す *)
(* reverse : 'a list -> 'a list *)
let reverse lst =
  (* 目的：( lstの逆順のリスト ) @ resultを返す *)
  (* ここでresultはこれまで処理してきたリストを逆順にしたものを示す *)
  let rec rev lst result = match lst with
      [] -> result
    | first :: rest -> rev rest ( first :: result )
  in rev lst []

(* テスト *)
let test1 = reverse [] = []
let test2 = reverse [1] = [1]
let test3 = reverse ["あ"; "い"; "う"] = ["う"; "い"; "あ"]









