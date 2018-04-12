(* 目的：現在の気温tから快適度を表す文字列を計算する *)
(* kion : int -> string *)
let kion t = 
  if 15 <= t && t <= 25 then "kaiteki"
                        else "futsu"

(*テスト*)
let test1 = kion 22 = "kaiteki"
let test2 = kion 10 = "futsu"
let test3 = kion 25 = "kaiteki"

(* 目的：現在の気温tが15以上25以下か計算する *)
(* kaiteki : int -> bool *)
let kaiteki t = 15 <= t && t <= 25 

(* テスト *)
let test1 = kaiteki 10 = false
let test2 = kaiteki 15 = true
let test3 = kaiteki 20 = true
let test4 = kaiteki 25 = true
let test5 = kaiteki 30 = false

(* 目的：現在の気温tから快適度を表す文字列を計算する *)
let kion t = if kaiteki t then "kaiteki"
                          else "futsu"

(* テスト *)
let test1 = kion 22 = "kaiteki"
let test2 = kion 10 = "futsu"
let test3 = kion 25 = "kaiteki"
