(* 目的：受け取った時間xが午前か午後か判定して返す *)
(* jikan : float -> string *)
let jikan x = 
  if x >= 0. && x < 12. then "gozen"
                      else "gogo"

(* テスト *)
let test1 = jikan 6.  = "gozen"
let test2 = jikan 20. = "gogo" 
let test3 = jikan 12. = "gogo"

