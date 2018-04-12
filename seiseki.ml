(* 目的：受け取った名前と成績の組から文字列を返す *)
(* string * string -> string *)
let seiseki pair = match pair with
  ( a , b ) -> a ^ "さんの評価は" ^ b ^ "です"

(* テスト *)
let test1 = seiseki ( "田中" , "A" ) = "田中さんの評価はAです"
let test2 = seiseki ( "maeda", "B" ) = "maedaさんの評価はBです"
let test3 = seiseki ( "野上" , "C" ) = "野上さんの評価はCです"
