(* string list は 
  - []            空リスト、あるいは
  - first :: rest 最初の要素がfirstで残りのリストがrest（restが自己参照のケース）
    という形
*)

(* 目的：受け取った文字列のリストlstの要素を前から順に全部くっつけた文字列を返す *)
(* concat : string list -> string *)
let rec concat lst = match lst with
    []            -> ""
  | first :: rest -> first ^ concat rest (* concat rest *)

(* テスト *)
let test1 = concat ["haru"; "natsu"] = "harunatsu"
let test2 = concat [] = ""
let test3 = concat ["haru"; "natsu"; "aki"; "fuyu"] = "harunatsuakifuyu"


