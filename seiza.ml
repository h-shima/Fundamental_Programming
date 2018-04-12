(* 目的：受けとった誕生日xから星座を計算する *)
(* seiza : float -> string *)
let seiza x = if x >= 1.21 && x <= 2.19  then "mizugame"
              else if x < 1.21 || x > 12.23 then "yagi"
              else if x <= 3.20          then "uo"
              else if x <= 4.20          then "ohitsuji"
              else if x <= 5.20          then "oushi"
              else if x <= 6.21          then "futago"
              else if x <= 7.23          then "kani"
              else if x <= 8.23          then "shishi"
              else if x <= 9.23          then "otome"
              else if x <= 10.23         then "tenbin"
              else if x <= 11.22         then "sasori"
              else "ite"

(* テスト *)
let test1 = seiza 1.21  = "mizugame"
let test2 = seiza 10.26 = "sasori"
let test3 = seiza 1.19  = "yagi" 
let test4 = seiza 9.07 = "otome"
let test5 = seiza 9.08 = "otome"
let test6 = seiza 9.01 = "otome" 

(* 人ひとり分のデータを表す型 *)
type person_t = {
  namae        : string;    (* 名前 *)
  tanjoubi     : float;     (* 誕生日(月、日) *)
  ketsuekigata : string;    (* 血液型 *)
}

(* 受け取ったperson_t型の要素を持つリストからおとめ座の人の名前のみからなるリストを返す *)
(* otomeza : person_t -> string list *)
let rec otomeza lst = match lst with
    [] -> []
  | {namae = n; tanjoubi = t; ketsuekigata = k} :: rest
      -> if seiza t = "otome" then n :: otomeza rest
         else otomeza rest (* otomeza rest *)


(* person_t型の要素を持つリスト例 *)

let lst1 = []
let lst2 = [{namae = "hiroki"; tanjoubi = 9.08; ketsuekigata = "A"}]
let lst3 = [{namae = "manabu"; tanjoubi = 7.31; ketsuekigata = "B"};
            {namae = "shin"; tanjoubi = 9.01; ketsuekigata = "O"};
            {namae = "taro"; tanjoubi = 9.07; ketsuekigata = "A"}]

(* テスト *)
let test1 = otomeza lst1 = []
let test2 = otomeza lst2 = ["hiroki"]
let test3 = otomeza lst3 = ["shin"; "taro"]


