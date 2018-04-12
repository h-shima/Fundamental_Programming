(* 人ひとり分のデータ（名前 身長(m)、体重(kg)、誕生日(月、日)、血液型）を表す型 *)
type person_t = {
  namae         : string; (* 名前 *)
  sintyo        : float; (* 身長 *)
  taiju         : float; (* 体重 *)
  tanjoubi  : int * int; (* 誕生日 *)
  ketsuekigata : string; (* 血液型 *)
}

(* person_t list は
    - []             空リストあるいは
    - first::rest    最初の要素がfirstで残りのリストがrest
    （firstはperson_t型、restが自己参照のケース）
  という形 *)

(* person_t list 型のデータの例 *)
let lst1 = []
let lst2 = [{namae = "ken"; sintyo = 179.0; taiju = 70.4; tanjoubi = (1, 3); ketsuekigata = "A"}]
let lst3 = [{namae = "shin"; sintyo = 169.3; taiju = 65.0; tanjoubi = (8, 29); ketsuekigata = "AB"};
            {namae = "nobu"; sintyo = 178.9; taiju = 60.0; tanjoubi = (9, 8); ketsuekigata = "B"}]


(* 目的：person_t型のリストlstのうち、指定された血液型の人数を返す *)
(* count_ketsueki : person_t list -> string -> int *)
let rec count_ketsueki lst ketsueki0 = match lst with
    [] -> 0
  | {namae = n; sintyo = s; taiju = t; tanjoubi = (m, d); ketsuekigata = k}::rest ->
      if k = ketsueki0 then 1 + count_ketsueki rest ketsueki0 (* count_ketsueki rest ketsueki0 *)
                       else count_ketsueki rest ketsueki0 (* count_ketsueki rest ketsueki0 *)



(* テスト *)
let test1 = count_ketsueki lst1 "A" = 0
let test2 = count_ketsueki lst2 "A" = 1
let test3 = count_ketsueki lst3 "A" = 0


(* 目的：person_t型のリストlstのうち、血液型がA型の人の数を返す *)
(* count_ketsueki_A : person_t -> int *)
let rec  count_ketsueki_A lst = match lst with
    [] -> 0
  | {namae = n; sintyo = s; taiju = t; tanjoubi = (m, d); ketsuekigata = k} :: rest  
      -> if k = "A" then 1 + count_ketsueki_A rest
         else count_ketsueki_A rest (* count_ketsueki_A rest *)

(* テスト *)
let test4 = count_ketsueki_A lst1 = 0
let test5 = count_ketsueki_A lst2 = 1
let test6 = count_ketsueki_A lst3 = 0


