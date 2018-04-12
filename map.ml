
(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae   : string; (* 名前 *)
  tensuu  : int;    (* 点数 *)
  seiseki : string; (* 成績 *)
}

(* 目的：関数fとリストlstを受け取りfを施したリストを返す *)
(* map : ( 'a -> 'b ) -> 'a list -> 'b list *)
let rec map f lst = match lst with
    [] -> []
  | first :: rest -> f first :: map f rest (* map rest *)

(* 目的：実数のリストlstを受け取り、各要素の平方根のリストを返す *)
(* map_sqrt : float list -> float list *)
let map_sqrt lst = map sqrt lst



(* 目的：学生のデータgakuseiを受け取り成績のついたデータを返す *)
(* hyouka : gakusei_t -> gakusei_t *)
let hyouka gakusei = match gakusei with
  { namae = n; tensuu = t; seiseki = s } ->
    if t >= 80      then { namae = n; tensuu = t; seiseki = "A" }
    else if t >= 70 then { namae = n; tensuu = t; seiseki = "B" }
    else if t >= 60 then { namae = n; tensuu = t; seiseki = "C" }
    else                 { namae = n; tensuu = t; seiseki = "D" }


(* 目的：学生リストlstを受け取り、成績を入れたリストを返す *)
(* map_hyouka : gakusei_t list -> gakusei_t list *)
let map_hyouka lst = map hyouka lst


(* 人物ひとり分を表すデータの型 *)

type person_t = {
  namae        : string;
  sintyou      : float;
  taijuu       : float;
  tanjoubi     : int * int;
  ketsuekigata : string;
}

(* データ例 *)
let hiroki = {
  namae = "hiroki";
  sintyou = 178.5;
  taijuu = 60.3;
  tanjoubi = (9, 8);
  ketsuekigata = "A";
}

let kaede = {
  namae = "kaede";
  sintyou = 160.4;
  taijuu = 48.8;
  tanjoubi = (1, 6);
  ketsuekigata = "A";
}

let tomoko = {
  namae = "tomoko";
  sintyou = 156.3;
  taijuu = 56.3;
  tanjoubi = (3, 4);
  ketsuekigata = "O";
}

let mayuko = {
  namae = "mayuko";
  sintyou = 167.3;
  taijuu = 56.3;
  tanjoubi = (4, 30);
  ketsuekigata = "AB";
}

let taro = {
  namae = "taro";
  sintyou = 180.3;
  taijuu = 50.0;
  tanjoubi = (3, 3);
  ketsuekigata = "O";
}

(* person_t型のデータを受け取ったらその中の名前を返す *)
(* namae_value : person_t -> string *)
let namae_value person = match person with
  {namae = n; sintyou = s; taijuu = t; tanjoubi = (m, d); ketsuekigata = k} -> n


(* テスト *)
let test1 = namae_value {namae = "hiro"; sintyou = 178.4; taijuu = 83.2; tanjoubi = (2, 4); ketsuekigata = "A"} = "hiro"
let test2 = namae_value hiroki = "hiroki"


(* 目的：person_t型のリストlstを受け取ったら、その中に出てくる人の名前のリストを返す *)
(* person_namae : person_t list -> string list *)
let person_namae lst = List.map namae_value lst
 
(* テスト *)
let test3 = person_namae [] = []
let test4 = person_namae [hiroki; taro] = ["hiroki"; "taro"]

(* 目的：受け取ったリストlstの長さを求める *)
(* length : 'a list -> int *)
let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest (* length rest *)




