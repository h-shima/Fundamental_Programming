(* 人物一人分のデータ（名前、身長、体重、誕生日、血液型）を表す型 *)
type person_t = {
  namae       : string;    (* 名前 *)
  sintyou     : float;     (* 身長（m）*)
  taijuu      : float;     (* 体重（kg）*)
  tanjoubi    : int * int; (* 誕生日（月、日）*)
  ketsuekigata: string;    (* 血液型 *)
}

(* データ例 *)
let hiroki = {
  namae = "hiroki";
  sintyou = 178.5;
  taijuu = 62.5;
  tanjoubi = (9, 8);
  ketsuekigata = "A";
}

let kaede = {
  namae = "kaede";
  sintyou = 160.0;
  taijuu = 51.5;
  tanjoubi = (1, 6);
  ketsuekigata = "A";
}

let takashi = {
  namae = "takashi";
  sintyou = 198.2;
  taijuu = 78.7;
  tanjoubi = (3, 5);
  ketsuekigata = "AB";
}

let momoko = {
  namae = "momoko";
  sintyou = 145.3;
  taijuu = 43.2;
  tanjoubi = (7, 31);
  ketsuekigata = "O";
}

(* 目的：名前の順に整列したperson_t型のデータを要素に持つリストlstを受け取ったら、前から順に調べて順序を崩さない位置にpersonを挿入する *)
(* person_insert : person_t list -> person_t -> person_t list *)
let rec person_insert lst person = match lst with
    [] -> person :: []
  | ( { namae = n; sintyou = s; taijuu = t; tanjoubi = (m, d) ; ketsuekigata = k } as first ) :: rest 
      -> if n >= person.namae then person :: ( first :: rest )
         else first :: person_insert rest person (* person_sort rest *)

(* テスト *)
let test1 = person_insert [] hiroki = [hiroki]
let test2 = person_insert [hiroki; momoko] kaede = [hiroki; kaede; momoko]
let test3 = person_insert [hiroki; kaede] momoko = [hiroki; kaede; momoko]
let test4 = person_insert [kaede; momoko] takashi = [kaede; momoko; takashi]

(* 目的：person_t型のデータを要素に持つリストlstを受け取ったら、それを名前の順に整列させたリストを返す *)
(* person_sort : person_t list -> person_t list *)
let rec person_sort lst = match lst with
    [] -> []
  | first :: rest -> person_insert ( person_sort rest ) first

(* テスト *)
let test5 = person_sort [] = []
let test6 = person_sort [hiroki; momoko] = [hiroki; momoko]
let test7 = person_sort [kaede; takashi; hiroki] = [hiroki; kaede; takashi]
let test8 = person_sort [momoko; takashi; hiroki; kaede] = [hiroki; kaede; momoko; takashi]




