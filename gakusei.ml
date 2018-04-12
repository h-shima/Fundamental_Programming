(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae  : string; (* 名前 *)
  tensuu : int;    (* 点数 *)
  seiseki: string; (* 成績 *)
}

(* データ例 *)
let takeshi = {
  namae = "takeshi";
  tensuu = 78;
  seiseki = "B";
}

let tomoki = {
  namae = "tomoki";
  tensuu = 5;
  seiseki = "D";
}

let kaede = {
  namae = "kaede";
  tensuu = 89;
  seiseki = "A";
}

let hiroki = {
  namae = "hiroki";
  tensuu = 70;
  seiseki = "B";
}


(* あらかじめtensuuフィールドの降順に整列したリストlstを受け取ったら、lstを前から順に見ていき降順になる位置にデータgakuseiを挿入する *)
(* gakusei_insert : gakusei_t list -> gakusei_t list *)
let rec gakusei_insert lst gakusei = match lst with
    [] -> gakusei :: []
  | ( {namae = n; tensuu = t; seiseki = s} as first ) :: rest -> if t <= gakusei.tensuu then gakusei :: lst
                                                                 else first :: gakusei_insert rest gakusei (* gakusei_insert rest gakusei *)

(* テスト *)
let test1 = gakusei_insert [kaede; takeshi] tomoki = [kaede; takeshi; tomoki]
let test2 = gakusei_insert [] kaede = [kaede]
let test3 = gakusei_insert [kaede; takeshi; tomoki] hiroki = [kaede; takeshi; hiroki; tomoki]
let test4 = gakusei_insert [takeshi; hiroki] kaede = [kaede; takeshi; hiroki]



(* 目的：gakusei_t型のデータを要素に持つリストlstを受け取り、tensuuフィールドの降順に整列したリストを返す *)
(* gakusei_sort : gakusei_t list -> gakusei_t list *)
let rec gakusei_sort lst = match lst with
    [] -> []
  | first :: rest -> gakusei_insert ( gakusei_sort rest ) first (* gakusei_sort rest *)


(* テスト *)
let test5 = gakusei_sort [] = []
let test6 = gakusei_sort [takeshi] = [takeshi]
let test7 = gakusei_sort [takeshi; tomoki; hiroki] = [takeshi; hiroki; tomoki]
let test8 = gakusei_sort [kaede; tomoki; hiroki; takeshi] = [kaede; takeshi; hiroki; tomoki]

(* 目的：gakusei_t型のデータを要素に持つリストlstを受け取り、最高点を取った人のレコードを返す *)
(* gakusei_max : gakusei_t list -> gakusei_t *)
let rec gakusei_max lst = match lst with
    [] -> {namae = ""; tensuu = min_int; seiseki = ""}
  | {namae = n; tensuu = t; seiseki = s} as first :: rest -> 
      let max_rest = gakusei_max rest in
      if t > max_rest.tensuu then first
                             else max_rest

(* テスト *)
let test9 = gakusei_max [] = {namae = ""; tensuu = min_int; seiseki = ""}
let test10 = gakusei_max [hiroki] = hiroki
let test11 = gakusei_max [hiroki; kaede] = kaede
let test12 = gakusei_max [takeshi; tomoki; kaede; hiroki] = kaede

(* 目的：gakusei_t型のデータを要素に持つlstのうち各成績の人数を集計する *)
(* shukei : gakusei_t list -> int * int * int * int *)
let rec shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | { namae = n; tensuu = t; seiseki = s } :: rest -> 
      let (a, b, c, d) = shukei rest in
      if      s  = "A" then (a + 1, b, c, d)
      else if s  = "B" then (a, b + 1, c, d) 
      else if s  = "D" then (a, b, c + 1, d)
      else (a, b, c, d + 1)  (* shukei rest *)

(* テスト *)
let test13 = shukei [] = (0, 0, 0, 0)
let test14 = shukei [hiroki; kaede] = (1, 1, 0, 0)
let test15 = shukei [hiroki] = (0, 1, 0, 0)
let test16 = shukei [hiroki; kaede; tomoki; takeshi] = (1, 2, 1, 0)

