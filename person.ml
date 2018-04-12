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


(* 目的：人物のデータpersonを受け取り血液型の入った文字列を返す *)
(* ketsueki_hyoji : person_t -> string *)
let ketsueki_hyouji person = match person with
  { namae = n; sintyou = s; taijuu = t; tanjoubi = a; ketsuekigata = k }
    -> n ^ "さんの血液型は" ^ k ^ "型です"


let test1 = ketsueki_hyouji { namae = "kenta"; sintyou = 170.0; taijuu = 63.0; tanjoubi = ( 1 , 13 ); ketsuekigata = "A"; }
  = "kentaさんの血液型はA型です"
let test2 = ketsueki_hyouji {
  namae        = "hiroki" ;
  sintyou      = 178.5 ;
  taijuu       = 61.5 ;
  tanjoubi     = ( 9 , 8 ) ;
  ketsuekigata = "A" ;
}
  = "hirokiさんの血液型はA型です" 

let test3 = ketsueki_hyouji {
  namae        = "kaede" ;
  sintyou      = 161.3 ;
  taijuu       = 50.0 ;
  tanjoubi     = ( 1 , 6 ) ;
  ketsuekigata = "B" ;
}
  = "kaedeさんの血液型はB型です"


(* 目的：person_t型のデータを要素に持つリストlstから各血液型の人の人数を組にして返す *)
(* ketsueki_shukei : person_t list -> int * int * int * int *)
let rec ketsueki_shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {namae = n; sintyou = s; taijuu = t; tanjoubi = (m, d); ketsuekigata = k} :: rest -> 
      let (a, b, c, d) = ketsueki_shukei rest in
      if k = "A" then (a + 1, b, c, d)
      else if k = "B" then (a, b + 1, c, d)
      else if k = "O" then (a, b, c + 1, d)
      else (a, b, c, d + 1)      (* ketsueki_shukei rest *)


(* テスト *)
let test4 = ketsueki_shukei [] = (0, 0, 0, 0)
let test5 = ketsueki_shukei [hiroki] = (1, 0, 0, 0)
let test6 = ketsueki_shukei [tomoko; mayuko; kaede] = (1, 0, 1, 1)
let test7 = ketsueki_shukei [hiroki; mayuko; kaede; tomoko] = (2, 0, 1, 1)

(* 整数を要素に持つリストから最も数の大きいものを返す *)
(* maximum : int list -> int *)
let rec maximum lst = match lst with
    [] -> min_int
  | first :: rest -> if first >=  maximum rest then first
                                               else maximum rest  (* maximum rest *)
(* テスト *)
let test8 = maximum [] = min_int
let test9 = maximum [23; 12; -2] = 23
let test10 = maximum [1; 0; 23; 33] = 33
let test11 = maximum [23; 3; 44; 2] = 44



(* 目的：person_t型のデータを要素に持つリストlstから最も人数の多い血液型を返す *)
(* saita_ketsueki : person_t list -> string *)
let saita_ketsueki lst = match lst with
    [] -> ""
  | first :: rest -> 
    let (a, b, c, d) =  ketsueki_shukei(first :: rest) in
     let saidai_ninzuu =  maximum [a; b; c; d] in
       if saidai_ninzuu = a then "A"
       else if saidai_ninzuu = b then "B"
       else if saidai_ninzuu = c then "O"
       else "AB"

  (* saita_ketsueki rest *)
      
(* テスト *)
let test12 = saita_ketsueki [] = ""
let test13 = saita_ketsueki [hiroki; kaede] = "A"
let test15 = saita_ketsueki [tomoko] = "O"
let test16 = saita_ketsueki [taro; tomoko; hiroki] = "O"

(* 別解 *)
let saita_ketsueki2 lst = 
  let (a, b, o, ab) = ketsueki_shukei lst in
    let saidai = max (max a b)(max o ab) in
      if saidai = a then "A"    
      else if saidai = b then "B"
      else if saidai = o then "O"
      else "AB"

(* テスト let test = saita_ketsueki2 [] = ""は、saita_ketsueki2にパターンマッチが含まれていないため、捕捉できずにテストはfalseになる *)
let test17 = saita_ketsueki2 [hiroki; kaede] = "A"
let test18 = saita_ketsueki2 [tomoko] = "O"
let test19 = saita_ketsueki2 [taro; tomoko; hiroki] = "O"


