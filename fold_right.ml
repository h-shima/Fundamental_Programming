(* 目的：initから始めてlstの要素を右から順にfに施しこむ *)
(* ( 'a -> 'b -> 'b ) -> 'a list -> b' -> b' *)
let rec fold_right f lst init = match lst with
    [] -> init
  | first :: rest -> f first ( fold_right f rest init ) (* fold_right f rest init *)


(*目的：firstとrest_resultを加える *)
(* add_int : int -> int -> int *)
let add_int first rest_result = first + rest_result

(* 目的：受け取ったリストlstの各要素の和を求める *)
(* int list -> int *)
let sum lst = fold_right add_int lst 0

(* テスト *)
let test1 = sum [] = 0
let test2 = sum [1; 2; 3; 4; 3; 10] = 23

(* 目的：firstは無視して、restに1を加える *)
(* add_one : int -> int -> int *)
let add_one first rest_result = 1 + rest_result

(* 目的：受け取ったリストlstの長さを求める *)
(* length : 'a list -> int *)
let length lst = fold_right add_one lst 0

(* テスト *)
let test3 = length [] = 0
let test4 = length ["hiro"; "masao"] = 2

(* 目的：firstをrest_resultの先頭に加える *)
(* cons : 'a -> 'a list -> 'a list *)
let cons first rest_result = first :: rest_result

(* 目的：lst1とlst2を受け取り、それらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 = fold_right cons lst1 lst2

(* テスト *)
let test5 = append [] [1; 2] = [1; 2]
let test6 = append ["hiro"; "taka"] ["usa"; "moe"] = ["hiro"; "taka"; "usa"; "moe"]

(* 目的：文字列firstとrest_resultをくっつけた文字列を返す *)
(* con : string -> string -> string *)
let con first rest_result = first ^ rest_result

(* 文字列のリストlstを受け取り、要素を前から順にくっつけた文字列を返す *)
(* concat : string list -> string *)
let concat lst = fold_right con lst ""

(* テスト *)
let test7 = concat [] = ""
let test8 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"

(* 学生一人分のデータ *)
type gakusei_t = {
  namae: string;
  tensuu: int;
  seiseki: string;
}

(* 学生リストlstのfirstとrest_resultを足し合わせる *)
(* add_gakusei : gakusei_t list -> int -> int *)
let add_gakusei first rest_result = match first with
  {namae = n; tensuu = t; seiseki = s} -> t + rest_result

(* gakusei_t型のリストを受け取ったら、全員の得点の合計を返す *)
let gakusei_sum lst = fold_right add_gakusei lst 0

(* テスト *)
let test9 = gakusei_sum [] = 0
let test10 = gakusei_sum [{namae = "hiro"; tensuu = 80; seiseki = "A"};
                          {namae = "tomo"; tensuu = 65; seiseki = "C"}] = 145


(* 目的：受け取ったリストlstを各要素の和を求める（局所関数定義を用いる）*)
(* sum : int list -> int *)
let sum lst = 
  (* 目的：firstとrest_resultを加える *)
  (* add_int : int -> int -> int *)
  let add_int first rest_result = first + rest_result 
  in fold_right add_int lst 0

(* テスト *)
let test11 = sum [] = 0
let test12 = sum [1; 2; 3; 4; 5] = 15

(* 目的：受け取ったリストlstの長さを求める（局所関数定義を用いる）*)
(* length : 'a list -> int *)
let length lst = 
  (* 目的：firstは無視してrest_resultに1を加える *)
  (* add_one : 'a -> int -> int *)
  let add_one first rest_result = 1 + rest_result
  in fold_right add_one lst 0

(* テスト *)
let test13 = length [] = 0
let test14 = length ["田中"; "山本"] = 2

(* 目的：lst1とlst2を受け取りそれらを結合したリストを返す (局所関数定義を用いる)  *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 =
  (* 目的：firstをリストrest_resultの先頭に加える *)
  (* cons : 'a -> 'a list -> 'a list *)
  let cons first rest_result = first :: rest_result
  in fold_right cons lst1 lst2

(* テスト *)
let test15 = append [] [1] = [1]
let test16 = append ["田中"; "元田"] ["安田"] = ["田中"; "元田"; "安田"]

(* 目的：文字列のリストlstを受け取り、要素を前から順にくっつけた文字列を返す（局所関数定義を用いる） *)
(* concat : string list -> string *)
let concat lst =
  (* 目的：firstとrest_resultをくっつける *)
  (* con : string -> string -> string *)
  let con first rest_result = first ^ rest_result
  in fold_right con lst ""

(* テスト *)
let test15 = concat [] = ""
let test16 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"

(* 目的：学生リストlstを受け取り、全員の得点の合計を返す *)
(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum lst =
  (* 目的：firstの得点とrest_resultを足し合わせる *)
  let sum first rest_result = match first with
    {namae = n; tensuu = t; seiseki = s} -> t + rest_result
  in fold_right sum lst 0

(* テスト *) 
let test17 = gakusei_sum [] = 0
let test18 = gakusei_sum [{namae = "hiro"; tensuu = 80; seiseki = "A"}; {namae = "tomo"; tensuu = 55; seiseki = "D"}]
             = 135

(* 学生データ例 *)
let hiroki = {namae = "hiroki"; tensuu = 80; seiseki = "A"}
let kaede = {namae = "kaede"; tensuu = 70; seiseki = "B"}
let masa = {namae = "masa"; tensuu = 50; seiseki = "D"}
let tomo = {namae = "tomo"; tensuu = 100; seiseki = "A"}
let naga = {namae = "naga"; tensuu = 0; seiseki = "D"}
let omochi = {namae = "omochi"; tensuu = 75; seiseki = "B"}
let kiyo = {namae = "kiyo"; tensuu = 90; seiseki = "A"}

(*　目的：受け取ったgakusei_t型のリストlstから成績がseiseki0の人の数を返す *)
(* count : gakusei_t list -> string -> int *)

let count lst seiseki0 =
  (* 目的：成績がseiseki0の人のリストを返す *)
  (* lst_result : gakusei_t list *)
  let lst_result = 
    (* 目的：成績がseiseki0か判定する *)
    (* is_seiseki0 : gakusei_t -> bool *)
    let is_seiseki0 m = match m with
      {namae = n; tensuu = t; seiseki = s} -> if s = seiseki0 then true
                                                              else false
    in List.filter is_seiseki0 lst
  in List.length lst_result
 
(* テスト *)
let test19 = count [] "A" = 0
let test20 = count [hiroki] "A" = 1
let test21 = count [hiroki; kiyo; omochi] "A" = 2
let test22 = count [hiroki; kaede; naga; tomo; kiyo; masa] "B" = 1

(* 以下、名前のない関数を使って各関数を再定義する *)

(* 目的：受け取ったリストlsの各要素の和を求める *)
(* sum : int list -> int *)
let sum lst =
  fold_right ( fun first rest_result -> first + rest_result ) lst 0

(* テスト *)
let test23 = sum [] = 0
let test24 = sum [5; 2; 3] = 10

(* 目的：受け取ったリストlstの長さを求める *)
(* length : 'a list -> int *)
let length lst = 
  fold_right ( fun first rest_result -> 1 + rest_result ) lst 0

(* テスト *)
let test25 = length [] = 0
let tste26 = length ["hiro"; "taka"] = 2

(* 目的：lst1とlst2を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 = 
  fold_right ( fun first rest_result -> first :: rest_result ) lst1 lst2 

(* テスト *)
let test27 = append [1; 2] [3; 4] = [1; 2; 3; 4]

(* 目的：整数を受け取ったら、その二乗から１を引いた数を返す無名関数 *)
(* int -> int *)
let sqrt = fun x -> x * x - 1

(* テスト *)
let test28 = sqrt 5 = 24



(* person_t 型のデータを受け取ったら、その名前フィールドを取り出す無名関数 *)
(* person_t -> string *)

(* person_t 型の定義 *)
type person_t = {
  namae: string; (* 名前 *)
  sintyo: float; (* 身長m *)
  taijuu: float;  (* 体重kg *)
  tanjoubi: int * int; (* 誕生日(月、日) *)
  ketsueki: string; (* 血液型 *)
}


let pick_namae = fun person -> match person with
                   {namae = n; sintyo = s; taijuu = t; tanjoubi = (m, d); ketsueki = k} -> n

(* テスト *)
let test29 = pick_namae {namae = "hiro"; sintyo = 170.3; taijuu =  60.3; tanjoubi = (9, 8); ketsueki = "A"}

(* 目的：整数のリストを受け取ったら、その中の偶数の要素のみを含むリストを返す無名関数 *)
(* int list -> int list *)
let even = fun lst -> List.filter ( fun x -> x mod 2 = 0 ) lst

(* テスト *)
let test30 = even [0; 1; 2] = [0; 2]

(* 目的：文字列のリストlstを受け取ったら、要素をすべて繋げた文字列を返す 無名関数を使って定義 *)
(* concat : string list -> string *)
let concat lst = fold_right ( fun first rest_result -> first ^ rest_result ) lst ""

(* テスト *)
let test31 = concat [] = ""
let test32 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"

(* 目的：gakusei_t型のリストlstを受け取ったら全員の得点の合計を返す関数　無名関数を使って定義 *)
(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum lst = fold_right ( fun first rest_result -> match first with {namae = n; tensuu = t; seiseki = s} -> t + rest_result ) lst 0

(* テスト *)
let test33 = gakusei_sum [] = 0
let test34 = gakusei_sum [hiroki; kaede; masa] = 200

(* 目的：受け取ったリストlstの各要素の和を求める prefix関数で一行で定義する *)
(* sum : int list -> int *)
let sum lst = fold_right ( + ) lst 0

(* テスト *)
let test35 = sum [] = 0
let test36 = sum [3; 5; 1] = 9

(* 目的：文字列のリストlstを受け取り、各要素を結合した文字列を返す prefix関数で一行で定義する *)
(* concat : string list -> string *)
let concat lst = fold_right ( ^ ) lst ""

(* テスト *)
let test37 = concat [] = ""
let test38 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"

(* 目的：nから1までのリストを作る *)
(* enumerate : int -> int list *)
let rec enumerate n =
  if n = 0 then []
           else n :: enumerate ( n - 1 ) (* enumerate (n - 1) *)

(* テスト *)
let test39 = enumerate 0 = []
let test40 = enumerate 4 = [4; 3; 2; 1]

(* 目的：nの約数のリストを返す *)
(* divisor : int -> int list *)
let divisor n = List.filter ( fun x -> n mod x = 0 ) ( enumerate n )

(* テスト *)
let test41 = divisor 6 = [6; 3; 2; 1]

(* 目的：m以下の完全数のリストを返す *)
(* perfect : int list -> int list *)
let perfect m =
  List.filter ( fun n -> fold_right ( + ) ( divisor n ) 0 - n = n ) ( enumerate m )

(* テスト *)
let test42 = perfect 10000 = [8128; 496; 28; 6]

(* 1から受け取った自然数nまでの合計を返す *)
(* one_to_n : int -> int *)
let one_to_n n = fold_right ( + ) ( enumerate n ) 0

(* テスト *)
let test43 = one_to_n 3 = 6
let test44 = one_to_n 5 = 15

(* 受け取った整数nの階乗を返す *)
(* fac : int -> int *)
let fac n = fold_right ( * ) ( enumerate n ) 1

(* テスト *)
let test45 = fac 0 = 1
let test46 = fac 5 = 120




