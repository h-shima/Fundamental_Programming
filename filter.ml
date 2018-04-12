(* 目的 : 受け取ったリストlstから正の要素のみを取り出す *)
(* filter_positive : int list -> int list *)
let rec filter_positive lst = match lst with
    [] -> []
  | first :: rest ->
      if first > 0 then first :: filter_positive rest (* filter_positive rest *)
                   else filter_positive rest (* filter_positive rest *)   
(* テスト *)
let test1 = filter_positive [-2; -1; 0; 1; 2] = [1; 2]

(* 目的 : 整数nが3で割ると1余るかを調べる *)
(* filter_mod3_1 : int -> bool *)
let rec is_mod3_1 n = n mod 3 = 1

(* テスト *)
let test2 = is_mod3_1 4 = true

(* 目的 : リストlstから3で割ると1余る要素のみ取り出す *)
(* filter_mod3_1 : int list -> int list *)
let rec filter_mod3_1 lst = match lst with
    [] -> []
  | first :: rest -> if is_mod3_1 first then first :: filter_mod3_1 rest (* filter_mod3_1 rest *)
                                        else filter_mod3_1 rest (* filter_mod3_1 rest *)

(* テスト *)
let test3 = filter_mod3_1 [1; 2; 3; 4; 5] = [1; 4]

(* 目的 : リストlstの中から条件pを満たす要素のみを取り出す *)
(* ( 'a -> bool ) -> 'a list -> 'a list *)
let rec filter p lst = match lst with
    [] -> []
  | first :: rest -> if p first then first :: filter p rest (* filter p rest *)
                                else filter p rest (* filter p rest *)

(* 目的 : リストlstから3で割ると1余る要素のみを取り出す ※関数filterを使って定義 *)
(* filter_mod3_1 : int list -> int list *)
let filter_mod3_1 lst = filter is_mod3_1 lst

(* テスト *)
let test4 = filter_mod3_1 [1; 2; 3; 4; 5] = [1; 4]

(* 目的：整数nが正かどうか調べる *)
(* is_positive : int -> bool *)
let is_positive n = n > 0

(* テスト *)
let test5 = is_positive 3 = true

(* 受け取ったリストlstから正の要素のみを取り出す ※関数filerを使って定義 *)
(* filter_positive : int list -> int list *)
let filter_positive lst = filter is_positive lst

(* テスト *) 
let test6 = filter_positive [-1; 3; -3; 2] = [3; 2]

(* 受けとった整数nが偶数かどうか判別する *)
(* even : int -> bool *)
let even n = n mod 2 = 0

(* テスト *)
let test7 = even 0 = true
let test8 = even 1 = false
let test9 = even 2 = true

(* 目的：整数のリストlstを受け取ったら、偶数の要素のみを取り出してリストで返す *)
(*  filter_even  : int list -> int list *)
let filter_even lst = filter even lst 

(* テスト *)
let test10 = filter_even [-2; -1; 0; 1; 2] = [-2; 0; 2]
let test11 = filter_even [2; 1; 6; 4; 7] = [2; 6; 4]




(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae: string; (* 名前 *)
  tensuu: int;   (* 点数 *)
  seiseki: string; (* 成績 *)
}


(* 受け取った学生データの成績がAかどうか判別する *)
(* is_a : gakusei_t -> bool *)
let is_a g = match g with
  {namae = n; tensuu = t; seiseki = s} -> 
      if s = "A" then true
                 else false    


(* テスト *)
let test12 = is_a {namae = "hiro"; tensuu = 87; seiseki = "A"} = true
let test13 = is_a {namae = "tomo"; tensuu = 70; seiseki = "B"} = false





(* 目的：学生リストlstのうち成績がAの人の数を返す *)
(* count_A : gakusei_t list -> int *)
let count_A lst = let a_list = filter is_a lst in 
                               List.length a_list

(* 学生データ例 *)
let hiroki = { namae = "hiroki"; tensuu = 90; seiseki = "A"}
let kaede = { namae = "kaede"; tensuu = 100; seiseki = "A" }
let masa = {namae = "masa"; tensuu = 70; seiseki = "B"}
let nobu = {namae = "nobu"; tensuu = 50; seiseki = "D"}

(* テスト *)
let test14 = count_A [hiroki; kaede] = 2
let test15 = count_A [masa; nobu] = 0
let test16 = count_A [masa; hiroki; nobu; kaede] = 2





