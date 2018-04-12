(* 目的：昇順に並んでいるリストlst1とlst2をマージ（結合）する *)
(* merge : int list -> int list -> int list *)
let rec merge lst1 lst2 = match (lst1, lst2) with
    ([], []) -> []
  | ([], first :: rest) -> lst2
  | (first :: rest, []) -> lst1
  | (first1 :: rest1, first2 :: rest2) ->
      if first1 < first2 then first1 :: merge rest1 lst2
                         else first2 :: merge lst1 rest2


(* テスト *)
let test1 = merge [] [] = []
let test2 = merge [] [1; 2] = [1; 2]
let test3 = merge [1; 2] [] = [1; 2]
let test4 = merge [1; 3][2; 4] = [1; 2; 3; 4]
let test5 = merge [1; 4][1; 3] = [1; 1; 3; 4]


