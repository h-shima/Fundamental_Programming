(* 受け取ったリストlstをクイックソートを使って昇順に整列する *)
(* quick_sort : int list -> int list *)
let rec quick_sort lst =
  (* 目的：リストlstからnよりpである要素のみ取り出す *)
  (* take : int -> int list -> ( int -> int -> bool ) -> int list *)
  let take n lst p = List.filter ( fun item -> p item n ) lst
  (* 目的：リストlstからnより小さい要素のみを取り出す *)
  (* take_less : int -> int list -> int list *)
  in let take_less n lst = take n lst ( <= )
  (* 目的：リストlstからnより大きい要素のみを取り出す *)
  (* take_greater : int -> int list -> int list *)
  in let take_greater n lst = take n lst ( > )
  in match lst with
    [] -> []
  | first :: rest -> quick_sort ( take_less first rest )
                     @ [first]
                     @ quick_sort ( take_greater first rest )


(* 2以上n以下の自然数のリストlstを受け取ったら、要素が素数のもののみをリストにして返す *)
(* sieve : int list -> int list *)
let rec sieve lst = match lst with
    [] -> []
  | first :: rest -> first ::  sieve(  List.filter ( fun x -> not(x mod first = 0) ) rest )

(* 再帰呼び出しするごとにrestの要素は減っていくことから、最後は空リストになり停止することがわかる *)

(* テスト *)
let test1 = sieve [] = []
let test2 = sieve [2] = [2]
let test3 = sieve [2; 3; 4; 5; 6; 7; 8; 9; 10] = [2; 3; 5; 7]


(* 自然数nを受け取ったら、それ以下の素数のリストを返す *)
(* prime : int -> int list *)
let prime n = let rec enumerate x = if x = 1 then []
                                             else x :: enumerate (x - 1)
              in sieve ( quick_sort (enumerate n) )

(* テスト *)
let test4 = prime 2 = [2]
let test5 = prime 10 = [2; 3; 5; 7]











