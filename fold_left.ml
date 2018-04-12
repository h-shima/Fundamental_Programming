(* 目的：initからはじめて、リストlstの要素を左から順に関数fを施しこむ *)
(* fold_left : ('a -> 'b -> 'b ) -> 'b -> 'a list -> 'b *)
let rec fold_left f init lst = match lst with
    [] -> init
  | first :: rest -> fold_left f ( f init first ) rest
  
(* テスト *)
let test1 = fold_left (-) 0 [] = 0
let test2 = fold_left (-) 10 [4; 1; 3] = 2
let test3 = fold_left (fun lst a -> a :: lst) [] [1; 2; 3; 4] = [4; 3; 2; 1]

































