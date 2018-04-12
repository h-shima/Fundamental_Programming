(* 1 *)
let q1 x = x

(* 2 *)
let q2 a b = a

(* 3 *)
let q3 a b = b

(* 4 *)
let q4 x f = f x

(* 5 *)
let q5 f g x = g ( f x )

(* 目的：関数をふたつ受け取ったら、そのふたつの関数を合成した関数を返す関数を作る *)
(* compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
let compose f g =
  let h x = f ( g x )
  in h

(* テスト *)
let times2 x = x * 2
let add3 x = x + 3

let test =( compose times2 add3 ) 4 = 14

(* 目的：その関数を2回適用するような関数を作る *)
(* twice : ('a -> 'a) -> 'a -> 'a *)
let twice f =
  let g x = f ( f x )
  in g








