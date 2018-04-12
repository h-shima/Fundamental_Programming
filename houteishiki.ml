(* 目的：受け取った二次方程式の係数a、b、cから判別式の値を計算する *)
(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = b ** 2. -.( 4. *. a *. c )

(* テスト *)
let test1 = hanbetsushiki 1. 2. 3. = -8.
let test2 = hanbetsushiki 2. 2. 2. = -12.
let test3 = hanbetsushiki 1. 5. 1. = 21.

(* 目的：受け取った二次方程式の係数a、b、cから解の個数を計算する *)
(* float -> float -> float -> int *)
let kai_no_kosuu a b c = if hanbetsushiki a b c > 0. then 2
                                                    else if hanbetsushiki a b c = 0. then 1
                                                                                    else 0

(* テスト *)
let test1 = kai_no_kosuu 1. 2. 1. = 1
let test2 = kai_no_kosuu 1. 2. 3. = 0
let test3 = kai_no_kosuu 1. 5. 1. = 2

(* 目的：受け取った二次方程式の係数a、b、cから二次方程式が虚数解を持つか判定する *)
(* kyosuukai : float -> float -> float -> bool *)
let kyosuukai a b c = if hanbetsushiki a b c < 0. then true
                                                  else false

(* テスト *)
let test1 = kyosuukai 1. 2. 1. = false
let test2 = kyosuukai 1. 5. 1. = false
let test3 = kyosuukai 1. 2. 3. = true
