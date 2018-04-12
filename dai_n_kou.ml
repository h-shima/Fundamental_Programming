(* 目的：級数の第n項の値を求める *)
(* dai_n_kou : int -> float *)
let rec dai_n_kou n =
  if n = 0 then 1.0
           else dai_n_kou ( n - 1 ) /. float_of_int n

(* 目的：自然対数eの近似値を求める *)
(* e : int -> float *)
let rec e n =
  let d = dai_n_kou n in
  if d < 0.00001 then d
                 else d +. e ( n + 1 )

