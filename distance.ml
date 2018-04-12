(* 距離と距離の合計を持っている型 *)
type distance_t = {
  kyori : float; (* 距離  *)
  total : float; (* 距離の合計 *)
}

(* 目的：先頭からリスト中の各点までの距離の合計を計算する *)
(* total_distance : distance_t list -> distance_t list *)
let total_distance lst = 
  (* 目的：先頭からリスト中の各点までの距離の合計を計算する *)
  (* ここでtotal0はこれまでの距離の合計 *)
  (* hojo : distance_t list -> float -> distance_t list *)
  let rec hojo lst total0 = match lst with
    [] -> []
  | {kyori = k; total = t} :: rest ->
      {kyori = k; total = total0 +. k}
      :: hojo rest (total0 +. k)
  in hojo lst 0.0






