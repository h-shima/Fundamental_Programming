(* 目的：受け取った身長ｍと体重ｋからbmi指数を計算する *)
(* bmi : float -> float -> float *)
let bmi m k = k /.( m ** 2.)

(* テスト *)
let test1 = bmi 2. 60. = 15.
let test2 = bmi 1. 40. = 40.
let test3 = bmi 1. 25. = 25.

(* 目的：受け取った身長mと体重kから体型を判別する *)
(* float -> float -> string *)
let taikei m k = if bmi m k < 18. then "yase"
                                  else if 18. <= bmi m k && bmi m k < 25. then "hyoujun"
                                                                          else if 25. <= bmi m k && bmi m k < 30. then "himan"
                                                                                                                  else "koudohiman"

(* テスト *)
let test1 = taikei 2. 60. = "yase"
let test2 = taikei 1. 40. = "koudohiman"
let test3 = taikei 1. 25. = "himan"
