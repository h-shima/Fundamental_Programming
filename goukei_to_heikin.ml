(* 目的：受け取った５教科の点数から合計点を計算する *)
(* float -> float -> float -> float -> float -> float *)
let goukei kokugo suugaku eigo rika syakai = kokugo +. suugaku +. eigo +. rika +. syakai

(* テスト  *)
let test1 = goukei 50. 50. 50. 30. 70. = 250.0
let test2 = goukei 30. 40. 50. 60. 70. = 250.0
let test3 = goukei 50. 70. 100. 50. 60. = 330.0

(* 目的：受け取った５教科の点数から平均点を計算する *)
(* float -> float -> float -> float -> float -> float *)
let heikin kokugo suugaku eigo rika syakai = goukei kokugo suugaku eigo rika syakai /. 5.

(* テスト *)
let test1 = heikin 50. 50. 50. 30. 70. = 50.0
let test2 = heikin 30. 40. 50. 60. 70. = 50.0
let test3 = heikin 50. 70. 100. 50. 60. = 66.0

(* 受け取った5教科の点数から合計点と平均点を計算し、組にして返す *)
(* float -> float -> float -> float -> float -> float * float *)
let goukei_to_heikin kokugo sansuu eigo rika syakai = ( goukei kokugo sansuu eigo rika syakai , heikin kokugo sansuu eigo rika syakai )

(* テスト *)
let test1 = goukei_to_heikin 50. 50. 50. 30. 70. = ( 250.0 , 50.0 )
let test2 = goukei_to_heikin 30. 40. 50. 60. 70. = ( 250.0 , 50.0 )
let test3 = goukei_to_heikin 50. 70. 100. 50. 60. = ( 330.0 , 66.0 )

