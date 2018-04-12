(* 目的：受け取ったx座標とy座標からx軸について対称な点の座標を計算する *)
(* float * float -> float * float *)
let taisho_x pair = match pair with
  ( a , b ) -> ( a , -. b )

(* テスト *)
let test1 = taisho_x ( 1.0 , 2.0 ) = ( 1.0 , -. 2.0 )
let test2 = taisho_x ( 3.0 , 3.0 ) = ( 3.0 , -. 3.0 )
let test3 = taisho_x ( 0.0 , 4.5 ) = ( 0.0 , -. 4.5 )

(* 目的：x座標とy座標の組で表された平面座標を２つ受け取り、その中点を計算する *)
(* float * float -> float * float -> float * float *)
let chuten pair = match pair with
  (( a , b ) , ( c , d )) -> (( ( a +. c ) /. 2. ) ,( ( b +. d ) /. 2.) )

(* テスト *)
let test1 = chuten (( 1.0 , 2.0 ),( 3.0 , 4.0 )) = ( 2.0 , 3.0 )
let test2 = chuten (( 3.0 , 5.0 ),( 4.0 , 3.0 )) = ( 3.5 , 4.0 )
let test3 = chuten (( 6.0 , 3.5 ),( 2.0 , 5.0 )) = ( 4.0 , 4.25 )
