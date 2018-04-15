(* 月日を表す型 *)
type year_t =  January of int
             | February of int
             | March of int
             | April of int
             | May of int
             | June of int
             | July of int
             | August of int
             | September of int
             | October of int
             | November of int
             | December of int

(* １２星座を表す型 *)
type seiza_t =  Mizugame
              | Uo
              | Ohitsuji
              | Oushi
              | Futago
              | Kani
              | Shishi
              | Otome
              | Tenbin
              | Sasori
              | Ite
              | Yagi


(* year_t型の値を受け取ったら、seiza_t型の星座を返す *)
(* seiza : year_t -> seiza_t *)
let seiza year = match year with
    January (n) -> if n <= 19 then Yagi else Mizugame
  | February (n) -> if n <= 18 then Mizugame else Uo
  | March (n) -> if n<= 20 then Uo else Ohitsuji
  | April (n) -> if n <= 19 then Ohitsuji else Oushi
  | May (n) -> if n <= 20 then Oushi else Futago
  | June (n) -> if n <= 21 then Futago else Kani
  | July (n) -> if n <= 22 then Kani else Shishi
  | August (n) -> if n <= 22 then Shishi else Otome
  | September (n) -> if n <= 22 then Otome else Tenbin
  | October (n) -> if n <= 23 then Tenbin else Sasori
  | November (n) -> if n <= 22 then Sasori else Ite
  | December (n) -> if n <= 21 then Ite else Yagi
 
(* テスト *)
let test = seiza (September (8)) = Otome





