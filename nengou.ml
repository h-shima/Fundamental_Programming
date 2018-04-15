(* 年号を表す型 *)
type nengou_t = Meiji of int (* 明治 *)
              | Taisho of int  (* 大正 *)
              | Syowa of int  (* 昭和 *)
              | Heisei of int  (* 平成 *)

(* 目的：年号を受け取ったら対応する西暦年を返す *)
(* to_seireki : nengou_t -> int *)
let to_seireki nengou = match nengou with
    Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Syowa (n) -> n + 1925
  | Heisei (n) -> n + 1988

(* テスト *)
let test1 = to_seireki (Heisei(30)) = 2018 

(* 目的：誕生年(nengou_t)と現在の年(nengou_t)を受け取ったら、年齢を返す *)
(* nenrei : nengou_t -> nengou_t -> int *)
let nenrei tanjou genzai = let tanjou_seireki = to_seireki tanjou
                           in let genzai_seireki = to_seireki genzai
                              in genzai_seireki - tanjou_seireki



















