(* 紅組か白組かを返す型 *)
type team_t = Red | White

(* 目的：受け取ったチーム名を文字列で返す *)
(* team_string : team_t -> string *)
let team_string team = match team with
    Red -> "赤組"
  | White -> "白組"                         








