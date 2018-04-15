# Fundamental_Programming
書籍「プログラミングの基礎」の練習問題及びメトロネットワーク最短経路問題の解答

#### メトロネットワーク問題覚え書
メトロネットワーク最短経路問題の解答はeki.mlです。
Ocamlインタプリタ上で、
#use "eki.ml" ;;  (eki.mlとeki_data.mlの読み込み)
dijkstra <始点駅名（ローマ字）> <終点駅名（ローマ字）> ;;
と入力すると、始点と終点の駅名が漢字に変換され
終点の駅名、距離、最短経路が入ったレコードが返されます。

##### 例
dijkstra "ikebukuro" "komagome" ;;

{namae = "駒込"; saitan_kyori = 8.4;
temae_list =
  ["駒込"; "本駒込"; "東大前"; "後楽園"; "茗荷谷"; "新大塚";
   "池袋"]}
