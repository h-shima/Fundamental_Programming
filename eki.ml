#use "eki_data.ml"

type ekikan_tree_t =   Empty (* 空の木 *)
                     | Node of ekikan_tree_t * string * ( string * float ) list * ekikan_tree_t (* 節 構造は木, 駅名、直
接つながっている駅名とその駅までの距離の組、のリスト、木 *)


(* 目的：「駅名」と「駅名と距離の組を要素として持つリスト」を受け取ったら、その駅までの距離を返す *)                                                                    (* assoc : string -> string * float list -> float *)
let rec assoc ekimei1 lst = match lst with
    [] -> infinity
  | (ekimei2, kyori) :: rest -> if ekimei1 = ekimei2 then kyori
                                                     else assoc ekimei1 rest


(* テスト *)
let test1 = assoc "後楽園" [] = infinity
let test2 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8
let test3 = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = infinity


(* 目的：受け取った駅名のデータから路線名、駅名（かな）の文字列を返す *)
(* hyoji : ekimei_t -> string *)
let hyoji eki = match eki with
  { kanji = k; kana = ka; romaji = r; shozoku = s }
    -> s ^ "、" ^ k ^ "（" ^ ka ^ "）"

(* テスト *)
let test1 = hyoji { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸ノ内線" }
            = "丸ノ内線、茗荷谷（みょうがだに）"

(* 駅名（漢字）、最短距離（実数）、駅名（漢字）のリストを表す型 *)

type eki_t = {
  namae        : string;
  saitan_kyori : float;
  temae_list   : string list;
}

(* 目的：ローマ字の駅名sと駅名リストlstを受け取り、駅名の漢字表記の文字列を返す *) 
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji s lst = match (s, lst) with
    ("", []) -> ""
  | (s, [])  -> ""
  | ("", {kanji = k; kana = ka; romaji = r; shozoku = sh}::rest) -> ""
  | (s,{kanji = k; kana = ka; romaji = r; shozoku = sh}::rest) -> 
      if r = s then k
               else romaji_to_kanji s rest (* romaji_to_kanji s rest *) 


(* テスト *)
let test2 = romaji_to_kanji "" [] = ""
let test3 = romaji_to_kanji "toyota" global_ekimei_list = ""
let test4 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let test5 = romaji_to_kanji "kasumigaseki" global_ekimei_list = "霞ヶ関"

(* 目的：受け取ったkiten,shuten,kyoriをekikan_treeに挿入した木を返す *)
(* insert1 : ekikan_tree_t -> string -> string -> float -> ekikan_tree_t *)
let rec insert1 ekikan_tree kiten shuten kyori = match ekikan_tree with
    Empty -> Node (Empty, kiten, [(shuten, kyori)], Empty)
  | Node (left, ekimei, lst, right) ->
      if kiten < ekimei
      then Node (insert1 left kiten shuten kyori, ekimei, lst, right)
      else if ekimei < kiten
      then Node (left, ekimei, lst, insert1 right kiten shuten kyori)
      else Node (left, ekimei, (shuten, kyori) :: lst, right)

(* 目的：受け取ったekikan情報をekikan_treeに挿入した木を返す *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let insert_ekikan ekikan_tree ekikan = match ekikan with
  {kiten = k; shuten = s; keiyu = y; kyori = r; jikan = j} ->
    insert1 (insert1 ekikan_tree s k r) k s r

(* 駅間の例 *)
let ekikan1 = {kiten = "池袋"; shuten = "新大塚"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 3}
let ekikan2 = {kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2}
let ekikan3 = {kiten = "茗荷谷"; shuten = "後楽園"; keiyu = "丸ノ内線"; kyori = 1.8; jikan = 2}

(* テスト *)
let tree1 = insert_ekikan Empty ekikan1
let test1 = tree1 =
  Node (Empty, "新大塚", [("池袋", 1.8)], Node (Empty, "池袋", [("新大塚", 1.8)], Empty))


(* 目的：ekikan_tree_t型の木とekikan_t list型の駅間のリストを受け取ったら、リストに含まれる駅間をすべて挿入した木を返す *)
(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan ekikan_tree ekikan_list =
  List.fold_right (fun ekikan tree -> insert_ekikan tree ekikan)
                                      ekikan_list ekikan_tree


(* 目的：漢字の駅名を２つとekikan_t型のレコードを要素に持つリストを受け取り、２駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori eki1 eki2 tree = match tree with
    Empty -> infinity
  | Node (left, k, lst, right) ->
      if eki1 < k then get_ekikan_kyori eki1 eki2 left
      else if k < eki1 then get_ekikan_kyori eki1 eki2 right
      else assoc eki2 lst
  
(* テスト *)
let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2
let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree = infinity
let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_tree = 0.6

(*
(* 目的：ローマ字の駅名を２つ受け取ったら距離を調べ、直接つながっている場合、つながっていない場合、そもそも入力されローマ字の駅名が存在しなかった場合それぞれに対応する文字列を返す *)
(* kyori_wo_hyoji : string -> string -> string *)

let kyori_wo_hyoji ekimei1 ekimei2 = match (ekimei1, ekimei2) with
    (ekimei1, ekimei2) -> let k_ekimei1 = romaji_to_kanji ekimei1 global_ekimei_list in
                          let k_ekimei2 = romaji_to_kanji ekimei2 global_ekimei_list in
                           let ekikan_kyori = get_ekikan_kyori k_ekimei1 k_ekimei2 global_ekikan_list in
                             if k_ekimei1 = "" && k_ekimei2 = "" then ekimei1 ^ "と" ^ ekimei2 ^ "という駅は存在しません"
                             else if k_ekimei1 = "" then ekimei1 ^ "という駅は存在しません"
                             else if k_ekimei2 = "" then ekimei2 ^ "という駅は存在しません"                            
                             else if ekikan_kyori = infinity then k_ekimei1 ^ "駅と" ^ k_ekimei2 ^ "駅はつながっていません" 
                             else k_ekimei1 ^ "駅から" ^ k_ekimei2 ^ "駅までは" ^ string_of_float ekikan_kyori ^ "kmです"


(* テスト *)
let test11 = kyori_wo_hyoji "toyota" "myogadani" = "toyotaという駅は存在しません"
let test12 = kyori_wo_hyoji "miyoshi" "josui" = "miyoshiとjosuiという駅は存在しません"
let test13 = kyori_wo_hyoji "myogadani" "shinjuku" = "茗荷谷駅と新宿駅はつながっていません"
let test14 = kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷駅から新大塚駅までは1.2kmです"
let test15 = kyori_wo_hyoji "tsukishima" "toyosu" = "月島駅から豊洲駅までは1.4kmです"
*)


(* 目的：ekimei_t型のリストと起点（漢字駅名）を受け取ったら、eki_t型のリストを作り、その際に起点を駅名に持つデータに関しては、saitan_kyoriに0.0を、temae_listにその駅名を  入れて返す *)
(* make_initial_eki_list : ekimei_t list -> string -> eki_t list *)
let make_initial_eki_list ekimei_list kiten = 
  List.map (fun ekimei -> match ekimei with
              {kanji = k; kana = a; romaji =r; shozoku =s} ->
                if k = kiten
                then {namae = k; saitan_kyori = 0.; temae_list = [k]}
                else {namae = k; saitan_kyori = infinity; temae_list = []})
            ekimei_list

(* make_eki_listとshokika make_initial_eki_listにより不要

(* make_eki_list : ekimei_t list -> eki_t list *)


let make_eki_list lst = List.map ( fun ekimei -> match ekimei with   
                                                  {kanji = k; kana = ka; romaji = r; shozoku = s} -> {namae = k; saitan_kyori = infinity; temae_list = []} ) lst


(*　無名関数とList.mapにより下記不要
let rec make_eki_list lst = match lst with
    [] -> []
|   {kanji = k; kana = ka; romaji = r; shozoku = s;}::rest ->
      {namae = k; saitan_kyori = infinity; temae_list = []}:: make_eki_list rest (* make_eki_list rest *)
*)




(* テスト *)
let test16 = make_eki_list [] = [] 
let test17 = make_eki_list [{kanji = "代々木上原"; kana = "よよぎうえはら"; romaji = "yoyogiuehara"; shozoku = "千代田線"}] 
             = [{namae = "代々木上原"; saitan_kyori = infinity; temae_list = []}]




(* 目的：eki_t型のデータを要素に持つリストと起点（漢字駅名）を受け取ったら、起点を漢字駅名として持つeki_t型のデータのみについては
　　　　saitan_kyoriが0.0、temae_listは起点の駅名のみからなるリストとなるeki_t型のリストを返す *)
(* shokika : eki_t list -> string -> eki_t list *)
let shokika lst kiten = List.map ( fun eki -> match eki with
                                               {namae = n; saitan_kyori = s; temae_list = []} -> 
                                               if n = kiten then {namae = n; saitan_kyori = 0.0; temae_list = n::[]}
                                                            else eki ) lst

(* mapとList.mapにより下記不要
let rec shokika lst kiten = match lst with
   [] -> []
|  ({namae = n; saitan_kyori = s; temae_list = []} as first )::rest -> 
     if n = kiten then {namae = n; saitan_kyori = 0.0; temae_list = [n]}:: shokika rest kiten (* shokika rest kiten *) 
                  else first :: shokika rest kiten (* shokika rest st *)
*)



(* eki_t型のデータを要素に持つリスト例 *)
let eki_list = [ 
  {namae="池袋"; saitan_kyori = infinity; temae_list = []}; 
  {namae="新大塚"; saitan_kyori = infinity; temae_list = []}; 
  {namae="茗荷谷"; saitan_kyori = infinity; temae_list = []}; 
  {namae="後楽園"; saitan_kyori = infinity; temae_list = []}; 
  {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []}; 
  {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []} 
] 

(* テスト *)
let test18 = shokika [] "茗荷谷" = []
let test19 = shokika eki_list "茗荷谷" =  [
  {namae="池袋"; saitan_kyori = infinity; temae_list = []};
  {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
  {namae="茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"]};
  {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
  {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
  {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}
]

ここまでmake_initial_eki_listに代用されるため不要  *)




(* koushin1はkoushin内で局所関数として定義したため、以下不要


(* 目的：直前に確定した駅p(eki_t型)と未確定の駅ｑ(eki_t型)を受け取り、pとqがつながっていたらqの最短距離と手前リストを必要に応じて更新したもの、
　　　　つながっていなかったらもとのqをそのまま返す *)
(* koushin1 : eki_t -> eki_t -> eki_t *)
let koushin1 p q = match p with
  {namae = pn; saitan_kyori = ps; temae_list = pt} -> 
    match q with {namae = qn; saitan_kyori = qs; temae_list = qt} ->
      let kyori = get_ekikan_kyori pn qn global_ekikan_list in
        if kyori = infinity then q
        else if kyori +. ps < qs then {namae = qn; saitan_kyori = kyori +. ps; temae_list = qn :: pt }
        else q
(* テスト *)
let test20 = koushin1 {namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"]} {namae = "新大塚"; saitan_kyori = infinity; temae_list = []}
             = {namae = "新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"] }

*)


(* 目的：直前に確定した駅p（eki_t型）と未確定の駅のリストv（eki_t list型）、駅間のリストlst(ekikan_t list)を受け取ったら、必要な更新処理を行った後の未確定の駅のリストを返す *)
(* koushin : eki_t -> eki_t list -> ekikan_t list -> eki_t list *)

let koushin p v ekikan_tree = match p with
  {namae = pn; saitan_kyori = ps; temae_list = pt} ->
    List.map (fun q -> match q with
                {namae = qn; saitan_kyori = qs; temae_list = qt} ->
                  let kyori = get_ekikan_kyori pn qn ekikan_tree in
                  if kyori = infinity
                  then q
                  else if ps +. kyori < qs
                  then {namae = qn; saitan_kyori = ps +. kyori; temae_list = qn :: pt}
                  else q )
              v


(* koushin1を無名関数としたkoushinを上に作成したため、以下不要
let koushin p v =
  let koushin1 p q = match p with
  {namae = pn; saitan_kyori = ps; temae_list = pt} ->
    match q with {namae = qn; saitan_kyori = qs; temae_list = qt} ->
      let kyori = get_ekikan_kyori pn qn global_ekikan_list in
        if kyori = infinity then q
        else if kyori +. ps < qs then {namae = qn; saitan_kyori = kyori +. ps; temae_list = qn :: pt }
        else q
  in let f q = koushin1 p q in List.map f v
*)


(* 目的： ひらがなの順に並んでいるekimei_t型のデータを要素に持つリストを受け取り、、ひらがな順になる位置にekimei_t型のデータを挿入する、駅名の重複は削除する *)
(* insert_ekimei : ekimei_t list -> ekimei_t -> ekimei_t list *)
let rec insert_ekimei lst ekimei = match lst with
    [] -> [ekimei]
  | ({kanji = k1; kana = n1; romaji = r1; shozoku = s1} as first) ::rest ->                   
      match ekimei with {kanji = k2; kana = n2; romaji = r2; shozoku = s2} ->
        if n1 = n2 then insert_ekimei rest ekimei
        else if n2 < n1 then ekimei :: lst
        else first :: insert_ekimei rest ekimei (* insert_ekimei rest ekimei *)


(* テスト*)
let test22 = insert_ekimei [{kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線"};
                            {kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "南北線"}] 
                            {kanji = "御茶ノ水"; kana = "おちゃのみず"; romaji = "otyanomizu"; shozoku = "丸ノ内線"} 
                               = [{kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線"};
                                  {kanji = "御茶ノ水"; kana = "おちゃのみず"; romaji = "otyanomizu"; shozoku = "丸ノ内線"};
                                  {kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "南北線"}]


(* 目的：受け取ったekimei_t型のデータを要素に持つリストをひらがな順に並べ替えたリストを返す *)
(* ins_sort_ekimei : ekimei_t list -> ekimei_t list *)
let rec ins_sort_ekimei lst = match lst with
    [] -> []
  | first::rest -> 
    insert_ekimei (ins_sort_ekimei rest) first



(* テスト *)
let test23 = ins_sort_ekimei [] = []
let test24 = ins_sort_ekimei [{kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線"}]
                                     =  [{kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線"}]
let test25 = ins_sort_ekimei [{kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線"};
                              {kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線"};
                              {kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "JR山手線"};
                              {kanji = "御茶ノ水"; kana = "おちゃのみず"; romaji = "otyanomizu"; shozoku = "丸ノ内線"}
                               ]
                                  = [{kanji = "池袋"; kana = "いけぶくろ"; romaji = "ikebukuro"; shozoku = "丸ノ内線"};
                                     {kanji = "御茶ノ水"; kana = "おちゃのみず"; romaji = "otyanomizu"; shozoku = "丸ノ内線"};
                                     {kanji = "後楽園"; kana = "こうらくえん"; romaji = "korakuen"; shozoku = "丸ノ内線"} 
                                      ]
                                   

(* 目的：ekimei_lstをひらがなの順に整列しながら駅の重複を取り除く *)
(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec seiretsu ekimei_lst = match ekimei_lst with
    [] -> []
  | first :: rest -> insert_ekimei (seiretsu rest) first






(* 自分で理解の上模範解答を記したので下記は不要

(* 目的：最短距離最小の駅を求める *)
(* minimum_kyori_eki : eki_t list -> eki_t *)
let rec minimum_kyori_eki lst = match lst with
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []}
  | {namae = n; saitan_kyori = s; temae_list = t} as first :: rest -> 
      let minimum_kyori_rest = minimum_kyori_eki rest in
        match minimum_kyori_rest with
          {namae = rn; saitan_kyori = rs; temae_list = rt} -> 
            if s <= rs then first
                       else minimum_kyori_rest


(* テスト *)
let test26 = minimum_kyori_eki  [
  {namae="池袋"; saitan_kyori = infinity; temae_list = []};
  {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
  {namae="茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"]};
  {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
  {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
  {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}]
   = {namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"]}


(* 目的：最短距離最小の駅ekiを未確定の駅のリストvから取り除いたものを返す *)
(* saitan_torinozoku : eki_t -> eki_t list -> eki_t list *)
let rec saitan_torinozoku eki v = match eki with
  {namae = n; saitan_kyori = s; temae_list = t} ->
    match v with 
        [] -> []
      | {namae = vn; saitan_kyori = vs; temae_list = vt} as first :: rest ->
          if n = vn then rest
                    else first :: saitan_torinozoku eki rest

(* テスト *)
let test27 = saitan_torinozoku {namae = "茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"]} [
  {namae="池袋"; saitan_kyori = infinity; temae_list = []};
  {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
  {namae="茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"]};
  {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
  {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
  {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}
]
    =  [
  {namae="池袋"; saitan_kyori = infinity; temae_list = []};
  {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
  {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
  {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
  {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}
]




(* 目的：eki_t list 型のリストvを受け取ったら、最短距離最小の駅と、最短距離最小の駅以外からなるリストの組を返す *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)

let rec saitan_wo_bunri v = let minimum_eki = minimum_kyori_eki v
                              in let saitan_torinozoku_list = saitan_torinozoku minimum_eki v
                                in (minimum_eki, saitan_torinozoku_list)

(* テスト *)
let test28 = saitan_wo_bunri [
  {namae="池袋"; saitan_kyori = infinity; temae_list = []};
  {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
  {namae="茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"]};
  {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
  {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
  {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}
] =
  ({namae="茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"]},
   [{namae="池袋"; saitan_kyori = infinity; temae_list = []};
    {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
    {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
    {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
    {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}] )  

*)

(* 目的：eki_t list 型のリストを受け取ったら、最短距離最小の駅と、最短距離最小の駅以外からなるリストの組を返す *)
(* saitan_wo_bunri : eki_t -> eki_t list -> eki_t * eki_t list *)
let rec saitan_wo_bunri eki eki_list = match eki_list with
    [] -> (eki, [])
  | first :: rest ->
      let (p, v) = saitan_wo_bunri first rest in
        match (eki, p) with 
          ({namae = fn; saitan_kyori = fs; temae_list = ft},
           {namae = sn; saitan_kyori = ss; temae_list = st}) ->
            if fs < ss then (eki, p ::v)
            else (p, eki :: v)




(* テスト *)

let test28 = saitan_wo_bunri 
  {namae="池袋"; saitan_kyori = infinity; temae_list = []}
[
  {namae="新大塚"; saitan_kyori = infinity; temae_list = []};
  {namae="茗荷谷"; saitan_kyori = 0.0; temae_list = ["茗荷谷"]};
  {namae="後楽園"; saitan_kyori = infinity; temae_list = []};
  {namae="本郷三丁目"; saitan_kyori = infinity; temae_list = []};
  {namae="御茶ノ水"; saitan_kyori = infinity; temae_list = []}
] 




(* 目的：未確定の駅のリストeki_lst(eki_t list型)と駅間のリスト(ekikan_t list型)を受け取ったら、各駅について最短距離と最短経路が入ったリスト(eki_t list型)を返す *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main eki_list ekikan_tree = match eki_list with
    [] -> []
  | first :: rest ->
      let (saitan, nokori) = saitan_wo_bunri first rest in
      let eki_list2 = koushin saitan nokori ekikan_tree in
      saitan :: dijkstra_main eki_list2 ekikan_tree

(* 目的：受け取ったeki_listからshutenのレコードを探し出す *)
(* find : string -> eki_t list -> eki_t *)
let rec find shuten eki_list = match eki_list with
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []}
  | ({namae = n; saitan_kyori = s; temae_list = t} as first) :: rest ->
      if n = shuten then first else find shuten rest



(* 目的：始点の駅名(ローマ字の文字列)と終点の駅名(ローマ字の文字列)を受け取ったら、最短距離と最短経路の入ったeki_t型の終点データを返す *)(* dijkstra : string -> string -> eki_t *)
let dijkstra romaji_kiten romaji_shuten =
  let kiten = romaji_to_kanji romaji_kiten global_ekimei_list in
  let shuten = romaji_to_kanji romaji_shuten global_ekimei_list in
  let eki_list = make_initial_eki_list global_ekimei_list kiten in
  let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list in
  let eki_list2 = dijkstra_main eki_list global_ekikan_tree in
  find shuten eki_list2

(* テスト *) 
let test1 = dijkstra "shibuya" "gokokuji" = 
  {namae = "護国寺"; saitan_kyori = 9.8; 
   temae_list = 
     ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; 
      "青山一丁目"; "表参道"; "渋谷"]} 
let test2 = dijkstra "myogadani" "meguro" = 
  {namae = "目黒"; saitan_kyori = 12.7000000000000028; 
   temae_list = 
     ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王"; 
      "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]} 
 
(* 最短距離が 12.7 にならないのは、小数を２進数で表現するときの誤差のため。 
   ここではテスト結果も書いたが、これをテスト作成時に予想するのは無理なので 
   テストとして書く意味はあまりない。*) 




