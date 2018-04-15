(* 多相の木を表す型 *)

type 'a tree_t =  Empty (* 空の木 *)
               | Leaf of 'a (* 葉 *)
               | Node of 'a tree_t * 'a * 'a tree_t (* 節 *)

(* treeは
    - Empty                空の木、あるいは
    - Leaf (n) 　　　　　　値がnの葉、あるいは
    - Node (t1, n, t2)   　左の木がt1、値がn、右の木がt2であるような節
　　　　　　　　　　　　　 (t1とt2が自己参照のケース)
という形 *)

(* 木の例 *)
let tree1 = Empty
let tree2 = Leaf (3)
let tree3 = Node (tree1, 4, tree2)
let tree4 = Node (tree2, 5, tree3)

(* 目的：treeに含まれる整数を全て加える *)
(* sum_tree : tree_t -> int *)
let rec sum_tree tree = match tree with
    Empty -> 0
  | Leaf (n) -> n
  | Node (t1, n, t2) -> sum_tree t1 + n + sum_tree t2 (* sum_tree t1 *) (* sum_tree t2 *)

(* 0と+を使っているため、受け取る型は'a treeではなくint treeである *)

(* テスト *)
let test1 = sum_tree tree1 = 0
let test2 = sum_tree tree2 = 3
let test3 = sum_tree tree3 = 7
let test4 = sum_tree tree4 = 15

(* 目的：treeの葉や節に入っている値をすべて2倍にした木を返す *)
(* tree_double : tree_t -> tree_t *)
let rec tree_double tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (n * 2)
  | Node (t1, n, t2) -> Node (tree_double t1, n * 2, tree_double t2) (* tree_double t1 *) (* tree_double t2 *)


(* テスト *)
let test5 = tree_double tree1 = Empty
let test6 = tree_double tree2 = Leaf (6)
let test7 = tree_double tree3 = Node (Empty, 8, Leaf(6))
let test8 = tree_double tree4 = Node (Leaf(6), 10, Node(Empty, 8, Leaf(6)))

(* 目的： (int -> int)型の関数fとtree_t型の木を受け取ったら、節や葉に入っている値すべてにfを適用した木を返す *)
(* tree_map : (int -> int) -> tree_t -> tree_t *)
let rec tree_map f tree = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf (f n)
  | Node (t1, n, t2) -> Node (tree_map f t1, f n, tree_map f t2) (* tree_map f t1 *) (* tree_map f t2 *)



(* テスト *)
let test9 = tree_map (fun x -> x * 2) tree1 = Empty
let test10 = tree_map (fun x -> x * 2) tree2 = Leaf (6)
let test11 = tree_map (fun x -> x * 2) tree3 = Node (Empty, 8, Leaf (6))
let test12 = tree_map (fun x -> x * 2) tree4 = Node (Leaf (6), 10, Node (Empty, 8, Leaf (6)))

(* 目的：tree_t型の木を受け取ったら、節と葉の合計がいくつあるかを返す *)
(* tree_length : tree_t -> int *)
let rec tree_length tree = match tree with
    Empty -> 0
  | Leaf (n) -> 1
  | Node (t1, n, t2) -> tree_length t1 + 1 + tree_length t2

(* テスト *)
let test13 = tree_length tree1 = 0
let test14 = tree_length tree2 = 1
let test15 = tree_length tree3 = 2
let test16 = tree_length tree4 = 4

(* 目的：tree_t型の木を受け取ったら、節の数の合計を返す *)
(* tree_depth : tree_t -> int *)
let rec tree_depth tree = match tree with
    Empty -> 0
  | Leaf (n) -> 0
  | Node (t1, n, t2) -> 1 + max (tree_depth t1) ( tree_depth t2) (* tree_depth t1 *) (* tree_depth t2 *)


(* テスト *)
let test17 = tree_depth tree1 = 0
let test18 = tree_depth tree2 = 0
let test19 = tree_depth tree3 = 1
let test20 = tree_depth tree4 = 2

(* 目的：dataが2分探索木treeに含まれているかを調べる *)
(* search : tree_t -> int -> bool *)
let rec search tree data = match tree with
    Empty -> false
  | Leaf (n) -> if n = data then true else false
  | Node (t1, n, t2) -> if n = data then true
                                    else if data < n then search t1 data
                                                     else search t2 data
                                                    (* search t1 data *) (* search t2 data *)

(* 2分探索木の例 *)
let tree1 = Empty
let tree2 = Leaf (3)
let tree3 = Node (Leaf (1), 2, Leaf (3))
let tree4 = Node (Empty, 7, Leaf (9))
let tree5 = Node (tree3, 6, tree4)

(* テスト *)
let test1 = search tree1 3 =  false
let test2 = search tree2 3 = true
let test3 = search tree2 4 = false
let test4 = search tree5 6 = true
let test5 = search tree5 2 = true
let test6 = search tree5 1 = true
let test7 = search tree5 4 = false
let test8 = search tree5 7 = true
let test9 = search tree5 8 = false


(* 目的：2分探索木treeにdataを追加した2分探索木を返す *)
(* insert_tree : tree_t -> int -> tree_t *)
let rec insert_tree tree data = match tree with
    Empty -> Leaf (data)
  | Leaf (n) -> if n = data then Leaf (n)
                            else if data < n then Node ( Leaf (data), n, Empty)
                                             else Node ( Empty, n, Leaf (data))
  | Node (t1, n, t2) -> if n = data then Node (t1, n, t2)
                                    else if data < n then Node ( insert_tree t1 data, n, t2 ) (* insert_tree t1 data *)
                                                     else Node ( t1, n, insert_tree t2 data ) (* insert_tree t2 data *)



(* テスト *)
let test1 = insert_tree Empty 3 = Leaf (3)
let test2 = insert_tree ( Leaf (3)) 2 = Node ( Leaf (2), 3, Empty )
let test3 = insert_tree ( Leaf (3)) 3 = Leaf (3)
let test4 = insert_tree ( Leaf (3)) 4 = Node ( Empty, 3, Leaf (4))
let test5 = insert_tree tree5 4 = Node (Node (Leaf (1), 2, Node (Empty, 3, Leaf (4))),
                                  6,
                                  Node (Empty, 7, Leaf (9)))


















