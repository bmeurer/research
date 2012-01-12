(* tree.ml - Implementation des Tree Moduls *)

type 'a t = Node of 'a * ('a t) list;;

type path = int list;;

let rec subtree (t:'a t) (p:path): 'a t =
  let rec nth_subtree (n:int) (tl:('a t) list): 'a t =
    match n, tl with
        0, t :: _ -> t
      | n, _ :: tl -> nth_subtree (n - 1) tl
      | _ -> raise Not_found
  in match p with
      [] -> t
    | o :: p -> let Node (_, tl) = subtree t p in nth_subtree o tl
;;

let paths (t:'a t): path list =
  let rec paths_of_subtree_list (tl:('a t) list) (o:int) (p:path) (pl:path list): path list =
    match tl with
        [] -> pl
      | t :: tl -> paths_of_subtree_list tl (o + 1) p (paths_of_subtree t (o :: p) pl)
  and paths_of_subtree (t:'a t) (p:path) (pl:path list): path list =
    let Node (_, tl) = t in
      paths_of_subtree_list tl 0 p (p :: pl)
  in paths_of_subtree t [] []
;;

let label (t:'a t): 'a =
  let Node (a, _) = t in a
;;

let create (a:'a) (tl:('a t) list): 'a t =
  Node (a, tl)
;;
