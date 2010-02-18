(*** tree.ml - Implementation of the Tree module ***)

type 'a t = Node of 'a * 'a t list

let nth (Node (_, tl):'a t) (n:int): 'a t =
  List.nth tl n

let subtrees (tree:'a t) (p:Path.t): 'a t list =
  let Node (_, tl) = Path.follow p nth tree in tl

let label (tree:'a t) (p:Path.t): 'a =
  let Node (lbl, _) = Path.follow p nth tree in lbl

let create (lbl:'a) (tl:'a t list): 'a t =
  Node (lbl, tl)
