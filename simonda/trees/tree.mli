(*** tree.mli - Interface of the Tree module ***)

(* The actual tree type, where 'a is the type of the labels *)
type 'a t

(* Returns the root label of the subtree starting at the specified path.
   Raise Invalid_argument if the path is not valid for this tree. *)
val label : 'a t -> Path.t -> 'a

(* Returns the list of subtrees below the node starting at the specified
   path. Raise Invalid_argument if the path is not valid for this tree. *)
val subtrees : 'a t -> Path.t -> 'a t list

(* Create a new tree using the label and the given list of subtrees. *)
val create : 'a -> 'a t list -> 'a t
