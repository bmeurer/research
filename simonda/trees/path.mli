(*** path.mli - Interface of the Path module ***)

(* The actual type of a path *)
type t

(* The root path *)
val root : t

(* Returns the path to the nth child of a given path *)
val child : t -> int -> t

(* Returns the parent path. Raise Invalid_argument if the path has no parent. *)
val parent : t -> t

(* Applies the path to a recursive data structure, represented by a goto function
   and a starting point. This is used for working with paths within the Tree
   module. *)
val follow : t -> ('a -> int -> 'a) -> 'a -> 'a
