(* tree.mli - Interface des Tree Moduls *)

(* t: Typ eines Baums, 'a ist der Typ der Labels *)
type 'a t

(* path: Typ eines Pfades im Baum *)
type path

(* paths: Liefert eine Liste aller Pfade im Baum *)
val paths: 'a t -> path list

(* subtree: Liefert den Unterbaum beginnend ab einem Pfad, wirft Not_found falls der Pfad ungueltig ist *)
val subtree: 'a t -> path -> 'a t

(* label: Liefert das Label der Wurzel *)
val label: 'a t -> 'a

(* create: Neuen Baum erzeugen mit dem label an der Wurzel und den gegebenen Kindern *)
val create: 'a -> ('a t) list -> 'a t
