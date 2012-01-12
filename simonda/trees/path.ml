(*** path.ml - Implementation of the Path module ***)

type t = int list

let root = []

let child (p:t) (n:int): t =
  if n < 0 then raise (Invalid_argument "Path.child")
  else n :: p

let parent (p:t): t =
  match p with
      [] -> raise (Invalid_argument "Path.parent")
    | _ :: p -> p

let follow (p:t) (f:'a -> int -> 'a) (s:'a): 'a =
  let rec follow_helper p s =
    match p with
        [] -> s
      | n :: p -> f (follow_helper p s) n
  in follow_helper p s
