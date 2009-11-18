type ('a, 'b) union =
    A of ('a)
  | B of ('b);;

type ('a, 'b) dependency =
    'a -> 'b list -> ('a, 'b) union;;

let recursion_by (g: ('a, 'b) dependency): 'a -> 'b =
  let rec f (x: 'a): 'b =
    let rec f' (a: 'a) (w: 'b list): 'b =
      match g x w with
          A a' -> let b' = f a' in f' x (b' :: w)
        | B b -> b
    in f' x []
  in f;;


let g_fact (a:int) (w:int list): (int, int) union =
  match (a, w) with
      (0, _) -> B 1
    | (x, []) -> A (x - 1)
    | (x, [y]) -> B (x * y);;

let fact = recursion_by g_fact in fact 4;;

