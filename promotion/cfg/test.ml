type ('t, 'nt) symbol =
    Terminal of 't
  | NonTerminal of 'nt
and ('t, 'nt) sentence =
    ('t, 'nt) symbol list
and ('t, 'nt) grammar =
    { terminals: 't list;
      nonterminals: 'nt list;
      productions: ('nt * ('t, 'nt) sentence) list }
;;

let rec is_valid_terminal (g:('t, 'nt) grammar) (t:'t): bool =
  List.mem t g.terminals
and is_valid_nonterminal (g:('t, 'nt) grammar) (nt:'nt): bool =
  List.mem nt g.nonterminals
and is_valid_symbol (g:('t, 'nt) grammar) (s:('t, 'nt) symbol): bool =
  match s with
      Terminal t -> is_valid_terminal g t
    | NonTerminal nt -> is_valid_nonterminal g nt
;;

type ('t, 'nt) tree =
    Node of ('nt) * ('t, 'nt) tree list
  | Leaf of ('t)
;;

type ('t, 'nt) pattern =
    Pat_symbol of ('t, 'nt) symbol
  | Pat_any
  | Pat_var of string
  | Pat_production of ('t, 'nt) pattern list
;;    

let rec is_valid_pattern (g:('t, 'nt) grammar) (s:('t, 'nt) symbol) (p:('t, 'nt) pattern): bool =
  match (s,p) with
      _, Pat_symbol s' -> s = s'
    | _, Pat_any -> true
    | _, Pat_var _ -> true
    | (NonTerminal nt, Pat_production pl) ->
        let rec is_valid_pattern_list (sl:('t, 'nt) symbol list) (pl:('t, 'nt) pattern list): bool =
          match sl, pl with
              [], [] -> true
            | s :: sl, p :: pl -> (is_valid_pattern g s p) && (is_valid_pattern_list sl pl)
            | _ -> false
        and is_valid_pattern_for_productions (productions:(('nt * ('t, 'nt) symbol list) list)): bool =
          match productions with
              [] -> false
            | (nt', sl) :: productions ->
                ((nt' == nt) && (is_valid_pattern_list sl pl))
                || (is_valid_pattern_for_productions productions)
        in (pl <> []) && (is_valid_pattern_for_productions g.productions)
    | _ -> false
;;

type ('t, 'nt) substitution =
    (string * ('t, 'nt) tree) list
;;

exception Unknown_variable of string;;
exception No_match;;

let rec subst_lookup (s:('t, 'nt) substitution) (x:string): ('t, 'nt) tree =
  match s with
      [] -> raise (Unknown_variable x)
    | (x', t') :: s' -> if x = x' then t' else subst_lookup s' x
and subst_extend (s:('t, 'nt) substitution) (x:string) (t:('t, 'nt) tree): ('t, 'nt) substitution =
  try
    if t = subst_lookup s x then s else raise No_match
  with Unknown_variable _ -> (x, t) :: s
;;

let rec match_pattern (t:('t, 'nt) tree) (p:('t, 'nt) pattern) (s:('t, 'nt) substitution): ('t, 'nt) substitution =
  match t, p with
      _, Pat_any -> s
    | Leaf a, Pat_symbol (Terminal a')
    | Node (a, _), Pat_symbol (NonTerminal a') -> if a = a' then s else raise No_match
    | t, Pat_var x -> subst_extend s x t
    | Node (_, tl), Pat_production pl -> match_pattern_list tl pl s
    | _ -> raise No_match
and match_pattern_list tl pl s =
  match tl, pl with
      [], [] -> s
    | t :: tl, p :: pl -> match_pattern_list tl pl (match_pattern t p s)
    | _ -> raise No_match
;;

type ('t, 'nt) constructor =
    Cons_terminal of 't
  | Cons_var of string
  | Cons_production of 'nt * ('t, 'nt) constructor list
;;

let rec is_valid_constructor (g:('t, 'nt) grammar) (s:('t, 'nt) symbol) (c:('t, 'nt) constructor): bool =
  match s, c with
      Terminal a, Cons_terminal a' -> a = a'
    | _, Cons_var _ -> true
    | NonTerminal nt, Cons_production (nt', cl) -> 
        let rec is_valid_constructor_list sl cl =
          match sl, cl with
              [], [] -> true
            | s :: sl, c :: cl -> (is_valid_constructor g s c) && (is_valid_constructor_list sl cl)
            | _ -> false
        and is_valid_constructor_for_productions productions =
          match productions with
              [] -> false
            | (nt', sl) :: productions ->
                ((nt = nt') && (is_valid_constructor_list sl cl))
                || (is_valid_constructor_for_productions productions)
        in (nt = nt') && (cl <> []) && (is_valid_constructor_for_productions g.productions)
    | _ -> false
;;

let rec construct (c:('t, 'nt) constructor) (s:('t, 'nt) substitution): ('t, 'nt) tree =
  match c with
      Cons_terminal a -> Leaf a
    | Cons_var x -> subst_lookup s x
    | Cons_production (nt, cl) -> Node (nt, List.map (fun c -> construct c s) cl)
;;

type ('a, 'b) union =
    A of ('a)
  | B of ('b)
;;

type ('a, 'b) dependency =
    'a -> 'b list -> ('a, 'b) union
;;

let recursion_by (g: ('a, 'b) dependency): 'a -> 'b =
  let rec f (x: 'a): 'b =
    let rec f' (a: 'a) (w: 'b list): 'b =
      match g x w with
          A a' -> let b' = f a' in f' x (b' :: w)
        | B b -> b
    in f' x []
  in f
;;

type ('t, 'nt) rule =
    (('t, 'nt) pattern) * (('t, 'nt) pattern list) * (('t, 'nt) constructor, ('t, 'nt) constructor) union
;;

type ('t, 'nt) specification =
    ('t, 'nt) rule list
;;

exception Abort;;

let gen_dependency (s:('t, 'nt) specification) (a:('t, 'nt) tree) (bl:('t, 'nt) tree list) =
  let rec rule_list_helper rl =
    match rl with
        [] -> raise Abort
      | (pa, pbl, A c) :: rl ->
          (try 
             A (construct c (match_pattern_list (a :: bl) (pa :: pbl) []))
           with No_match -> rule_list_helper rl)
      | (pa, pbl, B c) :: rl ->
          (try 
             B (construct c (match_pattern_list (a :: bl) (pa :: pbl) []))
           with No_match -> rule_list_helper rl)
  in rule_list_helper s
;;

