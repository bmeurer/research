type domain =
    Dvoid
  | Dname of string
  | Drec of string * domain
  | Dtuple of domain list
  | Dunion of (string * domain) list
;;

type constant =
    Cvoid
;;

type pattern =
    Pany
  | Pvar of string
  | Pconst of constant
  | Pconstr of string * pattern
  | Ptuple of pattern list
;;

type expression =
    Evar of string
  | Econst of constant
  | Econstr of string * expression
  | Etuple of expression list
;;

type node =
    Kconst of constant
  | Kconstr of string * node
  | Ktuple of node list
;;

type assignment = (string * node) list;;

exception No_match;;

let rec pattern_match (p:pattern) (k:node): assignment =
  match (p, k) with
      Pany, _ -> []
    | Pvar x, k -> [x,k]
    | Pconst pc, Kconst kc -> if pc = kc then [] else raise No_match
    | Pconstr (pcn, p), Kconstr (kcn, k) -> if pcn = kcn then pattern_match p k else raise No_match
    | Ptuple (pl), Ktuple (kl) -> pattern_match_list pl kl
    | _ -> raise No_match
and pattern_match_list (pl: pattern list) (kl: node list): assignment =
  let rec pml_helper pl kl al =
    match (pl, kl) with
        [], [] -> al
      | p :: pl, k :: kl -> pml_helper pl kl (pattern_join_assignments (pattern_match p k) al)
      | _ -> raise No_match
  in pml_helper pl kl []
and pattern_join_assignments (al: assignment) (al': assignment): assignment =
  match al with
      (x, k) :: al ->
        (try
           let k' = List.assoc x al' in
             if k <> k' then raise No_match
             else pattern_join_assignments al al'
         with Not_found -> (x, k) :: (pattern_join_assignments al al'))
    | [] -> al'
;;

pattern_match (Ptuple [Pvar "x"; Pvar "x"]) (Ktuple [Kconst Cvoid; Kconst Cvoid]);;

type ('a, 'b) union =
    A of ('a)
  | B of ('b)
;;

type rule =
    pattern * pattern list * (expression, expression) union
;;
type dependency =
    rule list
;;

let rec subst (e: expression) (s: assignment): node =
  match e with
      Evar x -> List.assoc x s
    | Econst c -> Kconst c
    | Econstr (cn, e) -> Kconstr (cn, subst e s)
    | Etuple el -> Ktuple (List.map (fun e -> subst e s) el)
;;

exception Stuck of node * node list;;

let rec dependency_match (g: dependency) (a: node) (bl: node list): (node, node) union =
  match g with
      (apat, bpatl, eunion) :: g -> 
        (try
           let s = pattern_join_assignments (pattern_match_list bpatl bl) (pattern_match apat a) in
             match eunion with
                 A a -> A (subst a s)
               | B b -> B (subst b s)
         with No_match -> match_dependency g a bl)
    | _ -> raise (Stuck (a, bl))
;;

let generate_interpreter (g: dependency): node -> node =
  let rec f (x: node): node =
    let rec f' (a: node) (w: node list): node =
      match (dependency_match g a w) with
          A a -> let b' = f a in f' x (b' :: w)
        | B b -> b
    in f' x []
  in f
;;


let l1:dependency =
  [
    (Ptuple [Pconstr ("ABSTR", Ptuple [Pvar "id"; Pvar "e"]); Pvar "eta"],
     [],
     B (Econstr ("CLOSURE", Etuple [Evar "id"; Evar "e"; Evar "eta"])));

    (Ptuple [Pconstr ("APP", Ptuple [Pvar "e1"; Pvar "e2"]); Pvar "eta"],
     [],
     A (Etuple [Evar "e1"; Evar "eta"]));
  ]
;;

let l1a: node =
  Ktuple [Kconstr ("ABSTR", Ktuple [Kconst Cvoid; Kconst Cvoid]); Kconstr ("EMPTY", Kconst Cvoid)]
;;

let l1i = generate_interpreter l1;;

l1i l1a;;
