type pattern =
    Ppat_any
  | Ppat_var of string
  | Ppat_term of string
  | Ppat_prod of int * pattern list
and expression =
    Pexp_var of string
  | Pexp_term of string
  | Pexp_prod of int * expression list
and value =
    Pval_term of string
  | Pval_prod of int * value list
and substitution =
    (string * value) list
;;

exception No_match;;

let rec match_pattern (p:pattern) (v:value): substitution =
  match p, v with
      Ppat_any, _ -> []
    | Ppat_var x, v -> [x, v]
    | Ppat_term pt, Pval_term vt -> if pt = vt then [] else raise No_match
    | Ppat_prod (pn, pl), Pval_prod (vn, vl) -> if pn = vn then match_pattern_list pl vl else raise No_match
    | _ -> raise No_match
and match_pattern_list (pl:pattern list) (vl:value list): substitution =
  match pl, vl with
      [], [] -> []
    | p :: pl, v :: vl -> merge_substitutions (match_pattern p v) (match_pattern_list pl vl)
and merge_substitutions (s1:substitution) (s2:substitution): substitution =
  match s1, s2 with
      [], s -> s
    | (x, v) :: s1, s2 -> 
        let s = merge_substitutions s1 s2 in 
          try
            if v <> List.assoc x s then raise No_match else s
          with Not_found -> (x,v) :: s
;;


let p = Ppat_prod(1, [Ppat_var "x"; Ppat_var "y"])
and v = Pval_prod(1, [Pval_term "1"; Pval_term "2"])
in match_pattern p v
;;


type domain =
    { pdomain_desc: domain_desc }
and domain_desc =
    Pdomain_constr of string
  | Pdomain_tuple of domain list
and domain_declaration =
    { pdomain_kind: domain_kind }
and domain_kind =
    Pdomain_simple of domain
  | Pdomain_variant of (string * domain) list
;;

type pattern =
    { ppat_desc: pattern_desc }
and pattern_desc =
    Ppat_any
  | Ppat_var of string
  | Ppat_const of string
  | Ppat_tuple of pattern list
  | Ppat_constr of string * pattern option
;;

type expression =
    { pexp_desc: expression_desc }
and expression_desc =
    Pexp_ident of string
  | Pexp_const of string
  | Pexp_tuple of expression list
  | Pexp_constr of string * expression option
;;



type domain =
    Dstring
  | Dlink of domain ref
  | Dtuple of domain list
  | Dvariant of domain_constr list
and domain_constr =
    Dconstr0 of string
  | Dconstr1 of string * domain
;;



type expr =
    Evar of string
  | Estring of string
  | Etuple of expr list
  | Econstr0 of string
  | Econstr1 of string * expr
;;

type pattern =
    Pwild
  | Pvar of string
  | Pstring of string
  | Ptuple of pattern list
  | Pconstr0 of string
  | Pconstr1 of string * pattern
;;



let d_ENV_ref = ref (Dstring)
and d_EXP_ref = ref (Dstring)
and d_VAL_ref = ref (Dstring);;

let d_ENV = Dvariant ([Dconstr0 ("EMPTY");
                       Dconstr1 ("ENTRY",
                                 Dtuple [Dstring;
                                         Dlink d_VAL_ref;
                                         Dlink d_ENV_ref])])
and d_EXP = Dvariant ([Dconstr0 ("ZERO");
                       Dconstr0 ("SUCC");
                       Dconstr1 ("ID",
                                 Dtuple [Dstring]);
                       Dconstr1 ("APP",
                                 Dtuple [Dlink d_EXP_ref;
                                         Dlink d_EXP_ref]);
                       Dconstr1 ("ABSTR",
                                 Dtuple [Dstring;
                                         Dlink d_EXP_ref])])
and d_VAL = Dvariant ([Dconstr0 ("ZEROVAL");
                       Dconstr1 ("SUCCVAL",
                                 Dtuple [Dlink d_VAL_ref]);
                       Dconstr1 ("CLOSURE",
                                 Dtuple [Dstring;
                                         Dlink d_EXP_ref;
                                         Dlink d_ENV_ref])])
;;
d_ENV_ref := d_ENV; d_EXP_ref := d_EXP; d_VAL_ref := d_VAL;;

let p_APP1 =
  [d_EXP, Pconstr1 ("APP", Ptuple [Pvar "e1"; Pvar "e2"]);
   d_ENV, Pvar "eta"];;
let e_APP1 =
  [d_EXP, Pvar "e1";
   d_ENV, Pvar "eta"];;
