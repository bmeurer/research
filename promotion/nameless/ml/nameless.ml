#load "Tree.cmo";;
open Tree;;

module type Ast =
sig
  type node
  type ident
  type binder
  type operator =
      Op_add | Op_sub | Op_mul
    | Op_lss | Op_grt | Op_leq | Op_geq | Op_eq | Op_neq
  and constant =
      Const_unit
    | Const_bool of bool
    | Const_int of int
    | Const_proj of int
    | Const_op of operator
  and label =
      Lbl_const of constant
    | Lbl_ident of ident
    | Lbl_abstr of binder
    | Lbl_app
    | Lbl_let of binder
    | Lbl_letrec of binder
    | Lbl_tuple
  val label: node -> label
  val children: node -> node list
  val parent: node -> node
end

module type Interpreter =
sig
  type environment
  type node
  type value
  val eval : node -> environment -> value
end

module type MakeInterpreter = functor (A:Ast) -> Interpreter

module MakeNamedInterpreter: MakeInterpreter = functor (A:Ast) ->
struct
  type value =
      Val_const of A.constant
    | Val_closure of string list * node * environment
    | Val_tuple of value list
  and node =
      A.node
  and environment =
      (string * value) list

  exception Unknown_identifier of string

  let rec lookup (id:string) (eta:environment): value =
    match eta with
        (id', v') :: eta' -> if id = id' then v' else lookup id eta'
      | _ -> raise (Unknown_identifier id)

  exception Tuple_mismatch

  let rec update (eta:environment) (idl:string list) (vl:value list): environment =
    match idl, vl with
        [], [] -> eta
      | (id :: idl), (v :: vl) -> (id, v) :: (update eta idl vl)
      | _ -> raise Tuple_mismatch

  exception Eval_abort

  let rec eval_op (op:A.operator) (z1:int) (z2:int): A.constant =
    match op with
        A.Op_add -> A.Const_int  (z1 +  z2)
      | A.Op_sub -> A.Const_int  (z1 -  z2)
      | A.Op_mul -> A.Const_int  (z1 *  z2)
      | A.Op_lss -> A.Const_bool (z1 <  z2)
      | A.Op_grt -> A.Const_bool (z1 >  z2)
      | A.Op_leq -> A.Const_bool (z1 <= z2)
      | A.Op_geq -> A.Const_bool (z1 >= z2)
      | A.Op_eq  -> A.Const_bool (z1 =  z2)
      | A.Op_neq -> A.Const_bool (z1 <> z2)

  let rec eval (k:node) (eta:environment): value =
    match A.label k with
        A.Lbl_const c -> Val_const c
      | A.Lbl_ident id -> lookup id eta
      | A.Lbl_abstr idl -> Val_closure (idl, k, eta)
      | A.Lbl_app ->
          (match A.children k with
               k1 :: k2 :: [] ->
                 (match (eval k1 eta), (eval k2 eta) with
                      Val_const (A.Const_proj i), Val_tuple (vl) -> 
                        (try List.nth vl i with _ -> raise Eval_abort)
                    | Val_const (A.Const_op op), Val_tuple [Val_const (A.Const_int z1); Val_const (A.Const_int z2)] ->
                        Val_const (eval_op op z1 z2)
                    | Val_closure (id' :: [], k', eta'), v' ->
                        eval (List.hd (A.children k')) (update eta' [id'] [v'])
                    | Val_closure (idl', k', eta'), Val_tuple (vl') ->
                        eval (List.hd (A.children k')) (update eta' idl' vl')
                    | _ -> raise Eval_abort)
             | _ -> raise Eval_abort)
      | A.Lbl_let (id :: []) ->
          (match A.children k with
               k1 :: k2 :: [] -> eval k2 (update eta [id] [eval k1 eta])
             | _ -> raise Eval_abort)
      | A.Lbl_let idl ->
          (match A.children k with
               k1 :: k2 :: [] ->
                 (match eval k1 eta with
                      Val_tuple (vl) -> eval k2 (update eta idl vl)
                    | _ -> raise Eval_abort)
             | _ -> raise Eval_abort)
      | A.Lbl_tuple -> Val_tuple (List.map (fun k' -> eval k' eta) (A.children k))
end

(*
type ('env) expression =
    Exp_const of constant
  | Exp_ident of ('env -> 'env value)
  | Exp_abstr of ('env -> 'env value -> 'env) * ('env expression)
  | Exp_app of ('env expression) * ('env expression)
  | Exp_cond of ('env expression) * ('env expression) * ('env expression)
  | Exp_infix of ('env expression) * operator * ('env expression)
  | Exp_let of ('env -> 'env value -> 'env) * ('env expression) * ('env expression)
and constant =
    Const_unit
  | Const_bool of bool
  | Const_int of int
and operator =
    Op_add | Op_sub | Op_mul
  | Op_lss | Op_grt | Op_leq | Op_geq | Op_eq | Op_neq
and ('env) value =
    Val_const of constant
  | Val_closure of ('env value -> 'env) * ('env expression)
;;

exception Eval_abort;;

let rec eval (e:'env expression) (eta:'env): ('env) value =
  match e with
      Exp_const c -> Val_const c
    | Exp_ident lookup -> lookup eta
    | Exp_app (e1, e2) -> 
        (match (eval e1 eta), (eval e2 eta) with
             Val_closure (bind, e), v -> eval e (bind v)
           | _ -> raise Eval_abort)
    | Exp_abstr (update, e) -> Val_closure (update eta, e)
    | Exp_cond (e0, e1, e2) ->
        (match (eval e0 eta) with
             (Val_const (Const_bool true)) -> eval e1 eta
           | (Val_const (Const_bool false)) -> eval e2 eta
           | _ -> raise Eval_abort)
    | Exp_infix (e1, op, e2) ->
        (match (eval e1 eta), (eval e2 eta) with
             (Val_const (Const_int z1)), (Val_const (Const_int z2)) -> Val_const (eval_op op z1 z2)
           | _ -> raise Eval_abort)
and eval_op (op:operator) (z1:int) (z2:int): constant =
  match op with
      Op_add -> Const_int (z1 + z2)
    | Op_sub -> Const_int (z1 - z2)
    | Op_mul -> Const_int (z1 * z2)
    | Op_lss -> Const_bool (z1 < z2)
    | Op_grt -> Const_bool (z1 > z2)
    | Op_leq -> Const_bool (z1 <= z2)
    | Op_geq -> Const_bool (z1 >= z2)
    | Op_eq  -> Const_bool (z1 = z2)
    | Op_neq -> Const_bool (z1 <> z2)
;;


module Identifiers =
struct
  type t =
      string
  and environment =
      Env_empty
    | Env_entry of t * environment value * environment
  ;;

  exception Unknown_identifier of t;;

  let rec make_id (id:t): environment expression =
    let rec lookup (eta:environment): environment value =
      match eta with
          Env_entry (id', v', eta') -> if id = id' then v' else lookup eta'
        | Env_empty -> raise (Unknown_identifier id)
    in Exp_ident (lookup)
  and make_abstr (id:t) (e:environment expression): environment expression =
    let update (eta:environment) (v:environment value): environment =
      Env_entry (id, v, eta)
    in Exp_abstr (update, e)
end
*)

(*
let e =
  Exp_app ((Identifiers.make_abstr "x"
              (Exp_infix (Identifiers.make_id "x",
                          Op_mul,
                          Identifiers.make_id "x"))),
           Exp_const (Const_int 6))
in
  eval e Identifiers.Env_empty
;;
*)
