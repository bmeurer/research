#load "Tree.cmo";;
open Tree;;

module type Ast =
sig
  type access
  type binder
  type environment
  type operator =
      Op_add | Op_sub | Op_mul
    | Op_lss | Op_grt | Op_leq | Op_geq | Op_eq | Op_neq
  ;;
  type constant =
      Const_unit
    | Const_bool of bool
    | Const_int of int
    | Const_proj of int
  ;;
  type label =
      Lbl_const of constant
    | Lbl_access of access
    | Lbl_abstr of binder
    | Lbl_app
  ;;
  type node
  type value =
      Val_const of constant
    | Val_closure of binder * node * environment
  ;;
  val label: node -> label
  val children: node -> node list
  val parent: node -> node
end

module type Binding =
sig
  type access
  type binder
  type environment
  type value
  val lookup : access -> environment -> value
  val update : binder -> environment -> value -> environment
end

module NameBinding:Binding =
struct
  type access = string
  and binder = string
  and environment = Env of (string * value) list
  and value
  let rec lookup (id:string) (eta:environment): value =
    let Env l = eta in List.assoc id l
  let rec update (id:string) (eta:environment) (v:value): environment =
    let Env l = eta in Env ((id, v) :: l)
end

module MakeSemantic =
  functor (A:Ast) ->
    functor (B:Binding with type access = A.access
                       and type binder = A.binder
                       and type environment = A.environment
                       and type value = A.value) ->
struct
  exception Eval_abort
  let rec eval (k:A.node) (eta:A.environment): A.value =
    match A.label k with
        A.Lbl_const c -> A.Val_const c
      | A.Lbl_access a -> B.lookup a eta
      | A.Lbl_abstr b -> A.Val_closure (b, (List.hd (A.children k)), eta)
      | A.Lbl_app ->
          let k1 :: k2 :: kl = A.children k in
            match (eval k1 eta), (eval k2 eta) with
                (A.Val_closure (b, k, eta)), v -> eval k (B.update b eta v)
              | _ -> raise Eval_abort
end;;





module type MakeBoundAst =
  functor (A:Ast)
    -> functor (B:Binding
                with type access = A.access
                and type binder = A.binder
                and type environment = A.environment
                and type value = A.value)
      -> Ast with type access = A.access
             and type binder = A.binder
             and type environment = A.environment
;;

module type MakeBinding = functor (B:Base) -> Binding
  with type binder = B.binder
  and type environment = B.environment
  and type value = B.value
;;

module type Ast =
sig
  include Base
  type access
  type label =
      Lbl_const of constant
    | Lbl_access of access
    | Lbl_abstr of binder
    | Lbl_app
  ;;
end;;

module type MakeAst = functor (B:Binding) -> Ast
  with type access = B.access
  and type binder = B.binder
  and type environment = B.environment
;;


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


module type Ast =
sig
  type ('env) t
  and ('env) label =
      Lbl_const of constant
    | Lbl_ident of ('env -> 'env value)
    | Lbl_abstr of ('env -> 'env value -> 'env)
    | Lbl_app
    | Lbl_cond
    | Lbl_infix
    | Lbl_let of ('env -> 'env value -> 'env)
  and ('env) value =
      Val_const of constant
    | Val_closure of ('env value -> 'env) * Tree.path
  ;;
end;;



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
  ;;
end;;

let e =
  Exp_app ((Identifiers.make_abstr "x"
              (Exp_infix (Identifiers.make_id "x",
                          Op_mul,
                          Identifiers.make_id "x"))),
           Exp_const (Const_int 6))
in
  eval e Identifiers.Env_empty
;;
