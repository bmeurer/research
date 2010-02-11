module Constants =
struct
  type operator =
      Op_add
    | Op_sub
    | Op_mul
    | Op_lss
    | Op_grt
    | Op_leq
    | Op_geq
    | Op_eq

  type constant =
      Const_int of int
    | Const_bool of bool
end

module Stack =
struct
  include Constants

  type expression =
      Exp_const of constant
    | Exp_index of int
    | Exp_abstr of abstraction
    | Exp_app of expression * expression
    | Exp_letrec of abstraction * expression
    | Exp_infix of expression * operator * expression
    | Exp_cond of expression * expression * expression
  and abstraction =
      expression

  type value =
      Val_const of constant
    | Val_closure of abstraction * int
  and entry =
      Entry_value of value * int
    | Entry_abstr of abstraction
  and stack =
      entry list

  exception Abort

  let substack (s:stack) (n:int): stack =
    let rec substack_helper s n =
      if n = 0 then []
      else let d :: s = s in
        d :: (substack_helper s (n - 1))
    in substack_helper (List.rev s) n

  let rec lookup (s:stack) (index:int): value =
    match s with
        (Entry_value (v, n)) :: s -> 
          if index = 0 then v
          else lookup (substack s n) (index - 1)
      | (Entry_abstr a) :: s ->
          if index = 0 then Val_closure (a, (List.length s) + 1)
          else lookup s (index - 1)

  let update (s:stack) (d:entry): stack =
    d :: s

  let remove1 (s:stack): stack =
    let _ :: s = s in s

  let rec eval (e:expression) (s:stack): value =
    match e with
        Exp_const c -> Val_const c
      | Exp_index i -> lookup s i
      | Exp_abstr a -> Val_closure (a, List.length s)
      | Exp_app (e1, e2) ->
          (match (eval e1 s, eval e2 s) with
               Val_closure (a, n), v -> 
                 eval a (update s (Entry_value (v, n)))
             | _ -> raise Abort)
      | Exp_letrec (a, e) ->
          eval e (update s (Entry_abstr a))
      | Exp_infix (e1, op, e2) ->
          (match (eval e1 s, eval e2 s) with
               Val_const (Const_int z1), Val_const (Const_int z2) ->
                 Val_const (match op with
                                Op_add -> Const_int (z1 + z2)
                              | Op_sub -> Const_int (z1 - z2)
                              | Op_mul -> Const_int (z1 * z2)
                              | Op_lss -> Const_bool (z1 < z2)
                              | Op_grt -> Const_bool (z1 > z2)
                              | Op_leq -> Const_bool (z1 <= z2)
                              | Op_geq -> Const_bool (z1 >= z2)
                              | Op_eq -> Const_bool (z1 = z2))
             | _ -> raise Abort)
      | Exp_cond (e0, e1, e2) ->
          (match eval e0 s with
               Val_const (Const_bool true) -> eval e1 s
             | Val_const (Const_bool false) -> eval e2 s
             | _ -> raise Abort)

  let eval_iter (e:expression): value =
    let s = ref [] in
    let rec eval (e:expression): value =
      match e with
          Exp_const c -> Val_const c
        | Exp_index i -> lookup !s i
        | Exp_abstr a -> Val_closure (a, List.length !s)
        | Exp_app (e1, e2) ->
            (match (eval e1, eval e2) with
                 Val_closure (a, n), v -> 
                   s := update !s (Entry_value (v, n));
                   let v = eval a in
                     s := remove1 !s; v
               | _ -> raise Abort)
        | Exp_letrec (a, e) ->
            s := update !s (Entry_abstr a);
            let v = eval e in
              s := remove1 !s; v
        | Exp_infix (e1, op, e2) ->
            (match (eval e1, eval e2) with
                 Val_const (Const_int z1), Val_const (Const_int z2) ->
                   Val_const (match op with
                                  Op_add -> Const_int (z1 + z2)
                                | Op_sub -> Const_int (z1 - z2)
                                | Op_mul -> Const_int (z1 * z2)
                                | Op_lss -> Const_bool (z1 < z2)
                                | Op_grt -> Const_bool (z1 > z2)
                                | Op_leq -> Const_bool (z1 <= z2)
                                | Op_geq -> Const_bool (z1 >= z2)
                                | Op_eq -> Const_bool (z1 = z2))
               | _ -> raise Abort)
        | Exp_cond (e0, e1, e2) ->
            (match eval e0 with
                 Val_const (Const_bool true) -> eval e1
               | Val_const (Const_bool false) -> eval e2
               | _ -> raise Abort)
    in eval e
end

module Ast =
struct
  include Constants

  type expression =
      Exp_const of constant
    | Exp_ident of string
    | Exp_abstr of abstraction
    | Exp_app of expression * expression
    | Exp_letrec of string * abstraction * expression
    | Exp_infix of expression * operator * expression
    | Exp_cond of expression * expression * expression
  and abstraction =
      string * expression

  type environment =
      Env_empty
    | Env_entry of string * environment

  exception Unknown_identifier of string

  let slookup (env:environment) (id:string): int =
    let rec slookup_helper env offset =
      match env with
          Env_entry (id', env') -> if id = id' then offset else slookup_helper env' (offset + 1)
        | _ -> raise (Unknown_identifier id)
    in slookup_helper env 0

  let supdate (env:environment) (id:string): environment =
    Env_entry (id, env)

  let rec translate (e:expression) (env:environment): Stack.expression =
    match e with
        Exp_const c -> Stack.Exp_const c
      | Exp_ident id -> Stack.Exp_index (slookup env id)
      | Exp_abstr (id, e) -> Stack.Exp_abstr (translate e (supdate env id))
      | Exp_app (e1, e2) -> Stack.Exp_app (translate e1 env, translate e2 env)
      | Exp_letrec (id, (id', e'), e) ->
          let env = supdate env id in
            Stack.Exp_letrec (translate e' (supdate env id'), translate e env)
      | Exp_infix (e1, op, e2) -> Stack.Exp_infix (translate e1 env, op, translate e2 env)
      | Exp_cond (e0, e1, e2) -> Stack.Exp_cond (translate e0 env, translate e1 env, translate e2 env)
end

open Ast

let fact x =
  Exp_letrec ("fact",
              ("x", Exp_cond (Exp_infix (Exp_ident "x", Op_eq, Exp_const (Const_int 0)),
                              Exp_const (Const_int 1),
                              Exp_infix (Exp_ident "x",
                                         Op_mul,
                                         Exp_app (Exp_ident "fact",
                                                  Exp_infix (Exp_ident "x", Op_sub, Exp_const (Const_int 1)))))),
              Exp_app (Exp_ident "fact", Exp_const (Const_int x)))
;;

let fact0 x =
  Ast.translate (fact x) Ast.Env_empty
;;

Stack.eval (fact0 4) [];;
Stack.eval_iter (fact0 7);;

