#load "Tree.cmo"
open Tree

module type Primitives =
sig
  type operator =
      Op_add | Op_sub | Op_mul
    | Op_lt | Op_gt | Op_le | Op_ge | Op_eq
  and constant =
      Const_int of int
    | Const_bool of bool
    | Const_op of operator
end

module type Ast =
sig
  include Primitives
  type ident
  type node
  type pattern =
  type expression =
      Exp_constant of constant
    | Exp_ident of ident
    | Exp_app of node * node
end
