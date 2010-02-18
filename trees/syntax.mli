(*** syntax.mli - Interface of the Syntax module ***)

(* Read an Ast.expression from a string *)
val expression_from_string : string -> Ast.expression

(* Read an Ast.expression from an input channel *)
val expression_from_channel : in_channel -> Ast.expression

(* Create a Tree from an expression *)
val tree_from_expression : Ast.expression -> Ast.expression Tree.t
