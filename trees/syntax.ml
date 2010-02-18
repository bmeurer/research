(*** syntax.ml - Implementation of the Syntax module ***)

open Ast

let expression_from_string (input:string): Ast.expression =
  Parser.main Lexer.token (Lexing.from_string input)

let expression_from_channel (input:in_channel): Ast.expression =
  Parser.main Lexer.token (Lexing.from_channel input)

let rec tree_from_expression (e:Ast.expression): Ast.expression Tree.t =
  match e with
      Exp_const _
    | Exp_ident _
    | Exp_abstr _ -> Tree.create e []
    | Exp_tuple el -> Tree.create e (List.map tree_from_expression el)
    | Exp_app (e1, e2) -> Tree.create e [tree_from_expression e1; tree_from_expression e2]
    | Exp_if (e0, e1, e2) -> Tree.create e [tree_from_expression e0; tree_from_expression e1; tree_from_expression e2]
    | Exp_let (_, e1, e2)
    | Exp_letrec (_, e1, e2) -> Tree.create e [tree_from_expression e1; tree_from_expression e2]

