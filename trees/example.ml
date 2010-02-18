#load "path.cmo";;
#load "tree.cmo";;
#load "ast.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";;
#load "syntax.cmo";;

let fact_source = "let rec fact = lambda x.if =(x,0) then 1 else *(x,fact (-(x,1))) in fact 6";;

let fact_expr = Syntax.expression_from_string fact_source;;

let fact_tree = Syntax.tree_from_expression fact_expr;;
