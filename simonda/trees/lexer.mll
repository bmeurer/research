(*** lexer.mll - ocamllex lexer for Ast ***)

{
  open Parser
}
rule token = parse
    [ ' ' '\t' '\n' ] { token lexbuf }
  | [ '0'-'9' ]+ as lxm { INT(int_of_string lxm) }
  | "true" { BOOL(true) }
  | "false" { BOOL(false) }
  | '#' [ '0'-'9' ]+ as lxm { PROJ(int_of_string lxm) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '<' { LSS }
  | '>' { GRT }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | '=' { EQ }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "lambda" { LAMBDA }
  | '.' { DOT }
  | ',' { COMMA }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as id { ID(id) }
  | eof { EOF }
