(* lexer.mll - Lexer input file *)

{
}

let ident = ['a' - 'z' 'A' - 'Z'] ['a' - 'z' 'A' - 'Z' '0' - '9']*;;

rule main = parse
    [' '
  | ident as text
  | _ as c
      {
        
      }
  | eof { }
