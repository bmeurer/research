(* parser.mly - Parser input file *)

%{
  open Lexing
%}

%token DOMAINS
%token RULES

%start input

