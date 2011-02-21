module Symbol = struct
  type t = string;;
  let compare (s1:t) (s2:t): int = String.compare s1 s2;;
end;;

module SymbolSet = Set.Make(Symbol);;

type terminal =
    Symbol.t
and nonterminal =
    Symbol.t
and symbol =
    Terminal of terminal
  | Nonterminal of nonterminal
and word =
    symbol list
;;

module Production = struct
  type t = nonterminal * word;;
end;;

type grammar =
    SymbolSet.t * SymbolSet.t * 
