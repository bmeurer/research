#use "graphic.ml";;
#use "ea_sets.ml";;

(* =========================================================================================== *)
(* Effiziente Umsetzung von Rekursion in Iteration                                             *)
(* =========================================================================================== *)

type ('arg, 'res) union = 
   Arg of 'arg 
 | Res of 'res;;

type ('arg, 'res) dependency = 
   'arg -> 'res list -> ('arg, 'res) union;;

let recursion_by (g: ('arg, 'res) dependency): 'arg -> 'res =
 let rec f (x: 'arg): 'res =
   let rec f' (x: 'arg) (l: 'res list): 'res = 
     match g x l with 
	  Res y -> y 
	| Arg x' -> let y' = f x' in f' x (y' :: l) in 
     f' x [] in 
   f;;

class ['a] stack = 
object (st)
 val mutable l = []
 method push (x: 'a) = l <- x :: l
 method pop = let x = List.hd l in l <- List.tl l; x
 method _Push (l': 'a list) = l <- l' @ l
 method _Pop (n: int) = if n = 0 then [] else let x = st#pop in x :: st#_Pop (n - 1) 
 method reset = l <- []
 method contents = l
end;;

type dot = L | R;;

exception No_pred;;

class ['arg, 'res] stack_machine
 (g: ('arg, 'res) dependency) 
 (pred: 'arg -> 'arg) 
 (pos: 'arg -> int) 
 (init: 'arg) =
object (sm)
 val mutable dot = L
 val mutable current_arg = init
 val arg_stack = new stack
 val res_stack = new stack
 method step = 
   let a = 
     if dot = L 
     then current_arg 
     else 
	try 
	  pred current_arg 
	with 
	    No_pred -> arg_stack#pop
   and bs = 
     if dot = L 
     then [] 
     else 
	res_stack#_Pop (pos current_arg) in 
     match g a bs with 
	  Res b ->
	    dot <- R; 
	    current_arg <- a;
	    res_stack#push b 
	| Arg a' -> 
	    dot <- L;
	    current_arg <- a'; 
	    (try 
	       ignore (pred a')
	     with 
		 No_pred -> arg_stack#push a);
	    res_stack#_Push bs
 method run =
   try 
     while true do sm#step done
   with 
	_ -> ()
end;;


(* =========================================================================================== *)
(* Anwendung auf die Umgebungssemantik                                                         *)
(* =========================================================================================== *)

(* ------------------------------------------------------------------------------------------- *)
(* Ausdrücke                                                                                   *)
(* ------------------------------------------------------------------------------------------- *)

type identifier = string;;

type operator = 
   Add | Sub | Mul | Div | Mod | Lss | Grt | Leq | Geq | Eq | Neq;;

type constant = 
   Unit 
 | Bool of bool
 | Int of int 
 | Op of operator
 | Proj of int;;

type expression = 
   Const of constant 
 | Id of identifier 
 | App of expression * expression
 | If of expression * expression * expression
 | And of expression * expression
 | Or of expression * expression
 | Infix of expression * operator * expression 
 | Lambda of identifier list * expression
 | Let of identifier list * expression * expression
 | Letval of identifier list * expression * expression
 | Letrec of identifier list * expression * expression
 | Tuple of expression list;;

(* ------------------------------------------------------------------------------------------- *)
(* Syntaxbäume                                                                                 *)
(* ------------------------------------------------------------------------------------------- *)

type 'a label = 
   CONST of constant
 | ID of identifier
 | INDEX of identifier * 'a 
 | APP 
 | IF
 | AND
 | OR 
 | INFIX of operator
 | LAMBDA of identifier list
 | LET of identifier list
 | LETVAL of identifier list
 | LETREC of identifier list
 | TUPLE of int;;

let label (e: expression): 'a label = 
 match e with 
     Const c                -> CONST c
   | Id id                  -> ID id
   | App (e_1, e_2)         -> APP
   | If (e_1, e_2, e_3)     -> IF
   | And (e_1, e_2)         -> AND 
   | Or (e_1, e_2)          -> OR
   | Infix (e_1, op, e_2)   -> INFIX op
   | Lambda (ids, e_1)      -> LAMBDA ids
   | Let (ids, e_1, e_2)    -> LET ids
   | Letval (ids, e_1, e_2) -> LETVAL ids
   | Letrec (ids, e_1, e_2) -> LETREC ids
   | Tuple es               -> TUPLE (List.length es);;

let sub_expressions (e: expression): expression list = 
 match e with 		       
     Const c                -> []
   | Id id                  -> []
   | App (e_1, e_2)         -> [e_1; e_2]
   | If (e_1, e_2, e_3)     -> [e_1; e_2; e_3]
   | And (e_1, e_2)         -> [e_1; e_2]
   | Or (e_1, e_2)          -> [e_1; e_2]
   | Infix (e_1, op, e_2)   -> [e_1; e_2]
   | Lambda (ids, e_1)      -> [e_1]
   | Let (ids, e_1, e_2)    -> [e_1; e_2]
   | Letval (ids, e_1, e_2) -> [e_1; e_2]
   | Letrec (ids, e_1, e_2) -> [e_1; e_2]
   | Tuple es               -> es;;

class counter (init: int) = 
object 
 val mutable x = init - 1
 method get = x <- x + 1; x
 method reset = x <- init - 1
end;;

type address = int;;

type 'a node = 'a label * address * address list;;

let node_counter = new counter 0;;

let rec node_list (parent: address) (e: expression): address * 'a node list = 
 let u = node_counter#get
 and es = sub_expressions e in 
 let node_lists = List.map (node_list u) es in
   u, (label e, parent, List.map fst node_lists) :: List.flatten (List.map snd node_lists);;

type 'a syntax_tree = 'a node array;;

let syntax_tree (e: expression): 'a syntax_tree = 
 node_counter#reset;
 Array.of_list (snd (node_list (-1) e));;

type 'a program = 'a syntax_tree ref;;

let node_at (u: address) (p: 'a program): 'a node = 
 (! p).(u);;

let label_at (u: address) (p: 'a program): 'a label = 
 let l, _, _ = node_at u p in l;;

let parent (u: address) (p: 'a program): address = 
   let _, v, _ = node_at u p in v;;

let children (u: address) (p: 'a program): address list =
 let _, _, vs = node_at u p in vs;;

let child i (u: address) (p: 'a program): address = 
 List.nth (children u p) i;;

let op_latex (op: operator): string = 
 match op with 
     Add -> "\\mathop{+}"
   | Sub -> "\\mathop{-}"
   | Mul -> "\\mathop{*}"
   | Div -> "\\mathop{/}"
   | Mod -> "\\mathop{\\mbox{\\bf mod}}"
   | Lss -> "\\mathop{<}"
   | Grt -> "\\mathop{>}"
   | Leq -> "\\mathop{\\leq}"
   | Geq -> "\\mathop{\\geq}"
   | Eq  -> "\\mathop{=}"
   | Neq -> "\\mathop{\\neq}";;

let const_latex (c: constant): string = 
 match c with
     Unit    -> "()"
   | Bool b  -> "\\" ^ string_of_bool b
   | Int n   -> string_of_int n
   | Op op   -> op_latex op
   | Proj i  -> "\\#" ^ int_latex i;;

let id_latex (id: identifier): string = "\\name{" ^ id ^ "}";;
let ids_latex ids = 
 String.concat ", " (List.map id_latex ids);;


(* ------------------------------------------------------------------------------------------- *)
(* Syntaxbaum des Gesamtprogramms                                                              *)
(* ------------------------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------------------------- *)
(* Übersetzung des Gesamtprogramms in die namenlose Notation                                   *)
(* (die Namen werden beibehalten, um die Lesbarkeit zu erhöhen)                                *)
(* ------------------------------------------------------------------------------------------- *)

type except = 
   Abort
 | Division_by_Zero

type ptr = int

(* ------------------------------------------------------------------------------------------- *)
(* LaTeX-Ausgabe für Closures und Umgebungen                                                   *)
(* ------------------------------------------------------------------------------------------- *)

let ptr_latex i = 
 "\\ptr{" ^ int_latex i ^ "}";;

let except_latex ex = 
 match ex with 
     Abort -> "\\stuck"
   | Division_by_Zero -> "\\mbox{\\bf Division\\_by\\_zero}";;

let rec exp_latex (e: expression): string = 
 match e with
     Const c -> 
	const_latex c 
   | App (e_1, e_2) -> 
	exp_latex e_1 
	^ "\\," 
	^ exp_latex e_2;;

module type NAMELESS = 
sig 
 type s_value 
 type s_environment 
 type d_environment 
 type d_pair = address * d_environment
 type d_value = 
     Val_d of expression 
   | Cl_d of d_pair 
   | Ptr_d of address * ptr
   | W_Tuple_d of d_value list
   | Except_d of except
 val all_d_envs: d_environment list ref
 val program: s_value syntax_tree ref
 val initial_s_environment: s_environment
 val s_update: address -> s_environment -> s_environment
 val s_lookup: s_environment -> identifier -> s_value 
 val d_update: 'a label -> d_environment -> d_value list -> d_environment
 val d_lookup: d_environment -> s_value -> d_value
 val empty_env_d: d_environment
 val reset_d_env: unit -> unit
 val tail_d: int -> d_environment -> d_environment 
 val num_d: d_environment -> int 
 val d_components: d_value -> d_value list
 val s_value_latex: s_value -> string 
 val d_value_latex: d_value -> string
 val d_pair_latex: d_pair -> string
 val new_d_env_latex: d_environment -> string
 exception Number_of_Arguments
end;;

type distance = int;;
type offset = int;;

let distance_latex = int_latex;;
let offset_latex = int_latex;;
let address_latex = int_latex;;

module Nameless: NAMELESS = 
struct 

 type s_value = 
     D of distance * offset
   | S of address * distance

 type s_environment = int * (identifier * s_value) list

 type d_value = 
     Val_d of expression 
   | Cl_d of d_pair 
   | Ptr_d of address * ptr
   | W_Tuple_d of d_value list
   | Except_d of except
 and d_pair = address * d_environment
 and d_frame = d_value list
 and d_environment = 
     Empty_env_d
   | Env_d of int * d_frame * d_environment;;

 let empty_env_d = Empty_env_d;;

 let program = ref (syntax_tree (Const (Int 0)));;

 let initial_s_environment = 0, []

 let s_update (u: address) (d, _Gamma: s_environment): s_environment = 
   let rec par_list ids depth offset = 
     match ids with 
	  [] -> []
	| id :: ids' -> (id, D (depth, offset)) :: par_list ids' depth (offset + 1)
   and decl_list ids (u: address) depth offset = 
     match ids with 
	  [] -> []
	| id :: ids' -> (id, S (child offset u program, depth)) :: decl_list ids' u depth (offset + 1) in 
     match label_at u program with 
	  LAMBDA ids | LET ids -> d + 1, par_list ids (d + 1) 0 @ _Gamma
	| LETVAL ids | LETREC ids -> d, decl_list ids (if List.length ids = 1 then u else child 0 u program) d 0 @ _Gamma

 let s_lookup (d, _Gamma: s_environment) (id: identifier): s_value = 
   match List.assoc id _Gamma with 
	D (depth, offset) -> D (d - depth, offset)
     | S (u, depth) -> S (u, d - depth)

 let all_d_envs = ref [Empty_env_d]

 let (get_d_env, reset_d_env) =
   let x = ref 0 in
     ((fun () -> x := ! x + 1; ! x),
      (fun () -> all_d_envs := [Empty_env_d]; x := 0));;

 exception Number_of_Arguments

 let d_update (bm: 'a label) (rho: d_environment) (ws: d_value list): d_environment = 
   match bm with 
	LETVAL ids | LETREC ids -> 
	  if List.length ids = List.length ws 
	  then rho 
	  else raise Number_of_Arguments
     | LET ids | LAMBDA ids -> 
	  if List.length ids = List.length ws 
	  then 
	    let i = get_d_env () in 
	    let rho' = Env_d (i, ws, rho) in 
	      all_d_envs := rho' :: ! all_d_envs;
	      rho'
	  else raise Number_of_Arguments

 exception Tail

 let rec tail_d i (rho: d_environment): d_environment = 
   if i = 0 
   then rho
   else 
     match rho with 
	  Env_d (_, _, rho_1) -> tail_d (i - 1) rho_1
	| _ -> raise Tail

 let d_expand (rho: d_environment) (w: d_value): d_value = 
   match w with 
	Ptr_d (p, i) -> Cl_d (p, tail_d i rho)
     | _ -> w

 let rec d_lookup rho sv = 
   match sv with 
	D (distance, offset) -> 
	  let Env_d (_, fr, _) as rho' = tail_d distance rho in 
	    d_expand rho' (List.nth fr offset)
     | S (addr, distance) -> 
	  let rho' = tail_d distance rho in 
	    Cl_d (addr, rho')

 let num_d (rho: d_environment): int = 
   match rho with 
	Empty_env_d -> 0
     | Env_d (i, _, _) -> i;;

 let d_components (w: d_value): d_value list = 
   match w with 
	W_Tuple_d ws -> ws
     | _            -> [w];;

 let s_value_latex (sv: s_value) = 
   match sv with n
	D (d, o) -> "{" ^ distance_latex d ^ "}^{" ^ offset_latex o ^ "}"
     | S (u, d)   -> "{" ^ address_latex u ^ "}^{" ^ distance_latex d ^ "}";;

 let rec d_value_latex w =
   match w with 
	Val_d v -> exp_latex v
     | Cl_d (u, rho) -> 
	  d_pair_latex (u, rho)
     | Ptr_d (u, i) ->
	  "(" ^ address_latex u ^ "\\, ,\\," ^ ptr_latex i ^ ")"	
     | W_Tuple_d ws ->
	  "(" ^ String.concat ",\\," (List.map d_value_latex ws) ^ ")"
     | Except_d ex ->
	  "\\uparrow " ^ except_latex ex
 and d_pair_latex (u, rho) = 
   "(" ^ address_latex u ^ "\\, ,\\," ^ d_env_latex rho ^ ")"
 and d_frame_latex fr = 
   "[" ^ String.concat ", " (List.map d_value_latex fr) ^ "]"
 and d_env_latex rho = 
   "\\eta_{" ^ int_latex (num_d rho) ^ "}";;

 let new_d_env_latex rho = 
   match rho with 
	Empty_env_d -> 
	  "\\markenv{\\eta_0 = [\\,]}"
     | Env_d (i, fr, rho') -> 
	  "\\markenv{\\eta_{" ^ int_latex i ^ "} = " ^ d_frame_latex fr ^ ";\\," ^ d_env_latex rho' ^ "}";;

end;;

open Nameless;;

let rec translate (_Gamma: s_environment) (u: address) = 
 let a = ! program in 
   match a.(u) with 
     ID id, p, ch              -> a.(u) <- INDEX (id, s_lookup _Gamma id), p, ch
   | LET ids, _, [u_1; u_2]    -> translate _Gamma u_1; translate (s_update u _Gamma) u_2
   | LETVAL ids, _, [u_1; u_2] -> translate _Gamma u_1; translate (s_update u _Gamma) u_2
   | LAMBDA ids, _, [u_1]      -> translate (s_update u _Gamma) u_1
   | LETREC ids, _, [u_1; u_2] -> translate (s_update u _Gamma) u_1; translate (s_update u _Gamma) u_2
   | label, _, ch              -> List.iter (translate _Gamma) ch;;

let root = 0;;

let translate_program () = 
 translate initial_s_environment root;;

let d_abort = Except_d Abort;;

let d_interpretation op (n_1, n_2): d_value = 
 match op with 
     Add -> Val_d (Const (Int (n_1 + n_2))) 
   | Sub -> Val_d (Const (Int (n_1 - n_2))) 
   | Mul -> Val_d (Const (Int (n_1 * n_2))) 
   | Div -> if n_2 = 0 then Except_d Division_by_Zero else Val_d (Const (Int (n_1 / n_2))) 
   | Mod -> if n_2 = 0 then Except_d Division_by_Zero else Val_d (Const (Int (n_1 mod n_2))) 
   | Lss -> Val_d (Const (Bool (n_1 < n_2))) 
   | Grt -> Val_d (Const (Bool (n_1 > n_2))) 
   | Leq -> Val_d (Const (Bool (n_1 <= n_2))) 
   | Geq -> Val_d (Const (Bool (n_1 >= n_2))) 
   | Eq -> Val_d (Const (Bool (n_1 = n_2)))
   | Neq -> Val_d (Const (Bool (n_1 <> n_2)));;


(* ------------------------------------------------------------------------------------------- *)
(* Definition der Funktion g_eval_d                                                            *)
(* ------------------------------------------------------------------------------------------- *)

let g_CONST_d: (d_pair, d_value) dependency = 
 fun (u, _) [] -> 
   match label_at u program with 
	CONST c -> Res (Val_d (Const c));;

let g_CLOSURE_d: (d_pair, d_value) dependency = 
 fun (u, rho) [] -> 
   match label_at u program with 
	LAMBDA _ -> Res (Cl_d (u, rho));;

let g_INDEX: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	INDEX (_, i) ->
	  match l with 
	      [] ->
		(try 
		   let w = d_lookup rho i in 
		     Res w
		 with 
		     Match_failure _ -> Res d_abort)
	    | [w] -> Res w;;

let g_APP_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	APP ->
	  match l with 
	      [] -> Arg (child 0 u program, rho)
	    | [_] -> Arg (child 1 u program, rho)
	    | [_; _] -> Res d_abort;;

let g_OP_1_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program, l with 
	APP, [w; Val_d (Const (Op op))] ->
	  match w with 
	      Val_d (Const (Int n)) -> Res (Val_d (App (Const (Op op), Const (Int n))))
	    | _ -> Res d_abort;;

let g_OP_2_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program, l with 
	APP, [w; Val_d (App (Const (Op op), Const (Int n_1)))] ->
	  match w with 
	      Val_d (Const (Int n_2)) -> Res (d_interpretation op (n_1, n_2))
	    | _ -> Res d_abort;;

let g_PROJ_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program, l with 
	APP, [w; Val_d (Const (Proj i))] ->
	  match w with 
	      W_Tuple_d ws -> if i <= List.length ws then Res (List.nth ws (i - 1)) else Res d_abort
	    | _ -> Res d_abort;;

let g_BETA_V_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	APP ->
	  match l with 
	      [w; Cl_d (u', rho')] -> 
		(match label_at u' program with 
		     LAMBDA ids -> 
		       (try 
			  let rho_1 = d_update (LAMBDA ids) rho' (d_components w)
			  and u_1 = child 0 u' program in 
			    Arg (u_1, rho_1)
			with 
			    Number_of_Arguments -> Res d_abort))
	    | [w; _; _] -> Res w;;

let g_COND_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	IF -> 
	  match l with 
	      [] -> Arg (child 0 u program, rho)
	    | [Val_d (Const (Bool b))] ->
		if b 
		then Arg (child 1 u program, rho)
		else Arg (child 2 u program, rho)
	    | [_] -> Res d_abort
	    | [w; Val_d (Const (Bool b))] ->
		Res w;;

let g_AND_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	AND -> 
	  match l with  
	      [] -> Arg (child 0 u program, rho)
	    | [Val_d (Const (Bool b)) as w] -> 
		if b  
		then Arg (child 1 u program, rho)
		else Res w
	    | [Val_d (Const (Bool b)) as w; _] -> Res w
	    | _ -> Res d_abort;;

let g_OR_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	OR -> 
	  match l with  
	      [] -> Arg (child 0 u program, rho)
	    | [Val_d (Const (Bool b)) as w] -> 
		if b  
		then Res w
		else Arg (child 1 u program, rho)
	    | [Val_d (Const (Bool b)) as w; _] -> Res w
	    | _ -> Res d_abort;;

let g_INFIX_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	INFIX op ->
	  match l with 
	      [] -> Arg (child 0 u program, rho)
	    | [_] -> Arg (child 1 u program, rho)
	    | [Val_d (Const (Int n_2)); Val_d (Const (Int n_1))] -> Res (d_interpretation op (n_1, n_2))
	    | _ -> Res d_abort;;

let g_TUPLE_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	TUPLE n -> 
	  let i = List.length l in 
	    if i = n 
	    then Res (W_Tuple_d (List.rev l))
	    else Arg (child i u program, rho);;

let g_LET_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	LET ids ->
	  match l with 
	      [] -> 
		Arg (child 0 u program, rho)
	    | [w] -> 
		(try 
		   let rho_1 = d_update (LET ids) rho (d_components w) in
		     Arg (child 1 u program, rho_1)
		 with 
		     Number_of_Arguments -> Res d_abort)
	    | [w; _] -> 
		Res w;;

let rec d_add_pointers i u = 
 match label_at u program with 
     TUPLE n -> W_Tuple_d (List.map (fun j -> d_add_pointers i (child j u program)) (interval 0 (n - 1)))
   | _ -> Ptr_d (u, i);; 

let g_LETVAL_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	LETVAL ids ->
	  match l with 
	      [] -> 
		(try 
		   let w = d_add_pointers 1 (child 0 u program) in 
		   let rho' = d_update (LETVAL ids) rho (d_components w) in 
		     Arg (child 1 u program, rho')
		 with 
		     Number_of_Arguments -> Res d_abort)
	    | [w] -> Res w;;

let g_LETREC_d: (d_pair, d_value) dependency = 
 fun (u, rho) l ->
   match label_at u program with 
	LETREC ids -> 
	  match l with 
	      [] -> 
		(try 
		   let w = d_add_pointers 0 (child 0 u program) in 
		   let rho' = d_update (LETREC ids) rho (d_components w) in 
		     Arg (child 1 u program, rho')
		 with 
		     Number_of_Arguments -> Res d_abort)
	    | [w] -> Res w;;

let g_EXN_d: (d_pair, d_value) dependency = 
 fun _ (Except_d ex :: _) -> Res (Except_d ex);;

exception No_rule;;

let rec one_of gs (x: 'arg) (l: 'res list) =
 match gs with 
     g :: gs' ->
	(try 
	   g x l
	 with 
	   Match_failure _ -> one_of gs' x l)
   | [] -> raise No_rule;;

let g_eval_d: (d_pair, d_value) dependency = 
 one_of [g_EXN_d; 
	  g_CONST_d; 
	  g_CLOSURE_d; 
	  g_OP_1_d; 
	  g_OP_2_d; 
	  g_PROJ_d; 
	  g_BETA_V_d; 
	  g_APP_d; 
	  g_INFIX_d; 
	  g_COND_d; 
	  g_AND_d; 
	  g_OR_d; 
	  g_TUPLE_d; 
	  g_INDEX; 
	  g_LET_d; 
	  g_LETVAL_d; 
	  g_LETREC_d];;

(* ------------------------------------------------------------------------------------------- *)
(* Definition der zugehörigen pred- und pos-Funktion                                           *)
(* ------------------------------------------------------------------------------------------- *)

let pred_eval_d (u, rho) = 
 if u < 0 
 then 
   raise No_pred 
 else 
   let u' = parent u program in 
     match label_at u' program with 
	  LAMBDA _ -> raise No_pred
	| LET _    -> u', if u = child 0 u' program then rho else tail_d 1 rho
	| LETREC _ -> u', rho
	| LETVAL _ -> u', rho
	| _        -> u', rho;;

let pos_eval_d (u, eta) = 
 let u' = parent u program in 
   match label_at u' program with 
	LAMBDA _ -> 3
     | LETREC _ -> 1
     | LETVAL _ -> 1
     | IF       -> if u = child 0 u' program then 1 else 2
     | _ -> position u (children u' program) + 1;;

(* ------------------------------------------------------------------------------------------- *)
(* Definition des iterativen Interpreters                                                      *)
(* ------------------------------------------------------------------------------------------- *)

let init_eval_d = 
 root, empty_env_d;;

class interpreter (e: expression) =
object
 inherit [d_pair, d_value] stack_machine g_eval_d pred_eval_d pos_eval_d init_eval_d
 initializer program := syntax_tree e; translate_program (); reset_d_env ()
end;;

class virtual ['a] monitor =
object (m)
 val mutable history: 'a list = []
 method virtual current_state: 'a
 method snapshot = history <- m#current_state :: history
 method history = List.rev history
end;;

type ('arg, 'res) configuration = 
   dot * 'arg * 'arg list * 'res list * d_environment list;;

class animated_interpreter (e: expression) = 
object (sm)
 inherit interpreter e 
 inherit [(d_pair, d_value) configuration] monitor 
 method current_state =
   dot, current_arg, arg_stack#contents, res_stack#contents, ! all_d_envs 
 method animate = 
   (try 
      while true do sm#snapshot; sm#step done
    with 
	 _ -> sm#snapshot);
   sm#history	  
end;;

(* ------------------------------------------------------------------------------------------- *)
(* LaTeX-Ausgabe der Syntaxbäume                                                               *)
(* ------------------------------------------------------------------------------------------- *)

let label_latex (l: s_value label) = 
 let normal s = "\\mbox{\\normalsize " ^ s ^ "}" 
 and small s = "\\mbox{\\small " ^ s ^ "}" in 
   match l with 
	CONST c         -> normal ("$" ^ const_latex c ^ "$")
     | ID id           -> normal ("$" ^ id_latex id ^ "$")
     | INDEX (id, sv)  -> normal ("$" ^ id_latex id ^ "_" ^ s_value_latex sv ^ "$")
     | APP             -> small "APP"
     | IF              -> small "IF"
     | AND             -> small "AND"
     | OR              -> small "OR"
     | INFIX op        -> normal ("$" ^ op_latex op ^ "$")
     | LAMBDA ids      -> normal ("\\begin{tabular}{c} $\\lambda$\\\\ " ^ ids_latex ids ^ "\\end{tabular}")
     | LET ids         -> normal ("\\begin{tabular}{c} " ^ small "LET" ^ "\\\\ " ^ ids_latex ids ^ "\\end{tabular}")
     | LETVAL ids      -> normal ("\\begin{tabular}{c} " ^ small "LETVAL" ^ "\\\\ " ^ ids_latex ids ^ "\\end{tabular}")
     | LETREC ids      -> normal ("\\begin{tabular}{c} " ^ small "LETREC" ^ "\\\\ " ^ ids_latex ids ^ "\\end{tabular}")
     | TUPLE n         -> normal ("\\begin{tabular}{c} " ^ small "TUP" ^ "\\\\ " ^ int_latex n ^ "\\end{tabular}");;

let node_latex (u: address) (cm: color map) (nm: node_map) = 
 let point = coordinate_latex (List.assoc u nm)
 and num = address_latex u
 and label = label_latex (label_at u program)
 and color = node_color u cm in
   "\\rput" ^ point ^ "{\\nnode{" ^ num ^ "}{" ^ label ^ "}{" ^ color ^ "}}";;

let edge_latex (u, v: edge) (cm: color map) = 
 "\\ncline{-}{" ^ address_latex u ^ "}{" ^ address_latex v ^ "}";;

let edges_latex (u: address) (cm: color map) = 
 String.concat "\n" (List.map (fun v -> edge_latex (u, v) cm) (children u program));;

let rec tree_geometry (x, y: coordinate) (u: address): geometry =
 let ch = children u program in 
   match ch with 
     [] -> (2., 0.), [u, (x +. 1., y)]
   | _ -> 
	let (w, h), nm' = forest_geometry (x, y -. 2.) ch in
	let x, _ = List.assoc (List.hd ch) nm'
	and x', _ = List.assoc (last ch) nm' in 
	let mid = (x +. x') /. 2. in 
	  (w, h +. 2.), (u, (mid, y)) :: nm'
and forest_geometry (x, y: coordinate) us: geometry = 
 match us with 
     [] -> (0., 0.), []
   | u :: us' ->
	let (w, h), nm = tree_geometry (x, y) u in 
	let (w', h'), nm' = forest_geometry (x +. w, y) us' in 
	  (w +. w', max h h'), nm @ nm';;

let program_geometry () = 
 let (w, h), nm as g = tree_geometry (0., 0.) 0 in 
   if w > 18. then scale (18. /. w) g else g;;

let program_latex (cm: color map) = 
 let n = Array.length (! program) - 1 in 
 let indices = interval 0 n in 
 let (w, h), nm = program_geometry () in
   "\n\n\\hspace{" ^ float_latex (9. -. w /. 2.) ^ "cm}\n"
   ^ String.concat "\n" (List.map (fun u -> node_latex u cm nm) indices)
   ^ "\n"
   ^ String.concat "\n" (List.map (fun u -> edges_latex u cm) indices)
   ^ "\n\\vspace{" ^ float_latex (h +. 2.) ^ "cm}";;



(* ------------------------------------------------------------------------------------------- *)
(* LaTeX-Ausgabe für Konfigurationen                                                           *)
(* ------------------------------------------------------------------------------------------- *)

let rec d_entries_latex st indices = 
 match st with 
     [] -> []
   | Arg (e, eta) :: st' -> 
	let i = num_d eta 
	and s = "\\argcl{" ^ d_pair_latex (e, eta) ^ "}" in 
	  if List.mem i indices 
	  then s :: d_entries_latex st' indices
	  else s :: d_entries_latex st' (i :: indices)
   | Res w :: st' -> 
	("\\rescl{" ^ d_value_latex w ^ "}") ::  d_entries_latex st' indices;;

let d_stack_latex (st: ('arg, 'res) union list) = 
 "\n\n$\\blong\n"
 ^ String.concat "\\\\\n" (d_entries_latex st [])
 ^ "\n\\elong$\n\n";;

let d_env_list_latex envs = 
 "\n\n$\\blong\n"
 ^ String.concat "\\\\\n" (List.rev_map new_d_env_latex envs)
 ^ "\n\\elong$\n\n";;

let d_configuration_latex (dot, current_arg, arg_stack, res_stack, envs: (d_pair, d_value) configuration) =
 let cm = [fst current_arg, match dot with L -> "green" | R -> "red"], [] in
   page 
     (program_latex cm 
      ^ "\n\n\\vspace{1cm}"
      ^ "\n\n\\begin{tabular}{llll}\n"
      ^ "Aktuelles Argument\\hspace{1cm}"
      ^ " & " 
      ^ "Resultatstack\\hspace{1cm}"
      ^ " & " 
      ^ "Argumentstack\\hspace{1cm}" 
      ^ " & " 
      ^ "Umgebungen" 
      ^ "\\\\[3mm]\n"
      ^ "$" ^ d_pair_latex current_arg ^ "$"
      ^ " & "
      ^ d_stack_latex (List.rev_map (fun b -> Res b) res_stack)
      ^ " & "
      ^ d_stack_latex (List.rev_map (fun a -> Arg a) arg_stack) 
      ^ " & "
      ^ d_env_list_latex envs
      ^ "\\end{tabular}\n\n");;

let print_d_small_steps file_name e = 
 let ai = new animated_interpreter e
 and f = file_name ^ "_d_small" in
   print_latex_file f (String.concat "" (List.map d_configuration_latex ai#animate));
   pdf_latex f;;

(* =========================================================================================== *)
(* Anwendungs=Beispiele                                                                        *)
(* =========================================================================================== *)

let int n = Const (Int n);;
let bool b = Const (Bool b);;
let proj i e = App (Const (Proj i), e);;

let one = int 1;;
let two = Infix (one, Add, one);;
let four = Infix (two, Add, two);;
let if_two = If (Infix (one, Eq, two), one, two);;
let succ = App (Const (Op Add), int 1);;
let id = Lambda (["x"], Id "x");;
let id_one = App (id, one);;
let id_two = App (id, two);;
let id_id = App (id, id);;
let id_id_two = App (id, id_two);;
let let_123 = Infix (Let (["x"], int 1, Infix (Id "x", Add, int 2)), Mul, int 3);;
let square = Lambda (["x"], Infix (Id "x", Mul, Id "x"));;
let sos = Letval (["square"], square, Infix (App (Id "square", int 2), Add, App (Id "square", int 3)));;
let succ_2 = Letval (["succ"], Lambda (["x"], Infix (Id "x", Add, int 1)), App (Id "succ", int 2));;
let sqs = Infix (App (square, int 2), Add, int 1);;
let twice = Lambda (["f"], Lambda (["x"], App (Id "f", App (Id "f", Id "x"))));;
let twice_succ = App (twice, succ);;
let three = App (twice_succ, one);;
let comp = Lambda (["f"], Lambda (["g"], Lambda (["x"], App (Id "g", App (Id "f", Id "x")))));;
let twice_o_twice = Letval (["comp"], comp, Letval (["twice"], twice, App (App (Id "comp", Id "twice"), Id "twice")));;
let five = App (App (twice_o_twice, succ), one);;
let tts_1 = Letval (["twice"], twice, App (App (App (Id "twice", Id "twice"), succ), int 1));;

let _KP_Vermutung = 
 Letval (["f"], Lambda (["g"], App (Id "g", int 0)), Letval (["h"], Lambda (["x"], Id "x"), App (Id "f", Id "h")));;

let information_hiding = 
 Let (["f"], Let (["x"], int 1, Lambda (["y"], Id "x")), App (Id "f", int 0));;

let static_binding = 
 Let (["x"], int 1, Letval (["f"], Lambda (["y"], Infix (Id "x", Add, Id "y")), Let (["x"], int 2, App (Id "f", Id "x"))));;

let local_function = 
 Letval (["f"], Lambda (["x"], Letval (["g"], Lambda (["y"], Infix (Id "x", Add, Id "y")), App (Id "g", int 1))), App (Id "f", int 2));;

let sos = 
 Letval (["square"], 
      Lambda (["x"], Infix (Id "x", Mul, Id "x")), 
      Letval (["sos"], 
	    Lambda (["x"; "y"], Infix (App (Id "square", Id "x"), Add, App (Id "square", Id "y"))),
	    App (Id "sos", Tuple [int 2; int 3])));;

let fact_body = Lambda (["x"], If (Infix (Id "x", Eq, int 0), int 1, Infix (Id "x", Mul, App (Id "fact", Infix (Id "x", Sub, int 1)))));;
let fact n = Letrec (["fact"], fact_body, App (Id "fact", int n));;

let fact_2_1 = Letrec (["fact"], fact_body, Tuple [App (Id "fact", int 2); App (Id "fact", int 1)]);;
let snd_fact_2_1 = proj 2 fact_2_1;;

let even_body = Lambda (["x"], Or (Infix (Id "x", Eq, int 0), App (Id "odd", Infix (Id "x", Sub, int 1))));;
let odd_body = Lambda (["x"], And (Infix (Id "x", Neq, int 0), App (Id "even", Infix (Id "x", Sub, int 1))));;

let even n = 
 Letrec (["even"; "odd"], Tuple [even_body; odd_body], App (Id "even", int n));;

let odd n = 
 Letrec (["even"; "odd"], Tuple [even_body; odd_body], App (Id "odd", int n));;

let exp_body = 
 Lambda (["x"; "y"], If (Infix (Id "y", Eq, int 0), int 1, Infix (Id "x", Mul, App (Id "exp", Tuple [Id "x"; Infix (Id "y", Sub, int 1)]))));;
let exp m n = Letrec (["exp"], exp_body, App (Id "exp", Tuple [int m; int n]));;

let gcd_body = 
 Lambda (["x"; "y"], If (Infix (Id "y", Eq, int 0), Id "x", App (Id "gcd", Tuple [Id "y"; Infix (Id "x", Mod, Id "y")])));;
let gcd m n = 
 Letrec (["gcd"], gcd_body, App (Id "gcd", Tuple [int m; int n]));;

let diverge_body = 
 Lambda (["x"], If (Infix (Id "x", Eq, int 0), int 0, App (Id "f", Infix (Id "x", Sub, int 1))));;
let diverge = 
 Letrec (["f"], diverge_body, App (Id "f", int (20)));;

let stuck_1 = App (Lambda (["x"], Id "x"), App (int 1, int 2));;
let stuck_2 = If (int 1, int 2, int 3);;
let stuck_3 = If (bool true, App (int 1, int 2), int 3);;
let stuck_4 = If (bool false, App (int 1, int 2), int 3);;
let stuck_5 = Infix (int 1, Add, bool true);;
let stuck_6 = App (Lambda (["x"; "y"], Id "x"), int 1);;
let stuck_7 = Letrec (["f"; "g"], Lambda (["x"], int 0), int 1);;

print_d_small_steps "four" four;;
print_d_small_steps "id_id_two" id_id_two;;
print_d_small_steps "let_123" let_123;;
print_d_small_steps "sos" sos;;
print_d_small_steps "tts_1" tts_1;;
print_d_small_steps "twice_o_twice" twice_o_twice;;

print_d_small_steps "KP_Vermutung" _KP_Vermutung;;
print_d_small_steps "information_hiding" information_hiding;;
print_d_small_steps "static_binding" static_binding;;
print_d_small_steps "local_function" local_function;;

print_d_small_steps "fact_0" (fact 0);;
print_d_small_steps "fact_1" (fact 1);;
print_d_small_steps "fact_2" (fact 2);;
print_d_small_steps "fact_3" (fact 3);;
print_d_small_steps "snd_fact" snd_fact_2_1;;

print_d_small_steps "exp_2_3" (exp 2 3);;
print_d_small_steps "exp_3_2" (exp 3 2);;

print_d_small_steps "even_1" (even 1);;
print_d_small_steps "even_2" (even 2);;
print_d_small_steps "odd_1" (odd 1);;
print_d_small_steps "odd_2" (odd 2);;

print_d_small_steps "gcd_20_12" (gcd 20 12);;

print_d_small_steps "stuck_1" stuck_1;;
print_d_small_steps "stuck_2" stuck_2;;
print_d_small_steps "stuck_3" stuck_3;;
print_d_small_steps "stuck_4" stuck_4;;
print_d_small_steps "stuck_5" stuck_5;;
print_d_small_steps "stuck_6" stuck_6;;
print_d_small_steps "stuck_7" stuck_7;;
