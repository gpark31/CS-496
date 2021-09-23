
(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
   GaYoung Park
   Date: 2/21/2021
   I pledge my honor that I have abided by the Stevens Honor System.
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2"); ("q3",'a',"q4")];
          final= ["q2"]
         }
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* ******************************************** *)
(*Simulating automata *)
(* ******************************************** *)

let create_automaton (s: state list) (st: state) (tran: tf) (f: state list) : fa =
	{states = s; start = st; tf = tran; final = f}
	
(* DONE applies the transition function and returns the following state*)
let rec apply_transition_function (f: tf) (st: state) (sym: symbol) : state option = 
  match f with
  | [] -> None
  | (a, b, c)::t -> 
    if a = st && b = sym
    then Some c
    else apply_transition_function t st sym

(* DONE remove duplicate items in the list *)
let rec remove_duplicate (l: 'a list) : 'a list = 
	match l with 
	| [] -> []
	| h::t -> 
		if List.mem h t
		then remove_duplicate t 
		else h::remove_duplicate t 
	
(* prints a list of tuples (start state, final state) when given b *)
let rec accept_helper (trans: tf) (s: symbol) : state list =
	match trans with 
	| [] -> []
	| (a,b,c)::t -> 
		if s = b 
		then a::c::accept_helper t  s
		else accept_helper t s
	
let rec accept_helper2 (a: fa) (i: input): state list =
	match i with 
	| [] -> []
	| h::t -> 
		(accept_helper a.tf h)@(accept_helper2 a t)
	
let rec accept_helper3 (stl: state list) (len: int) : bool = 
	if (len < 2) then true
	else if ((List.nth stl (len)) = (List.nth stl (len-1)))
		then accept_helper3 stl (len-2)
		else false

(* Determines whether a word is accepted by a finite automataton *) 
let accept (a: fa) (i: input) : bool =
	let slist = (accept_helper2 a i) in
	let len = ((List.length slist) - 1) in
	if ((List.nth slist 0) = a.start) && (List.mem (List.nth slist len) a.final)
		then if accept_helper3 slist (len-1)
			then true
			else false
	else false


(* DONE returns the sucessors of the state in a list
 case of a loop?  q0 -> q1 && q1-> q0 member check *)
let rec next (trans: tf) (s: state) (sym: symbol) : state list = 
  match trans with 
  | [] -> []
  | (a,b,c)::t -> 
    if (a = s) && (b = sym) 
    then c::(next t s sym)
    else (next t s sym)

let rec d_helper (trans: tf) : bool =
	let copy = trans in
	match trans with 
	| [] -> true
	| (a,b,c)::t -> 
		if (List.length (next copy a b)) > 1
		then false
		else d_helper t
	
(* determines whether the automaton is deterministic or not *)
let deterministic (a: fa) : bool = 
	d_helper a.tf
	
(* DONE determines if the final states are in the list of states *)
let rec valid_helper (states: state list) (final: state list) : bool = 
  match final with
  | [] -> true
  | h::t -> 
    if List.mem h states
    then valid_helper states t 
    else false
	
(* DONE determines if the start and final states belong to set of states and if deterministic *)
let rec valid (a: fa) : bool =
	if (List.mem a.start a.states) && deterministic a && (valid_helper a.states a.final)
	then true
	else false
  
let rec diff l1 l2 = 
	match l1 with 
	| [] -> []
	| h::t when (List.mem h l2) -> diff t l2
	| h::t -> h:: (diff t l2)
	
let rec outgoing (lst: tf)(target: state) : state list = 
	match lst with 
	| [] -> []
	| (a,b,c)::t -> 
		if a = target 
		then c::outgoing t target
		else outgoing t target
		
let reachable (a: fa) : state list =
	let rec reachable_helper (visited: state list) (current: state list) : state list = 
		match current with 
		| [] -> visited 
		| h::t -> reachable_helper (h::visited) ((diff (outgoing a.tf h) visited) @ t)
	in
	remove_duplicate (reachable_helper [] [a.start])

let rec new_states (l: state list) (sl: state list) : state list = 
	match sl with 
	| [] -> []
	| h::t -> 
		if (List.mem h l)
		then h::new_states l t
		else new_states l t
		
let rec new_transitions (trans: tf) (sl: state list) : tf = 
	match trans with 
	| [] -> []
	| (a,b,c)::t -> 	
		if (List.mem a sl) && (List.mem c sl) 
		then (a,b,c)::new_transitions t sl
		else new_transitions t sl

(* removes unused states *)
let remove_dead_states (a: fa) : fa =
  let reach = reachable a
  in 
  create_automaton (new_states a.states reach) a.start (new_transitions a.tf reach) (new_states a.final reach)