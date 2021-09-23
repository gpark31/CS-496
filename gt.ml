(* 

   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place the names of the group members here:

    Name1: Luca Pieples
    Name2: GaYoung Park
    I pledge my honor that I have abided by the Stevens Honor System.

*)

type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])

let rec height t =
  match t with
    | Node (a, []) -> 1
    | Node (a, l) -> 1 + List.fold_left max 0 (List.map height l)
    
let rec size t =  
  match t with 
    | Node(_,l) -> List.fold_left (fun n t1 -> n + size t1) 1 l

let paths_to_leaves_helper i l = 
  List.map (fun sl -> i::sl) l

let rec paths_to_leaves t =
  match t with
    | Node(_, []) -> [[]]
    | Node(_, l) -> List.flatten (List.mapi paths_to_leaves_helper (List.map paths_to_leaves l))

let rec perfect_helper l =
  match l with
  | [] | [_] -> true
  | n::m::t -> n=m && perfect_helper (m::t)

let rec is_perfect t =
  perfect_helper (List.map List.length (paths_to_leaves t))

let rec preorder (Node(d,ch)) =
  match ch with
    | [] -> [d]
    | h::t -> d::(List.flatten (List.map preorder ch))
                
let rec mirror (Node(d,ch)) =
  match ch with
    | [] -> Node(d, ch)
    | h::t -> Node(d, (List.rev (List.map mirror ch)))

let rec mapt f (Node(d,ch)) =
  match ch with
    | [] -> Node((f d), [])
    | h::t -> Node((f d), (List.map (mapt f) ch))
  
let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
  match ch with
    | [] -> f d []
    | h::t -> f d (List.map (foldt f) ch)

let sumt t =
  foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

let memt t e = 
  foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t

let mirror' t  = 
  foldt (fun i rs -> Node(i, (List.rev rs))) t
