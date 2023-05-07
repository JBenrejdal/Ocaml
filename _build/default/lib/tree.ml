open Base

type ('v, 'w) t =
  | Node of 'v * ('v,'w) t * ('v,'w) t
  | Leaf of 'w [@@deriving show]

let return (w:'w) = Leaf w 

let combine (v:'v )(l1:('v,'w)t) (l2:('v,'w)t) = Node (v, l1, l2)

let%test "n" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine 4 l1 l2 in
  let n2 = combine 5 n1 l3 in
  Stdlib.(n2 = (Node(5,Node(4, Leaf 1, Leaf 2), Leaf 3)))

let is_leaf (l:('v,'w)t) = match l with
  | Leaf _ -> true
  | Node _ -> false;;

let%test "leaf1" = is_leaf (Leaf 1)
let%test "leaf2" = is_leaf (Node (1, Leaf 1, Leaf 1)) |> not



let get_leaf_data (l:('v,'w)t) = match l with
  | Node _ -> None
  | Leaf w -> Some w;;

let%test "gld1" =  match get_leaf_data (Leaf 1) with
  | None -> false
  | Some o -> Int.(o = 1)


let%test "gld2" = match get_leaf_data (Node (1, Leaf 2, Leaf 3)) with
  | None -> true
  | _ -> false


let get_node_data (l:('v,'w)t) = match l with
  | Leaf _ -> None
  | Node (v, _, _) -> Some v

let%test "gnd1" =  match get_node_data (Leaf 1) with
  | None -> true
  | _ -> false


let%test "gnd2" = match get_node_data (Node (1, Leaf 2, Leaf 3)) with
  | None -> false
  | Some o -> Int.(o = 1)


let rec map (f,g) d =   match d with
  | Leaf w -> Leaf (g w)
  | Node (v, l, r) -> Node (f v, map(f, g) l, map(f, g) r)

let%test "map" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine "four" l1 l2 in
  let n2 = combine "five" n1 l3 in
  let g x = x * 2 in
  let f x = x ^ x in
  let n3 = map (f,g) n2 in
  Stdlib.(n3 = (Node("fivefive",Node("fourfour", Leaf 2, Leaf 4), Leaf 6)))


let rec iter (f,g) d = match d with
  | Leaf w -> g w
  | Node (v, l, r) ->
      f v;
      iter (f, g) l;
      iter (f, g)r


type ('v, 'w) z = TZ of ('v,'w) context * ('v,'w) t
and ('v,'w) context =
  | Top
  | LNContext of 'v * ('v,'w) context * ('v,'w) t
  | RNContext of ('v,'w) t * 'v * ('v,'w) context [@@deriving show]



let from_tree d = TZ (Top,d)



let change (TZ(c, _):('v,'w)z) s = TZ(c, s)

let change_up (TZ(c , t):('v,'w)z) (nw:'v) =  match c with
  | Top -> failwith "Cannot change value at top of tree"
  | LNContext(_, c, r) -> TZ(c, Node(nw, r, t))
  | RNContext(l, _, c) -> TZ(c, Node(nw, l, t))

let go_down (TZ(c,t):('v,'w)z) = match t with
| Leaf _ -> None
| Node(n,l , r) -> Some (TZ(LNContext(n, c, r), l))

let go_down_r (TZ(c,t):('v,'w)z) = match t with
| Leaf _ -> None
| Node(n,l , r) -> Some (TZ(RNContext(l, n, c), r))
let%test "gd1" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine "four" l1 l2 in
  let n2 = combine "five" n1 l3 in
  let z = from_tree n2 in
  match go_down z with
  | Some z' -> Stdlib.(z' = TZ (LNContext ("five", Top,Leaf 3),Node("four", Leaf 1, Leaf 2)))
  | None -> false

let%test "gd2" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine "four" l1 l2 in
  let n2 = combine "five" n1 l3 in
  let z = from_tree n2 in
  match Option.(Some z >>= go_down >>= go_down) with
  | Some z' -> Stdlib.(z' = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1))
  | None -> false


let%test "gd3" =
  let l1 = return 1 in
  let l2 = return 2 in
  let l3 = return 3 in
  let n1 = combine "four" l1 l2 in
  let n2 = combine "five" n1 l3 in
  let z = from_tree n2 in
  match Option.(Some z >>= go_down >>= go_down >>= go_down) with
  | Some _ -> false
  | None -> true

let go_up (TZ(c,t):('v,'w)z)=   match c with
  | Top -> None
  | LNContext(v, c', r) -> Some (TZ(c', Node(v, t, r)))
  | RNContext(l, v, c') -> Some (TZ(c', Node(v, l, t)))


let%test "gu1" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  match go_up z with
  | Some z' -> Stdlib.(z' = TZ (LNContext ("five", Top,Leaf 3),Node("four", Leaf 1, Leaf 2)))
  | None -> false

let%test "gu2" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  match Option.(Some z >>= go_up >>= go_up) with
  | Some z' -> Stdlib.(z' = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)))
  | None -> false

let%test "gu3" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  match Option.(Some z >>= go_up >>= go_up >>= go_up) with
  | Some _  -> false
  | None -> true

let go_left (TZ(c,t):('v,'w)z)=   match c with
  | Top -> None
  | LNContext(_, _, _) -> None
  | RNContext(l, v, c) -> Some (TZ(LNContext(v, c, t), l))


let%test "gl1" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  match go_left z with
  | Some z' -> Stdlib.(z' = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1))
  | None -> false

let%test "gl2" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match go_left z with
  | Some _ -> false
  | None -> true

  let go_right (TZ(c,t):('v, 'w) z)=   match c with
  | Top -> None
  | LNContext(v, c, r) -> Some (TZ(RNContext(t, v, c), r))
  | RNContext(_, _, _) -> None

let%test "gr1" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match go_right z with
  | Some z' -> Stdlib.(z' = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2))
  | None -> false

let%test "gl2" =
let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
match go_right z with
  | Some _ -> false
  | None -> true


let rec reflexive_transitive f z = match f z with
  | None -> z
  | Some z' -> reflexive_transitive f z'

let%test "rf1" =
  let z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2) in
  Stdlib.(reflexive_transitive go_up z = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)))

let%test "rf2" =
  let z =   TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  Stdlib.(reflexive_transitive go_up z = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)))


let%test "rf3" =
  let z = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)) in
  Stdlib.(reflexive_transitive go_up z = z)

let focus_first_leaf (t:('v,'w)t) = match go_down(TZ(Top,t)) with
  |None -> TZ(Top,t)
  |Some z-> reflexive_transitive go_down(z)


let%test "ffl1" =
  let t = Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3) in
  Stdlib.(focus_first_leaf t = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1))


  let remove_leaf (TZ(c,t):('v, 'w) z)=   match c with
  | Top -> None
  | LNContext(v, c', r') ->(match t with 
    | Node(_, _, _) -> None
    | Leaf _ -> Some ((TZ(c',r')),v))
  | RNContext(l', v, c') ->(match t with
    | Node(_, _, _) -> None
    | Leaf _ -> Some ((TZ(c',l')),v))
    
  


    let%test "rl1" =
    let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
    match remove_leaf z with
    | None -> false
    | Some (z, v) -> Stdlib.((z = TZ (LNContext ("five", Top, Leaf 3), Leaf 2)) && (v="four"))
  
  
  let%test "rl2" =
    let z = TZ (LNContext ("five", Top,Leaf 3),Node("four", Leaf 1, Leaf 2)) in
    match remove_leaf z with
    | None -> true
    | _ -> false
  



let is_left_context  (TZ(c,_)) =  match  c with
| Top -> false
| LNContext(_, _, _) -> true
| RNContext(_, _, _) -> false

let is_right_context (TZ(c,_)) =  match  c with
| Top -> false
| LNContext(_, _, _) -> false
| RNContext(_, _, _) -> true

let is_top_context   (TZ(c,_)) =  match  c with
| Top -> true
| LNContext(_, _, _) -> false
| RNContext(_, _, _) -> false



let rec move_until f p z = match p z with
  | true -> Some z
  | false -> 
    match f z with
    | None -> Some z
    | Some z' -> move_until f p z'


let%test "mv1" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  let p = fun (TZ(_,s)) -> match get_node_data s with | None -> false | Some v -> String.(v = "five") in
  match move_until go_up p z with
  | None -> false
  | Some z -> Stdlib.(z = TZ( Top,  Node("five",Node("four", Leaf 1, Leaf 2), Leaf 3)))

let%test "mv2" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  let p = fun (TZ(_,s)) -> match get_node_data s with | None -> false | Some v -> String.(v = "four") in
  match move_until go_up p z with
  | None -> false
  | Some z -> Stdlib.(z = TZ (LNContext ("five", Top,Leaf 3),Node("four", Leaf 1, Leaf 2)))



  let rec go_topr(TZ(c,t)) = match  go_up  (TZ(c,t)) with
  |None -> None
  |Some z -> match go_right z with
      |None -> go_topr z
      |Some r ->Some (reflexive_transitive go_down r)


                
let next_leaf(TZ(c,t)) :('v,'w) z option =  match t with
    |Node(_,_,_) -> Some (reflexive_transitive go_down_r (TZ(c,t)))
    |Leaf _ ->  match go_right(TZ(c,t)) with 
          | Some (TZ(c',t')) -> (match t' with
                | Node(_,_,_) -> Some (reflexive_transitive go_down (TZ(c',t')) )
                | Leaf _ -> Some (TZ(c',t')))
          | None ->  go_topr (TZ(c,t))
                              
        
let%test "nl1" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match next_leaf z with
  | None -> false
  | Some z -> Stdlib.(z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2))

let%test "nl2" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match Option.(Some z >>= next_leaf >>= next_leaf) with
  | None -> false
  | Some z -> Stdlib.(z = TZ (RNContext (Node ("four", Leaf 1, Leaf 2), "five", Top), Leaf 3))


let%test "nl3" =
  let z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1) in
  match Option.(Some z >>= next_leaf >>= next_leaf >>= next_leaf) with
  | None -> true
  | _ -> false

let rec go_topl(TZ(c,t)) = match go_up (TZ(c,t)) with
  |None -> None
  |Some z -> match go_left (z) with
      |None-> go_topl z
      |Some r ->Some (reflexive_transitive go_down_r r)
      
    
let previous_leaf(TZ(c,t)) :('v,'w) z option =  match t with


  |Node(_,_,_) -> Some (reflexive_transitive go_down (TZ(c,t)))
  |Leaf _ ->  match go_left(TZ(c,t)) with 
      | Some (TZ(c',t')) -> (match t' with
          | Node(_,_,_) -> Some (reflexive_transitive go_down_r (TZ(c',t')) )
          | Leaf _ -> Some (TZ(c',t')))
      | None ->  go_topl (TZ(c,t))
          
let%test "pl1" =
  let z = TZ (RNContext (Node ("four", Leaf 1, Leaf 2), "five", Top), Leaf 3) in
  match previous_leaf z with
  | None -> false
  | Some z -> Stdlib.(z = TZ(RNContext(Leaf 1, "four", LNContext ("five", Top,Leaf 3)), Leaf 2))

let%test "pl2" =
  let z = TZ (RNContext (Node ("four", Leaf 1, Leaf 2), "five", Top), Leaf 3) in
  match Option.(Some z >>= previous_leaf >>= previous_leaf) with
  | None -> false
  | Some z -> Stdlib.(z = TZ(LNContext("four", LNContext ("five", Top,Leaf 3), Leaf 2), Leaf 1))


let%test "pl3" =
  let z = TZ (RNContext (Node ("four", Leaf 1, Leaf 2), "five", Top), Leaf 3) in
  match Option.(Some z >>= previous_leaf >>= previous_leaf >>= previous_leaf) with
  | None -> true
  | _ -> false
