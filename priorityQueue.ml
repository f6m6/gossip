(* integer for priority, smaller is more important *)
type priority = int

(* binary tree representation of priority queue *)
type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

(* alias for empty queue *)
let empty = Empty

(* insert element with priority by traversing tree *)
let rec insert queue prio elt =
  match queue with
    Empty -> Node(prio, elt, Empty, Empty)
  | Node(p, e, left, right) ->
      if prio <= p
      then Node(prio, elt, insert right p e, left)
      else Node(p, e, insert right prio elt, left)

(* exception for empty queue *)
exception Queue_is_empty

(* remove top returns queue without top element.
   rebalances tree, preserving priority order
 *)
let rec remove_top = function
    Empty -> raise Queue_is_empty
  | Node(prio, elt, left, Empty) -> left
  | Node(prio, elt, Empty, right) -> right
  | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
         (Node(rprio, relt, _, _) as right)) ->
           if lprio <= rprio
           then Node(lprio, lelt, remove_top left, right)
           else Node(rprio, relt, left, remove_top right)

(* returns 3-tuple of priority of top elt, top elt and queue without top elt *)
let extract = function
    Empty -> raise Queue_is_empty
  | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
