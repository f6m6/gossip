open Network

(* for ntwk evnts - info transfer & fail messages for gossip protocol *)
type message =
    Transfer of G.E.t * bool
  | Fail of G.vertex

(* simulation events - cd be message or local/global "step" functions *)
type event =
    Message of message
  | VertexStep of G.vertex
  | Step ;;

(* tells whether an event is a network event *)
let is_traffic event = match event with
    Message _ -> true
  | _ -> false

(* monotonic integer representation of time *)
type time = int

(* event queue is mutable: ref to tuple of priority queue and traffic as int *)
type 'a queue = ('a PriorityQueue.queue * int) ref

(* gets current number of network events in queue *)
let get_traffic queue = snd(!queue)

(* push an event due at time to queue *)
let push time event queue =
  let pqueue = fst(!queue) in
  let traffic = snd(!queue) in
  let new_queue = PriorityQueue.insert pqueue time event in
  queue := (new_queue, traffic + if (is_traffic event) then 1 else 0)

(* exception used to signal emptiness of queue *)
exception Queue_is_empty

(* pops queue i.e. destructively returns top of queue *)
let pop queue =
  let pqueue = fst(!queue) in
  let traffic = snd(!queue) in
  try
    let (time, event, new_queue) = PriorityQueue.extract pqueue in
    let _ = queue := (new_queue, traffic + if (is_traffic event) then -1 else 0) in
    (time, event)
  with
    PriorityQueue.Queue_is_empty -> raise Queue_is_empty

(* constructor - initialises queue with event *)
let init time event =
  ref ((PriorityQueue.insert PriorityQueue.empty time event), 0)
