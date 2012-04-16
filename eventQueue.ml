open Network

type message =
    Transfer of G.E.t * bool
  | Fail of G.vertex

type event = 
    Message of message
  | VertexStep of G.vertex
  | Step ;;

type time = int

type 'a queue = 'a PriorityQueue.queue ref
      
let push time event queue = 
  let new_queue = PriorityQueue.insert !queue time event in
  queue := new_queue
      
exception Queue_is_empty
    
let pop queue =
  try
    let (time, event, new_queue) = PriorityQueue.extract !queue in
    let _ = queue := new_queue in
    (time, event) 
  with
    PriorityQueue.Queue_is_empty -> raise Queue_is_empty
	
let init time event =
  ref (PriorityQueue.insert PriorityQueue.empty time event)

