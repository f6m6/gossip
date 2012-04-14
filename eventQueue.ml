type time = int
type ('a, 'b) event = Fail of 'a | Step of 'a | Transfer of 'b
type ('a, 'b) queue = ('a, 'b) event PriorityQueue.queue ref
      
let push (time: time) (event: ('a, 'b) event) (queue:('a, 'b) queue) = 
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

