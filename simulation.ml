open Network 
open EventQueue
open Extensions
open AlgorithmPack

module Simulator(Algorithm: ALGORITHM) =
  struct
    let clock = ref 0
    let elapsed () = !clock
    let run g t =
      let queue = init 0 Step in

      let process time message = match message with
	Transfer (e, msg) ->
	  let u = G.E.src e in
	  let v = G.E.dst e in
	  let d = Node.id (G.E.label e) in
	  if (Node.received v) then
	    Algorithm.already_received u d time queue
	  else
	    if msg then
	      let _ = Node.receive v time in
	      Algorithm.just_received v time queue
	    else
	      ()
      | Fail v -> Node.fail v in

      let eval time event = 

	match event with
	  Message message -> process time message
	| VertexStep v -> 
	    Algorithm.vertex_step g v time queue
	    
	| Step ->
	    Algorithm.step g time queue
      in
      let _ = 
	try
	  let exists_susceptible g =
	    let p v b = b || not (Node.received v) in
	    G.fold_vertex p g false in
	  while exists_susceptible g do
	    let (time, event) = pop queue in
	    clock := time;
	    eval time event;
	  done;
	with Queue_is_empty -> ()
      in elapsed()
  end
