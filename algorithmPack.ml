open Network
open SelectPack
open TransferPack
open Printf

module type ALGORITHM = 
    sig
      val step: G.t -> G.vertex -> (G.vertex, G.E.t) EventQueue.queue -> int -> unit
      val transfer: G.E.t -> int ->( G.vertex, G.E.t) EventQueue.queue -> unit
    end

module Algorithm(T: TRANSFER)(S: SELECT): ALGORITHM =
  struct
    type time = int
    let transfer = T.transfer 
    let select = S.select
    let period = 1000
    let rec step g vtx q time =
      let edges = select g vtx in
      List.iter
	(fun edge ->
	  let label = Network.G.E.label edge in
	  let len = Node.id label in
	  let exec_time = time + len in
	  let new_event = EventQueue.Transfer(edge) in
	  EventQueue.push exec_time new_event q;
	  if (Node.active vtx)
	  then
	    EventQueue.push (time + period) (EventQueue.Step(vtx)) q
	  else ()
	)
	edges
  end

module Serial = Algorithm(Push)(AllSuccessors)
module Gossip = Algorithm(Push)(RandomSuccessor)
