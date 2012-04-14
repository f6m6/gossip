open Network
open AlgorithmPack
open Printf
open EventQueue

module type SIMULATOR =
  sig
    type time = int
    val run: G.t -> time -> bool -> int
  end

module Simulator(A : ALGORITHM) : SIMULATOR =
  struct
    type time = int
    let time = ref 0


    let run g t stop_cond  =
      let _ = try
	let v = List.find (Node.received) (Network.vertices g) in
	let queue = EventQueue.init 0 (EventQueue.Step(v)) in
	while (true) do
	  let t, e = EventQueue.pop queue in
	  time := t;
	  let _ = match e with
	    Step(v) -> A.step g v queue t
	  | Fail(v) -> Node.fail v
	  | Transfer(e) -> A.transfer e t queue  in
	  ()
	done
      with EventQueue.Queue_is_empty -> () in
      !time
  end
