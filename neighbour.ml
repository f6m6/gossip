open EventQueue
open Network
open Extensions

let name = "neighbour"

(*
   push a fail message onto the queue if someone
   sends me the message when I already have it
*)
let already_received u d time queue =
  push (time + d) (Message (Fail u)) queue

(*
  when I receive for the first time,
  push my (first) periodic local step onto the queue
*)
let just_received v time queue =
  push time (VertexStep v) queue

(*
  my local periodic behaviour is nothing if my
  fail count has exceeded failure tolerance
  otherwise I transfer along a random
  outgoing edge and schedule my next vertex step
  with n neighbours, the probability of choosing
  the nearest is 1/2 and then the probability of choosing
  any one of the others is 1/2n (i.e. uniform over the others)
*)
let vertex_step g v time queue ft period =
  if (Node.fail_count v < ft) then
    let outgoing_edges = G.succ_e g v in
    let compare_edges e1 e2 = (edge_length e1) - (edge_length e2) in
    let sorted_edges = List.sort compare_edges outgoing_edges in
    let shortest_edge = List.hd sorted_edges in
    let random_edge =
      let _ = Random.self_init () in
      if Random.bool ()
      then shortest_edge
      else List.random (
        List.filter (fun x -> not (x = shortest_edge)) outgoing_edges
      ) in
    let length = edge_length random_edge in
    push
      (time + length)
      (Message (Transfer (random_edge, (Node.received v))))
      queue;
    push (time + period) (VertexStep v) queue
  else ()

(*
  my global step, which only happens once at the start,
  is simply to push the first vertex step at the node
  which is the injection point / originator
*)
let step g time queue period =
  let vertices = Network.vertices g in
  let with_msg = List.filter Node.received vertices in
  let v = List.hd with_msg in
  push time (VertexStep v) queue
