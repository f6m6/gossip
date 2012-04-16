open EventQueue
open Network
open Extensions

let already_received u d time queue =
  push (time + d) (Message (Fail u)) queue

let just_received v time queue =
  push time (VertexStep v) queue

let vertex_step g v time queue =
  let outgoing_edges = G.succ_e g v in
  let random_edge = List.random outgoing_edges in
  let length = edge_length random_edge in
  push
    (time + length)
    (Message (Transfer (random_edge, (Node.received v))))
    queue;
  if (Node.fail_count v < 3) then
    push (time + 10) (VertexStep v) queue
  else ()

let step g time queue =
  let vertices = Network.vertices g in
  let with_msg = List.filter Node.received vertices in
  let v = List.hd with_msg in
  push time (VertexStep v) queue
