open EventQueue
open Network
open Extensions

let already_received u d time queue = ()

let just_received v time queue = ()

let vertex_step g v time queue = ()

let step g time queue =
  let vertices = Network.vertices g in
  let u = List.random vertices in
  let remaining = List.filter (fun w -> not (w = u)) vertices in
  let v = List.random remaining in 
  let uv = G.find_edge g u v in
  let len_uv = edge_length uv in
  let vu = G.find_edge g v u in
  let len_vu = edge_length vu in
  (push
     (time + len_uv)
    (Message (Transfer (uv, (Node.received u))))
    queue;
  push
    (time + len_vu)
    (Message (Transfer (vu, (Node.received u))))
    queue;
  push (time + 10) Step queue)
