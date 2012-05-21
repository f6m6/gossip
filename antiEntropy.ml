open EventQueue
open Network
open Extensions

let name = "antientropy"

let already_received u d time queue = ()

let just_received v time queue = ()

let vertex_step g v time queue ft period = ()

(*
  anti-entropy is totally top-down so other functions are unit value.
  choose two distinct random vertices and "unify"
  i.e. each one sends what it has to the other
  note that since the graph is directed the link is asymmetric
*)
let step g time queue period =
  let _ = Random.self_init () in
  let vertices = Network.vertices g in
  let u = List.random vertices in
  let remaining = List.filter (fun w -> not (w = u)) vertices in
  let v = List.random remaining in
  let uv = G.find_edge g u v in
  let len_uv = edge_length uv in
  let vu = G.find_edge g v u in
  let len_vu = edge_length vu in
  push
     (time + len_uv)
    (Message (Transfer (uv, (Node.received u))))
    queue;
  push
    (time + len_vu)
    (Message (Transfer (vu, (Node.received u))))
    queue;
  if (exists_susceptible g)
  then push (time + period) Step queue
  else ()
