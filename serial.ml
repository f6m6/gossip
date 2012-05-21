open EventQueue
open Network
open Extensions

let name = "serial"

let already_received u d time queue = ()

let just_received v time queue = ()

let vertex_step g v time queue ft period = ()

(*
  top down so other functions are ().
  push simultaneous events forwarding from originator
  to all other nodes
*)
let step g time queue period =
  let vertices = Network.vertices g in
  let with_msg = List.filter Node.received vertices in
  let v = List.hd with_msg in
  let outgoing_edges = G.succ_e g v in
  List.iter
    (fun edge ->
      let length = edge_length edge in
      push
        (time + length)
        (Message (Transfer(edge, (Node.received v))))
        queue;)
    outgoing_edges
