open Network

module type TRANSFER =
  sig
    val transfer: G.E.t -> int -> (G.vertex, G.E.t) EventQueue.queue -> unit
  end 

module Push: TRANSFER =
  struct
    let transfer edge time queue = 
       let label = Network.G.E.label edge in
       let len = Node.id label in
       let src = Network.G.E.src edge in
       let dst = Network.G.E.dst edge in
       Node.receive src dst time len queue
  end

(* module PushPull: TRANSFER =
  struct
    let transfer v1 v2 t = ()
  end
 *)
