open EventQueue
open Network

module type ALGORITHM = 
    sig
      val already_received: G.vertex -> int -> int -> event queue -> unit
      val just_received: G.vertex -> int -> event queue -> unit
      val vertex_step: G.t -> G.vertex -> int -> event queue -> unit
      val step: G.t -> int -> event queue -> unit
    end
