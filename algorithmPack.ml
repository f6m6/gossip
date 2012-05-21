open EventQueue
open Network

(* signature for algorithms to be run in simulator *)
module type ALGORITHM =
    sig
      val already_received: G.vertex -> int -> time -> event queue -> unit
      val just_received: G.vertex -> time -> event queue -> unit
      val vertex_step: G.t -> G.vertex -> time -> event queue -> int -> int -> unit
      val step: G.t -> time -> event queue -> int -> unit
      val name: string
    end
