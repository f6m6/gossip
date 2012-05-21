type time = int

type message =
    Transfer of Network.G.E.t * bool
  | Fail of Network.G.vertex

type event =
    Message of message
  | VertexStep of Network.G.vertex
  | Step ;;

type 'a queue
val is_traffic: event -> bool
val get_traffic: event queue -> int
val push: time -> event -> event queue -> unit
val pop: event queue -> time * event
val init: time -> event -> event queue
exception Queue_is_empty
