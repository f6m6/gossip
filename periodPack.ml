open Network

module type PERIOD =
  sig
    val period: G.vertex -> int -> bool
  end

module Once: PERIOD =
  struct
    let period v t = ((time_received v) = 0)
  end

