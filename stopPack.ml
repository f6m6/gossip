open Network

module type STOP =
  sig
    val condition: G.t -> bool
  end

module AllReceived: STOP =
  struct
    let condition g  =
      let p v b = b && Node.received v in
      G.fold_vertex p g true
  end

module NoContributors: STOP =
  struct
    let condition g =
      let p v b = b || Node.active v && Node.received v in
      let exists_contributor = G.fold_vertex p g false in
      not exists_contributor
  end
