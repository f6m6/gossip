open Network

module type SELECT =
  sig 
    type time
    val select: G.t -> G.vertex -> G.E.t list
  end

module AllSuccessors: SELECT =
  struct
    type time = int
    let select g v = G.succ_e g v
  end

open Extensions

module RandomSuccessor: SELECT =
  struct
    type time = int
    let select g v = [ List.random (G.succ_e g v) ] 
  end

(*module RandomPair: SELECT = 
  struct
    type time = int

    let select g v =
      let v1 = List.random (vertices g) in
      let v2 = List.random (vertices g) in
      (v1, [ v2 ] )
  end
*)
