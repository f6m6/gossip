open Network
open EventQueue
open Extensions
open AlgorithmPack
open Printf

module Simulator(Algorithm: ALGORITHM) =
  struct
    (* initialise clock to zero *)
    let clock = ref 0

    (* function to get current clock value *)
    let elapsed () = !clock

(* run takes graph, initial time, failure tolerance and out chnl for traffic*)
    let run g t ft num_nodes v_period g_period o =
      (* process deals with network-level events of type Message *)
      let queue = init 0 Step in
      let traffic_chan = open_out (Algorithm.name ^ "_traffic.dat") in
      let process time message = match message with
        Transfer (e, msg) ->
          let u = G.E.src e in
          let v = G.E.dst e in
          let d = Node.id (G.E.label e) in
          if (Node.received v) then
            Algorithm.already_received u d time queue
          else
            if msg then
              let _ = Node.receive v time in
              Algorithm.just_received v time queue
            else
              ()
      | Fail v -> Node.fail v in

      (* eval deals with all queue events and writes out traffic logs *)
      let eval time event =
        let _ =
          if o then (fprintf
            traffic_chan
           "%d\t%d\n"
            time
            (EventQueue.get_traffic queue)) else ()
        in
        match event with
          Message message -> process time message
        | VertexStep v ->
            Algorithm.vertex_step g v time queue ft v_period

        | Step ->
            Algorithm.step g time queue g_period
      in
      (* this is the main event loop that runs until all nodes reached *)
      let prop_time = ref 0 in
      let max_traf = ref 0 in
      let _ =
        let propagation_complete = ref false in
        try
          while true do
            let (time, event) = pop queue in
            clock := time;
            eval time event;
            max_traf := max (get_traffic queue) (!max_traf);
            if not (exists_susceptible g) && not !propagation_complete
            then let _ = propagation_complete := true in
                 let _ = prop_time := time in
                 if o
                 then
                   printf
                     "%s %d %d %d %d %d\n"
                     Algorithm.name
                     num_nodes
                     ft
                     v_period
                     !prop_time
                     !max_traf
                 else ()
            else ()
          done
        with Queue_is_empty -> ()
      in
      close_out traffic_chan;
      let propagation_chan = open_out (Algorithm.name ^ "_propagation.dat") in
      if o then Table.run propagation_chan g !prop_time num_nodes else ();
      close_out propagation_chan;
      let dot_chan = open_out (Algorithm.name ^ ".dot") in
      Network.DotOutput.output_graph dot_chan g;
      close_out dot_chan;
      Topology.reset g;
      (!prop_time, !max_traf)
  end

module GossipSim = Simulator(Gossip)
module AntiEntropySim = Simulator(AntiEntropy)
module SerialSim = Simulator(Serial)
module NeighbourSim = Simulator(Neighbour)
