open Simulation

module GossipSim = Simulator(Gossip) 
module AntiEntropySim = Simulator(AntiEntropy) 
module SerialSim = Simulator(Serial)

let args = Sys.argv

let num_nodes = int_of_string args.(1) 

let g = Topology.full 0 num_nodes true 

let elapsed = match args.(2) with
 "g" -> GossipSim.run g 0
| "ae" -> AntiEntropySim.run g 0
| "s" -> SerialSim.run g 0
| _ -> print_endline "the fuck"; 0

let _ = Table.run g elapsed num_nodes 

let _ = 
  let channel = open_out "out.dot" in
  Network.DotOutput.output_graph channel g;
  close_out channel;
