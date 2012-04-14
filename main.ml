open SimulatorPack
open AlgorithmPack

module S = Simulator(Gossip)

let args = Sys.argv

let num_nodes = int_of_string args.(1)

let g = Topology.full 0 num_nodes true

let elapsed = S.run g 0 false

let _ = Table.run g elapsed num_nodes


let f = 
  let channel = open_out "out.dot" in
  Network.DotOutput.output_graph channel g;
  close_out channel;
