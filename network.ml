open Graph

module G = Imperative.Digraph.ConcreteLabeled(Node.Ordered)(Node.FlowEntry) ;;

module Display = struct
  include G
  let vertex_name v = "\"" ^ (Node.string_of_node v) ^ "\""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes e = [ `Label (Node.string_of_entry (E.label e))  ]
  let get_subgraph _ = None
end

module DotOutput = Graphviz.Dot(Display)

let vertices g = 
  G.fold_vertex (fun v acc -> v :: acc) g [] ;;

let dead_end g v = G.succ g v = []

let edge_length e =
  let label = G.E.label e in
  let length = Node.id label in
  length
