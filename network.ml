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



let exists_active g =
  let p v b = b || Node.active v in
  G.fold_vertex p g false ;;

let vertices g = 
G.fold_vertex (fun v acc -> v :: acc) g [] ;;

let dead_end g v = G.succ g v = []
