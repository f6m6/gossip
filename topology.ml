open Graph

let full offset n orig =
  let a = Array.init n (fun i -> Node.make_node(string_of_int (i+offset))) in
  let g = Network.G.create() in
  a.(0) <- Node.make_orig "orig";
  for i = 1 to n - 1 do
    let v = a.(i) in
    Network.G.add_vertex g v
  done;
  
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if not (i = j) then
	let v1 = a.(i) in
	let v2 = a.(j) in
	let distance = 0 in
	let entry = Node.make_entry distance in
	let edge = Network.G.E.create v1 entry v2 in
	Network.G.add_edge_e g edge;
      else
	()
    done
  done;
  g 

let unit_graph () =
  let g = Network.G.create() in
  let originator = Node.make_orig "unit" in
  Network.G.add_vertex g originator;
  g 

let union g1 g2 =
  let g3 = Network.G.create () in
  let take_vertices g = Network.G.iter_vertex
      (fun v -> Network.G.add_vertex g3 v) g in
  take_vertices g1;
  take_vertices g2;
  let take_edges g = Network.G.iter_edges_e
      (fun e -> Network.G.add_edge_e g3 e) g in
  take_edges g1;
  take_edges g2;
  g3 (* add all vertices and all edges from both graphs. *)

let join v1 v2 g1 g2 =
  let g3 = union g1 g2 in
  let entry = Node.make_entry 0 in
  let edge = Network.G.E.create v1 entry v2 in
  Network.G.add_edge_e g3 edge;
  g3 

open Extensions

let two n = 
  let m = n/2 in
  let g1 = full 0 m true in
  let g2 = full m (n - m) false in
  let v1 = List.random (Network.vertices g1) in
  let v2 = List.random (Network.vertices g2) in
  let g = join v1 v2 g1 g2 in
  g

let dot g = Network.DotOutput.output_graph stdout g 

