open Extensions

let add_to_table elapsed tbl vtx = 
  let t = Node.time_received vtx in
  if (t > elapsed)
  then ()
  else 
    if (Hashtbl.mem tbl t)
    then
      let prev = Hashtbl.find tbl t in
      Hashtbl.replace tbl t (prev + 1)
    else
      Hashtbl.add tbl t 1

let run g elapsed num_nodes = 
  let dist_tbl = Hashtbl.create elapsed in
  for i = 0 to elapsed do Hashtbl.add dist_tbl i 0 done;
  Network.G.iter_vertex (add_to_table elapsed dist_tbl) g;
  for i = 1 to elapsed do
    let prev = Hashtbl.find dist_tbl (i-1) in
    let curr = Hashtbl.find dist_tbl i in
    let cml = prev + curr in
    Hashtbl.replace dist_tbl i cml
  done;
  let dist_list = Hashtbl.to_list dist_tbl in
  let cmp (k1, v1) (k2, v2) = k1 - k2 in 
  let dist_sorted = List.sort cmp dist_list in
  List.iter (fun (k, v) -> Printf.printf "%d\t%d\n" k v) dist_sorted
  
