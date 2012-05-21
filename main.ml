open Printf
open Arg
open Simulation

(* flags - whether to run each algorithm *)
let gossip_flag = ref false
let neighbour_flag = ref false
let anti_entropy_flag = ref false
let serial_flag = ref false

(* periods for use in algorithms *)
let step_period = ref 1
let vertex_step_period = ref 1
let vertex_step_period_neighbour = ref 1

(* graph properties *)
let graph_layout = ref "full"
let graph_size = ref 5

(* for use in gossip protocol *)
let failure_tolerance = ref 1
let failure_tolerance_neighbour = ref 1

let output_mode = ref false
let avg_lim = ref 1

(* command line options to override defaults above *)
let speclist = [
  ("-g", (Set gossip_flag) , "run gossip protocol");
  ("-a", (Set anti_entropy_flag), "run anti-entropy algorithm");
  ("-s", (Set serial_flag), "run serial algorithm");
  ("-n", (Set neighbour_flag), "run nearest neighbour algorithm");
  ("-t", (Set_int vertex_step_period), "set vertex step period for uniform gossip");
  ( "-tn", (Set_int vertex_step_period_neighbour), "set vertex step period for neighbour");
  ("-T", (Set_int step_period), "set global step period");
  ("-l", (Set_string graph_layout), "set graph layout");
  ("-size", (Set_int graph_size), "set graph size");
  ("-f", (Set_int failure_tolerance), "set gossip failure tolerance");
  ("-fn", (Set_int failure_tolerance_neighbour), "set neighbour failure tolerance");
("-o", (Set output_mode), "set output mode ");
 ("-avg", (Set_int avg_lim), "set average limit")
]

let main =
  parse speclist (fun x -> ()) "run serial, gossip and anti-entropy algs";
  let n = !graph_size in
  let ft = !failure_tolerance in
  let ftn = !failure_tolerance_neighbour in
  let vsp = !vertex_step_period in
  let vspn = !vertex_step_period_neighbour in
  let sp = !step_period in
  let o = !output_mode in
    let g = match !graph_layout with
      "full" -> Topology.full 0 !graph_size true
      | _ -> Topology.full 0 6 true in
(* single run mode *)
if o then
    let _ =
      if !gossip_flag
      then let _ = GossipSim.run g 0 ft n vsp sp o in ()
      else () in
    let _ =
      if !anti_entropy_flag
      then let _ = AntiEntropySim.run g 0 ft n vsp sp o in ()
      else () in
    let _ =
      if !serial_flag
      then let _ = SerialSim.run g 0 ft n vsp sp o in ()
      else () in
    let _ =
      if !neighbour_flag
      then let _ = NeighbourSim.run g 0 ftn n vspn sp o in ()
      else () in
  ()
else
(* average mode *)
    let mean_time = ref 0.0 in
    let mean_traf = ref 0.0 in
    let lim = !avg_lim in
    let run =
      if !gossip_flag
      then GossipSim.run
      else if !neighbour_flag
      then NeighbourSim.run
      else if !serial_flag
      then SerialSim.run
      else if !anti_entropy_flag
      then AntiEntropySim.run
      else SerialSim.run in
    for i = 1 to lim do
      let (time, traf) = run g 0 ft n vsp sp o in
      let (ftime, ftraf) = (float time, float traf) in
      mean_time := !mean_time +. ftime;
      mean_traf := !mean_traf +. ftraf;
    done;
    printf "%d %f %f\n" ft (!mean_time /.  float lim) (!mean_traf /. float lim)
