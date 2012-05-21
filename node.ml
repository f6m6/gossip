(* define node as record with mutable state for algorithms *)
type node = {
  name: string;
  mutable received: bool;
  mutable time_received: int;
  mutable fail_count: int;
}

(* functions to be used outside the module *)
let name v = v.name
let received v = v.received
let time_received v = v.time_received
let fail_count v = v.fail_count
let fail v = v.fail_count <- v.fail_count + 1

(* node v receives at time t *)
let receive v t =
  v.time_received <- t;
  v.received <- true

(* string representation of node *)
let string_of_node v = v.name ^ " (" ^ (string_of_int v.time_received) ^ ")"

(* make a new node that hasn't received yet *)
let make_node new_name = {
  name = new_name;
  received = false;
  time_received = max_int;
  fail_count = 0;
}

(*
  make an "originator" - node that has already received message
  this represents the "injection point" into the network
*)
let make_orig new_name =
  let n = make_node new_name in
  n.received <- true;
  n.time_received <- 0;
  n

(* reset a node in a network generated by Topology.full *)
let reset n =
  if ("orig" = name n)
  then (n.received <- true;
        n.time_received <- 0;
        n.fail_count <- 0)
  else (n.received <- false;
        n.time_received <- max_int;
        n.fail_count <- 0)

(* module to be used in graph functor imposing an ordering on nodes *)
module Ordered = struct
  type t = node
  let compare (x:t) (y:t) = compare x y
  let hash (x:t) = Hashtbl.hash x.name
  let equal (x:t) (y:t) = x == y
end

(* edges are labelled with integer distances *)
type entry = { id: int ; }
let id e = e.id
let string_of_entry e = string_of_int e.id
let make_entry new_id = { id = new_id; }

(* similar to Ordered but for FlowEntry, again for graph functor *)
module FlowEntry = struct
  type t = entry
  let compare x y = y.id - x.id
  let default = { id = (-1) }
end
