type node = {
  name: string;
  mutable received: bool;
  mutable time_received: int;
  mutable fail_count: int;
}

let name v = v.name
let received v = v.received
let time_received v = v.time_received
let fail_count v = v.fail_count
let fail v = v.fail_count <- v.fail_count + 1

let receive v t =
  v.time_received <- t;
  v.received <- true

let string_of_node v = v.name ^ " (" ^ (string_of_int v.time_received) ^ ")"

let make_node new_name = {
  name = new_name;
  received = false;
  time_received = max_int;
  fail_count = 0;
}

let make_orig new_name =
  let n = make_node new_name in
  n.received <- true;
  n.time_received <- 0;
  n

module Ordered = struct
  type t = node
  let compare (x:t) (y:t) = compare x y
  let hash (x:t) = Hashtbl.hash x.name
  let equal (x:t) (y:t) = x == y
end

type entry = { id: int ; }
let id e = e.id 
let string_of_entry e = string_of_int e.id
let make_entry new_id = { id = new_id; }

module FlowEntry = struct
  type t = entry
  let compare x y = y.id - x.id
  let default = { id = (-1) }
end 

