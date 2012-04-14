open Graph


type node = {
  name: string;
  mutable received: bool;
  mutable active: bool;
  mutable fail_count: int;
  mutable time_received: int;
}

let name v = v.name
let received v = v.received
let fail_count v = v.fail_count
let active v = v.active
let time_received v = v.time_received 

let string_of_node v = v.name ^ " (" ^ (string_of_int v.time_received) ^ ")"

let make_node new_name = {
  name = new_name;
  fail_count = 0;
  active = true;
  received = false;
  time_received = max_int
}

let make_orig new_name =
  let n = make_node new_name in
  n.received <- true;
  n.time_received <- 0;
  n

let print v =
  let n = v.name
  and a = string_of_bool v.active
  and r = string_of_bool v.received
  and fc = string_of_int v.fail_count
  and tr = string_of_int v.time_received in
  let s = n ^ " " ^ a ^ " " ^ r ^ " " ^ fc ^ " " ^ tr ^ "\n" in
  print_string s ;;

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

let fail v = v.active <- false ;;

module Q = EventQueue

let receive src dst time len queue =
  if not dst.received
  then
    let _ = dst.received <- true in
    let _ = dst.time_received <- time in
    Q.push time (Q.Step(dst)) queue
  else
    Q.push (time + len) 
      (Q.Fail(src))
      queue;;
