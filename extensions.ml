module List =
  struct
    include List
    let random l =
      let len = List.length l in
      let n = Random.int len in
      List.nth l n
    let make_string delim list = 
      match list with 
	[] -> ""
      | _ -> 
	  let delim_len = String.length delim in
	  let s = List.fold_right (fun s1 s2 -> s1 ^ delim ^ s2) list delim in
	  let total_len = String.length s in
	  String.sub s 0 (total_len - 2 * delim_len) ;;
  end

module Hashtbl =
  struct
    include Hashtbl
    let count h = Hashtbl.fold (fun k v acc -> 1 + acc) h 0
    let to_list h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h [] 
  end
