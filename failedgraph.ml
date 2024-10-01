  (* let uid_list =  *)
  (* graph testing code *)
  (* let _ = fdecl_to_cgraph f live in *)
  (* let g1 : cgraph = {k = 10; graph = [("a", (["b"; "c"], None)); ("b", (["a"], None)) ; ("c", (["a"], None))]} in
     print_cgraph g1.graph;
     let g2 = add_node g1 "d" ["a"; "c"] in
     let g3 = add_node g2 "e" ["a"; "c" ; "b"] in
     print_cgraph g3.graph; *)
(* 

     let last_char = uid.[String.length uid - 1] in
        let target_last_char = string_of_int (int_of_char last_char - 48 - 1) in
        let target_last_char =
          if target_last_char = "-1" then "9" else target_last_char
        in
        let new_string =
          String.sub uid 0 (String.length uid - 1) ^ target_last_char
        in

        let rec check_args (uid : string) (l : uid list) (acc : int) : Alloc.loc
            =
          match l with
          | [] -> LocSet.choose available_locs
          | hd :: tl ->
              if new_string = hd then arg_loc acc
              else check_args uid tl (acc + 1)
        in
        (* Printf.printf "wholestring: %s, last char %s\n" uid new_string; *)
        check_args new_string f.f_param 0 *)


type node = string
type color = int option
type graph = (node * (node list * color)) list
type cgraph = { k : int; graph : graph }

let int_of_intoption = function None -> -1 | Some n -> n

let rec print_cgraph_helper (g : graph) : unit =
  match g with
  | [] -> ()
  | (cur, (edges, col)) :: tl ->
      Printf.printf "%s : %d : [ " cur (int_of_intoption col);
      List.iter (Printf.printf "%s ") edges;
      Printf.printf "]\n";
      print_cgraph_helper tl

let print_cgraph (g : graph) : unit =
  Printf.printf "printing cgraph: \n";
  print_cgraph_helper g;
  Printf.printf "\n"

let add_edge (g : cgraph) (n : node) (e : node) : cgraph =
  let edges, col = List.assoc n g.graph in
  { k = g.k; graph = (n, (e :: edges, col)) :: g.graph }

(* for all nodes in l, add n to their list of edges *)
let rec add_all_edges (g : cgraph) (n : node) (l : node list) : cgraph =
  match l with
  | [] -> g
  | hd :: tl -> (
      let entry = List.assoc_opt hd g.graph in
      match entry with
      | None -> add_all_edges g n tl
      | Some (nl, col) ->
          let g2 = List.remove_assoc hd g.graph in
          add_all_edges { k = g.k; graph = (hd, (n :: nl, col)) :: g2 } n tl)

(* let g2 = { k = g.k; graph = (n, (e, None)) :: g.graph } *)

let add_node (g : cgraph) (n : node) (e : node list) : cgraph =
  let g2 = { k = g.k; graph = (n, (e, None)) :: g.graph } in
  add_all_edges g2 n e

let add_node_no_update (g : cgraph) (n : node) (e : node list) : cgraph =
  { k = g.k; graph = (n, (e, None)) :: g.graph }

(* let fdecl_to_cgraph (f : Ll.fdecl) (live : liveness) : cgraph =
   let rec insns_to_node_list (insns : (node * insn) list) (acc : graph) : graph
       =
     match insns with
     | [] -> acc
     | (n, _) :: tl ->
       live.live_in

       insns_to_node_list tl (() :: acc)
   in

   (* function params *)
   (* let first_arg = List.hd f.f_param in *)
   (* (try
        let lfirst_arg = live.live_out "_x13" in

        (* Printf.printf "asdf %s\n" (lfirst_arg); *)
        Printf.printf "out %s\n" (UidSet.to_string lfirst_arg);
        Printf.printf "in %s\n" (UidSet.to_string (live.live_in "_x13"))

      with Not_found -> ()); *)
   { k = 1; graph = [ ("a", ([], None)) ] } *)
(* failwith "unimpliamented" *)
(* let add_edge (g: cgraph) () *)

(* let cgraph_of_block (b : Ll.block) : (node * (node list * color)) list *)
