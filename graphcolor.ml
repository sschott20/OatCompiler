
module UidMap = Datastructures.UidM

type graph = {
  interference : UidSet.t UidMap.t;
  precolored : Alloc.loc UidMap.t;
  preference : LocSet.t UidMap.t;
}

let rax_uid = Parsing.gensym "rax"
let rcx_uid = Parsing.gensym "rcx"

let empty =
  {
    interference = UidMap.empty;
    precolored = UidMap.empty;
    preference = UidMap.empty;
  }

let ppi m =
  Llutil.mapcat "\n"
    (fun (u, s) -> Printf.sprintf "%s: %s" u (UidSet.to_string s))
    (UidMap.bindings m)

let pppre m =
  Llutil.mapcat "\n"
    (fun (u, s) -> Printf.sprintf "%s: %s" u (Alloc.str_loc s))
    (UidMap.bindings m)

let pppref m =
  Llutil.mapcat "\n"
    (fun (u, s) -> Printf.sprintf "%s: %s" u (str_locset s))
    (UidMap.bindings m)

let print_g { interference; precolored; preference } =
  Printf.printf "interference:\n%s\n\nprecolored:\n%s\n\npreference:\n%s\n"
    (ppi interference) (pppre precolored) (pppref preference)


let n_spill = ref 0 in

   let spill () =
     incr n_spill;
     Alloc.LStk (- !n_spill)
   in
   let rec update_from_list (g : graph) (uids : uid list) (master : uid list) :
       graph =
     match uids with
     | [] -> g
     | hd :: tl ->
         let existing_connections =
           match UidMap.find_opt hd g.interference with
           | Some x -> x
           | None -> UidSet.empty
         in
         let connections = List.filter (fun x -> x <> hd) master in
         let connections_set =
           UidSet.union existing_connections (UidSet.of_list connections)
         in
         let new_interference = UidMap.add hd connections_set g.interference in
         let new_g = { g with interference = new_interference } in
         update_from_list new_g tl master
   in

   let fcfg_to_uid_list (cfg : Ll.cfg) : uid list =
     let entry, blocks = cfg in
     let block_to_uidl (block : Ll.block) : uid list =
       List.map (fun (uid, _) -> uid) block.insns
     in
     let block_uids =
       List.concat (List.map (fun (_, block) -> block_to_uidl block) blocks)
     in
     block_to_uidl entry @ block_uids
   in
   let set_to_list (set : UidSet.t) : uid list =
     UidSet.fold (fun elt acc -> elt :: acc) set []
   in
   let update_all (lists : uid list list) (graph : graph) : graph =
     List.fold_left (fun g l -> update_from_list g l l) graph lists
   in
   let pal =
     LocSet.(caller_save |> remove (Alloc.LReg Rax) |> remove (Alloc.LReg Rcx))
   in
   let get_neighbor_colors (graph : graph) (uid : string) : LocSet.t =
     let neighbors =
       try UidSet.elements (UidMap.find uid graph.interference)
       with Not_found -> []
     in
     List.fold_left
       (fun acc neighbor ->
         match UidMap.find_opt neighbor graph.precolored with
         | Some color -> LocSet.add color acc
         | None -> acc)
       LocSet.empty neighbors
   in
   let rec color (g : graph) (stack : uid list) : graph =
     match stack with
     | [] -> g
     | uid :: tl ->
         (* check if already colored *)
         if UidMap.mem uid g.precolored then color g tl
         else
           let used_locs = get_neighbor_colors g uid in
           let available_locs = LocSet.diff pal used_locs in
           let loc =
             if LocSet.is_empty available_locs then (* raise Spill  *)
               spill ()
             else LocSet.choose available_locs
           in
           let g' = { g with precolored = UidMap.add uid loc g.precolored } in
           color g' tl
   in

   let num_edges (g : graph) (uid : uid) : int =
     match UidMap.find_opt uid g.interference with
     | None -> 0
     | Some neighbors -> UidSet.cardinal neighbors
   in

   let rec kempe_order (g : graph) (acc : uid list) : uid list =
     let remove_node (uid : string) (g : graph) : graph =
       let neighbors = UidMap.find_opt uid g.interference in
       match neighbors with
       | None -> g
       | Some set ->
           let remove_neighbor neighbor_id (g' : graph) =
             match UidMap.find_opt neighbor_id g'.interference with
             | None -> g'
             | Some set' ->
                 let new_set = UidSet.remove uid set' in
                 let new_interference =
                   UidMap.add neighbor_id new_set g'.interference
                 in
                 { g' with interference = new_interference }
           in
           let new_graph = UidSet.fold remove_neighbor set g in
           let new_interference = UidMap.remove uid new_graph.interference in
           { new_graph with interference = new_interference }
     in
     let find_min (g : graph) : lbl option =
       let min_uid = ref None in
       let min_edges = ref max_int in
       UidMap.iter
         (fun uid set ->
           let cur_edges = num_edges g uid in
           if cur_edges < !min_edges then min_uid := Some uid;
           min_edges := cur_edges)
         g.interference;
       !min_uid
     in
     let min = find_min g in
     match min with
     | Some x -> kempe_order (remove_node x g) (x :: acc)
     | None -> acc
   in

   let g : graph = empty in

   let uid_list = fcfg_to_uid_list f.f_cfg in

   let live_in_set = List.map live.live_in uid_list in
   let live_out_set = List.map live.live_out uid_list in

   let live_in = List.map set_to_list live_in_set in
   let live_out = List.map set_to_list live_out_set in

   let g = update_all live_in g in
   let g = update_all live_out g in

   let rec color_args (g : graph) (args : lbl list) (n_arg : int) : graph =
     match args with
     | [] -> g
     | arg :: tl ->
         let precolored' = UidMap.add arg (arg_loc n_arg) g.precolored in
         color_args { g with precolored = precolored' } tl (n_arg + 1)
   in

   let g = color_args g f.f_param 0 in

   let rec color_and_spill (g : graph) (stack : uid list) : graph =
     let find_max (g : graph) : lbl option =
       let max_uid = ref None in
       let max_edges = ref max_int in
       UidMap.iter
         (fun uid set ->
           let cur_edges = num_edges g uid in
           if cur_edges > !max_edges then max_uid := Some uid;
           max_edges := cur_edges)
         g.interference;
       !max_uid
     in

     try color g stack
     with Spill -> (
       let max_node = find_max g in
       match max_node with
       | Some uid ->
           color_and_spill
             { g with precolored = UidMap.add uid (spill ()) g.precolored }
             stack
       | None -> failwith "asdlfkjasdkfl")
   in

   let final_graph = color g uid_list in
   let allocate lo uid =
     (* let colors = color g uid_list in  *)
     let loc =
       try UidMap.find uid final_graph.precolored
       with Not_found -> failwith "should never spill here, shoud spill before"
     in
     Platform.verb
     @@ Printf.sprintf "allocated: %s <- %s\n" (Alloc.str_loc loc) uid;
     loc
   in

   let lo =
     fold_fdecl
       (fun lo (x, _) -> (x, allocate lo x) :: lo)
       (fun lo l -> (l, Alloc.LLbl (Platform.mangle l)) :: lo)
       (fun lo (x, i) ->
         if insn_assigns i then (x, allocate lo x) :: lo
         else (x, Alloc.LVoid) :: lo)
       (fun lo _ -> lo)
       [] f
   in
   { uid_loc = (fun x -> List.assoc x lo); spill_bytes = 8 * !n_spill }