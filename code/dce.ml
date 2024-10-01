open Ll
(** Dead Code Elimination  *)

open Datastructures

(* expose a top-level analysis operation ------------------------------------ *)
(* TASK: This function should optimize a block by removing dead instructions
   - lb: a function from uids to the live-OUT set at the
     corresponding program point
   - ab: the alias map flowing IN to each program point in the block
   - b: the current ll block

   Note:
     Call instructions are never considered to be dead (they might produce
     side effects)

     Store instructions are not dead if the pointer written to is live _or_
     the pointer written to may be aliased.

     Other instructions are dead if the value they compute is not live.

   Hint: Consider using List.filter7
*)
let check_live (id : uid) (live : Liveness.Fact.t) : bool = UidS.mem id live

let filtering (lb : uid -> Liveness.Fact.t) (ab : uid -> Alias.fact)
    ((id, instruction) : uid * insn) : bool =
  match instruction with
  | Call _ -> true
  | Store (_, _, Id store_id) ->
      let aliased =
        try
          let alias = ab id in
          match UidM.find_opt store_id alias with
          | None -> false
          | Some Unique -> false
          | _ -> true
        with Not_found -> false
      in
      let lived =
        try
          let live = lb id in
          check_live store_id live
        with Not_found -> false
      in
      aliased || lived
  | Store (_, _, Gid store_id) ->
      let aliased =
        try
          let alias = ab id in
          match UidM.find_opt store_id alias with
          | None -> true
          | Some Unique -> false
          | _ -> true
        with Not_found -> false
      in
      let lived =
        try
          let live = lb id in
          check_live store_id live
        with Not_found -> false
      in
      aliased || lived
  | _ ->
      let live = lb id in
      check_live id live

let dce_block (lb : uid -> Liveness.Fact.t) (ab : uid -> Alias.fact)
    (b : Ll.block) : Ll.block =
  { insns = List.filter (filtering lb ab) b.insns; term = b.term }

let run (lg : Liveness.Graph.t) (ag : Alias.Graph.t) (cfg : Cfg.t) : Cfg.t =
  LblS.fold
    (fun l cfg ->
      let b = Cfg.block cfg l in

      (* compute liveness at each program point for the block *)
      let lb = Liveness.Graph.uid_out lg l in

      (* compute aliases at each program point for the block *)
      let ab = Alias.Graph.uid_in ag l in

      (* compute optimized block *)
      let b' = dce_block lb ab b in
      Cfg.add_block l b' cfg)
    (Cfg.nodes cfg) cfg
