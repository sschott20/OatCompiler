(** Alias Analysis *)

open Ll
open Datastructures
open Llutil

(* The lattice of abstract pointers ----------------------------------------- *)
module SymPtr =
  struct
    type t = MayAlias           (* uid names a pointer that may be aliased *)
           | Unique             (* uid is the unique name for a pointer *)
           | UndefAlias         (* uid is not in scope or not a pointer *)

    let compare : t -> t -> int = Stdlib.compare

    let to_string = function
      | MayAlias -> "MayAlias"
      | Unique -> "Unique"
      | UndefAlias -> "UndefAlias"


  end

(* The analysis computes, at each program point, which UIDs in scope are a *unique* name
   for a stack slot and which may have aliases. *)
type fact = SymPtr.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* TASK: complete the flow function for alias analysis. 

   - After an alloca, the defined UID is the unique name for a stack slot
   - A pointer returned by a load, call, bitcast, or GEP may be aliased
   - A pointer passed as an argument to a call, bitcast, GEP, or store
     (as the value being stored) may be aliased
   - Other instructions do not define pointers

 *)
let rec handle_call (arg_list : (ty * operand) list) (d : fact) : fact = 
  match arg_list with 
  | (typ, Id(id))::tl -> handle_call tl (UidM.add id SymPtr.MayAlias d)
  | _::tl -> handle_call tl d
  | _ -> d 

let insn_flow ((u,i):uid * insn) (d:fact) : fact =
  match i with
  | Alloca _ -> UidM.add u SymPtr.Unique d
  | Load(_, Id(id)) -> if(UidM.mem id d) then d else UidM.add id SymPtr.Unique d
  (* | Load(_, Gid(id)) -> if(UidM.mem id d) then d else UidM.add id SymPtr.MayAlias d *)
  (* | Store(_, Id(id1), Id(id2)) -> let map = 
    if(UidM.mem id2 d) then d else UidM.add id2 SymPtr.Unique d in 
    if(UidM.mem id1 map) then map else UidM.add id1 SymPtr.MayAlias map *)
  | Store(_, _, Id(id)) -> 
    if(UidM.mem id d) then d else UidM.add id SymPtr.Unique d 
  | Store(_, _, Gid(id)) -> 
    if(UidM.mem id d) then d else UidM.add id SymPtr.MayAlias d 
  | Call (_,_, arg_list) -> UidM.add u SymPtr.MayAlias (handle_call arg_list d) 
  | Gep(_, Id(id), _) ->  UidM.add u SymPtr.MayAlias (UidM.add id SymPtr.MayAlias d) 
  | Bitcast(_,Id(id),_) -> UidM.add u SymPtr.MayAlias (UidM.add id SymPtr.MayAlias d)
  | Bitcast(_,_,_) -> UidM.add u SymPtr.MayAlias d
  | Gep(_, _, _) ->  UidM.add u SymPtr.MayAlias d 
  | _ -> d


(* The flow function across terminators is trivial: they never change alias info *)
let terminator_flow t (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =  
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow
    
    (* UndefAlias is logically the same as not having a mapping in the fact. To
       compare dataflow facts, we first remove all of these *)
    let normalize : fact -> fact = 
      UidM.filter (fun _ v -> v != SymPtr.UndefAlias)

    let compare (d:fact) (e:fact) : int = 
      UidM.compare SymPtr.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymPtr.to_string v)

    (* TASK: complete the "combine" operation for alias analysis.

       The alias analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful.

       It may be useful to define a helper function that knows how to take the
       meet of two SymPtr.t facts.
    *)
    let helper_merge (v1 : SymPtr.t) (v2 : SymPtr.t) : SymPtr.t = 
      match v1 with 
      | SymPtr.MayAlias -> SymPtr.MayAlias
      | SymPtr.Unique -> (match v2 with | SymPtr.MayAlias -> SymPtr.MayAlias | _ -> SymPtr.Unique)
      | SymPtr.UndefAlias -> v2

    let merge_f (key : uid) (v1_opt : SymPtr.t option) (v2_opt : SymPtr.t option) : SymPtr.t option =
      match v1_opt, v2_opt with
      | Some v1, Some v2 -> Some (helper_merge v1 v2)
      | Some v1, None -> Some v1
      | None, Some v2 -> Some v2
      | None, None -> None
    
    let combine_two (fact1 : fact) (fact2 : fact list) : fact = 
      if(List.length fact2 = 0) then fact1 else 
        UidM.merge merge_f fact1 (List.hd fact2)
    
    let combine (ds:fact list) : fact =
      if(List.length ds == 0) then UidM.empty else 
      (List.hd (List.fold_left (fun acc x -> [combine_two x acc]) [] ds))
    
    let empty = UidM.empty

  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefAlias *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any pointer parameter 
     to the function may be aliased *)
  let alias_in = 
    List.fold_right 
      (fun (u,t) -> match t with
                    | Ptr _ -> UidM.add u SymPtr.MayAlias
                    | _ -> fun m -> m) 
      g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init alias_in g in
  Solver.solve fg

