open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst = struct
  type t =
    | NonConst (* Uid may take on multiple values at runtime *)
    | Const of int64 (* Uid will always evaluate to const i64 or i1 *)
    | UndefConst (* Uid is not defined at the point *)

  let compare s t =
    match (s, t) with
    | Const i, Const j -> Int64.compare i j
    | NonConst, NonConst | UndefConst, UndefConst -> 0
    | NonConst, _ | _, UndefConst -> 1
    | UndefConst, _ | _, NonConst -> -1

  let to_string : t -> string = function
    | NonConst -> "NonConst"
    | Const i -> Printf.sprintf "Const (%LdL)" i
    | UndefConst -> "UndefConst"
end

(* The analysis computes, at each program point, which UIDs in scope will evaluate
   to integer constants *)
type fact = SymConst.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
*)
let cal_binop (b_op : bop) (op1 : int64) (op2 : int64) : int64 =
  match b_op with
  | Add -> Int64.add op1 op2
  | Sub -> Int64.sub op1 op2
  | Mul -> Int64.mul op1 op2
  | Shl -> Int64.shift_left op1 (Int64.to_int op2)
  | Lshr -> Int64.shift_right_logical op1 (Int64.to_int op2)
  | Ashr -> Int64.shift_right op1 (Int64.to_int op2)
  | And -> Int64.logand op1 op2
  | Or -> Int64.logor op1 op2
  | Xor -> Int64.logxor op1 op2

let cal_cmp (cond : cnd) (op1 : int64) (op2 : int64) : int64 =
  match cond with
  | Eq -> if Int64.equal op1 op2 then 1L else 0L
  | Ne -> if Int64.equal op1 op2 then 0L else 1L
  | Slt -> if Int64.sub op1 op2 < 0L then 1L else 0L
  | Sle -> if Int64.sub op1 op2 <= 0L then 1L else 0L
  | Sgt -> if Int64.sub op1 op2 > 0L then 1L else 0L
  | Sge -> if Int64.sub op1 op2 >= 0L then 1L else 0L

let handle_binop (d : fact) (b_op : bop) (op1 : uid) (op2 : uid) : SymConst.t =
  let op1Type =
    match UidM.find_opt op1 d with Some x -> x | None -> SymConst.UndefConst
  in
  let op2Type =
    match UidM.find_opt op2 d with Some x -> x | None -> SymConst.UndefConst
  in
  match op1Type with
  | SymConst.UndefConst -> SymConst.UndefConst
  | SymConst.NonConst -> SymConst.NonConst
  | SymConst.Const val1 -> (
      match op2Type with
      | SymConst.UndefConst -> SymConst.UndefConst
      | SymConst.NonConst -> SymConst.NonConst
      | SymConst.Const val2 -> SymConst.Const (cal_binop b_op val1 val2))

let handle_binop_one (d : fact) (b_op : bop) (value : int64) (op2 : uid)
    (reverse : bool) : SymConst.t =
  let op2Type =
    match UidM.find_opt op2 d with Some x -> x | None -> SymConst.UndefConst
  in
  match op2Type with
  | SymConst.UndefConst -> SymConst.UndefConst
  | SymConst.NonConst -> SymConst.NonConst
  | SymConst.Const val2 ->
      if reverse then SymConst.Const (cal_binop b_op val2 value)
      else SymConst.Const (cal_binop b_op value val2)

let handle_cmp (d : fact) (cond : cnd) (op1 : uid) (op2 : uid) : SymConst.t =
  let op1Type =
    match UidM.find_opt op1 d with Some x -> x | None -> SymConst.UndefConst
  in
  let op2Type =
    match UidM.find_opt op2 d with Some x -> x | None -> SymConst.UndefConst
  in
  match op1Type with
  | SymConst.UndefConst -> SymConst.UndefConst
  | SymConst.NonConst -> SymConst.NonConst
  | SymConst.Const val1 -> (
      match op2Type with
      | SymConst.UndefConst -> SymConst.UndefConst
      | SymConst.NonConst -> SymConst.NonConst
      | SymConst.Const val2 -> SymConst.Const (cal_cmp cond val1 val2))

let handle_cmp_one (d : fact) (cond : cnd) (value : int64) (op2 : uid)
    (reverse : bool) : SymConst.t =
  let op2Type =
    match UidM.find_opt op2 d with Some x -> x | None -> SymConst.UndefConst
  in
  match op2Type with
  | SymConst.UndefConst -> SymConst.UndefConst
  | SymConst.NonConst -> SymConst.NonConst
  | SymConst.Const val2 ->
      if reverse then SymConst.Const (cal_cmp cond val2 value)
      else SymConst.Const (cal_cmp cond value val2)

let insn_flow ((u, i) : uid * insn) (d : fact) : fact =
  match i with
  | Binop (bop_typ, typ, Id id1, Id id2) ->
      UidM.add u (handle_binop d bop_typ id1 id2) d
  | Binop (bop_typ, typ, Const value, Id id) ->
      UidM.add u (handle_binop_one d bop_typ value id false) d
  | Binop (bop_typ, typ, Id id, Const value) ->
      UidM.add u (handle_binop_one d bop_typ value id true) d
  | Binop (bop_typ, typ, Const value1, Const value2) ->
      UidM.add u (SymConst.Const (cal_binop bop_typ value1 value2)) d
  | Icmp (condition, typ, Id id1, Id id2) ->
      UidM.add u (handle_cmp d condition id1 id2) d
  | Icmp (condition, typ, Const value, Id id) ->
      UidM.add u (handle_cmp_one d condition value id false) d
  | Icmp (condition, typ, Id id, Const value) ->
      UidM.add u (handle_cmp_one d condition value id true) d
  | Icmp (condition, typ, Const val1, Const val2) ->
      UidM.add u (SymConst.Const (cal_cmp condition val1 val2)) d
  | Store _ -> UidM.add u SymConst.UndefConst d
  | Call (Void, _, _) -> UidM.add u SymConst.UndefConst d
  | _ -> UidM.add u SymConst.NonConst d

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t : terminator) (d : fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact = struct
  type t = fact

  let forwards = true
  let insn_flow = insn_flow
  let terminator_flow = terminator_flow

  let normalize : fact -> fact =
    UidM.filter (fun _ v -> v != SymConst.UndefConst)

  let compare (d : fact) (e : fact) : int =
    UidM.compare SymConst.compare (normalize d) (normalize e)

  let to_string : fact -> string =
    UidM.to_string (fun _ v -> SymConst.to_string v)

  (* The constprop analysis should take the meet over predecessors to compute the
     flow into a node. You may find the UidM.merge function useful *)
  let helper_merge (v1 : SymConst.t) (v2 : SymConst.t) : SymConst.t =
    match v1 with
    | SymConst.UndefConst -> v2
    | SymConst.NonConst -> SymConst.NonConst
    | SymConst.Const val1 -> v2

  let merge_f (key : uid) (v1_opt : SymConst.t option)
      (v2_opt : SymConst.t option) : SymConst.t option =
    match (v1_opt, v2_opt) with
    | Some v1, Some v2 -> Some (helper_merge v1 v2)
    | Some v1, None -> Some v1
    | None, Some v2 -> Some v2
    | None, None -> None

  let combine_two (fact1 : fact) (fact2 : fact list) : fact =
    if List.length fact2 = 0 then fact1
    else UidM.merge merge_f fact1 (List.hd fact2)

  let combine (ds : fact list) : fact =
    if List.length ds == 0 then UidM.empty
    else List.hd (List.fold_left (fun acc x -> [ combine_two x acc ]) [] ds)
  
end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g : Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in =
    List.fold_right
      (fun (u, _) -> UidM.add u SymConst.NonConst)
      g.Cfg.args UidM.empty
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg

(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper
   functions. *)
let const_prop (d : fact) (op : operand) : operand =
  match op with
  | Id id -> (
      match UidM.find_opt id d with
      | Some (SymConst.Const c) -> Const c
      | _ -> op)
  | Const _ -> op
  | _ -> op

let handle_ins (d : fact) (instruction : insn) : insn =
  match instruction with
  | Binop (b_op, typ, op1, op2) ->
      Binop (b_op, typ, const_prop d op1, const_prop d op2)
  | Icmp (cond, typ, op1, op2) ->
      Icmp (cond, typ, const_prop d op1, const_prop d op2)
  | Load (typ, op) -> Load (typ, const_prop d op)
  | Store (typ, op1, op2) -> Store (typ, const_prop d op1, const_prop d op2)
  | Call (typ, op1, lst) ->
      let temp_func (a, b) = (a, (const_prop d) b) in
      Call (typ, const_prop d op1, List.map temp_func lst)
  | Bitcast (typ1, op1, typ2) -> Bitcast (typ1, const_prop d op1, typ2)
  | Gep (typ, op1, op_list) ->
      Gep (typ, const_prop d op1, List.map (const_prop d) op_list)
  | _ -> instruction

let handle_term (d : fact) (arnold : terminator) : terminator =
  match arnold with
  | Ret (typ, Some op1) -> Ret (typ, Some (const_prop d op1))
  | Cbr (op1, lbl1, lbl2) -> Cbr (const_prop d op1, lbl1, lbl2)
  | _ -> arnold

let run (cg : Graph.t) (cfg : Cfg.t) : Cfg.t =
  let open SymConst in
  let cp_block (l : Ll.lbl) (cfg : Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in

    let update_ins (u, i) = (u, handle_ins (cb u) i) in
    let update_term (u, i) = (u, handle_term (cb u) i) in
    let new_block = List.map update_ins b.insns in
    let new_term = update_term b.term in
    Cfg.add_block l { insns = new_block; term = new_term } cfg
  in

  LblS.fold cp_block (Cfg.nodes cfg) cfg
