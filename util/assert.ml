(* CIS341 Assertion Testing and Grading Infrastructure *)
(* Author: Steve Zdancewic                             *)

(* Do NOT modify this file -- we will overwrite it     *)
(* with our own version when testing your code.        *)

(* An assertion is just a unit->unit function that either *)
(* succeeds silently or throws an Failure exception.       *)
type assertion = unit -> unit

type 'a test =
  | GradedTest of string * int * (string * 'a) list
  | Test of string * (string * 'a) list
  | ManualTest of string * int * (string * 'a) list

type suite = assertion test list

(**************)
(* Assertions *)

let assert_eq v1 v2 : assertion =
 fun () -> if v1 <> v2 then failwith "not equal" else ()


let assert_eqf f v2 : assertion =
 fun () -> if f () <> v2 then failwith "not equal" else ()


let assert_eqfs f v2 : assertion =
 fun () ->
  let s1 = f () in
  if s1 <> v2
  then failwith @@ Printf.sprintf "not equal\n\texpected:%s\n\tgot:%s\n" v2 s1
  else ()


let assert_fail : assertion = fun () -> failwith "assert fail"

exception Timeout

let timeout_assert (time : int) (a : assertion) : assertion =
 fun () ->
  let handler = Sys.Signal_handle (fun _ -> raise Timeout) in
  let old = Sys.signal Sys.sigalrm handler in
  let reset_sigalrm () = Sys.set_signal Sys.sigalrm old in
  ignore (Unix.alarm time) ;
  try
    a () ;
    reset_sigalrm ()
  with
  | Timeout ->
      reset_sigalrm () ;
      failwith @@ Printf.sprintf "Timed out after %d seconds" time
  | exc ->
      reset_sigalrm () ;
      raise exc


let timeout_test (time : int) (t : assertion test) : assertion test =
  let map_timeout l = List.map (fun (i, a) -> (i, timeout_assert time a)) l in
  match t with
  | GradedTest (s, i, ls) ->
      GradedTest (s, i, map_timeout ls)
  | Test (s, ls) ->
      Test (s, map_timeout ls)
  | ManualTest (s, i, ls) ->
      GradedTest (s, i, map_timeout ls)


let timeout_suite (time : int) (s : suite) : suite =
  List.map (timeout_test time) s


(***************************)
(* Generating Test Results *)

type result =
  | Pass
  | Fail of string
  | Inconclusive

type outcome = result test list

let run_assertion (f : assertion) : result =
  try
    f () ;
    Pass
  with
  | Failure m ->
      Fail m
  | e ->
      Fail ("test threw exception: " ^ Printexc.to_string e)


let run_test (t : assertion test) : result test =
  let run_case (cn, f) = (cn, run_assertion f) in
  let inconclusive_case (cn, f) = (cn, Inconclusive) in
  match t with
  | GradedTest (n, s, cases) ->
      Printf.eprintf "Running test %s\n%!" n ;
      GradedTest (n, s, List.map run_case cases)
  | Test (n, cases) ->
      Printf.eprintf "Running test %s\n%!" n ;
      Test (n, List.map run_case cases)
  | ManualTest (n, s, cases) -> 
      Printf.eprintf "Manual  test %s\n%!" n;
      ManualTest (n, s, List.map inconclusive_case cases)


let run_suite (s : suite) : outcome = List.map run_test s

(***********************)
(* Reporting functions *)

let result_test_to_string (name_pts : string) (r : result test) : string =
  let string_of_case (name, res) =
    match res with
    | Pass ->
        "passed - " ^ name
    | Fail msg ->
        "FAILED - " ^ name ^ ": " ^ msg
    | Inconclusive -> 
        "manual - " ^ name
  in
  match r with
  | GradedTest (_, _, cases) | Test (_, cases) | ManualTest (_, _, cases) ->
      name_pts
      ^ List.fold_left
          (fun rest case -> rest ^ "\n" ^ string_of_case case)
          ""
          cases

(* returns (name_pts, passed, failed, total, points_earned, max_given, max_hidden, max_skipped) *)
let get_results (t : result test) =
  let num_passed cases =
    List.fold_left
      (fun cnt (_, r) -> match r with Pass -> cnt + 1 | _ -> cnt)
      0
      cases
  in
  let num_failed cases =
    List.fold_left
      (fun cnt (_, r) -> match r with Fail _ -> cnt + 1 | _ -> cnt)
      0
      cases
  in
  match t with
  | GradedTest (name, pts, cases) ->
      let passed = num_passed cases in
      let failed = num_failed cases in
      let total = List.length cases in
      if total > 0
      then
        let points_earned =
          float_of_int passed /. float_of_int total *. float_of_int pts
        in
        let name_pts =
          Printf.sprintf "%s (%1.f/%d points)" name points_earned pts
        in
        (name_pts, passed, failed, total, points_earned, pts, 0, 0)
      else
        let name_pts = Printf.sprintf "%s (?/%d points)" name pts in
        (name_pts, passed, failed, total, 0.0, 0, pts, 0)
  | ManualTest (name, pts, cases) ->
      let name_pts = Printf.sprintf "%s (?/%d points)" name pts in
      (name_pts, 0, 0, 0, 0.0, 0, 0, pts)
  | Test (name, cases) ->
      let total = List.length cases in
      let passed = num_passed cases in
      let failed = num_failed cases in
      (name, passed, failed, total, 0.0, 0, 0, 0)


let outcome_to_string (o : outcome) : string =
  let sep = "\n---------------------------------------------------\n" in
  let helper (passed, failed, total, pts, maxg, maxh, maxs, str) (t : result test) =
    let name_pts, p, f, tot, s, mg, mh, ms = get_results t in
    ( passed + p
    , failed + f
    , total + tot
    , s +. pts
    , maxg + mg
    , maxh + mh
    , maxs + ms
    , str
      ^ "\n"
      ^
      if f > 0
      then result_test_to_string name_pts t
      else if tot > 0
      then name_pts ^ ":\n  OK"
      else if ms > 0
      then name_pts ^ ":\n  Manually graded"
      else name_pts ^ ":\n  Hidden" )
  in
  let p, f, tot, pts, maxg, maxh, maxs, str =
    List.fold_left helper (0, 0, 0, 0.0, 0, 0, 0, "") o
  in
  let hidden_str =
    if maxh > 0
    then Printf.sprintf "\n\       ?/%d (hidden)" maxh
    else ""
  in
  let manual_str =
    if maxs > 0
    then Printf.sprintf "\n\       ?/%d (manual)" maxs
    else ""
  in
  str
  ^ sep
  ^ Printf.sprintf
      "Automatic Test Passed: %d/%d\n\
       Automatic Test Failed: %d/%d\n\
       Score: %1.f/%d (given)"
       p tot
       f tot
       pts maxg
  ^ hidden_str
  ^ manual_str