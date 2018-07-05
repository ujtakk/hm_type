open Syntax
open Show
open Assign
open Infer

let default_env = [
  (* ("true", Type(Bool));  *)
  (* ("false", Type(Bool)); *)
  ("nat", Type(List(Int)));
  ("first", Type(Func(List(Int), Int)));
  (* ("not", Type(Func(Bool, Bool))); *)
  ("is_even", Type(Func(Int, Bool)));
  (* ("succ", Type(Func(Int, Int))); *)
  (* ("add", Type(Func(Int, Func(Int, Int)))); *)
  (* ("equal", Type(Func(Int, Func(Int, Bool)))); *)
  (* ("map", Type(Func(Func(Func(TVar("'a"), TVar("'b")), List(TVar("'a"))), List(TVar("'b"))))); *)
]
;;

(*
let () =
  let env = [("x", Type(Bool))] in
  let expr = Var("x") in
  let scheme = Scheme("a", Type(Bool)) in
  if infer env expr scheme then
    print_endline "success"
  else
    print_endline "failed"
;;
*)

let () =
  let e = Apply(Var("is_even"), Apply(Var("first"), Var("nat"))) in
  let (_, t) = assign default_env e in
  t |> show_type |> print_endline
;;
