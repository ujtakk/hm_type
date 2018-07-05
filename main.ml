open Syntax
open Show
open Assign
open Infer

let default_env = [
  (* ("true", Type(Bool)); *)
  (* ("false", Type(Bool)); *)
  ("zero", Type(Int));
  ("nat", Type(List(Int)));
  ("head", Type(Func(List(Int), Int)));
  ("tail", Type(Func(List(Int), List(Int))));
  (* ("not", Type(Func(Bool, Bool))); *)
  ("even", Type(Func(Int, Bool)));
  (* ("succ", Type(Func(Int, Int))); *)
  ("add", Type(Func(Int, Func(Int, Int))));
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

let check e =
  let (_, t) = assign default_env e in
  print_endline ("assigned to "^(show_type t));

  if infer default_env e (Type(t)) then
    print_endline "infered as valid type"
  else
    print_endline "infered as invalid type"
;;

let () =
  Apply(Var("even"), Apply(Var("head"), Var("nat"))) |> check;
  print_newline ();
  let e =
    Define("add2",
           Lambda("x",
                  Apply(Apply(Var("add"),
                              Apply(Var("head"),
                                    Apply(Var("tail"),
                                          Var("nat")))),
                        Var("x"))),
           Apply(Var("add2"),
                 Var("zero")))
  in
  let (_, t) = assign default_env e in
  t |> show_type |> print_endline;
;;
