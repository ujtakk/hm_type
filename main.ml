open Syntax
open Show
open Assign
open Infer

let default_env = [
  ("true", Type(Bool));
  ("false", Type(Bool));
  ("zero", Type(Int));
  ("nat", Type(List(Int)));
  ("head", Type(Func(List(Int), Int)));
  ("tail", Type(Func(List(Int), List(Int))));
  ("not", Type(Func(Bool, Bool)));
  ("even", Type(Func(Int, Bool)));
  ("succ", Type(Func(Int, Int)));
  ("add", Type(Func(Int,
                    Func(Int, Int))));
  ("equal", Type(Func(Int,
                      Func(Int, Bool))));
  ("map", Type(Func(Func(TVar("'a"),
                         TVar("'b")),
                    Func(List(TVar("'a")),
                         List(TVar("'b"))))));
]
;;

let check e =
  let (_, t) = assign default_env e in
  print_endline ("assigned to "^(show_type t));

  if infer default_env e (Type(t)) then
    print_endline "inference success"
  else
    print_endline "inference failed"
;;

let () =
  let e = Apply(Var("even"), Apply(Var("head"), Var("nat"))) in
  e |> show_expr |> print_endline;
  e |> check;

  print_newline ();
;;

let () =
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
  e |> show_expr |> print_endline;

  let (_, t) = assign default_env e in
  t |> show_type |> print_endline;

  print_newline ();
;;

let () =
  let map = Var("map") in
  let even = Var("even") in

  let (_, t) = assign default_env map in
  t |> show_type |> print_endline;

  let (_, t) = assign default_env even in
  t |> show_type |> print_endline;

  let (_, t) = assign default_env (Apply(map, even)) in
  t |> show_type |> print_endline;

  print_newline ();
;;
