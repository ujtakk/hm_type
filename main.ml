open Syntax
open Show
open Assign
open Infer

let default_env = [
  ("not", MyType(MyFunc(MyBool, MyBool)))
]
;;

(*
let () =
  let env = [("x", MyType(MyBool))] in
  let expr = MyVar("x") in
  let scheme = MyScheme("a", MyType(MyBool)) in
  if infer env expr scheme then
    print_endline "success"
  else
    print_endline "failed"
;;
*)

let () =
  let a = MyVar("not") in
  let b = MyVar("x") in
  let e = MyLambda("x", MyApply(a, b)) in
  let (_, t) = assign default_env e in
  t |> show_type |> print_endline
;;
