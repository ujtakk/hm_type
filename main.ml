open Syntax
open Infer

let () =
  let env = [("x", MyType(MyBool))] in
  let expr = MyVar("x") in
  let scheme = MyScheme("a", MyType(MyBool)) in
  if infer env expr scheme then
    print_endline "success"
  else
    print_endline "failed"
;;
