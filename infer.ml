open Syntax
open Assign

(* my_env -> my_expr -> my_scheme -> bool *)
let rec infer a e s : bool =
  Printf.printf "%s |- %s : %s\n" (show_env a) (show_expr e) (show_scheme s);
  match e, s with
  | MyVar(x), s when List.mem (x, s) a ->
      true
  | e, s' when let (sub, typ) = assign a e in MyType(typ) > s' ->
      infer a e s
  (* | e', MyScheme(i, s') if i not free in a -> *)
  | e, MyScheme(i, s) ->
      infer a e s
  | MyApply(e, e'), t ->
      let (sub, MyFunc(typ', typ)) = assign a e in
      infer a e (MyType(MyFunc(typ', typ))) && infer a e' (MyType(typ'))
  | MyLambda(x, e), MyType(MyFunc(typ', typ)) ->
      infer ((x, MyType(typ')) :: a) e (MyType(typ))
  | MyDefine(x, e, e'), typ ->
      let (sub, typ') = assign a e in
      infer a e (MyType(typ')) && infer ((x, s) :: a) e' typ
  | _, _ ->
      false
;;
