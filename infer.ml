(* my_env -> my_expr -> my_scheme -> bool *)
let rec infer a e s : bool =
  Printf.printf "%s |- %s : %s\n" (show_env a) (show_expr e) (show_scheme s);
  match e, s with
  | MyVar(x), s when List.mem (x, s) a ->
      true
  | e, s' when let s = assign a e in s > s' ->
      infer a e s
  (* | e', MyScheme(i, s') if i not free in a -> *)
  | e, MyScheme(i, s) ->
      infer a e s
  | MyApply(e, e'), t ->
      let MyType(MyFunc(t', t)) = assign a e in
      infer a e (MyType(MyFunc(t', t))) && infer a e' (MyType(t'))
  | MyLambda(x, e), MyType(MyFunc(t', t)) ->
      infer ((x, MyType(t')) :: a) e (MyType(t))
  | MyDefine(x, e, e'), t ->
      let s = assign a e in
      infer a e s && infer ((x, s) :: a) e' t
  | _, _ ->
      false
;;
