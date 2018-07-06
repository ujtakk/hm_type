open Syntax
open Show
open Assign

(* my_env -> my_expr -> my_scheme -> bool *)
let rec infer a e s : bool =
  (* Printf.printf "%s |- %s : %s\n" (show_env a) (show_expr e) (show_scheme s); *)
  match e, s with
  | Var(x), s  ->
      List.mem (x, s) a
  (* TODO: precise implementation for s > s' *)
  | e, s' when let (sub, typ) = assign a e in Type(typ) > s' ->
      infer a e s
  (* | e', Scheme(i, s') if i not free in a -> *)
  | e, Scheme(i, s) ->
      infer a e s
  | Apply(e, e'), t ->
      begin match assign a e with
      | sub, Func(typ', typ) ->
          infer a e (Type(Func(typ', typ))) && infer a e' (Type(typ'))
      | _, _ ->
          false
      end
  | Lambda(x, e), Type(Func(typ', typ)) ->
      infer ((x, Type(typ')) :: a) e (Type(typ))
  | Define(x, e, e'), typ ->
      let (sub, typ') = assign a e in
      infer a e (Type(typ')) && infer ((x, s) :: a) e' typ
  | _, _ ->
      false
;;
