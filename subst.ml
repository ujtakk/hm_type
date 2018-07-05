open Syntax

(* TODO: merge into subst_type *)
let swap_id ids typ =
  let rec subst_func sc en = match sc with
    | TVar i ->
        let (j, j') = en in
        if i == j then TVar(j') else TVar(i)
    | List t ->
        let t' = subst_func t en in
        List(t')
    | Func(s, t) ->
        let s' = subst_func s en in
        let t' = subst_func t en in
        Func(s', t')
    | t -> t
  in
  List.fold_left subst_func typ ids
;;

let subst_type subs typ =
  let rec subst_func ty en = match ty with
    | List t ->
        let t' = subst_func t en in
        List(t')
    | Func(s, t) ->
        let s' = subst_func s en in
        let t' = subst_func t en in
        Func(s', t')
    | s ->
        let (t, t') = en in
        if s == t then t' else s
  in
  List.fold_left subst_func typ subs
;;

let rec subst_scheme subs sch = match sch with
  | Type(t) -> Type(subst_type subs t)
  | Scheme(i, s) -> Scheme(i, (subst_scheme subs s))
;;

let subst_env subs env =
  let subst_func = fun (i, s) -> (i, (subst_scheme subs s)) in
  List.map subst_func env
;;

