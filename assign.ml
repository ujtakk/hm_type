open Syntax
open Subst
open Unify

exception Type_assignment_failed

let rec lookup env var = match env with
  | [] ->
      None
  | (id, sch) :: rest ->
      if id == var then
        Some sch
      else
        lookup rest var
;;

(* find a type variable which is not bounded in the environment *)
let new_tvar a =
  let tvar_dict idx =
    let prim =
      ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
       "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"]
    in
    let prim_len = List.length prim in
    let alpha = List.nth prim (idx mod prim_len) in
    if idx < prim_len then
      "'" ^ alpha
    else
      "'" ^ alpha ^ (idx / prim_len |> string_of_int)
  in
  let rec dive_dict idx = function
    | [] -> tvar_dict idx
    | (i, s) :: iss ->
        let rec next_tvar sch idx =
          match sch with
          | MyType(MyTVar(x)) ->
              let cur_tvar = tvar_dict idx in
              if x == cur_tvar then idx+1 else idx
          | MyType(MyBool) ->
              0
          | MyType(MyList(x)) ->
              next_tvar (MyType(x)) idx
          | MyType(MyFunc(s, t)) ->
              let idx_s = next_tvar (MyType(s)) idx in
              let idx_t = next_tvar (MyType(t)) idx_s in
              idx_t
          | MyScheme(j, s) ->
              next_tvar s idx
        in
        dive_dict (next_tvar s idx) iss
  in
  dive_dict 0 a
;;

(* TODO: (my_sub * my_type) option *)
(* val assign : my_env -> my_expr -> (my_sub * my_type) *)
let rec assign (a : my_env) (e : my_expr) : my_sub * my_type = match e with
  | MyVar(x) when lookup a x != None ->
      let Some(s) = lookup a x in
      let t =
        let rec annotate acc = function
          | MyType t -> swap_id acc t
          | MyScheme(j, s') ->
              let j' = new_tvar a in
              annotate ((j, j') :: acc) s'
        in
        annotate [] s
      in
      ([], t)
  | MyApply(e1, e2) ->
      let (s1, t1) = assign a e1 in
      let (s2, t2) = assign (subst_env s1 a) e2 in
      let b = MyTVar(new_tvar a) in
      let v = unify (subst_type s2 t1) (MyFunc(t2, b)) in
      (s1 @ s2 @ v, subst_type v b)
  | MyLambda(x, e1) ->
      let b = MyTVar(new_tvar a) in
      let (s1, t1) = assign ((x, MyType(b)) :: a) e1 in
      (s1, MyFunc((subst_type s1 b), t1))
  | MyDefine(x, e1, e2) ->
      let (s1, t1) = assign a e1 in
      let (s2, t2) = assign ((x, MyType(t1)) :: (subst_env s1 a)) e2 in
      (s1 @ s2, t2)
  | _ ->
      raise Type_assignment_failed
;;

