open Syntax
open Subst

exception Not_unifiable

let disagree t =
;;

(* TODO *)
let unify t t' : my_sub =
  let rec unify_iter subs init goal =
    if subst_type subs init == goal then
      subs
    else
      let v :: u :: _ = disagree t in
      if v is variable and not occur in u then
        unify_iter (what) init goal
      else
        raise Not_unifiable
  in
  unify_iter [] t t'
;;
