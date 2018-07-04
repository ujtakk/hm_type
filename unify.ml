open Syntax
open Show
open Subst

exception Not_unifiable of string

(*
 * If A is any set of well-formed expressions,
 * we call the set B * the disagreement set of A
 * whenever B is the set of all well-formed subexpressions
 * of the well-formed expressions in A,
 * which begin at the first symbol position at which
 * not all well-formed expressions in A have the same symbol.
 *)
let disagree ts =
  let rec iter diff acc = function
    | [] -> []
    | t :: [] ->
        if diff then
          t :: acc
        else
          []
    | s :: t :: rest ->
        if s = t then
          iter diff (s :: acc) (t :: rest)
        else
          iter true (s :: acc) (t :: rest)
  in
  iter false [] ts |> List.rev
;;

let rec occur v u =
  match v with
  | MyTVar x ->
      begin match u with
      | MyTVar y -> x = y
      | MyBool -> false
      | MyList t -> occur v t
      | MyFunc(s, t) -> occur v s || occur v t
      end
  | _ ->
      raise (Not_unifiable "occur: v is not MyTVar")
;;

let unify t t' : my_sub =
  Printf.printf "%s %s\n" (show_type t) (show_type t');
  let rec unify_iter subs init goal =
    let init' = subst_type subs init in
    let goal' = subst_type subs goal in
    if init' = goal' then
      subs
    else
      match disagree (init' :: goal' :: []) with
      | v :: u :: _ ->
          v |> show_type |> print_endline;
          begin match v with
          | MyTVar x when not (occur v u) ->
              unify_iter ((v, u) :: subs) init goal
          | _ ->
              raise (Not_unifiable "v occured in u")
          end
      | _ ->
          raise (Not_unifiable "disagree not found")
  in
  unify_iter [] t t'
;;
