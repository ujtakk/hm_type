open Syntax
open Show
open Subst

exception Not_unifiable of string

let disagree ts =
  let child p =
    match p with
    | TVar x -> [TVar x]
    | List t -> [t]
    | Func(s, t) ->
        [s; t]
    | t -> [t]
  in
  let rec iter childs =
    let rec all_same = function
      | [] -> false
      | t :: [] -> true
      | s :: t :: rest ->
          if s = t then all_same (t :: rest) else false
    in
    let pop ll =
      let rec pop_iter heads tails = function
        | [] -> (heads, tails)
        | s :: rest ->
            pop_iter ((List.hd s) :: heads) ((List.tl s) :: tails) rest
      in
      pop_iter [] [] ll
    in
    let (heads, tails) = pop childs in
    if all_same heads then
      iter tails
    else
      heads
  in
  List.map child ts |> iter
;;

let rec occur v u =
  match v with
  | TVar x ->
      begin match u with
      | TVar y -> x = y
      | List t -> occur v t
      | Func(s, t) -> occur v s || occur v t
      | _ -> false
      end
  | _ ->
      raise (Not_unifiable "occur: v is not TVar")
;;

let unify t t' : my_sub =
  let rec unify_iter subs init goal =
    let init' = subst_type subs init in
    let goal' = subst_type subs goal in
    if init' = goal' then
      subs
    else
      match disagree (init' :: goal' :: []) with
      | v :: u :: _ ->
          (* TODO: more complete way of matching *)
          begin match v, u with
          | TVar x, _ when not (occur v u) ->
              unify_iter ((v, u) :: subs) init goal
          | _, TVar x when not (occur u v) ->
              unify_iter ((u, v) :: subs) init goal
          | _ ->
              raise (Not_unifiable "v occured in u")
          end
      | _ ->
          raise (Not_unifiable "disagree not found")
  in
  unify_iter [] t t'
;;
