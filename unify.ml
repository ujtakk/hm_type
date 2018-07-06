open Syntax
open Show
open Subst

exception Not_unifiable of string

let disagree ts =
  let rec child p =
    match p with
    | TVar x -> [TVar x]
    | List t -> [t]
    (* | List t -> child t *)
    | Func(s, t) -> [s; t]
    (* | Func(s, t) -> (child s) @ (child t) *)
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

let rec occur v u = match v, u with
  | TVar x, TVar y -> x = y
  | TVar x, List t -> occur v t
  | TVar x, Func(s, t) -> occur v s || occur v t
  | TVar x, _ -> false
  | _, _ -> raise (Not_unifiable "occur: v is not TVar")
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
          | a, b ->
              let sub' = unify_iter [] a b in
              unify_iter (sub' @ subs) init goal
              (* raise (Not_unifiable "v occured in u") *)
          end
      | _ ->
          raise (Not_unifiable "disagree not found")
  in
  unify_iter [] t t'
;;
