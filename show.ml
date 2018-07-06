open Syntax

let verbose = false;;

let rec show_expr_verbose = function
  | Var(x) -> "Var("^x^")"
  | Apply(f, x) -> "Apply("^(show_expr_verbose f)^", "^(show_expr_verbose x)^")"
  | Lambda(x, e) -> "Lambda("^x^", "^(show_expr_verbose e)^")"
  | Define(a, b, c) ->
      "Define("^a^", "^(show_expr_verbose b)^", "^(show_expr_verbose c)^")"
;;

let rec show_expr_simple = function
  | Var(x) -> x
  | Apply(f, x) -> "("^(show_expr_simple f)^" "^(show_expr_simple x)^")"
  | Lambda(x, e) -> "\\"^x^".("^(show_expr_simple e)^")"
  | Define(a, b, c) ->
      "let "^a^" = "^(show_expr_simple b)^" in "^(show_expr_simple c)
;;

let show_expr = if verbose then show_expr_verbose else show_expr_simple

let rec show_type_verbose = function
  | TVar x -> "TVar("^x^")"
  | List t -> "List("^(show_type_verbose t)^")"
  | Func(s, t) -> "Func("^(show_type_verbose s)^", "^(show_type_verbose t)^")"
  | Bool -> "Bool"
  | Int -> "Int"
  | Float -> "Float"
;;

let rec show_type_simple = function
  | TVar x -> x
  | List t -> (show_type_simple t)^" list"
  | Func(s, t) -> "("^(show_type_simple s)^" -> "^(show_type_simple t)^")"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
;;

let show_type = if verbose then show_type_verbose else show_type_simple

let rec show_scheme_verbose = function
  | Type t -> "Type("^(show_type t)^")"
  | Scheme(i, s) -> "Scheme("^i^", "^(show_scheme_verbose s)^")"
;;

let rec show_scheme_simple = function
  | Type t -> show_type t
  | Scheme(i, s) -> "\\"^i^".("^(show_scheme_simple s)^")"
;;

let show_scheme = if verbose then show_scheme_verbose else show_scheme_simple

let show_sub_verbose = fun a ->
  let show_func = fun (s, t) -> "("^(show_type s)^", "^(show_type t)^")" in
  "["^(String.concat "; " @@ List.map show_func a)^"]"
;;

let show_sub_simple = fun a ->
  let show_func = fun (s, t) -> (show_type s)^" => "^(show_type t) in
  "["^(String.concat ", " @@ List.map show_func a)^"]"
;;

let show_sub = if verbose then show_sub_verbose else show_sub_simple

let show_env_verbose = fun a ->
  let show_func = function (i, s) -> "("^i^", "^(show_scheme s)^")" in
  "["^(String.concat "; " @@ List.map show_func a)^"]"
;;

let show_env_simple = fun a ->
  let show_func = function (i, s) -> i^" : "^(show_scheme s) in
  String.concat ", " @@ List.map show_func a
;;

let show_env = if verbose then show_env_verbose else show_env_simple
