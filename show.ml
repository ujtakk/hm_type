open Syntax

let verbose = false;;

let rec show_expr_verbose = function
  | MyVar(x) -> "MyVar("^x^")"
  | MyApply(f, x) -> "MyApply("^(show_expr_verbose f)^", "^(show_expr_verbose x)^")"
  | MyLambda(x, e) -> "MyLambda("^x^", "^(show_expr_verbose e)^")"
  | MyDefine(a, b, c) ->
      "MyDefine("^a^", "^(show_expr_verbose b)^", "^(show_expr_verbose c)^")"
;;

let rec show_expr_simple = function
  | MyVar(x) -> x
  | MyApply(f, x) -> (show_expr_simple f)^" "^(show_expr_simple x)
  | MyLambda(x, e) -> "\\"^x^".("^(show_expr_simple e)^")"
  | MyDefine(a, b, c) ->
      "let "^a^" = "^(show_expr_simple b)^" in "^(show_expr_simple c)
;;

let show_expr = if verbose then show_expr_verbose else show_expr_simple

let rec show_type_verbose = function
  | MyTVar x -> "MyTVar("^x^")"
  | MyBool -> "MyBool"
  | MyList t -> "MyList("^(show_type_verbose t)^")"
  | MyFunc(s, t) -> "MyFunc("^(show_type_verbose s)^", "^(show_type_verbose t)^")"
;;

let rec show_type_simple = function
  | MyTVar x -> x
  | MyBool -> "bool"
  | MyList t -> (show_type_simple t)^" list"
  | MyFunc(s, t) -> (show_type_simple s)^" -> "^(show_type_simple t)
;;

let show_type = if verbose then show_type_verbose else show_type_simple

let rec show_scheme_verbose = function
  | MyType t -> "MyType("^(show_type t)^")"
  | MyScheme(i, s) -> "MyScheme("^i^", "^(show_scheme_verbose s)^")"
;;

let rec show_scheme_simple = function
  | MyType t -> show_type t
  | MyScheme(i, s) -> "\\"^i^".("^(show_scheme_simple s)^")"
;;

let show_scheme = if verbose then show_scheme_verbose else show_scheme_simple

let rec show_env_verbose = fun a ->
  let show_func = function (i, s) -> "("^i^", "^(show_scheme s)^")" in
  "["^(String.concat ", " @@ List.map show_func a)^"]"
;;

let rec show_env_simple = fun a ->
  let show_func = function (i, s) -> i^" : "^(show_scheme s) in
  String.concat ", " @@ List.map show_func a
;;

let show_env = if verbose then show_env_verbose else show_env_simple
