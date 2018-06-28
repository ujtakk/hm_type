type my_id = string

type my_expr = MyVar of my_id
             | MyApply of my_expr * my_expr
             | MyLambda of my_id * my_expr
             | MyDefine of my_id * my_expr * my_expr

let rec show_expr = function
  | MyVar(x) -> "MyVar("^x^")"
  | MyApply(f, x) -> "MyApply("^(show_expr f)^", "^(show_expr x)^")"
  | MyLambda(x, e) -> "MyLambda("^x^", "^(show_expr e)^")"
  | MyDefine(a, b, c) ->
      "MyDefine("^a^", "^(show_expr b)^", "^(show_expr c)^")"
;;

type my_type = MyTVar of my_id
             | MyBool
             | MyList of my_type
             | MyFunc of my_type * my_type

let rec show_type = function
  | MyTVar x -> "MyTVar("^x^")"
  | MyBool -> "MyBool"
  | MyList x -> "MyList("^(show_type x)^")"
  | MyFunc(s, t) -> "MyFunc("^(show_type s)^", "^(show_type t)^")"
;;

type my_scheme = MyType of my_type
               | MyScheme of my_id * my_scheme

let rec show_scheme = function
  | MyType t -> "MyType("^(show_type t)^")"
  | MyScheme(i, s) -> "MyScheme("^i^", "^(show_scheme s)^")"
;;

type my_sub = (my_type * my_type) list

type my_env = (my_id * my_scheme) list

let rec show_env = fun a ->
  let show_func = function (i, s) -> "("^i^", "^(show_scheme s)^")" in
  "["^(String.concat "; " @@ List.map show_func a)^"]"
;;
