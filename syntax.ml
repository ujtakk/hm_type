type my_id = string

type my_expr = Var of my_id
             | Apply of my_expr * my_expr
             | Lambda of my_id * my_expr
             | Define of my_id * my_expr * my_expr

type my_type = TVar of my_id
             | List of my_type
             | Func of my_type * my_type
             | Bool
             | Int
             | Float

type my_scheme = Type of my_type
               | Scheme of my_id * my_scheme

type my_sub = (my_type * my_type) list

type my_env = (my_id * my_scheme) list
