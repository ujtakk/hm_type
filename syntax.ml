type my_id = string

type my_expr = MyVar of my_id
             | MyApply of my_expr * my_expr
             | MyLambda of my_id * my_expr
             | MyDefine of my_id * my_expr * my_expr

type my_type = MyTVar of my_id
             | MyBool
             | MyList of my_type
             | MyFunc of my_type * my_type

type my_scheme = MyType of my_type
               | MyScheme of my_id * my_scheme

type my_sub = (my_type * my_type) list

type my_env = (my_id * my_scheme) list
