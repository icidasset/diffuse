module Conditional exposing (ifThenElse)

-- 🔱


ifThenElse : Bool -> a -> a -> a
ifThenElse bool x y =
    if bool then
        x

    else
        y
