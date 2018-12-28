module Conditional exposing (ifThenElse)

-- 🔱


ifThenElse : Bool -> a -> a -> a
ifThenElse bool x y =
    case bool of
        True ->
            x

        False ->
            y
