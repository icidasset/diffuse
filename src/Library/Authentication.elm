module Authentication exposing (Method(..), methodFromString, methodToString)

-- 🌳


type Method
    = Local


methodToString : Method -> String
methodToString method =
    case method of
        Local ->
            "LOCAL"


methodFromString : String -> Maybe Method
methodFromString string =
    case string of
        "LOCAL" ->
            Just Local

        _ ->
            Nothing
