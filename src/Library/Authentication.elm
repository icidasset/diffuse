module Authentication exposing (EnclosedUserData, HypaethralUserData, Method(..), methodFromString, methodToString)

import Sources



-- 🌳


type Method
    = Local


type alias EnclosedUserData =
    {}


type alias HypaethralUserData =
    { sources : Maybe (List Sources.Source)
    }



-- 🔱


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
