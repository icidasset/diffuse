module Authentication exposing (EnclosedUserData, HypaethralUserData, Method(..), methodFromString, methodToString)

import Sources
import Tracks



-- 🌳


type Method
    = Local


type alias EnclosedUserData =
    {}


type alias HypaethralUserData =
    { favourites : Maybe (List Tracks.Favourite)
    , sources : Maybe (List Sources.Source)
    , tracks : Maybe (List Tracks.Track)
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
