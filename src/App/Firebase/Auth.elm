port module Firebase.Auth exposing (..)


type alias User =
    { displayName : String
    , email : String
    , photoURL : Maybe String
    , refreshToken : String
    , uid : String
    }


port authenticate : () -> Cmd msg
