port module Users.Ports exposing (..)

import Json.Encode as Json
import Types exposing (User)


-- 💡


port authenticate : String -> Cmd msg


port deauthenticate : () -> Cmd msg


port storeFavourites : List Json.Value -> Cmd msg


port storeSources : List Json.Value -> Cmd msg


port storeTracks : List Json.Value -> Cmd msg



-- 🚽


port signIn : (User -> msg) -> Sub msg
