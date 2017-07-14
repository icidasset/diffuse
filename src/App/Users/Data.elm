port module Users.Data exposing (..)

import Json.Encode as Json


port storeFavourites : List Json.Value -> Cmd msg


port storeSources : List Json.Value -> Cmd msg


port storeTracks : List Json.Value -> Cmd msg
