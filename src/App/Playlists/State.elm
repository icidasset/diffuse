module Playlists.State exposing (..)

import Playlists.Types exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { collection = []
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        SetCollection col ->
            (!) { model | collection = col } []
