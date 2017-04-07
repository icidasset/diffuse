module Tracks.State exposing (..)

import Tracks.Encoding
import Tracks.Types exposing (..)
import Tracks.Utils exposing (..)
import Types as TopLevel


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { collection = decodeTracks flags
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        AddTracks newTracks ->
            let
                newCollection =
                    model.collection ++ newTracks
            in
                (!)
                    { model | collection = newCollection }
                    [ storeTracks newCollection ]
