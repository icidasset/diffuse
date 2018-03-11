module Settings.State exposing (..)

import List.Extra as List
import Response.Ext exposing (do)
import Settings.Types exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { chosenBackdrop = "7.jpg"
    , fadeInLastBackdrop = True
    , loadedBackdrops = []
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        SetChosenBackdrop filename ->
            (!)
                { model | chosenBackdrop = filename }
                [ do TopLevel.DebounceStoreUserData ]

        SetLoadedBackdrop filename ->
            (!)
                { model
                    | fadeInLastBackdrop = True
                    , loadedBackdrops = loadBackdrop model.loadedBackdrops filename
                }
                []



-- 🔥  ~  Loaded backdrop


loadBackdrop : List String -> String -> List String
loadBackdrop state backdrop =
    case List.last state of
        Just lastBackdrop ->
            if backdrop /= lastBackdrop then
                state ++ [ backdrop ]
            else
                state

        Nothing ->
            [ backdrop ]
